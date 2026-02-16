package analyzer

import (
	"fmt"
	"path/filepath"
	"regexp"
	"strings"

	"github.com/joeabbey/fault/pkg/git"
)

// MigrationAnalyzer detects dangerous database migration operations in diff hunks.
type MigrationAnalyzer struct{}

// NewMigrationAnalyzer creates a new migration safety analyzer.
func NewMigrationAnalyzer() *MigrationAnalyzer {
	return &MigrationAnalyzer{}
}

// Name returns the analyzer name.
func (a *MigrationAnalyzer) Name() string {
	return "migration"
}

// Analyze scans changed migration files for destructive or risky operations.
func (a *MigrationAnalyzer) Analyze(ctx *AnalysisContext) ([]Issue, error) {
	issues := make([]Issue, 0)

	if ctx.Diff == nil || len(ctx.Diff.Files) == 0 {
		return issues, nil
	}

	// Build set of all file paths in the diff for down-migration check
	diffPaths := make(map[string]bool)
	for _, fd := range ctx.Diff.Files {
		diffPaths[fd.Path] = true
	}

	for _, fileDiff := range ctx.Diff.Files {
		if fileDiff.Status == "deleted" || fileDiff.IsBinary {
			continue
		}
		if !isMigrationFile(fileDiff.Path) {
			continue
		}

		ext := strings.ToLower(filepath.Ext(fileDiff.Path))
		switch ext {
		case ".sql":
			issues = append(issues, checkSQLMigration(fileDiff)...)
			issues = append(issues, checkSQLTransactionWrapper(fileDiff)...)
		case ".rb":
			issues = append(issues, checkRailsMigration(fileDiff)...)
		case ".py":
			issues = append(issues, checkAlembicMigration(fileDiff)...)
		case ".js", ".ts":
			issues = append(issues, checkKnexMigration(fileDiff)...)
		}

		// Check for missing down migration
		issues = append(issues, checkMissingDownMigration(fileDiff, diffPaths)...)
	}

	return issues, nil
}

// isMigrationFile returns true if the file path looks like a database migration.
func isMigrationFile(path string) bool {
	lower := strings.ToLower(path)

	// golang-migrate up/down files
	if strings.HasSuffix(lower, ".up.sql") || strings.HasSuffix(lower, ".down.sql") {
		return true
	}

	// Path contains "migration" with a SQL extension
	if strings.Contains(lower, "migration") && strings.HasSuffix(lower, ".sql") {
		return true
	}

	// Common migration directory patterns
	patterns := []string{
		"migrations/",
		"migrate/",
		"db/migrate/",
		"alembic/versions/",
		"prisma/migrations/",
		"flyway/",
		"liquibase/",
		"knex/migrations/",
	}
	for _, pattern := range patterns {
		if strings.Contains(lower, pattern) {
			return true
		}
	}

	return false
}

// --- SQL migration checks ---

var sqlChecks = []struct {
	id         string
	pattern    *regexp.Regexp
	severity   Severity
	message    string
	suggestion string
}{
	{
		id:         "drop-table",
		pattern:    regexp.MustCompile(`(?i)\bDROP\s+TABLE\b`),
		severity:   SeverityError,
		message:    "DROP TABLE causes irreversible data loss",
		suggestion: "Consider renaming the table or creating a backup before dropping",
	},
	{
		id:         "drop-database",
		pattern:    regexp.MustCompile(`(?i)\bDROP\s+DATABASE\b`),
		severity:   SeverityError,
		message:    "DROP DATABASE is catastrophic and irreversible",
		suggestion: "This should almost never appear in a migration file",
	},
	{
		id:         "truncate-table",
		pattern:    regexp.MustCompile(`(?i)\bTRUNCATE\s+TABLE\b`),
		severity:   SeverityError,
		message:    "TRUNCATE TABLE causes irreversible data loss",
		suggestion: "Consider using DELETE with a WHERE clause if you need to remove specific rows",
	},
	{
		id:         "drop-column",
		pattern:    regexp.MustCompile(`(?i)\bALTER\s+TABLE\b.*\bDROP\s+COLUMN\b`),
		severity:   SeverityError,
		message:    "DROP COLUMN causes irreversible data loss",
		suggestion: "Consider marking the column as deprecated and removing it in a later migration",
	},
	{
		id:         "drop-index",
		pattern:    regexp.MustCompile(`(?i)\bDROP\s+INDEX\b`),
		severity:   SeverityInfo,
		message:    "DROP INDEX may cause performance degradation",
		suggestion: "Verify that queries previously using this index will still perform acceptably",
	},
	{
		id:         "rename-column",
		pattern:    regexp.MustCompile(`(?i)\bALTER\s+TABLE\b.*\bRENAME\s+COLUMN\b`),
		severity:   SeverityWarning,
		message:    "Renaming a column breaks dependent application code",
		suggestion: "Deploy application changes first, then rename the column, or use a two-phase migration",
	},
	{
		id:         "change-type",
		pattern:    regexp.MustCompile(`(?i)\bALTER\s+TABLE\b.*\bALTER\s+COLUMN\b.*\bTYPE\b`),
		severity:   SeverityWarning,
		message:    "Changing column type may cause data truncation or conversion errors",
		suggestion: "Test the type change on a copy of production data first",
	},
	{
		id:         "change-type-modify",
		pattern:    regexp.MustCompile(`(?i)\bALTER\s+TABLE\b.*\bMODIFY\s+COLUMN\b`),
		severity:   SeverityWarning,
		message:    "MODIFY COLUMN may cause data truncation or conversion errors",
		suggestion: "Test the column modification on a copy of production data first",
	},
	{
		id:         "drop-not-null",
		pattern:    regexp.MustCompile(`(?i)\bALTER\s+TABLE\b.*\bDROP\s+NOT\s+NULL\b`),
		severity:   SeverityWarning,
		message:    "Removing NOT NULL constraint weakens data integrity",
		suggestion: "Ensure application code handles NULL values for this column",
	},
	{
		id:         "set-null",
		pattern:    regexp.MustCompile(`(?i)\bALTER\s+TABLE\b.*\bSET\s+NULL\b`),
		severity:   SeverityWarning,
		message:    "SET NULL weakens data integrity by allowing NULL values",
		suggestion: "Ensure application code handles NULL values for this column",
	},
	{
		id:         "add-not-null",
		pattern:    regexp.MustCompile(`(?i)\bALTER\s+TABLE\b.*\bSET\s+NOT\s+NULL\b`),
		severity:   SeverityWarning,
		message:    "Adding NOT NULL constraint fails if existing rows contain NULL values",
		suggestion: "Backfill NULL values with a DEFAULT before adding the NOT NULL constraint",
	},
	{
		id:         "drop-constraint",
		pattern:    regexp.MustCompile(`(?i)\bALTER\s+TABLE\b.*\bDROP\s+CONSTRAINT\b`),
		severity:   SeverityWarning,
		message:    "Dropping a constraint weakens data integrity",
		suggestion: "Verify that the constraint is no longer needed and won't cause data corruption",
	},
	{
		id:         "drop-foreign-key",
		pattern:    regexp.MustCompile(`(?i)\bALTER\s+TABLE\b.*\bDROP\s+FOREIGN\s+KEY\b`),
		severity:   SeverityWarning,
		message:    "Dropping a foreign key removes referential integrity",
		suggestion: "Ensure application logic enforces the relationship if the FK is removed",
	},
}

// checkSQLMigration scans added lines in a SQL migration file for destructive operations.
func checkSQLMigration(fd git.FileDiff) []Issue {
	issues := make([]Issue, 0)

	for _, hunk := range fd.Hunks {
		for _, line := range hunk.Lines {
			if line.Type != "added" {
				continue
			}
			content := line.Content
			for _, check := range sqlChecks {
				if check.pattern.MatchString(content) {
					issues = append(issues, Issue{
						ID:         fmt.Sprintf("migration-%s-%s-%d", check.id, fd.Path, line.NewNum),
						FixID:      "migration-" + check.id,
						Severity:   check.severity,
						Category:   "migration",
						File:       fd.Path,
						Line:       line.NewNum,
						Message:    check.message,
						Suggestion: check.suggestion,
					})
				}
			}
		}
	}

	return issues
}

// checkSQLTransactionWrapper checks if a SQL migration lacks a transaction wrapper.
func checkSQLTransactionWrapper(fd git.FileDiff) []Issue {
	issues := make([]Issue, 0)

	hasTransaction := false
	hasContent := false

	for _, hunk := range fd.Hunks {
		for _, line := range hunk.Lines {
			if line.Type == "removed" {
				continue
			}
			upper := strings.ToUpper(strings.TrimSpace(line.Content))
			if upper == "" || strings.HasPrefix(upper, "--") {
				continue
			}
			hasContent = true
			if strings.Contains(upper, "BEGIN") || strings.Contains(upper, "START TRANSACTION") {
				hasTransaction = true
			}
		}
	}

	if hasContent && !hasTransaction {
		issues = append(issues, Issue{
			ID:         fmt.Sprintf("migration-no-transaction-%s", fd.Path),
			FixID:      "migration-no-transaction",
			Severity:   SeverityWarning,
			Category:   "migration",
			File:       fd.Path,
			Line:       1,
			Message:    "Migration lacks a transaction wrapper (BEGIN/COMMIT)",
			Suggestion: "Wrap migration statements in BEGIN/COMMIT to ensure atomicity",
		})
	}

	return issues
}

// --- Rails migration checks ---

var railsChecks = []struct {
	id         string
	pattern    *regexp.Regexp
	severity   Severity
	message    string
	suggestion string
}{
	{
		id:         "remove-column",
		pattern:    regexp.MustCompile(`\bremove_column\b`),
		severity:   SeverityError,
		message:    "remove_column causes irreversible data loss",
		suggestion: "Consider using a two-phase migration: ignore the column first, then remove it",
	},
	{
		id:         "drop-table",
		pattern:    regexp.MustCompile(`\bdrop_table\b`),
		severity:   SeverityError,
		message:    "drop_table causes irreversible data loss",
		suggestion: "Consider renaming the table or creating a backup before dropping",
	},
	{
		id:         "rename-column",
		pattern:    regexp.MustCompile(`\brename_column\b`),
		severity:   SeverityWarning,
		message:    "rename_column breaks dependent application code",
		suggestion: "Deploy application changes first, then rename the column",
	},
	{
		id:         "change-column",
		pattern:    regexp.MustCompile(`\bchange_column\b`),
		severity:   SeverityWarning,
		message:    "change_column may cause data truncation or conversion errors",
		suggestion: "Test the column change on a copy of production data first",
	},
}

// checkRailsMigration scans added lines in a Rails migration file.
func checkRailsMigration(fd git.FileDiff) []Issue {
	issues := make([]Issue, 0)

	for _, hunk := range fd.Hunks {
		for _, line := range hunk.Lines {
			if line.Type != "added" {
				continue
			}
			for _, check := range railsChecks {
				if check.pattern.MatchString(line.Content) {
					issues = append(issues, Issue{
						ID:         fmt.Sprintf("migration-%s-%s-%d", check.id, fd.Path, line.NewNum),
						FixID:      "migration-" + check.id,
						Severity:   check.severity,
						Category:   "migration",
						File:       fd.Path,
						Line:       line.NewNum,
						Message:    check.message,
						Suggestion: check.suggestion,
					})
				}
			}
		}
	}

	return issues
}

// --- Alembic/SQLAlchemy migration checks ---

var alembicChecks = []struct {
	id         string
	pattern    *regexp.Regexp
	severity   Severity
	message    string
	suggestion string
}{
	{
		id:         "drop-column",
		pattern:    regexp.MustCompile(`\bop\.drop_column\s*\(`),
		severity:   SeverityError,
		message:    "op.drop_column causes irreversible data loss",
		suggestion: "Consider a two-phase migration: stop using the column first, then drop it",
	},
	{
		id:         "drop-table",
		pattern:    regexp.MustCompile(`\bop\.drop_table\s*\(`),
		severity:   SeverityError,
		message:    "op.drop_table causes irreversible data loss",
		suggestion: "Consider renaming the table or creating a backup before dropping",
	},
	{
		id:         "alter-column-type",
		pattern:    regexp.MustCompile(`\bop\.alter_column\s*\(.*type_=`),
		severity:   SeverityWarning,
		message:    "op.alter_column with type change may cause data truncation",
		suggestion: "Test the type change on a copy of production data first",
	},
}

// checkAlembicMigration scans added lines in an Alembic migration file.
func checkAlembicMigration(fd git.FileDiff) []Issue {
	issues := make([]Issue, 0)

	for _, hunk := range fd.Hunks {
		for _, line := range hunk.Lines {
			if line.Type != "added" {
				continue
			}
			for _, check := range alembicChecks {
				if check.pattern.MatchString(line.Content) {
					issues = append(issues, Issue{
						ID:         fmt.Sprintf("migration-%s-%s-%d", check.id, fd.Path, line.NewNum),
						FixID:      "migration-" + check.id,
						Severity:   check.severity,
						Category:   "migration",
						File:       fd.Path,
						Line:       line.NewNum,
						Message:    check.message,
						Suggestion: check.suggestion,
					})
				}
			}
		}
	}

	return issues
}

// --- Knex migration checks ---

var knexChecks = []struct {
	id         string
	pattern    *regexp.Regexp
	severity   Severity
	message    string
	suggestion string
}{
	{
		id:         "drop-table",
		pattern:    regexp.MustCompile(`\.dropTable\s*\(`),
		severity:   SeverityError,
		message:    "dropTable causes irreversible data loss",
		suggestion: "Consider renaming the table or creating a backup before dropping",
	},
	{
		id:         "drop-column",
		pattern:    regexp.MustCompile(`\.dropColumn\s*\(`),
		severity:   SeverityError,
		message:    "dropColumn causes irreversible data loss",
		suggestion: "Consider a two-phase migration: stop using the column first, then drop it",
	},
	{
		id:         "rename-column",
		pattern:    regexp.MustCompile(`\.renameColumn\s*\(`),
		severity:   SeverityWarning,
		message:    "renameColumn breaks dependent application code",
		suggestion: "Deploy application changes first, then rename the column",
	},
}

// checkKnexMigration scans added lines in a Knex migration file.
func checkKnexMigration(fd git.FileDiff) []Issue {
	issues := make([]Issue, 0)

	for _, hunk := range fd.Hunks {
		for _, line := range hunk.Lines {
			if line.Type != "added" {
				continue
			}
			for _, check := range knexChecks {
				if check.pattern.MatchString(line.Content) {
					issues = append(issues, Issue{
						ID:         fmt.Sprintf("migration-%s-%s-%d", check.id, fd.Path, line.NewNum),
						FixID:      "migration-" + check.id,
						Severity:   check.severity,
						Category:   "migration",
						File:       fd.Path,
						Line:       line.NewNum,
						Message:    check.message,
						Suggestion: check.suggestion,
					})
				}
			}
		}
	}

	return issues
}

// --- Missing down migration check ---

// checkMissingDownMigration checks if a .up.sql file has a corresponding .down.sql in the diff.
func checkMissingDownMigration(fd git.FileDiff, diffPaths map[string]bool) []Issue {
	issues := make([]Issue, 0)

	lower := strings.ToLower(fd.Path)
	if !strings.HasSuffix(lower, ".up.sql") {
		return issues
	}

	// Derive the expected down migration path
	downPath := fd.Path[:len(fd.Path)-len(".up.sql")] + ".down.sql"

	if !diffPaths[downPath] {
		issues = append(issues, Issue{
			ID:         fmt.Sprintf("migration-no-down-%s", fd.Path),
			FixID:      "migration-no-down",
			Severity:   SeverityInfo,
			Category:   "migration",
			File:       fd.Path,
			Line:       1,
			Message:    "Up migration has no corresponding down migration",
			Suggestion: fmt.Sprintf("Create %s to allow rolling back this migration", filepath.Base(downPath)),
		})
	}

	return issues
}
