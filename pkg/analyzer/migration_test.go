package analyzer

import (
	"testing"

	"github.com/joeabbey/fault/pkg/config"
	"github.com/joeabbey/fault/pkg/git"
	"github.com/joeabbey/fault/pkg/parser"
)

func TestMigrationAnalyzerName(t *testing.T) {
	a := NewMigrationAnalyzer()
	if a.Name() != "migration" {
		t.Errorf("expected name %q, got %q", "migration", a.Name())
	}
}

func TestMigrationAnalyzerEmptyContext(t *testing.T) {
	a := NewMigrationAnalyzer()
	ctx := &AnalysisContext{
		RepoPath:    "/repo",
		Diff:        &git.Diff{Files: make([]git.FileDiff, 0)},
		ParsedFiles: make(map[string]*parser.ParsedFile),
		Config:      config.DefaultConfig(),
	}

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(issues) != 0 {
		t.Errorf("expected 0 issues, got %d", len(issues))
	}
}

func TestMigrationSQLDropTable(t *testing.T) {
	a := NewMigrationAnalyzer()
	ctx := &AnalysisContext{
		RepoPath: "/repo",
		Diff: &git.Diff{
			Files: []git.FileDiff{
				{
					Path:   "db/migrations/000003_remove_old.up.sql",
					Status: "added",
					Hunks: []git.Hunk{
						{
							NewStart: 1,
							NewCount: 3,
							Lines: []git.Line{
								{Type: "added", Content: "BEGIN;", NewNum: 1},
								{Type: "added", Content: "DROP TABLE old_users;", NewNum: 2},
								{Type: "added", Content: "COMMIT;", NewNum: 3},
							},
						},
					},
				},
				{
					Path:   "db/migrations/000003_remove_old.down.sql",
					Status: "added",
					Hunks:  []git.Hunk{},
				},
			},
			Mode: "staged",
		},
		ParsedFiles: make(map[string]*parser.ParsedFile),
		Config:      config.DefaultConfig(),
	}

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := false
	for _, issue := range issues {
		if issue.Category == "migration" && issue.Severity == SeverityError &&
			issue.FixID == "migration-drop-table" {
			found = true
			if issue.Line != 2 {
				t.Errorf("expected issue on line 2, got line %d", issue.Line)
			}
			break
		}
	}
	if !found {
		t.Error("expected error-severity issue for DROP TABLE in migration")
	}
}

func TestMigrationSQLDropColumn(t *testing.T) {
	a := NewMigrationAnalyzer()
	ctx := &AnalysisContext{
		RepoPath: "/repo",
		Diff: &git.Diff{
			Files: []git.FileDiff{
				{
					Path:   "db/migrations/000004_drop_email.up.sql",
					Status: "added",
					Hunks: []git.Hunk{
						{
							NewStart: 1,
							NewCount: 3,
							Lines: []git.Line{
								{Type: "added", Content: "BEGIN;", NewNum: 1},
								{Type: "added", Content: "ALTER TABLE users DROP COLUMN email;", NewNum: 2},
								{Type: "added", Content: "COMMIT;", NewNum: 3},
							},
						},
					},
				},
				{
					Path:   "db/migrations/000004_drop_email.down.sql",
					Status: "added",
					Hunks:  []git.Hunk{},
				},
			},
			Mode: "staged",
		},
		ParsedFiles: make(map[string]*parser.ParsedFile),
		Config:      config.DefaultConfig(),
	}

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := false
	for _, issue := range issues {
		if issue.Category == "migration" && issue.Severity == SeverityError &&
			issue.FixID == "migration-drop-column" {
			found = true
			break
		}
	}
	if !found {
		t.Error("expected error-severity issue for DROP COLUMN in migration")
	}
}

func TestMigrationSQLAlterColumnType(t *testing.T) {
	a := NewMigrationAnalyzer()
	ctx := &AnalysisContext{
		RepoPath: "/repo",
		Diff: &git.Diff{
			Files: []git.FileDiff{
				{
					Path:   "db/migrations/000005_change_type.up.sql",
					Status: "added",
					Hunks: []git.Hunk{
						{
							NewStart: 1,
							NewCount: 3,
							Lines: []git.Line{
								{Type: "added", Content: "BEGIN;", NewNum: 1},
								{Type: "added", Content: "ALTER TABLE users ALTER COLUMN name TYPE varchar(50);", NewNum: 2},
								{Type: "added", Content: "COMMIT;", NewNum: 3},
							},
						},
					},
				},
				{
					Path:   "db/migrations/000005_change_type.down.sql",
					Status: "added",
					Hunks:  []git.Hunk{},
				},
			},
			Mode: "staged",
		},
		ParsedFiles: make(map[string]*parser.ParsedFile),
		Config:      config.DefaultConfig(),
	}

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := false
	for _, issue := range issues {
		if issue.Category == "migration" && issue.Severity == SeverityWarning &&
			issue.FixID == "migration-change-type" {
			found = true
			break
		}
	}
	if !found {
		t.Error("expected warning-severity issue for ALTER COLUMN TYPE in migration")
	}
}

func TestMigrationNonMigrationSQLNotFlagged(t *testing.T) {
	a := NewMigrationAnalyzer()
	ctx := &AnalysisContext{
		RepoPath: "/repo",
		Diff: &git.Diff{
			Files: []git.FileDiff{
				{
					Path:   "scripts/seed_data.sql",
					Status: "added",
					Hunks: []git.Hunk{
						{
							NewStart: 1,
							NewCount: 1,
							Lines: []git.Line{
								{Type: "added", Content: "DROP TABLE IF EXISTS temp_data;", NewNum: 1},
							},
						},
					},
				},
			},
			Mode: "staged",
		},
		ParsedFiles: make(map[string]*parser.ParsedFile),
		Config:      config.DefaultConfig(),
	}

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	migrationIssues := make([]Issue, 0)
	for _, issue := range issues {
		if issue.Category == "migration" {
			migrationIssues = append(migrationIssues, issue)
		}
	}
	if len(migrationIssues) != 0 {
		t.Errorf("expected 0 migration issues for non-migration SQL file, got %d: %v", len(migrationIssues), migrationIssues)
	}
}

func TestMigrationRailsRemoveColumn(t *testing.T) {
	a := NewMigrationAnalyzer()
	ctx := &AnalysisContext{
		RepoPath: "/repo",
		Diff: &git.Diff{
			Files: []git.FileDiff{
				{
					Path:   "db/migrate/20240101_remove_name.rb",
					Status: "added",
					Hunks: []git.Hunk{
						{
							NewStart: 1,
							NewCount: 5,
							Lines: []git.Line{
								{Type: "added", Content: "class RemoveName < ActiveRecord::Migration[7.0]", NewNum: 1},
								{Type: "added", Content: "  def change", NewNum: 2},
								{Type: "added", Content: "    remove_column :users, :name, :string", NewNum: 3},
								{Type: "added", Content: "  end", NewNum: 4},
								{Type: "added", Content: "end", NewNum: 5},
							},
						},
					},
				},
			},
			Mode: "staged",
		},
		ParsedFiles: make(map[string]*parser.ParsedFile),
		Config:      config.DefaultConfig(),
	}

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := false
	for _, issue := range issues {
		if issue.Category == "migration" && issue.Severity == SeverityError &&
			issue.FixID == "migration-remove-column" {
			found = true
			break
		}
	}
	if !found {
		t.Error("expected error-severity issue for Rails remove_column")
	}
}

func TestMigrationAlembicDropTable(t *testing.T) {
	a := NewMigrationAnalyzer()
	ctx := &AnalysisContext{
		RepoPath: "/repo",
		Diff: &git.Diff{
			Files: []git.FileDiff{
				{
					Path:   "alembic/versions/abc123_drop_users.py",
					Status: "added",
					Hunks: []git.Hunk{
						{
							NewStart: 1,
							NewCount: 5,
							Lines: []git.Line{
								{Type: "added", Content: "def upgrade():", NewNum: 1},
								{Type: "added", Content: "    op.drop_table('users')", NewNum: 2},
								{Type: "added", Content: "", NewNum: 3},
								{Type: "added", Content: "def downgrade():", NewNum: 4},
								{Type: "added", Content: "    pass", NewNum: 5},
							},
						},
					},
				},
			},
			Mode: "staged",
		},
		ParsedFiles: make(map[string]*parser.ParsedFile),
		Config:      config.DefaultConfig(),
	}

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := false
	for _, issue := range issues {
		if issue.Category == "migration" && issue.Severity == SeverityError &&
			issue.FixID == "migration-drop-table" {
			found = true
			break
		}
	}
	if !found {
		t.Error("expected error-severity issue for Alembic op.drop_table")
	}
}

func TestMigrationKnexDropTable(t *testing.T) {
	a := NewMigrationAnalyzer()
	ctx := &AnalysisContext{
		RepoPath: "/repo",
		Diff: &git.Diff{
			Files: []git.FileDiff{
				{
					Path:   "knex/migrations/20240101_drop_users.js",
					Status: "added",
					Hunks: []git.Hunk{
						{
							NewStart: 1,
							NewCount: 5,
							Lines: []git.Line{
								{Type: "added", Content: "exports.up = function(knex) {", NewNum: 1},
								{Type: "added", Content: "  return knex.schema.dropTable('users');", NewNum: 2},
								{Type: "added", Content: "};", NewNum: 3},
								{Type: "added", Content: "exports.down = function(knex) {", NewNum: 4},
								{Type: "added", Content: "};", NewNum: 5},
							},
						},
					},
				},
			},
			Mode: "staged",
		},
		ParsedFiles: make(map[string]*parser.ParsedFile),
		Config:      config.DefaultConfig(),
	}

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := false
	for _, issue := range issues {
		if issue.Category == "migration" && issue.Severity == SeverityError &&
			issue.FixID == "migration-drop-table" {
			found = true
			break
		}
	}
	if !found {
		t.Error("expected error-severity issue for Knex dropTable")
	}
}

func TestMigrationMissingDownMigration(t *testing.T) {
	a := NewMigrationAnalyzer()
	ctx := &AnalysisContext{
		RepoPath: "/repo",
		Diff: &git.Diff{
			Files: []git.FileDiff{
				{
					Path:   "db/migrations/000006_add_column.up.sql",
					Status: "added",
					Hunks: []git.Hunk{
						{
							NewStart: 1,
							NewCount: 3,
							Lines: []git.Line{
								{Type: "added", Content: "BEGIN;", NewNum: 1},
								{Type: "added", Content: "ALTER TABLE users ADD COLUMN phone VARCHAR(20);", NewNum: 2},
								{Type: "added", Content: "COMMIT;", NewNum: 3},
							},
						},
					},
				},
				// No corresponding .down.sql file in the diff
			},
			Mode: "staged",
		},
		ParsedFiles: make(map[string]*parser.ParsedFile),
		Config:      config.DefaultConfig(),
	}

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := false
	for _, issue := range issues {
		if issue.Category == "migration" && issue.Severity == SeverityInfo &&
			issue.FixID == "migration-no-down" {
			found = true
			break
		}
	}
	if !found {
		t.Error("expected info-severity issue for missing down migration")
	}
}

func TestMigrationSafeOperationsPassClean(t *testing.T) {
	a := NewMigrationAnalyzer()
	ctx := &AnalysisContext{
		RepoPath: "/repo",
		Diff: &git.Diff{
			Files: []git.FileDiff{
				{
					Path:   "db/migrations/000007_add_index.up.sql",
					Status: "added",
					Hunks: []git.Hunk{
						{
							NewStart: 1,
							NewCount: 5,
							Lines: []git.Line{
								{Type: "added", Content: "BEGIN;", NewNum: 1},
								{Type: "added", Content: "ALTER TABLE users ADD COLUMN phone VARCHAR(20);", NewNum: 2},
								{Type: "added", Content: "CREATE INDEX idx_users_phone ON users(phone);", NewNum: 3},
								{Type: "added", Content: "CREATE TABLE audit_log (id SERIAL PRIMARY KEY, action TEXT);", NewNum: 4},
								{Type: "added", Content: "COMMIT;", NewNum: 5},
							},
						},
					},
				},
				{
					Path:   "db/migrations/000007_add_index.down.sql",
					Status: "added",
					Hunks: []git.Hunk{
						{
							NewStart: 1,
							NewCount: 3,
							Lines: []git.Line{
								{Type: "added", Content: "BEGIN;", NewNum: 1},
								{Type: "added", Content: "ALTER TABLE users DROP COLUMN phone;", NewNum: 2},
								{Type: "added", Content: "COMMIT;", NewNum: 3},
							},
						},
					},
				},
			},
			Mode: "staged",
		},
		ParsedFiles: make(map[string]*parser.ParsedFile),
		Config:      config.DefaultConfig(),
	}

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	// The up migration should only have safe operations (ADD COLUMN, CREATE INDEX, CREATE TABLE).
	// The down migration has DROP COLUMN which is expected — we only care about the up migration being clean.
	upIssues := make([]Issue, 0)
	for _, issue := range issues {
		if issue.Category == "migration" && issue.File == "db/migrations/000007_add_index.up.sql" {
			upIssues = append(upIssues, issue)
		}
	}
	if len(upIssues) != 0 {
		t.Errorf("expected 0 issues for safe up migration, got %d: %v", len(upIssues), upIssues)
	}
}

func TestMigrationNoTransactionWarning(t *testing.T) {
	a := NewMigrationAnalyzer()
	ctx := &AnalysisContext{
		RepoPath: "/repo",
		Diff: &git.Diff{
			Files: []git.FileDiff{
				{
					Path:   "db/migrations/000008_no_tx.up.sql",
					Status: "added",
					Hunks: []git.Hunk{
						{
							NewStart: 1,
							NewCount: 1,
							Lines: []git.Line{
								{Type: "added", Content: "ALTER TABLE users ADD COLUMN age INT;", NewNum: 1},
							},
						},
					},
				},
				{
					Path:   "db/migrations/000008_no_tx.down.sql",
					Status: "added",
					Hunks:  []git.Hunk{},
				},
			},
			Mode: "staged",
		},
		ParsedFiles: make(map[string]*parser.ParsedFile),
		Config:      config.DefaultConfig(),
	}

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := false
	for _, issue := range issues {
		if issue.Category == "migration" && issue.Severity == SeverityWarning &&
			issue.FixID == "migration-no-transaction" {
			found = true
			break
		}
	}
	if !found {
		t.Error("expected warning for migration without transaction wrapper")
	}
}

func TestMigrationDeletedFileSkipped(t *testing.T) {
	a := NewMigrationAnalyzer()
	ctx := &AnalysisContext{
		RepoPath: "/repo",
		Diff: &git.Diff{
			Files: []git.FileDiff{
				{
					Path:   "db/migrations/000001_init.up.sql",
					Status: "deleted",
					Hunks: []git.Hunk{
						{
							OldStart: 1,
							OldCount: 1,
							Lines: []git.Line{
								{Type: "removed", Content: "DROP TABLE everything;", OldNum: 1},
							},
						},
					},
				},
			},
			Mode: "staged",
		},
		ParsedFiles: make(map[string]*parser.ParsedFile),
		Config:      config.DefaultConfig(),
	}

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(issues) != 0 {
		t.Errorf("expected 0 issues for deleted migration file, got %d", len(issues))
	}
}

// --- isMigrationFile tests ---

func TestIsMigrationFile(t *testing.T) {
	tests := []struct {
		path string
		want bool
	}{
		{"db/migrations/000001_init.up.sql", true},
		{"db/migrations/000001_init.down.sql", true},
		{"pkg/storage/migrations/000002_add_col.up.sql", true},
		{"db/migrate/20240101_create_users.sql", true},
		{"db/migrate/20240101_create_users.rb", true},
		{"alembic/versions/abc123_init.py", true},
		{"prisma/migrations/20240101/migration.sql", true},
		{"flyway/V1__init.sql", true},
		{"liquibase/changelog.sql", true},
		{"knex/migrations/20240101_init.js", true},
		{"knex/migrations/20240101_init.ts", true},
		{"src/migration_helper.sql", true},
		// Non-migration files
		{"scripts/seed_data.sql", false},
		{"pkg/storage/queries.sql", false},
		{"src/models/user.go", false},
		{"README.md", false},
	}

	for _, tt := range tests {
		got := isMigrationFile(tt.path)
		if got != tt.want {
			t.Errorf("isMigrationFile(%q) = %v, want %v", tt.path, got, tt.want)
		}
	}
}
