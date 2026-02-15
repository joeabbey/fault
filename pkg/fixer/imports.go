package fixer

import (
	"os"
	"path/filepath"
	"regexp"
	"strings"

	"github.com/joeabbey/fault/pkg/analyzer"
)

// ImportFixer handles broken and missing-export imports.
type ImportFixer struct{}

func NewImportFixer() *ImportFixer { return &ImportFixer{} }

func (f *ImportFixer) FixIDs() []string {
	return []string{"import-broken", "import-missing-export"}
}

func (f *ImportFixer) GenerateFix(issue analyzer.Issue, repoRoot string) *Fix {
	if issue.Line == 0 || issue.File == "" {
		return nil
	}

	filePath := filepath.Join(repoRoot, issue.File)
	data, err := os.ReadFile(filePath)
	if err != nil {
		return nil
	}
	lines := strings.Split(string(data), "\n")

	if issue.Line < 1 || issue.Line > len(lines) {
		return nil
	}

	line := lines[issue.Line-1]
	lang := detectLanguage(issue.File)

	switch issue.FixID {
	case "import-broken":
		return f.fixBrokenImport(issue, lines, line, lang)
	case "import-missing-export":
		return f.fixMissingExport(issue, lines, line, lang)
	}

	return nil
}

// fixBrokenImport removes the entire import line/statement for unresolvable imports.
func (f *ImportFixer) fixBrokenImport(issue analyzer.Issue, lines []string, line string, lang string) *Fix {
	switch lang {
	case "go":
		return f.fixGoBrokenImport(issue, lines, line)
	case "typescript":
		return f.fixTSBrokenImport(issue, line)
	case "python":
		return f.fixPythonBrokenImport(issue, line)
	}
	return nil
}

// fixGoBrokenImport removes a Go import. Handles both single imports and block imports.
func (f *ImportFixer) fixGoBrokenImport(issue analyzer.Issue, lines []string, line string) *Fix {
	trimmed := strings.TrimSpace(line)

	// Single import: `import "pkg"`
	if strings.HasPrefix(trimmed, "import ") && !strings.HasPrefix(trimmed, "import (") {
		return &Fix{
			IssueID:     issue.ID,
			FixID:       issue.FixID,
			File:        issue.File,
			Description: "Remove broken import statement",
			Edits:       []Edit{{Line: issue.Line, OldText: line, NewText: ""}},
		}
	}

	// Block import: remove just this line from import (...) block
	return &Fix{
		IssueID:     issue.ID,
		FixID:       issue.FixID,
		File:        issue.File,
		Description: "Remove broken import from import block",
		Edits:       []Edit{{Line: issue.Line, OldText: line, NewText: ""}},
	}
}

// fixTSBrokenImport removes an entire TypeScript import statement.
func (f *ImportFixer) fixTSBrokenImport(issue analyzer.Issue, line string) *Fix {
	return &Fix{
		IssueID:     issue.ID,
		FixID:       issue.FixID,
		File:        issue.File,
		Description: "Remove broken import statement",
		Edits:       []Edit{{Line: issue.Line, OldText: line, NewText: ""}},
	}
}

// fixPythonBrokenImport removes a Python import statement.
func (f *ImportFixer) fixPythonBrokenImport(issue analyzer.Issue, line string) *Fix {
	return &Fix{
		IssueID:     issue.ID,
		FixID:       issue.FixID,
		File:        issue.File,
		Description: "Remove broken import statement",
		Edits:       []Edit{{Line: issue.Line, OldText: line, NewText: ""}},
	}
}

// fixMissingExport removes the named import that references a non-existent export.
func (f *ImportFixer) fixMissingExport(issue analyzer.Issue, lines []string, line string, lang string) *Fix {
	// Extract the missing name from the issue ID or message.
	missingName := extractMissingName(issue)
	if missingName == "" {
		return nil
	}

	switch lang {
	case "typescript":
		return f.fixTSMissingExport(issue, line, missingName)
	case "python":
		return f.fixPythonMissingExport(issue, line, missingName)
	default:
		// For Go and others, just remove the line
		return &Fix{
			IssueID:     issue.ID,
			FixID:       issue.FixID,
			File:        issue.File,
			Description: "Remove import of missing export " + missingName,
			Edits:       []Edit{{Line: issue.Line, OldText: line, NewText: ""}},
		}
	}
}

// fixTSMissingExport rewrites import { a, b } from './foo' to import { a } from './foo'
// when removing b. If b is the only name, removes the entire import.
func (f *ImportFixer) fixTSMissingExport(issue analyzer.Issue, line string, missingName string) *Fix {
	// Match import { names } from 'path'
	re := regexp.MustCompile(`^(\s*import\s*\{)([^}]+)(\}\s*from\s*.+)$`)
	m := re.FindStringSubmatch(line)
	if m == nil {
		// Can't parse, just remove the whole line
		return &Fix{
			IssueID:     issue.ID,
			FixID:       issue.FixID,
			File:        issue.File,
			Description: "Remove import with missing export " + missingName,
			Edits:       []Edit{{Line: issue.Line, OldText: line, NewText: ""}},
		}
	}

	names := splitImportNames(m[2])
	remaining := make([]string, 0, len(names))
	for _, name := range names {
		if strings.TrimSpace(name) != missingName {
			remaining = append(remaining, strings.TrimSpace(name))
		}
	}

	if len(remaining) == 0 {
		// No names left, remove entire import
		return &Fix{
			IssueID:     issue.ID,
			FixID:       issue.FixID,
			File:        issue.File,
			Description: "Remove import (no remaining exports)",
			Edits:       []Edit{{Line: issue.Line, OldText: line, NewText: ""}},
		}
	}

	newLine := m[1] + " " + strings.Join(remaining, ", ") + " " + m[3]
	return &Fix{
		IssueID:     issue.ID,
		FixID:       issue.FixID,
		File:        issue.File,
		Description: "Remove " + missingName + " from import",
		Edits:       []Edit{{Line: issue.Line, OldText: line, NewText: newLine}},
	}
}

// fixPythonMissingExport rewrites from foo import a, b to from foo import a
// when removing b. If b is the only name, removes the entire import.
func (f *ImportFixer) fixPythonMissingExport(issue analyzer.Issue, line string, missingName string) *Fix {
	// Match from ... import names
	re := regexp.MustCompile(`^(\s*from\s+\S+\s+import\s+)(.+)$`)
	m := re.FindStringSubmatch(line)
	if m == nil {
		return &Fix{
			IssueID:     issue.ID,
			FixID:       issue.FixID,
			File:        issue.File,
			Description: "Remove import with missing export " + missingName,
			Edits:       []Edit{{Line: issue.Line, OldText: line, NewText: ""}},
		}
	}

	names := splitImportNames(m[2])
	remaining := make([]string, 0, len(names))
	for _, name := range names {
		if strings.TrimSpace(name) != missingName {
			remaining = append(remaining, strings.TrimSpace(name))
		}
	}

	if len(remaining) == 0 {
		return &Fix{
			IssueID:     issue.ID,
			FixID:       issue.FixID,
			File:        issue.File,
			Description: "Remove import (no remaining exports)",
			Edits:       []Edit{{Line: issue.Line, OldText: line, NewText: ""}},
		}
	}

	newLine := m[1] + strings.Join(remaining, ", ")
	return &Fix{
		IssueID:     issue.ID,
		FixID:       issue.FixID,
		File:        issue.File,
		Description: "Remove " + missingName + " from import",
		Edits:       []Edit{{Line: issue.Line, OldText: line, NewText: newLine}},
	}
}

// extractMissingName extracts the missing export name from an issue.
// It looks at the issue ID (import-name-file-line-NAME or export-removed-...-NAME)
// and falls back to parsing the message.
func extractMissingName(issue analyzer.Issue) string {
	// Try parsing from issue message: `Name "foo" is not exported` or
	// `Import "foo" was removed from`
	re := regexp.MustCompile(`(?:Name|Import) "([^"]+)"`)
	if m := re.FindStringSubmatch(issue.Message); m != nil {
		return m[1]
	}

	// Try parsing from issue ID: import-name-file-line-NAME
	parts := strings.Split(issue.ID, "-")
	if len(parts) >= 2 {
		return parts[len(parts)-1]
	}

	return ""
}

// splitImportNames splits "a, b, c" into individual names.
func splitImportNames(s string) []string {
	parts := strings.Split(s, ",")
	result := make([]string, 0, len(parts))
	for _, p := range parts {
		p = strings.TrimSpace(p)
		if p != "" {
			result = append(result, p)
		}
	}
	return result
}

// detectLanguage returns a language identifier based on file extension.
func detectLanguage(path string) string {
	ext := strings.ToLower(filepath.Ext(path))
	switch ext {
	case ".go":
		return "go"
	case ".ts", ".tsx", ".js", ".jsx", ".mjs", ".cjs":
		return "typescript"
	case ".py":
		return "python"
	case ".java":
		return "java"
	case ".rs":
		return "rust"
	default:
		return ""
	}
}
