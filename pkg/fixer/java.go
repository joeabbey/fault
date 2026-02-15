package fixer

import (
	"os"
	"path/filepath"
	"strings"

	"github.com/joeabbey/fault/pkg/analyzer"
)

// JavaFixer handles Java-specific auto-fixes.
type JavaFixer struct{}

func NewJavaFixer() *JavaFixer { return &JavaFixer{} }

func (f *JavaFixer) FixIDs() []string {
	return []string{"anti-sysout", "anti-printstacktrace", "import-broken-java"}
}

func (f *JavaFixer) GenerateFix(issue analyzer.Issue, repoRoot string) *Fix {
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

	switch issue.FixID {
	case "anti-sysout":
		return f.fixSysout(issue, line)
	case "anti-printstacktrace":
		return f.fixPrintStackTrace(issue, line)
	case "import-broken-java":
		return f.fixBrokenImport(issue, line)
	}

	return nil
}

// fixSysout removes System.out.println/System.err.println lines.
func (f *JavaFixer) fixSysout(issue analyzer.Issue, line string) *Fix {
	return &Fix{
		IssueID:     issue.ID,
		FixID:       issue.FixID,
		File:        issue.File,
		Description: "Remove debug output: " + strings.TrimSpace(line),
		Edits:       []Edit{{Line: issue.Line, OldText: line, NewText: ""}},
	}
}

// fixPrintStackTrace replaces e.printStackTrace() with a TODO comment.
func (f *JavaFixer) fixPrintStackTrace(issue analyzer.Issue, line string) *Fix {
	newLine := strings.Replace(line, "e.printStackTrace()", "// TODO: use proper logging instead of printStackTrace()", 1)
	// Also handle ex.printStackTrace()
	if newLine == line {
		newLine = strings.Replace(line, "ex.printStackTrace()", "// TODO: use proper logging instead of printStackTrace()", 1)
	}
	// Generic: any variable.printStackTrace()
	if newLine == line {
		// Just replace the whole line with a TODO comment
		indent := leadingWhitespace(line)
		newLine = indent + "// TODO: use proper logging instead of printStackTrace()"
	}

	return &Fix{
		IssueID:     issue.ID,
		FixID:       issue.FixID,
		File:        issue.File,
		Description: "Replace printStackTrace() with logging TODO",
		Edits:       []Edit{{Line: issue.Line, OldText: line, NewText: newLine}},
	}
}

// fixBrokenImport removes a broken Java import statement.
func (f *JavaFixer) fixBrokenImport(issue analyzer.Issue, line string) *Fix {
	return &Fix{
		IssueID:     issue.ID,
		FixID:       issue.FixID,
		File:        issue.File,
		Description: "Remove broken import statement",
		Edits:       []Edit{{Line: issue.Line, OldText: line, NewText: ""}},
	}
}
