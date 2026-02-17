package fixer

import (
	"os"
	"path/filepath"
	"strings"

	"github.com/joeabbey/fault/pkg/analyzer"
)

// CSharpFixer handles C#-specific auto-fixes.
type CSharpFixer struct{}

func NewCSharpFixer() *CSharpFixer { return &CSharpFixer{} }

func (f *CSharpFixer) FixIDs() []string {
	return []string{"csharp-debug-print", "csharp-debug-trace"}
}

func (f *CSharpFixer) GenerateFix(issue analyzer.Issue, repoRoot string) *Fix {
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
	case "csharp-debug-print":
		return f.fixDebugPrint(issue, line)
	case "csharp-debug-trace":
		return f.fixDebugTrace(issue, line)
	}

	return nil
}

// fixDebugPrint removes lines with Console.WriteLine/Console.Write.
func (f *CSharpFixer) fixDebugPrint(issue analyzer.Issue, line string) *Fix {
	return &Fix{
		IssueID:     issue.ID,
		FixID:       issue.FixID,
		File:        issue.File,
		Description: "Remove debug output: " + strings.TrimSpace(line),
		Edits:       []Edit{{Line: issue.Line, OldText: line, NewText: ""}},
	}
}

// fixDebugTrace removes lines with Debug.WriteLine.
func (f *CSharpFixer) fixDebugTrace(issue analyzer.Issue, line string) *Fix {
	return &Fix{
		IssueID:     issue.ID,
		FixID:       issue.FixID,
		File:        issue.File,
		Description: "Remove debug trace: " + strings.TrimSpace(line),
		Edits:       []Edit{{Line: issue.Line, OldText: line, NewText: ""}},
	}
}
