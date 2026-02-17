package fixer

import (
	"os"
	"path/filepath"
	"strings"

	"github.com/joeabbey/fault/pkg/analyzer"
)

// GroovyFixer handles Groovy-specific auto-fixes.
type GroovyFixer struct{}

func NewGroovyFixer() *GroovyFixer { return &GroovyFixer{} }

func (f *GroovyFixer) FixIDs() []string {
	return []string{"groovy-debug-print"}
}

func (f *GroovyFixer) GenerateFix(issue analyzer.Issue, repoRoot string) *Fix {
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
	case "groovy-debug-print":
		return f.fixDebugPrint(issue, line)
	}

	return nil
}

// fixDebugPrint removes debug print lines (println, System.out.println).
func (f *GroovyFixer) fixDebugPrint(issue analyzer.Issue, line string) *Fix {
	return &Fix{
		IssueID:     issue.ID,
		FixID:       issue.FixID,
		File:        issue.File,
		Description: "Remove debug output: " + strings.TrimSpace(line),
		Edits:       []Edit{{Line: issue.Line, OldText: line, NewText: ""}},
	}
}
