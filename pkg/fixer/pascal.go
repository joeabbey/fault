package fixer

import (
	"os"
	"path/filepath"
	"strings"

	"github.com/joeabbey/fault/pkg/analyzer"
)

// PascalFixer handles Pascal-specific auto-fixes.
type PascalFixer struct{}

func NewPascalFixer() *PascalFixer { return &PascalFixer{} }

func (f *PascalFixer) FixIDs() []string {
	return []string{"pascal-debug-print"}
}

func (f *PascalFixer) GenerateFix(issue analyzer.Issue, repoRoot string) *Fix {
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
	case "pascal-debug-print":
		return &Fix{
			IssueID:     issue.ID,
			FixID:       issue.FixID,
			File:        issue.File,
			Description: "Remove debug output: " + strings.TrimSpace(line),
			Edits:       []Edit{{Line: issue.Line, OldText: line, NewText: ""}},
		}
	}

	return nil
}
