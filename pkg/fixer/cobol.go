package fixer

import (
	"os"
	"path/filepath"
	"strings"

	"github.com/joeabbey/fault/pkg/analyzer"
)

// COBOLFixer handles COBOL-specific auto-fixes.
type COBOLFixer struct{}

func NewCOBOLFixer() *COBOLFixer { return &COBOLFixer{} }

func (f *COBOLFixer) FixIDs() []string {
	return []string{"cobol-debug-print"}
}

func (f *COBOLFixer) GenerateFix(issue analyzer.Issue, repoRoot string) *Fix {
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
	case "cobol-debug-print":
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
