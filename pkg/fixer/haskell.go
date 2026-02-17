package fixer

import (
	"os"
	"path/filepath"
	"strings"

	"github.com/joeabbey/fault/pkg/analyzer"
)

// HaskellFixer handles Haskell-specific auto-fixes.
type HaskellFixer struct{}

func NewHaskellFixer() *HaskellFixer { return &HaskellFixer{} }

func (f *HaskellFixer) FixIDs() []string {
	return []string{"haskell-debug-trace"}
}

func (f *HaskellFixer) GenerateFix(issue analyzer.Issue, repoRoot string) *Fix {
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
	case "haskell-debug-trace":
		return f.fixDebugTrace(issue, line)
	}

	return nil
}

// fixDebugTrace removes lines with Debug.Trace calls.
func (f *HaskellFixer) fixDebugTrace(issue analyzer.Issue, line string) *Fix {
	return &Fix{
		IssueID:     issue.ID,
		FixID:       issue.FixID,
		File:        issue.File,
		Description: "Remove debug trace: " + strings.TrimSpace(line),
		Edits:       []Edit{{Line: issue.Line, OldText: line, NewText: ""}},
	}
}
