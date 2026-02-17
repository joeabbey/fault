package fixer

import (
	"os"
	"path/filepath"
	"strings"

	"github.com/joeabbey/fault/pkg/analyzer"
)

// ScalaFixer handles Scala-specific auto-fixes.
type ScalaFixer struct{}

func NewScalaFixer() *ScalaFixer { return &ScalaFixer{} }

func (f *ScalaFixer) FixIDs() []string {
	return []string{"scala-debug-print"}
}

func (f *ScalaFixer) GenerateFix(issue analyzer.Issue, repoRoot string) *Fix {
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
	case "scala-debug-print":
		return f.fixDebugPrint(issue, line)
	}

	return nil
}

// fixDebugPrint removes debug print lines (println, System.out.println).
func (f *ScalaFixer) fixDebugPrint(issue analyzer.Issue, line string) *Fix {
	return &Fix{
		IssueID:     issue.ID,
		FixID:       issue.FixID,
		File:        issue.File,
		Description: "Remove debug output: " + strings.TrimSpace(line),
		Edits:       []Edit{{Line: issue.Line, OldText: line, NewText: ""}},
	}
}
