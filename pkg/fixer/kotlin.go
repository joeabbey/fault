package fixer

import (
	"os"
	"path/filepath"
	"strings"

	"github.com/joeabbey/fault/pkg/analyzer"
)

// KotlinFixer handles Kotlin-specific auto-fixes.
type KotlinFixer struct{}

func NewKotlinFixer() *KotlinFixer { return &KotlinFixer{} }

func (f *KotlinFixer) FixIDs() []string {
	return []string{"kotlin-debug-print", "kotlin-todo"}
}

func (f *KotlinFixer) GenerateFix(issue analyzer.Issue, repoRoot string) *Fix {
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
	case "kotlin-debug-print":
		return f.fixDebugPrint(issue, line)
	case "kotlin-todo":
		return f.fixTodo(issue, line)
	}

	return nil
}

// fixDebugPrint removes println()/print() debug output lines.
func (f *KotlinFixer) fixDebugPrint(issue analyzer.Issue, line string) *Fix {
	return &Fix{
		IssueID:     issue.ID,
		FixID:       issue.FixID,
		File:        issue.File,
		Description: "Remove debug output: " + strings.TrimSpace(line),
		Edits:       []Edit{{Line: issue.Line, OldText: line, NewText: ""}},
	}
}

// fixTodo removes TODO() calls (Kotlin's built-in placeholder that throws NotImplementedError).
func (f *KotlinFixer) fixTodo(issue analyzer.Issue, line string) *Fix {
	return &Fix{
		IssueID:     issue.ID,
		FixID:       issue.FixID,
		File:        issue.File,
		Description: "Remove TODO() call: " + strings.TrimSpace(line),
		Edits:       []Edit{{Line: issue.Line, OldText: line, NewText: ""}},
	}
}
