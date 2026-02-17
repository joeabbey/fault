package fixer

import (
	"os"
	"path/filepath"
	"regexp"
	"strings"

	"github.com/joeabbey/fault/pkg/analyzer"
)

// CFixer handles C-specific auto-fixes: debug print statements.
type CFixer struct{}

func NewCFixer() *CFixer { return &CFixer{} }

func (f *CFixer) FixIDs() []string {
	return []string{"c-debug-print"}
}

func (f *CFixer) GenerateFix(issue analyzer.Issue, repoRoot string) *Fix {
	if issue.Line == 0 || issue.File == "" {
		return nil
	}

	// Only handle C files.
	ext := strings.ToLower(filepath.Ext(issue.File))
	if ext != ".c" && ext != ".h" {
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
	case "c-debug-print":
		return f.fixDebugPrint(issue, line)
	}

	return nil
}

var (
	reCPrintf  = regexp.MustCompile(`\bprintf\(`)
	reCFprintf = regexp.MustCompile(`\bfprintf\(\s*stderr\b`)
)

// fixDebugPrint removes lines containing printf() or fprintf(stderr, ...) calls.
func (f *CFixer) fixDebugPrint(issue analyzer.Issue, line string) *Fix {
	trimmed := strings.TrimSpace(line)

	if reCPrintf.MatchString(trimmed) || reCFprintf.MatchString(trimmed) {
		return &Fix{
			IssueID:     issue.ID,
			FixID:       issue.FixID,
			File:        issue.File,
			Description: "Remove debug print: " + trimmed,
			Edits:       []Edit{{Line: issue.Line, OldText: line, NewText: ""}},
		}
	}

	return nil
}
