package fixer

import (
	"os"
	"path/filepath"
	"strings"

	"github.com/joeabbey/fault/pkg/analyzer"
)

// PHPFixer handles PHP-specific auto-fixes.
type PHPFixer struct{}

func NewPHPFixer() *PHPFixer { return &PHPFixer{} }

func (f *PHPFixer) FixIDs() []string {
	return []string{"php-debug-print", "php-debug-dd"}
}

func (f *PHPFixer) GenerateFix(issue analyzer.Issue, repoRoot string) *Fix {
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
	case "php-debug-print":
		return f.fixDebugPrint(issue, line)
	case "php-debug-dd":
		return f.fixDebugDD(issue, line)
	}

	return nil
}

// fixDebugPrint removes lines with var_dump/print_r/echo debug calls.
func (f *PHPFixer) fixDebugPrint(issue analyzer.Issue, line string) *Fix {
	return &Fix{
		IssueID:     issue.ID,
		FixID:       issue.FixID,
		File:        issue.File,
		Description: "Remove debug output: " + strings.TrimSpace(line),
		Edits:       []Edit{{Line: issue.Line, OldText: line, NewText: ""}},
	}
}

// fixDebugDD removes lines with dd()/dump() calls.
func (f *PHPFixer) fixDebugDD(issue analyzer.Issue, line string) *Fix {
	return &Fix{
		IssueID:     issue.ID,
		FixID:       issue.FixID,
		File:        issue.File,
		Description: "Remove debug dump: " + strings.TrimSpace(line),
		Edits:       []Edit{{Line: issue.Line, OldText: line, NewText: ""}},
	}
}
