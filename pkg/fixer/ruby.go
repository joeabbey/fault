package fixer

import (
	"os"
	"path/filepath"
	"strings"

	"github.com/joeabbey/fault/pkg/analyzer"
)

// RubyFixer handles Ruby-specific auto-fixes.
type RubyFixer struct{}

func NewRubyFixer() *RubyFixer { return &RubyFixer{} }

func (f *RubyFixer) FixIDs() []string {
	return []string{"ruby-debug-print", "ruby-debugger"}
}

func (f *RubyFixer) GenerateFix(issue analyzer.Issue, repoRoot string) *Fix {
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
	case "ruby-debug-print":
		return f.fixDebugPrint(issue, line)
	case "ruby-debugger":
		return f.fixDebugger(issue, line)
	}

	return nil
}

// fixDebugPrint removes lines with puts/p/pp debug calls.
func (f *RubyFixer) fixDebugPrint(issue analyzer.Issue, line string) *Fix {
	return &Fix{
		IssueID:     issue.ID,
		FixID:       issue.FixID,
		File:        issue.File,
		Description: "Remove debug output: " + strings.TrimSpace(line),
		Edits:       []Edit{{Line: issue.Line, OldText: line, NewText: ""}},
	}
}

// fixDebugger removes lines with binding.pry or byebug.
func (f *RubyFixer) fixDebugger(issue analyzer.Issue, line string) *Fix {
	return &Fix{
		IssueID:     issue.ID,
		FixID:       issue.FixID,
		File:        issue.File,
		Description: "Remove debugger statement: " + strings.TrimSpace(line),
		Edits:       []Edit{{Line: issue.Line, OldText: line, NewText: ""}},
	}
}
