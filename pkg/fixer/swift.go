package fixer

import (
	"os"
	"path/filepath"
	"strings"

	"github.com/joeabbey/fault/pkg/analyzer"
)

// SwiftFixer handles Swift-specific auto-fixes.
type SwiftFixer struct{}

func NewSwiftFixer() *SwiftFixer { return &SwiftFixer{} }

func (f *SwiftFixer) FixIDs() []string {
	return []string{"swift-debug-print", "swift-debug-verbose"}
}

func (f *SwiftFixer) GenerateFix(issue analyzer.Issue, repoRoot string) *Fix {
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
	case "swift-debug-print":
		return f.fixDebugPrint(issue, line)
	case "swift-debug-verbose":
		return f.fixDebugVerbose(issue, line)
	}

	return nil
}

// fixDebugPrint removes lines with print() calls.
func (f *SwiftFixer) fixDebugPrint(issue analyzer.Issue, line string) *Fix {
	return &Fix{
		IssueID:     issue.ID,
		FixID:       issue.FixID,
		File:        issue.File,
		Description: "Remove debug output: " + strings.TrimSpace(line),
		Edits:       []Edit{{Line: issue.Line, OldText: line, NewText: ""}},
	}
}

// fixDebugVerbose removes lines with debugPrint() calls.
func (f *SwiftFixer) fixDebugVerbose(issue analyzer.Issue, line string) *Fix {
	return &Fix{
		IssueID:     issue.ID,
		FixID:       issue.FixID,
		File:        issue.File,
		Description: "Remove verbose debug output: " + strings.TrimSpace(line),
		Edits:       []Edit{{Line: issue.Line, OldText: line, NewText: ""}},
	}
}
