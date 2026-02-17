package fixer

import (
	"os"
	"path/filepath"
	"strings"

	"github.com/joeabbey/fault/pkg/analyzer"
)

// LuaFixer handles Lua-specific auto-fixes.
type LuaFixer struct{}

func NewLuaFixer() *LuaFixer { return &LuaFixer{} }

func (f *LuaFixer) FixIDs() []string {
	return []string{"lua-debug-print"}
}

func (f *LuaFixer) GenerateFix(issue analyzer.Issue, repoRoot string) *Fix {
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
	case "lua-debug-print":
		return f.fixDebugPrint(issue, line)
	}

	return nil
}

// fixDebugPrint removes print( debug output lines.
func (f *LuaFixer) fixDebugPrint(issue analyzer.Issue, line string) *Fix {
	return &Fix{
		IssueID:     issue.ID,
		FixID:       issue.FixID,
		File:        issue.File,
		Description: "Remove debug output: " + strings.TrimSpace(line),
		Edits:       []Edit{{Line: issue.Line, OldText: line, NewText: ""}},
	}
}
