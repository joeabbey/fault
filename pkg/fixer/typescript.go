package fixer

import (
	"os"
	"path/filepath"
	"regexp"
	"strings"

	"github.com/joeabbey/fault/pkg/analyzer"
)

// TypeScriptFixer handles TypeScript-specific auto-fixes.
type TypeScriptFixer struct{}

func NewTypeScriptFixer() *TypeScriptFixer { return &TypeScriptFixer{} }

func (f *TypeScriptFixer) FixIDs() []string {
	return []string{"ts-any-to-unknown", "ts-remove-ignore", "ts-unused-type-import"}
}

func (f *TypeScriptFixer) GenerateFix(issue analyzer.Issue, repoRoot string) *Fix {
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
	case "ts-any-to-unknown":
		return f.fixAnyToUnknown(issue, line)
	case "ts-remove-ignore":
		return f.fixRemoveIgnore(issue, line)
	case "ts-unused-type-import":
		return f.fixUnusedTypeImport(issue, line)
	}

	return nil
}

// anyReplacements replaces `any` with `unknown` in type annotations.
var anyReplacements = []*regexp.Regexp{
	regexp.MustCompile(`:\s*any\b`),
	regexp.MustCompile(`\bas\s+any\b`),
	regexp.MustCompile(`<any>`),
}

func (f *TypeScriptFixer) fixAnyToUnknown(issue analyzer.Issue, line string) *Fix {
	newLine := line

	// Replace : any with : unknown
	re1 := regexp.MustCompile(`(:\s*)any\b`)
	newLine = re1.ReplaceAllString(newLine, "${1}unknown")

	// Replace as any with as unknown
	re2 := regexp.MustCompile(`\bas\s+any\b`)
	newLine = re2.ReplaceAllString(newLine, "as unknown")

	// Replace <any> with <unknown>
	newLine = strings.ReplaceAll(newLine, "<any>", "<unknown>")

	if newLine == line {
		return nil
	}

	return &Fix{
		IssueID:     issue.ID,
		FixID:       issue.FixID,
		File:        issue.File,
		Description: "Replace `any` with `unknown` for type safety",
		Edits:       []Edit{{Line: issue.Line, OldText: line, NewText: newLine}},
	}
}

func (f *TypeScriptFixer) fixRemoveIgnore(issue analyzer.Issue, line string) *Fix {
	return &Fix{
		IssueID:     issue.ID,
		FixID:       issue.FixID,
		File:        issue.File,
		Description: "Remove @ts-ignore/@ts-nocheck directive",
		Edits:       []Edit{{Line: issue.Line, OldText: line, NewText: ""}},
	}
}

func (f *TypeScriptFixer) fixUnusedTypeImport(issue analyzer.Issue, line string) *Fix {
	return &Fix{
		IssueID:     issue.ID,
		FixID:       issue.FixID,
		File:        issue.File,
		Description: "Remove unused type-only import",
		Edits:       []Edit{{Line: issue.Line, OldText: line, NewText: ""}},
	}
}
