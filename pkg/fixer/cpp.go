package fixer

import (
	"os"
	"path/filepath"
	"regexp"
	"strings"

	"github.com/joeabbey/fault/pkg/analyzer"
)

// CppFixer handles C++-specific auto-fixes: debug print statements.
type CppFixer struct{}

func NewCppFixer() *CppFixer { return &CppFixer{} }

func (f *CppFixer) FixIDs() []string {
	return []string{"cpp-debug-print"}
}

func (f *CppFixer) GenerateFix(issue analyzer.Issue, repoRoot string) *Fix {
	if issue.Line == 0 || issue.File == "" {
		return nil
	}

	// Only handle C++ files.
	ext := strings.ToLower(filepath.Ext(issue.File))
	if ext != ".cpp" && ext != ".cc" && ext != ".cxx" && ext != ".hpp" {
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
	case "cpp-debug-print":
		return f.fixDebugPrint(issue, line)
	}

	return nil
}

var (
	reCppCout = regexp.MustCompile(`\bstd::cout\s*<<`)
	reCppCerr = regexp.MustCompile(`\bstd::cerr\s*<<`)
)

// fixDebugPrint removes lines containing std::cout << or std::cerr << output.
func (f *CppFixer) fixDebugPrint(issue analyzer.Issue, line string) *Fix {
	trimmed := strings.TrimSpace(line)

	if reCppCout.MatchString(trimmed) || reCppCerr.MatchString(trimmed) {
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
