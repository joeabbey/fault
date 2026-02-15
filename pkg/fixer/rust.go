package fixer

import (
	"os"
	"path/filepath"
	"regexp"
	"strings"

	"github.com/joeabbey/fault/pkg/analyzer"
)

// RustFixer handles Rust-specific auto-fixes: debug macros, todo macros, broken imports.
type RustFixer struct{}

func NewRustFixer() *RustFixer { return &RustFixer{} }

func (f *RustFixer) FixIDs() []string {
	return []string{"anti-debug-macro", "anti-todo-macro", "import-broken"}
}

func (f *RustFixer) GenerateFix(issue analyzer.Issue, repoRoot string) *Fix {
	if issue.Line == 0 || issue.File == "" {
		return nil
	}

	// Only handle .rs files for Rust-specific fixes.
	if !strings.HasSuffix(issue.File, ".rs") {
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
	case "anti-debug-macro":
		return f.fixDebugMacro(issue, line)
	case "anti-todo-macro":
		return f.fixTodoMacro(issue, line)
	case "import-broken":
		return f.fixBrokenUse(issue, line)
	}

	return nil
}

var (
	reDbgMacro     = regexp.MustCompile(`\bdbg!\([^)]*\)`)
	rePrintlnMacro = regexp.MustCompile(`\bprintln!\([^)]*\);?`)
	reEprintlnMacro = regexp.MustCompile(`\beprintln!\([^)]*\);?`)
	reTodoMacro    = regexp.MustCompile(`\btodo!\([^)]*\);?`)
	reUnimplMacro  = regexp.MustCompile(`\bunimplemented!\([^)]*\);?`)
)

// fixDebugMacro removes println!, eprintln!, and dbg! macro calls.
func (f *RustFixer) fixDebugMacro(issue analyzer.Issue, line string) *Fix {
	trimmed := strings.TrimSpace(line)

	// If the entire line is just the debug macro, delete the line.
	if rePrintlnMacro.MatchString(trimmed) && strings.TrimSuffix(strings.TrimSuffix(rePrintlnMacro.FindString(trimmed), ";"), " ") == strings.TrimSuffix(trimmed, ";") {
		return &Fix{
			IssueID:     issue.ID,
			FixID:       issue.FixID,
			File:        issue.File,
			Description: "Remove debug macro: " + strings.TrimSpace(line),
			Edits:       []Edit{{Line: issue.Line, OldText: line, NewText: ""}},
		}
	}
	if reEprintlnMacro.MatchString(trimmed) && strings.TrimSuffix(strings.TrimSuffix(reEprintlnMacro.FindString(trimmed), ";"), " ") == strings.TrimSuffix(trimmed, ";") {
		return &Fix{
			IssueID:     issue.ID,
			FixID:       issue.FixID,
			File:        issue.File,
			Description: "Remove debug macro: " + strings.TrimSpace(line),
			Edits:       []Edit{{Line: issue.Line, OldText: line, NewText: ""}},
		}
	}
	if reDbgMacro.MatchString(trimmed) {
		// dbg! can be inline: let x = dbg!(val); -> let x = val;
		newLine := reDbgMacro.ReplaceAllStringFunc(line, func(match string) string {
			// Extract inner expression: dbg!(expr) -> expr
			inner := strings.TrimPrefix(match, "dbg!(")
			inner = strings.TrimSuffix(inner, ")")
			return inner
		})
		if strings.TrimSpace(newLine) == "" {
			return &Fix{
				IssueID:     issue.ID,
				FixID:       issue.FixID,
				File:        issue.File,
				Description: "Remove debug macro: " + strings.TrimSpace(line),
				Edits:       []Edit{{Line: issue.Line, OldText: line, NewText: ""}},
			}
		}
		return &Fix{
			IssueID:     issue.ID,
			FixID:       issue.FixID,
			File:        issue.File,
			Description: "Remove dbg! macro, keep expression",
			Edits:       []Edit{{Line: issue.Line, OldText: line, NewText: newLine}},
		}
	}

	// Fallback: remove the whole line
	return &Fix{
		IssueID:     issue.ID,
		FixID:       issue.FixID,
		File:        issue.File,
		Description: "Remove debug macro: " + strings.TrimSpace(line),
		Edits:       []Edit{{Line: issue.Line, OldText: line, NewText: ""}},
	}
}

// fixTodoMacro replaces todo!() and unimplemented!() with a TODO comment.
func (f *RustFixer) fixTodoMacro(issue analyzer.Issue, line string) *Fix {
	newLine := line
	if reTodoMacro.MatchString(line) {
		newLine = reTodoMacro.ReplaceAllString(line, "// TODO: implement")
	} else if reUnimplMacro.MatchString(line) {
		newLine = reUnimplMacro.ReplaceAllString(line, "// TODO: implement")
	}

	if newLine == line {
		return nil
	}

	return &Fix{
		IssueID:     issue.ID,
		FixID:       issue.FixID,
		File:        issue.File,
		Description: "Replace todo!/unimplemented! with TODO comment",
		Edits:       []Edit{{Line: issue.Line, OldText: line, NewText: newLine}},
	}
}

// fixBrokenUse removes a broken use statement.
func (f *RustFixer) fixBrokenUse(issue analyzer.Issue, line string) *Fix {
	return &Fix{
		IssueID:     issue.ID,
		FixID:       issue.FixID,
		File:        issue.File,
		Description: "Remove broken use statement",
		Edits:       []Edit{{Line: issue.Line, OldText: line, NewText: ""}},
	}
}
