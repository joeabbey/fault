package fixer

import (
	"strings"
	"testing"

	"github.com/joeabbey/fault/pkg/analyzer"
)

func TestRustFixerFixIDs(t *testing.T) {
	f := NewRustFixer()
	ids := f.FixIDs()
	expected := map[string]bool{
		"anti-debug-macro": true,
		"anti-todo-macro":  true,
		"import-broken":    true,
	}
	for _, id := range ids {
		if !expected[id] {
			t.Errorf("unexpected fix ID: %q", id)
		}
	}
	if len(ids) != len(expected) {
		t.Errorf("expected %d fix IDs, got %d", len(expected), len(ids))
	}
}

func TestRustFixDebugPrintln(t *testing.T) {
	dir := t.TempDir()
	writeFile(t, dir, "main.rs", "fn main() {\n    println!(\"debug value\");\n    let x = 1;\n}\n")

	issue := analyzer.Issue{
		ID:    "patterns/console-debug",
		FixID: "anti-debug-macro",
		File:  "main.rs",
		Line:  2,
	}

	f := NewRustFixer()
	fix := f.GenerateFix(issue, dir)
	if fix == nil {
		t.Fatal("expected fix, got nil")
	}

	if err := Apply(fix, dir); err != nil {
		t.Fatalf("Apply: %v", err)
	}

	content := readFile(t, dir, "main.rs")
	if strings.Contains(content, "println!") {
		t.Errorf("println! should be removed, got:\n%s", content)
	}
	if !strings.Contains(content, "let x = 1") {
		t.Errorf("other code should remain, got:\n%s", content)
	}
}

func TestRustFixDebugDbg(t *testing.T) {
	dir := t.TempDir()
	writeFile(t, dir, "lib.rs", "fn process() {\n    let x = dbg!(compute());\n}\n")

	issue := analyzer.Issue{
		ID:    "patterns/console-debug",
		FixID: "anti-debug-macro",
		File:  "lib.rs",
		Line:  2,
	}

	f := NewRustFixer()
	fix := f.GenerateFix(issue, dir)
	if fix == nil {
		t.Fatal("expected fix, got nil")
	}

	if err := Apply(fix, dir); err != nil {
		t.Fatalf("Apply: %v", err)
	}

	content := readFile(t, dir, "lib.rs")
	if strings.Contains(content, "dbg!") {
		t.Errorf("dbg! should be removed, got:\n%s", content)
	}
	if !strings.Contains(content, "compute()") {
		t.Errorf("inner expression should remain, got:\n%s", content)
	}
}

func TestRustFixTodoMacro(t *testing.T) {
	dir := t.TempDir()
	writeFile(t, dir, "lib.rs", "fn process() {\n    todo!(\"implement later\");\n}\n")

	issue := analyzer.Issue{
		ID:    "patterns/todo-placeholder",
		FixID: "anti-todo-macro",
		File:  "lib.rs",
		Line:  2,
	}

	f := NewRustFixer()
	fix := f.GenerateFix(issue, dir)
	if fix == nil {
		t.Fatal("expected fix, got nil")
	}

	if err := Apply(fix, dir); err != nil {
		t.Fatalf("Apply: %v", err)
	}

	content := readFile(t, dir, "lib.rs")
	if strings.Contains(content, "todo!") {
		t.Errorf("todo! should be replaced, got:\n%s", content)
	}
	if !strings.Contains(content, "// TODO: implement") {
		t.Errorf("expected TODO comment, got:\n%s", content)
	}
}

func TestRustFixBrokenUse(t *testing.T) {
	dir := t.TempDir()
	writeFile(t, dir, "main.rs", "use phantom_crate::module;\nuse std::io;\n\nfn main() {}\n")

	issue := analyzer.Issue{
		ID:    "hallucination/phantom-import",
		FixID: "import-broken",
		File:  "main.rs",
		Line:  1,
	}

	f := NewRustFixer()
	fix := f.GenerateFix(issue, dir)
	if fix == nil {
		t.Fatal("expected fix, got nil")
	}

	if err := Apply(fix, dir); err != nil {
		t.Fatalf("Apply: %v", err)
	}

	content := readFile(t, dir, "main.rs")
	if strings.Contains(content, "phantom_crate") {
		t.Errorf("phantom import should be removed, got:\n%s", content)
	}
	if !strings.Contains(content, "use std::io") {
		t.Errorf("valid import should remain, got:\n%s", content)
	}
}

func TestRustFixerIgnoresNonRustFiles(t *testing.T) {
	dir := t.TempDir()
	writeFile(t, dir, "app.js", "console.log('test');\n")

	issue := analyzer.Issue{
		ID:    "patterns/console-debug",
		FixID: "anti-debug-macro",
		File:  "app.js",
		Line:  1,
	}

	f := NewRustFixer()
	fix := f.GenerateFix(issue, dir)
	if fix != nil {
		t.Error("expected nil fix for non-.rs file")
	}
}
