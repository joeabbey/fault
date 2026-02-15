package fixer

import (
	"strings"
	"testing"

	"github.com/joeabbey/fault/pkg/analyzer"
)

func TestFixConsoleLog(t *testing.T) {
	dir := t.TempDir()
	writeFile(t, dir, "app.js", "const x = 1;\nconsole.log('debug');\nconst y = 2;\n")

	issue := analyzer.Issue{
		ID:    "patterns/console-debug",
		FixID: "anti-console-log",
		File:  "app.js",
		Line:  2,
	}

	fixer := NewPatternFixer()
	fix := fixer.GenerateFix(issue, dir)
	if fix == nil {
		t.Fatal("expected fix, got nil")
	}

	if err := Apply(fix, dir); err != nil {
		t.Fatalf("Apply: %v", err)
	}

	content := readFile(t, dir, "app.js")
	if strings.Contains(content, "console.log") {
		t.Errorf("console.log should be removed, got:\n%s", content)
	}
	if !strings.Contains(content, "const x = 1") || !strings.Contains(content, "const y = 2") {
		t.Errorf("other code should remain, got:\n%s", content)
	}
}

func TestFixFmtPrintln(t *testing.T) {
	dir := t.TempDir()
	writeFile(t, dir, "main.go", "package main\n\nfunc foo() {\n\tfmt.Println(\"debug\")\n\tx := 1\n}\n")

	issue := analyzer.Issue{
		ID:    "patterns/console-debug",
		FixID: "anti-debug-print",
		File:  "main.go",
		Line:  4,
	}

	fixer := NewPatternFixer()
	fix := fixer.GenerateFix(issue, dir)
	if fix == nil {
		t.Fatal("expected fix, got nil")
	}

	if err := Apply(fix, dir); err != nil {
		t.Fatalf("Apply: %v", err)
	}

	content := readFile(t, dir, "main.go")
	if strings.Contains(content, "fmt.Println") {
		t.Errorf("fmt.Println should be removed, got:\n%s", content)
	}
	if !strings.Contains(content, "x := 1") {
		t.Errorf("other code should remain, got:\n%s", content)
	}
}

func TestFixPythonPrint(t *testing.T) {
	dir := t.TempDir()
	writeFile(t, dir, "app.py", "x = 1\nprint('debug')\ny = 2\n")

	issue := analyzer.Issue{
		ID:    "patterns/console-debug",
		FixID: "anti-debug-print",
		File:  "app.py",
		Line:  2,
	}

	fixer := NewPatternFixer()
	fix := fixer.GenerateFix(issue, dir)
	if fix == nil {
		t.Fatal("expected fix, got nil")
	}

	if err := Apply(fix, dir); err != nil {
		t.Fatalf("Apply: %v", err)
	}

	content := readFile(t, dir, "app.py")
	if strings.Contains(content, "print(") {
		t.Errorf("print should be removed, got:\n%s", content)
	}
	if !strings.Contains(content, "x = 1") || !strings.Contains(content, "y = 2") {
		t.Errorf("other code should remain, got:\n%s", content)
	}
}
