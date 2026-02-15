package fixer

import (
	"strings"
	"testing"

	"github.com/joeabbey/fault/pkg/analyzer"
)

func TestFixSysout(t *testing.T) {
	dir := t.TempDir()
	writeFile(t, dir, "App.java", "public class App {\n    public void run() {\n        System.out.println(\"debug\");\n        doWork();\n    }\n}\n")

	issue := analyzer.Issue{
		ID:    "patterns/console-debug",
		FixID: "anti-sysout",
		File:  "App.java",
		Line:  3,
	}

	fixer := NewJavaFixer()
	fix := fixer.GenerateFix(issue, dir)
	if fix == nil {
		t.Fatal("expected fix, got nil")
	}

	if err := Apply(fix, dir); err != nil {
		t.Fatalf("Apply: %v", err)
	}

	content := readFile(t, dir, "App.java")
	if strings.Contains(content, "System.out.println") {
		t.Errorf("System.out.println should be removed, got:\n%s", content)
	}
	if !strings.Contains(content, "doWork()") {
		t.Errorf("other code should remain, got:\n%s", content)
	}
}

func TestFixPrintStackTrace(t *testing.T) {
	dir := t.TempDir()
	writeFile(t, dir, "Handler.java", "public class Handler {\n    public void handle() {\n        try {\n            process();\n        } catch (Exception e) {\n            e.printStackTrace();\n        }\n    }\n}\n")

	issue := analyzer.Issue{
		ID:    "patterns/console-debug",
		FixID: "anti-printstacktrace",
		File:  "Handler.java",
		Line:  6,
	}

	fixer := NewJavaFixer()
	fix := fixer.GenerateFix(issue, dir)
	if fix == nil {
		t.Fatal("expected fix, got nil")
	}

	if err := Apply(fix, dir); err != nil {
		t.Fatalf("Apply: %v", err)
	}

	content := readFile(t, dir, "Handler.java")
	if strings.Contains(content, "e.printStackTrace()") {
		t.Errorf("e.printStackTrace() should be replaced, got:\n%s", content)
	}
	if !strings.Contains(content, "// TODO: use proper logging") {
		t.Errorf("should contain logging TODO, got:\n%s", content)
	}
}

func TestFixBrokenJavaImport(t *testing.T) {
	dir := t.TempDir()
	writeFile(t, dir, "App.java", "package com.example;\n\nimport com.nonexistent.FakeClass;\nimport java.util.List;\n\npublic class App {}\n")

	issue := analyzer.Issue{
		ID:    "hallucination/phantom-import",
		FixID: "import-broken-java",
		File:  "App.java",
		Line:  3,
	}

	fixer := NewJavaFixer()
	fix := fixer.GenerateFix(issue, dir)
	if fix == nil {
		t.Fatal("expected fix, got nil")
	}

	if err := Apply(fix, dir); err != nil {
		t.Fatalf("Apply: %v", err)
	}

	content := readFile(t, dir, "App.java")
	if strings.Contains(content, "com.nonexistent") {
		t.Errorf("broken import should be removed, got:\n%s", content)
	}
	if !strings.Contains(content, "java.util.List") {
		t.Errorf("valid import should remain, got:\n%s", content)
	}
}

func TestJavaFixerFixIDs(t *testing.T) {
	f := NewJavaFixer()
	ids := f.FixIDs()

	expected := map[string]bool{
		"anti-sysout":          true,
		"anti-printstacktrace": true,
		"import-broken-java":   true,
	}

	for _, id := range ids {
		if !expected[id] {
			t.Errorf("unexpected fix ID: %q", id)
		}
		delete(expected, id)
	}

	for id := range expected {
		t.Errorf("missing fix ID: %q", id)
	}
}
