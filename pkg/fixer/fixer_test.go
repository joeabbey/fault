package fixer

import (
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/joeabbey/fault/pkg/analyzer"
)

// stubFixer implements Fixer for testing.
type stubFixer struct {
	ids []string
	fix *Fix
}

func (s *stubFixer) FixIDs() []string { return s.ids }
func (s *stubFixer) GenerateFix(issue analyzer.Issue, repoRoot string) *Fix {
	return s.fix
}

func TestRegistryRouting(t *testing.T) {
	reg := NewRegistry()

	fixA := &Fix{IssueID: "a-1", FixID: "fix-a", File: "a.go", Description: "fix a"}
	fixB := &Fix{IssueID: "b-1", FixID: "fix-b", File: "b.go", Description: "fix b"}

	reg.Register(&stubFixer{ids: []string{"fix-a"}, fix: fixA})
	reg.Register(&stubFixer{ids: []string{"fix-b"}, fix: fixB})

	issueA := analyzer.Issue{ID: "a-1", FixID: "fix-a", File: "a.go"}
	issueB := analyzer.Issue{ID: "b-1", FixID: "fix-b", File: "b.go"}
	issueNone := analyzer.Issue{ID: "c-1", FixID: "fix-c", File: "c.go"}
	issueEmpty := analyzer.Issue{ID: "d-1", File: "d.go"}

	got := reg.GenerateFix(issueA, "/repo")
	if got != fixA {
		t.Errorf("expected fixA for fix-a, got %v", got)
	}

	got = reg.GenerateFix(issueB, "/repo")
	if got != fixB {
		t.Errorf("expected fixB for fix-b, got %v", got)
	}

	got = reg.GenerateFix(issueNone, "/repo")
	if got != nil {
		t.Errorf("expected nil for unknown FixID, got %v", got)
	}

	got = reg.GenerateFix(issueEmpty, "/repo")
	if got != nil {
		t.Errorf("expected nil for empty FixID, got %v", got)
	}
}

func TestApplyEdit(t *testing.T) {
	dir := t.TempDir()
	writeFile(t, dir, "test.go", "package main\n\nimport \"fmt\"\n\nfunc main() {\n\tfmt.Println(\"hello\")\n}\n")

	fix := &Fix{
		IssueID:     "test-1",
		FixID:       "test",
		File:        "test.go",
		Description: "replace println",
		Edits: []Edit{
			{Line: 6, OldText: "\tfmt.Println(\"hello\")", NewText: "\tlog.Println(\"hello\")"},
		},
	}

	if err := Apply(fix, dir); err != nil {
		t.Fatalf("Apply: %v", err)
	}

	content := readFile(t, dir, "test.go")
	if !strings.Contains(content, "log.Println(\"hello\")") {
		t.Errorf("expected replacement, got:\n%s", content)
	}
	if strings.Contains(content, "fmt.Println") {
		t.Errorf("old text should be gone, got:\n%s", content)
	}
}

func TestApplyDeleteLine(t *testing.T) {
	dir := t.TempDir()
	writeFile(t, dir, "test.js", "const a = 1;\nconsole.log(a);\nconst b = 2;\n")

	fix := &Fix{
		IssueID:     "test-1",
		FixID:       "test",
		File:        "test.js",
		Description: "remove console.log",
		Edits: []Edit{
			{Line: 2, OldText: "console.log(a);", NewText: ""},
		},
	}

	if err := Apply(fix, dir); err != nil {
		t.Fatalf("Apply: %v", err)
	}

	content := readFile(t, dir, "test.js")
	if strings.Contains(content, "console.log") {
		t.Errorf("console.log should be deleted, got:\n%s", content)
	}
	lines := strings.Split(content, "\n")
	// Should have 3 lines: "const a = 1;", "const b = 2;", "" (trailing)
	if len(lines) != 3 {
		t.Errorf("expected 3 lines, got %d: %v", len(lines), lines)
	}
}

func TestApplyVerificationFail(t *testing.T) {
	dir := t.TempDir()
	writeFile(t, dir, "test.go", "package main\n\nfunc main() {}\n")

	fix := &Fix{
		IssueID:     "test-1",
		FixID:       "test",
		File:        "test.go",
		Description: "bad fix",
		Edits: []Edit{
			{Line: 2, OldText: "this text does not exist", NewText: "replacement"},
		},
	}

	err := Apply(fix, dir)
	if err == nil {
		t.Fatal("expected verification error")
	}
	if !strings.Contains(err.Error(), "verification failed") {
		t.Errorf("expected verification failed error, got: %v", err)
	}
}

func TestApplyMultipleEdits(t *testing.T) {
	dir := t.TempDir()
	writeFile(t, dir, "test.py", "import os\nprint('debug1')\nx = 1\nprint('debug2')\ny = 2\n")

	fix := &Fix{
		IssueID:     "test-1",
		FixID:       "test",
		File:        "test.py",
		Description: "remove debug prints",
		Edits: []Edit{
			{Line: 2, OldText: "print('debug1')", NewText: ""},
			{Line: 4, OldText: "print('debug2')", NewText: ""},
		},
	}

	if err := Apply(fix, dir); err != nil {
		t.Fatalf("Apply: %v", err)
	}

	content := readFile(t, dir, "test.py")
	if strings.Contains(content, "print(") {
		t.Errorf("prints should be deleted, got:\n%s", content)
	}
	if !strings.Contains(content, "x = 1") || !strings.Contains(content, "y = 2") {
		t.Errorf("non-print lines should remain, got:\n%s", content)
	}
}

func TestDryRunOutput(t *testing.T) {
	fix := &Fix{
		IssueID:     "test-1",
		FixID:       "test",
		File:        "src/app.js",
		Description: "remove console.log",
		Edits: []Edit{
			{Line: 10, OldText: "console.log('test')", NewText: ""},
			{Line: 20, OldText: "Math.random()", NewText: "crypto.randomUUID()"},
		},
	}

	output := DryRun(fix)

	if !strings.Contains(output, "--- src/app.js") {
		t.Errorf("expected file header, got:\n%s", output)
	}
	if !strings.Contains(output, "remove console.log") {
		t.Errorf("expected description, got:\n%s", output)
	}
	if !strings.Contains(output, "- console.log('test')") {
		t.Errorf("expected removal line, got:\n%s", output)
	}
	if !strings.Contains(output, "+ crypto.randomUUID()") {
		t.Errorf("expected addition line, got:\n%s", output)
	}
}

// --- helpers ---

func writeFile(t *testing.T, dir, name, content string) {
	t.Helper()
	path := filepath.Join(dir, name)
	if err := os.MkdirAll(filepath.Dir(path), 0755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(path, []byte(content), 0644); err != nil {
		t.Fatal(err)
	}
}

func readFile(t *testing.T, dir, name string) string {
	t.Helper()
	data, err := os.ReadFile(filepath.Join(dir, name))
	if err != nil {
		t.Fatal(err)
	}
	return string(data)
}
