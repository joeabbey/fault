package fixer

import (
	"strings"
	"testing"

	"github.com/joeabbey/fault/pkg/analyzer"
)

func TestFixGoSingleImport(t *testing.T) {
	dir := t.TempDir()
	writeFile(t, dir, "main.go", "package main\n\nimport \"pkg/deleted\"\n\nfunc main() {}\n")

	issue := analyzer.Issue{
		ID:    "import-broken-main.go-3",
		FixID: "import-broken",
		File:  "main.go",
		Line:  3,
	}

	fixer := NewImportFixer()
	fix := fixer.GenerateFix(issue, dir)
	if fix == nil {
		t.Fatal("expected fix, got nil")
	}

	if err := Apply(fix, dir); err != nil {
		t.Fatalf("Apply: %v", err)
	}

	content := readFile(t, dir, "main.go")
	if strings.Contains(content, "pkg/deleted") {
		t.Errorf("broken import should be removed, got:\n%s", content)
	}
	if !strings.Contains(content, "package main") {
		t.Errorf("package declaration should remain, got:\n%s", content)
	}
}

func TestFixGoBlockImport(t *testing.T) {
	dir := t.TempDir()
	writeFile(t, dir, "main.go", "package main\n\nimport (\n\t\"fmt\"\n\t\"pkg/deleted\"\n)\n\nfunc main() {\n\tfmt.Println(\"hello\")\n}\n")

	issue := analyzer.Issue{
		ID:    "import-broken-main.go-5",
		FixID: "import-broken",
		File:  "main.go",
		Line:  5,
	}

	fixer := NewImportFixer()
	fix := fixer.GenerateFix(issue, dir)
	if fix == nil {
		t.Fatal("expected fix, got nil")
	}

	if err := Apply(fix, dir); err != nil {
		t.Fatalf("Apply: %v", err)
	}

	content := readFile(t, dir, "main.go")
	if strings.Contains(content, "pkg/deleted") {
		t.Errorf("broken import should be removed, got:\n%s", content)
	}
	if !strings.Contains(content, "\"fmt\"") {
		t.Errorf("valid import should remain, got:\n%s", content)
	}
}

func TestFixTSNamedImport(t *testing.T) {
	dir := t.TempDir()
	writeFile(t, dir, "app.ts", "import { a, b } from './foo'\n\nconsole.log(a)\n")

	issue := analyzer.Issue{
		ID:      "import-name-app.ts-1-b",
		FixID:   "import-missing-export",
		File:    "app.ts",
		Line:    1,
		Message: `Name "b" is not exported from "./foo"`,
	}

	fixer := NewImportFixer()
	fix := fixer.GenerateFix(issue, dir)
	if fix == nil {
		t.Fatal("expected fix, got nil")
	}

	if err := Apply(fix, dir); err != nil {
		t.Fatalf("Apply: %v", err)
	}

	content := readFile(t, dir, "app.ts")
	if strings.Contains(content, " b ") || strings.Contains(content, ", b}") || strings.Contains(content, ",b}") {
		t.Errorf("missing export b should be removed, got:\n%s", content)
	}
	if !strings.Contains(content, "a") {
		t.Errorf("valid import a should remain, got:\n%s", content)
	}
	if !strings.Contains(content, "from './foo'") {
		t.Errorf("import source should remain, got:\n%s", content)
	}
}

func TestFixTSFullImportRemoval(t *testing.T) {
	dir := t.TempDir()
	writeFile(t, dir, "app.ts", "import { b } from './foo'\n\nconst x = 1\n")

	issue := analyzer.Issue{
		ID:      "import-name-app.ts-1-b",
		FixID:   "import-missing-export",
		File:    "app.ts",
		Line:    1,
		Message: `Name "b" is not exported from "./foo"`,
	}

	fixer := NewImportFixer()
	fix := fixer.GenerateFix(issue, dir)
	if fix == nil {
		t.Fatal("expected fix, got nil")
	}

	if err := Apply(fix, dir); err != nil {
		t.Fatalf("Apply: %v", err)
	}

	content := readFile(t, dir, "app.ts")
	if strings.Contains(content, "import") {
		t.Errorf("entire import should be removed, got:\n%s", content)
	}
	if !strings.Contains(content, "const x = 1") {
		t.Errorf("other code should remain, got:\n%s", content)
	}
}

func TestFixPythonImport(t *testing.T) {
	dir := t.TempDir()
	writeFile(t, dir, "app.py", "from .utils import a, b\n\nprint(a)\n")

	issue := analyzer.Issue{
		ID:      "import-name-app.py-1-b",
		FixID:   "import-missing-export",
		File:    "app.py",
		Line:    1,
		Message: `Name "b" may not be exported from ".utils"`,
	}

	fixer := NewImportFixer()
	fix := fixer.GenerateFix(issue, dir)
	if fix == nil {
		t.Fatal("expected fix, got nil")
	}

	if err := Apply(fix, dir); err != nil {
		t.Fatalf("Apply: %v", err)
	}

	content := readFile(t, dir, "app.py")
	if strings.Contains(content, ", b") || strings.Contains(content, " b,") {
		t.Errorf("missing export b should be removed, got:\n%s", content)
	}
	if !strings.Contains(content, "from .utils import a") {
		t.Errorf("valid import a should remain, got:\n%s", content)
	}
}
