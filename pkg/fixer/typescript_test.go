package fixer

import (
	"strings"
	"testing"

	"github.com/joeabbey/fault/pkg/analyzer"
)

func TestFixAnyToUnknown(t *testing.T) {
	tests := []struct {
		name    string
		input   string
		line    int
		expect  string
		fixNil  bool
	}{
		{
			name:   "param any",
			input:  "function foo(x: any): void {}\n",
			line:   1,
			expect: "unknown",
		},
		{
			name:   "as any cast",
			input:  "const val = response as any;\n",
			line:   1,
			expect: "as unknown",
		},
		{
			name:   "angle bracket any",
			input:  "const val = <any>response;\n",
			line:   1,
			expect: "<unknown>",
		},
		{
			name:   "variable any",
			input:  "let data: any = null;\n",
			line:   1,
			expect: "unknown",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			dir := t.TempDir()
			writeFile(t, dir, "test.ts", tt.input)

			issue := analyzer.Issue{
				ID:    "patterns/ts-any-type",
				FixID: "ts-any-to-unknown",
				File:  "test.ts",
				Line:  tt.line,
			}

			fixer := NewTypeScriptFixer()
			fix := fixer.GenerateFix(issue, dir)
			if tt.fixNil {
				if fix != nil {
					t.Fatal("expected nil fix")
				}
				return
			}
			if fix == nil {
				t.Fatal("expected fix, got nil")
			}

			if err := Apply(fix, dir); err != nil {
				t.Fatalf("Apply: %v", err)
			}

			content := readFile(t, dir, "test.ts")
			if !strings.Contains(content, tt.expect) {
				t.Errorf("expected %q in result, got:\n%s", tt.expect, content)
			}
			if strings.Contains(content, ": any") || strings.Contains(content, "as any") || strings.Contains(content, "<any>") {
				t.Errorf("any type should be replaced, got:\n%s", content)
			}
		})
	}
}

func TestFixRemoveIgnore(t *testing.T) {
	dir := t.TempDir()
	writeFile(t, dir, "test.ts", "const x = 1;\n// @ts-ignore\nconst y: number = 'bad';\n")

	issue := analyzer.Issue{
		ID:    "patterns/ts-ignore",
		FixID: "ts-remove-ignore",
		File:  "test.ts",
		Line:  2,
	}

	fixer := NewTypeScriptFixer()
	fix := fixer.GenerateFix(issue, dir)
	if fix == nil {
		t.Fatal("expected fix, got nil")
	}

	if err := Apply(fix, dir); err != nil {
		t.Fatalf("Apply: %v", err)
	}

	content := readFile(t, dir, "test.ts")
	if strings.Contains(content, "@ts-ignore") {
		t.Errorf("@ts-ignore should be removed, got:\n%s", content)
	}
	if !strings.Contains(content, "const x = 1") || !strings.Contains(content, "const y") {
		t.Errorf("non-ignore lines should remain, got:\n%s", content)
	}
}

func TestFixUnusedTypeImport(t *testing.T) {
	dir := t.TempDir()
	writeFile(t, dir, "test.ts", "import type { Unused } from './types';\nconst x = 1;\n")

	issue := analyzer.Issue{
		ID:    "patterns/ts-unused-type-import",
		FixID: "ts-unused-type-import",
		File:  "test.ts",
		Line:  1,
	}

	fixer := NewTypeScriptFixer()
	fix := fixer.GenerateFix(issue, dir)
	if fix == nil {
		t.Fatal("expected fix, got nil")
	}

	if err := Apply(fix, dir); err != nil {
		t.Fatalf("Apply: %v", err)
	}

	content := readFile(t, dir, "test.ts")
	if strings.Contains(content, "Unused") {
		t.Errorf("unused import should be removed, got:\n%s", content)
	}
	if !strings.Contains(content, "const x = 1") {
		t.Errorf("other lines should remain, got:\n%s", content)
	}
}

func TestTypeScriptFixerFixIDs(t *testing.T) {
	fixer := NewTypeScriptFixer()
	ids := fixer.FixIDs()
	expected := map[string]bool{
		"ts-any-to-unknown":    true,
		"ts-remove-ignore":     true,
		"ts-unused-type-import": true,
	}
	for _, id := range ids {
		if !expected[id] {
			t.Errorf("unexpected FixID: %s", id)
		}
	}
	if len(ids) != len(expected) {
		t.Errorf("expected %d FixIDs, got %d", len(expected), len(ids))
	}
}
