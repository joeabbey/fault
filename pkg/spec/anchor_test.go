package spec

import (
	"os"
	"path/filepath"
	"testing"
)

func TestAnchorPattern(t *testing.T) {
	tests := []struct {
		input string
		want  []string // expected captured group 1 values
	}{
		{"// spec:REQ-001", []string{"REQ-001"}},
		{"# spec:AUTH-42 some comment", []string{"AUTH-42"}},
		{"/* spec:PERF-100 */", []string{"PERF-100"}},
		{"// spec:REQ-001 and spec:REQ-002", []string{"REQ-001", "REQ-002"}},
		{"no anchors here", nil},
		{"spec:bad (no trailing number)", nil},
		{"spec:123-bad (starts with digit)", nil},
		{"// spec:My_Feature-7", []string{"My_Feature-7"}},
	}

	for _, tt := range tests {
		matches := AnchorPattern.FindAllStringSubmatch(tt.input, -1)
		got := make([]string, 0, len(matches))
		for _, m := range matches {
			got = append(got, m[1])
		}

		if len(got) != len(tt.want) {
			t.Errorf("input=%q: got %v, want %v", tt.input, got, tt.want)
			continue
		}
		for i := range got {
			if got[i] != tt.want[i] {
				t.Errorf("input=%q [%d]: got %q, want %q", tt.input, i, got[i], tt.want[i])
			}
		}
	}
}

func TestExtractAnchorsFromFile(t *testing.T) {
	dir := t.TempDir()
	path := filepath.Join(dir, "example.go")

	content := `package main

// spec:REQ-001 - implements authentication
func Login() {}

// spec:REQ-002 - handles user sessions
// spec:REQ-003 - session timeout
func Session() {}
`
	if err := os.WriteFile(path, []byte(content), 0644); err != nil {
		t.Fatal(err)
	}

	anchors, err := ExtractAnchorsFromFile(path)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	if len(anchors) != 3 {
		t.Fatalf("len(anchors) = %d, want 3", len(anchors))
	}

	expected := []struct {
		reqID string
		line  int
	}{
		{"REQ-001", 3},
		{"REQ-002", 6},
		{"REQ-003", 7},
	}

	for i, exp := range expected {
		if anchors[i].ReqID != exp.reqID {
			t.Errorf("anchor[%d].ReqID = %q, want %q", i, anchors[i].ReqID, exp.reqID)
		}
		if anchors[i].Line != exp.line {
			t.Errorf("anchor[%d].Line = %d, want %d", i, anchors[i].Line, exp.line)
		}
	}
}

func TestExtractAnchorsFromDir(t *testing.T) {
	dir := t.TempDir()

	// Create source files with anchors
	srcDir := filepath.Join(dir, "pkg", "auth")
	if err := os.MkdirAll(srcDir, 0755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(srcDir, "login.go"), []byte("// spec:AUTH-001\nfunc Login() {}\n"), 0644); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(srcDir, "session.go"), []byte("// spec:AUTH-002\nfunc Session() {}\n"), 0644); err != nil {
		t.Fatal(err)
	}

	// Create a file in a skipped directory
	vendorDir := filepath.Join(dir, "vendor", "lib")
	if err := os.MkdirAll(vendorDir, 0755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(vendorDir, "lib.go"), []byte("// spec:VENDOR-001\n"), 0644); err != nil {
		t.Fatal(err)
	}

	// Create a binary-like file that should be skipped
	if err := os.WriteFile(filepath.Join(dir, "data.bin"), []byte("spec:BIN-001"), 0644); err != nil {
		t.Fatal(err)
	}

	anchors, err := ExtractAnchorsFromDir(dir)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	// Should find 2 anchors (vendor skipped, .bin skipped)
	if len(anchors) != 2 {
		t.Fatalf("len(anchors) = %d, want 2; anchors: %v", len(anchors), anchors)
	}

	// Check that paths are relative
	for _, a := range anchors {
		if filepath.IsAbs(a.File) {
			t.Errorf("anchor file should be relative, got %q", a.File)
		}
	}
}

func TestGroupAnchorsByReqID(t *testing.T) {
	anchors := []Anchor{
		{ReqID: "REQ-001", File: "a.go", Line: 1},
		{ReqID: "REQ-001", File: "b.go", Line: 5},
		{ReqID: "REQ-002", File: "c.go", Line: 10},
	}

	grouped := GroupAnchorsByReqID(anchors)

	if len(grouped["REQ-001"]) != 2 {
		t.Errorf("REQ-001 count = %d, want 2", len(grouped["REQ-001"]))
	}
	if len(grouped["REQ-002"]) != 1 {
		t.Errorf("REQ-002 count = %d, want 1", len(grouped["REQ-002"]))
	}
}

func TestIsSourceFile(t *testing.T) {
	source := []string{
		"main.go", "app.ts", "index.js", "lib.py", "auth.rs",
		"config.yaml", "schema.sql", "README.md",
	}
	for _, f := range source {
		if !isSourceFile(f) {
			t.Errorf("expected %q to be a source file", f)
		}
	}

	nonSource := []string{
		"image.png", "data.bin", "archive.zip", "app.exe",
	}
	for _, f := range nonSource {
		if isSourceFile(f) {
			t.Errorf("expected %q to NOT be a source file", f)
		}
	}
}
