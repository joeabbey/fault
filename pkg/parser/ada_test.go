package parser

import (
	"os"
	"testing"
)

func TestAdaParserLanguage(t *testing.T) {
	p := NewAdaParser()
	if p.Language() != "ada" {
		t.Errorf("expected 'ada', got %q", p.Language())
	}
}

func TestAdaImports(t *testing.T) {
	src := "with Ada.Text_IO;\nwith Ada.Integer_Text_IO;\nuse Ada.Text_IO;\n"
	p := NewAdaParser()
	pf, err := p.Parse("app.adb", []byte(src))
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(pf.Imports) != 3 {
		t.Fatalf("expected 3 imports, got %d", len(pf.Imports))
	}
}

func TestAdaProceduresAndFunctions(t *testing.T) {
	src := "procedure Hello is\nbegin\n   null;\nend Hello;\n\nfunction Add(A, B : Integer) return Integer is\nbegin\n   return A + B;\nend Add;\n"
	p := NewAdaParser()
	pf, err := p.Parse("hello.adb", []byte(src))
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	names := make(map[string]bool)
	for _, s := range pf.Symbols {
		if s.Kind == "function" {
			names[s.Name] = true
		}
	}
	if !names["Hello"] {
		t.Error("expected Hello procedure")
	}
	if !names["Add"] {
		t.Error("expected Add function")
	}
}

func TestAdaEmptyFile(t *testing.T) {
	p := NewAdaParser()
	pf, err := p.Parse("empty.adb", []byte(""))
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(pf.Imports) != 0 || len(pf.Symbols) != 0 {
		t.Error("expected empty results")
	}
}

func TestAdaCommentsSkipped(t *testing.T) {
	src := "-- with Ada.Text_IO;\n-- procedure Hidden is\n"
	p := NewAdaParser()
	pf, err := p.Parse("comments.adb", []byte(src))
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(pf.Imports) != 0 || len(pf.Symbols) != 0 {
		t.Error("expected empty results from comments")
	}
}

func TestAdaSampleFile(t *testing.T) {
	content, err := os.ReadFile("../../testdata/ada/sample.adb")
	if err != nil {
		t.Fatalf("failed to read sample: %v", err)
	}
	p := NewAdaParser()
	pf, err := p.Parse("sample.adb", content)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(pf.Imports) < 2 {
		t.Errorf("expected at least 2 imports, got %d", len(pf.Imports))
	}
	if len(pf.Symbols) < 3 {
		t.Errorf("expected at least 3 symbols, got %d", len(pf.Symbols))
	}
}
