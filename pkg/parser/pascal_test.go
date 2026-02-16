package parser

import (
	"os"
	"testing"
)

func TestPascalParserLanguage(t *testing.T) {
	p := NewPascalParser()
	if p.Language() != "pascal" {
		t.Errorf("expected 'pascal', got %q", p.Language())
	}
}

func TestPascalImports(t *testing.T) {
	src := "uses SysUtils, Classes, Math;\n"
	p := NewPascalParser()
	pf, err := p.Parse("app.pas", []byte(src))
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(pf.Imports) != 3 {
		t.Fatalf("expected 3 imports, got %d", len(pf.Imports))
	}
}

func TestPascalProceduresAndFunctions(t *testing.T) {
	src := "procedure Hello;\nbegin\n  WriteLn('Hello');\nend;\n\nfunction Add(A, B: Integer): Integer;\nbegin\n  Result := A + B;\nend;\n"
	p := NewPascalParser()
	pf, err := p.Parse("hello.pas", []byte(src))
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

func TestPascalEmptyFile(t *testing.T) {
	p := NewPascalParser()
	pf, err := p.Parse("empty.pas", []byte(""))
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(pf.Imports) != 0 || len(pf.Symbols) != 0 {
		t.Error("expected empty results")
	}
}

func TestPascalCommentsSkipped(t *testing.T) {
	src := "// uses FakeUnit;\n// procedure Hidden;\n"
	p := NewPascalParser()
	pf, err := p.Parse("comments.pas", []byte(src))
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(pf.Imports) != 0 || len(pf.Symbols) != 0 {
		t.Error("expected empty results from comments")
	}
}

func TestPascalSampleFile(t *testing.T) {
	content, err := os.ReadFile("../../testdata/pascal/sample.pas")
	if err != nil {
		t.Fatalf("failed to read sample: %v", err)
	}
	p := NewPascalParser()
	pf, err := p.Parse("sample.pas", content)
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
