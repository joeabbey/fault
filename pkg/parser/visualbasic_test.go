package parser

import (
	"os"
	"testing"
)

func TestVisualBasicParserLanguage(t *testing.T) {
	p := NewVisualBasicParser()
	if p.Language() != "visualbasic" {
		t.Errorf("expected 'visualbasic', got %q", p.Language())
	}
}

func TestVisualBasicImports(t *testing.T) {
	src := "Imports System\nImports System.Collections.Generic\nImports Microsoft.VisualBasic\n"
	p := NewVisualBasicParser()
	pf, err := p.Parse("app.vb", []byte(src))
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(pf.Imports) != 3 {
		t.Fatalf("expected 3 imports, got %d", len(pf.Imports))
	}
}

func TestVisualBasicClassAndMethods(t *testing.T) {
	src := `Public Class Calculator
    Public Function Add(a As Integer, b As Integer) As Integer
        Return a + b
    End Function
    Private Sub Reset()
    End Sub
End Class
`
	p := NewVisualBasicParser()
	pf, err := p.Parse("calc.vb", []byte(src))
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	names := make(map[string]bool)
	for _, s := range pf.Symbols {
		names[s.Name] = true
	}
	if !names["Calculator"] {
		t.Error("expected Calculator class")
	}
	if !names["Add"] {
		t.Error("expected Add function")
	}
	if !names["Reset"] {
		t.Error("expected Reset sub")
	}
}

func TestVisualBasicEmptyFile(t *testing.T) {
	p := NewVisualBasicParser()
	pf, err := p.Parse("empty.vb", []byte(""))
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(pf.Imports) != 0 || len(pf.Symbols) != 0 {
		t.Error("expected empty results")
	}
}

func TestVisualBasicCommentsSkipped(t *testing.T) {
	src := "' Imports System\n' Public Class Hidden\nREM This is a remark\n"
	p := NewVisualBasicParser()
	pf, err := p.Parse("comments.vb", []byte(src))
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(pf.Imports) != 0 || len(pf.Symbols) != 0 {
		t.Error("expected empty results from comments")
	}
}

func TestVisualBasicSampleFile(t *testing.T) {
	content, err := os.ReadFile("../../testdata/visualbasic/sample.vb")
	if err != nil {
		t.Fatalf("failed to read sample: %v", err)
	}
	p := NewVisualBasicParser()
	pf, err := p.Parse("sample.vb", content)
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
