package parser

import (
	"os"
	"testing"
)

func TestOcamlParserLanguage(t *testing.T) {
	p := NewOcamlParser()
	if p.Language() != "ocaml" {
		t.Errorf("expected 'ocaml', got %q", p.Language())
	}
}

func TestOcamlImports(t *testing.T) {
	src := `open Printf
open List
`
	p := NewOcamlParser()
	pf, err := p.Parse("app.ml", []byte(src))
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(pf.Imports) != 2 {
		t.Fatalf("expected 2 imports, got %d", len(pf.Imports))
	}
}

func TestOcamlFunctions(t *testing.T) {
	src := `let greet name =
  sprintf "Hello, %s" name

let rec factorial n =
  if n <= 1 then 1
  else n * factorial (n - 1)
`
	p := NewOcamlParser()
	pf, err := p.Parse("funcs.ml", []byte(src))
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	names := make(map[string]bool)
	for _, s := range pf.Symbols {
		if s.Kind == "function" {
			names[s.Name] = true
		}
	}
	if !names["greet"] {
		t.Error("expected greet function")
	}
	if !names["factorial"] {
		t.Error("expected factorial function")
	}
}

func TestOcamlEmptyFile(t *testing.T) {
	p := NewOcamlParser()
	pf, err := p.Parse("empty.ml", []byte(""))
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(pf.Imports) != 0 || len(pf.Symbols) != 0 {
		t.Error("expected empty results")
	}
}

func TestOcamlCommentsSkipped(t *testing.T) {
	src := `(* open Printf *)
(* let greet name = "hi" *)
`
	p := NewOcamlParser()
	pf, err := p.Parse("comments.ml", []byte(src))
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(pf.Imports) != 0 || len(pf.Symbols) != 0 {
		t.Error("expected empty results from comments")
	}
}

func TestOcamlSampleFile(t *testing.T) {
	content, err := os.ReadFile("../../testdata/ocaml/sample.ml")
	if err != nil {
		t.Fatalf("failed to read sample: %v", err)
	}
	p := NewOcamlParser()
	pf, err := p.Parse("sample.ml", content)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(pf.Imports) != 2 {
		t.Errorf("expected 2 imports, got %d", len(pf.Imports))
	}
	if len(pf.Symbols) < 3 {
		t.Errorf("expected at least 3 symbols, got %d", len(pf.Symbols))
	}
}
