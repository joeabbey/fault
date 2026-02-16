package parser

import (
	"os"
	"testing"
)

func TestJuliaParserLanguage(t *testing.T) {
	p := NewJuliaParser()
	if p.Language() != "julia" {
		t.Errorf("expected 'julia', got %q", p.Language())
	}
}

func TestJuliaImports(t *testing.T) {
	src := "using LinearAlgebra\nimport JSON\ninclude(\"utils.jl\")\n"
	p := NewJuliaParser()
	pf, err := p.Parse("app.jl", []byte(src))
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(pf.Imports) != 3 {
		t.Fatalf("expected 3 imports, got %d", len(pf.Imports))
	}
}

func TestJuliaFunctions(t *testing.T) {
	src := "function greet(name)\n  println(\"Hello, $name\")\nend\n\nfunction compute(x, y)\n  return x + y\nend\n"
	p := NewJuliaParser()
	pf, err := p.Parse("funcs.jl", []byte(src))
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
	if !names["compute"] {
		t.Error("expected compute function")
	}
}

func TestJuliaEmptyFile(t *testing.T) {
	p := NewJuliaParser()
	pf, err := p.Parse("empty.jl", []byte(""))
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(pf.Imports) != 0 || len(pf.Symbols) != 0 {
		t.Error("expected empty results")
	}
}

func TestJuliaCommentsSkipped(t *testing.T) {
	src := "# using FakeModule\n# function hidden() end\n"
	p := NewJuliaParser()
	pf, err := p.Parse("comments.jl", []byte(src))
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(pf.Imports) != 0 || len(pf.Symbols) != 0 {
		t.Error("expected empty results from comments")
	}
}

func TestJuliaSampleFile(t *testing.T) {
	content, err := os.ReadFile("../../testdata/julia/sample.jl")
	if err != nil {
		t.Fatalf("failed to read sample: %v", err)
	}
	p := NewJuliaParser()
	pf, err := p.Parse("sample.jl", content)
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
