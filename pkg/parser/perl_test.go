package parser

import (
	"os"
	"testing"
)

func TestPerlParserLanguage(t *testing.T) {
	p := NewPerlParser()
	if p.Language() != "perl" {
		t.Errorf("expected 'perl', got %q", p.Language())
	}
}

func TestPerlImports(t *testing.T) {
	src := "use strict;\nuse warnings;\nuse File::Basename;\nrequire Exporter;\n"
	p := NewPerlParser()
	pf, err := p.Parse("app.pl", []byte(src))
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(pf.Imports) != 4 {
		t.Fatalf("expected 4 imports, got %d", len(pf.Imports))
	}
}

func TestPerlFunctions(t *testing.T) {
	src := "sub greet {\n  my ($name) = @_;\n  print \"Hello, $name\\n\";\n}\n\nsub _private {\n  return 1;\n}\n"
	p := NewPerlParser()
	pf, err := p.Parse("funcs.pl", []byte(src))
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
	if !names["_private"] {
		t.Error("expected _private function")
	}
}

func TestPerlEmptyFile(t *testing.T) {
	p := NewPerlParser()
	pf, err := p.Parse("empty.pl", []byte(""))
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(pf.Imports) != 0 || len(pf.Symbols) != 0 {
		t.Error("expected empty results")
	}
}

func TestPerlCommentsSkipped(t *testing.T) {
	src := "# use Fake::Module;\n# sub hidden { }\n"
	p := NewPerlParser()
	pf, err := p.Parse("comments.pl", []byte(src))
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(pf.Imports) != 0 || len(pf.Symbols) != 0 {
		t.Error("expected empty results from comments")
	}
}

func TestPerlSampleFile(t *testing.T) {
	content, err := os.ReadFile("../../testdata/perl/sample.pl")
	if err != nil {
		t.Fatalf("failed to read sample: %v", err)
	}
	p := NewPerlParser()
	pf, err := p.Parse("sample.pl", content)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(pf.Imports) < 2 {
		t.Errorf("expected at least 2 imports, got %d", len(pf.Imports))
	}
	if len(pf.Symbols) < 2 {
		t.Errorf("expected at least 2 symbols, got %d", len(pf.Symbols))
	}
}
