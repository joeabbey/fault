package parser

import (
	"os"
	"testing"
)

func TestFortranParserLanguage(t *testing.T) {
	p := NewFortranParser()
	if p.Language() != "fortran" {
		t.Errorf("expected 'fortran', got %q", p.Language())
	}
}

func TestFortranImports(t *testing.T) {
	src := "use iso_fortran_env\nuse mpi\n"
	p := NewFortranParser()
	pf, err := p.Parse("app.f90", []byte(src))
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(pf.Imports) != 2 {
		t.Fatalf("expected 2 imports, got %d", len(pf.Imports))
	}
}

func TestFortranFunctions(t *testing.T) {
	src := "subroutine greet(name)\n  character(*), intent(in) :: name\n  print *, 'Hello, ', name\nend subroutine\n\ninteger function add(a, b)\n  integer, intent(in) :: a, b\n  add = a + b\nend function\n"
	p := NewFortranParser()
	pf, err := p.Parse("funcs.f90", []byte(src))
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
		t.Error("expected greet subroutine")
	}
	if !names["add"] {
		t.Error("expected add function")
	}
}

func TestFortranEmptyFile(t *testing.T) {
	p := NewFortranParser()
	pf, err := p.Parse("empty.f90", []byte(""))
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(pf.Imports) != 0 || len(pf.Symbols) != 0 {
		t.Error("expected empty results")
	}
}

func TestFortranCommentsSkipped(t *testing.T) {
	src := "! use fake_module\n! subroutine hidden()\n"
	p := NewFortranParser()
	pf, err := p.Parse("comments.f90", []byte(src))
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(pf.Imports) != 0 || len(pf.Symbols) != 0 {
		t.Error("expected empty results from comments")
	}
}

func TestFortranSampleFile(t *testing.T) {
	content, err := os.ReadFile("../../testdata/fortran/sample.f90")
	if err != nil {
		t.Fatalf("failed to read sample: %v", err)
	}
	p := NewFortranParser()
	pf, err := p.Parse("sample.f90", content)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(pf.Imports) < 1 {
		t.Errorf("expected at least 1 import, got %d", len(pf.Imports))
	}
	if len(pf.Symbols) < 3 {
		t.Errorf("expected at least 3 symbols, got %d", len(pf.Symbols))
	}
}
