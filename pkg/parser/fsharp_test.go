package parser

import (
	"os"
	"testing"
)

func TestFsharpParserLanguage(t *testing.T) {
	p := NewFsharpParser()
	if p.Language() != "fsharp" {
		t.Errorf("expected 'fsharp', got %q", p.Language())
	}
}

func TestFsharpImports(t *testing.T) {
	src := `open System
open System.IO
open System.Collections.Generic
`
	p := NewFsharpParser()
	pf, err := p.Parse("app.fs", []byte(src))
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(pf.Imports) != 3 {
		t.Fatalf("expected 3 imports, got %d", len(pf.Imports))
	}
}

func TestFsharpFunctions(t *testing.T) {
	src := `let greet name =
    sprintf "Hello, %s" name

let add x y = x + y

let private helper msg =
    printfn "%s" msg
`
	p := NewFsharpParser()
	pf, err := p.Parse("funcs.fs", []byte(src))
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	publicCount := 0
	privateCount := 0
	for _, s := range pf.Symbols {
		if s.Exported {
			publicCount++
		} else {
			privateCount++
		}
	}
	if publicCount != 2 {
		t.Errorf("expected 2 public bindings, got %d", publicCount)
	}
	if privateCount != 1 {
		t.Errorf("expected 1 private binding, got %d", privateCount)
	}
}

func TestFsharpEmptyFile(t *testing.T) {
	p := NewFsharpParser()
	pf, err := p.Parse("empty.fs", []byte(""))
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(pf.Imports) != 0 || len(pf.Symbols) != 0 {
		t.Error("expected empty results")
	}
}

func TestFsharpCommentsSkipped(t *testing.T) {
	src := `// open System
(* open System.IO *)
// let greet name = "hi"
`
	p := NewFsharpParser()
	pf, err := p.Parse("comments.fs", []byte(src))
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(pf.Imports) != 0 || len(pf.Symbols) != 0 {
		t.Error("expected empty results from comments")
	}
}

func TestFsharpSampleFile(t *testing.T) {
	content, err := os.ReadFile("../../testdata/fsharp/sample.fs")
	if err != nil {
		t.Fatalf("failed to read sample: %v", err)
	}
	p := NewFsharpParser()
	pf, err := p.Parse("sample.fs", content)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(pf.Imports) != 3 {
		t.Errorf("expected 3 imports, got %d", len(pf.Imports))
	}
	if len(pf.Symbols) < 3 {
		t.Errorf("expected at least 3 symbols, got %d", len(pf.Symbols))
	}
}
