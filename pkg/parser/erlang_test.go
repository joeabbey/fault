package parser

import (
	"os"
	"testing"
)

func TestErlangParserLanguage(t *testing.T) {
	p := NewErlangParser()
	if p.Language() != "erlang" {
		t.Errorf("expected 'erlang', got %q", p.Language())
	}
}

func TestErlangImports(t *testing.T) {
	src := `-include("records.hrl").
-include_lib("stdlib/include/assert.hrl").
`
	p := NewErlangParser()
	pf, err := p.Parse("app.erl", []byte(src))
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(pf.Imports) != 2 {
		t.Fatalf("expected 2 imports, got %d", len(pf.Imports))
	}
}

func TestErlangFunctions(t *testing.T) {
	src := `-module(mymod).
-export([start/0, greet/1]).

start() ->
    ok.

greet(Name) ->
    io:format("Hello ~s~n", [Name]).

helper(X) ->
    X * 2.
`
	p := NewErlangParser()
	pf, err := p.Parse("mymod.erl", []byte(src))
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	exported := 0
	unexported := 0
	for _, s := range pf.Symbols {
		if s.Kind == "function" {
			if s.Exported {
				exported++
			} else {
				unexported++
			}
		}
	}
	if exported != 2 {
		t.Errorf("expected 2 exported functions, got %d", exported)
	}
	if unexported != 1 {
		t.Errorf("expected 1 unexported function, got %d", unexported)
	}
}

func TestErlangEmptyFile(t *testing.T) {
	p := NewErlangParser()
	pf, err := p.Parse("empty.erl", []byte(""))
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(pf.Imports) != 0 || len(pf.Symbols) != 0 {
		t.Error("expected empty results")
	}
}

func TestErlangCommentsSkipped(t *testing.T) {
	src := `% -include("fake.hrl").
% start() -> ok.
`
	p := NewErlangParser()
	pf, err := p.Parse("comments.erl", []byte(src))
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(pf.Imports) != 0 || len(pf.Symbols) != 0 {
		t.Error("expected empty results from comments")
	}
}

func TestErlangSampleFile(t *testing.T) {
	content, err := os.ReadFile("../../testdata/erlang/sample.erl")
	if err != nil {
		t.Fatalf("failed to read sample: %v", err)
	}
	p := NewErlangParser()
	pf, err := p.Parse("sample.erl", content)
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
