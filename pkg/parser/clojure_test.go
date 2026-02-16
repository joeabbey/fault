package parser

import (
	"os"
	"testing"
)

func TestClojureParserLanguage(t *testing.T) {
	p := NewClojureParser()
	if p.Language() != "clojure" {
		t.Errorf("expected 'clojure', got %q", p.Language())
	}
}

func TestClojureImports(t *testing.T) {
	src := `(ns myapp.core
  (:require [clojure.string :as str]
            [clojure.set :as set])
  (:import (java.util Date)))
`
	p := NewClojureParser()
	pf, err := p.Parse("core.clj", []byte(src))
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(pf.Imports) < 2 {
		t.Fatalf("expected at least 2 imports, got %d", len(pf.Imports))
	}
}

func TestClojureFunctions(t *testing.T) {
	src := `(defn greet [name]
  (str "Hello, " name))

(defn- helper [x]
  (* x 2))
`
	p := NewClojureParser()
	pf, err := p.Parse("funcs.clj", []byte(src))
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	publicCount := 0
	privateCount := 0
	for _, s := range pf.Symbols {
		if s.Kind == "function" {
			if s.Exported {
				publicCount++
			} else {
				privateCount++
			}
		}
	}
	if publicCount != 1 {
		t.Errorf("expected 1 public function, got %d", publicCount)
	}
	if privateCount != 1 {
		t.Errorf("expected 1 private function, got %d", privateCount)
	}
}

func TestClojureEmptyFile(t *testing.T) {
	p := NewClojureParser()
	pf, err := p.Parse("empty.clj", []byte(""))
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(pf.Imports) != 0 || len(pf.Symbols) != 0 {
		t.Error("expected empty results")
	}
}

func TestClojureCommentsSkipped(t *testing.T) {
	src := `; (defn commented-out [x] x)
; (:require [fake.ns])
`
	p := NewClojureParser()
	pf, err := p.Parse("comments.clj", []byte(src))
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(pf.Imports) != 0 || len(pf.Symbols) != 0 {
		t.Error("expected empty results from comments")
	}
}

func TestClojureSampleFile(t *testing.T) {
	content, err := os.ReadFile("../../testdata/clojure/sample.clj")
	if err != nil {
		t.Fatalf("failed to read sample: %v", err)
	}
	p := NewClojureParser()
	pf, err := p.Parse("sample.clj", content)
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
