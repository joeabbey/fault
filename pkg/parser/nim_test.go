package parser

import (
	"os"
	"testing"
)

func TestNimParserLanguage(t *testing.T) {
	p := NewNimParser()
	if p.Language() != "nim" {
		t.Errorf("expected nim, got %q", p.Language())
	}
}

func TestNimParserImports(t *testing.T) {
	src := `import os
import strutils
from json import parseJson, JsonNode
include helpers
`
	p := NewNimParser()
	pf, err := p.Parse("main.nim", []byte(src))
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	if len(pf.Imports) != 4 {
		t.Fatalf("expected 4 imports, got %d", len(pf.Imports))
	}
	if pf.Imports[0].Path != "os" {
		t.Errorf("expected import os, got %q", pf.Imports[0].Path)
	}
	if pf.Imports[2].Path != "json" || len(pf.Imports[2].Names) != 2 {
		t.Errorf("expected from json import with 2 names, got path=%q names=%v", pf.Imports[2].Path, pf.Imports[2].Names)
	}
	if pf.Imports[3].Path != "helpers" {
		t.Errorf("expected include helpers, got %q", pf.Imports[3].Path)
	}
}

func TestNimParserFunctions(t *testing.T) {
	src := `proc init*(name: string): Config =
  discard
proc internalHelper(x: int): int =
  result = x * 2
func pureAdd*(x, y: int): int =
  result = x + y
method render*(self: Config): string =
  result = "test"
`
	p := NewNimParser()
	pf, err := p.Parse("main.nim", []byte(src))
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	symMap := make(map[string]Symbol)
	for _, s := range pf.Symbols {
		symMap[s.Name] = s
	}
	if s, ok := symMap["init"]; !ok || !s.Exported {
		t.Errorf("expected exported proc init, got %+v", symMap["init"])
	}
	if s, ok := symMap["internalHelper"]; !ok || s.Exported {
		t.Errorf("expected private proc internalHelper, got %+v", symMap["internalHelper"])
	}
	if s, ok := symMap["pureAdd"]; !ok || !s.Exported || s.Kind != "function" {
		t.Errorf("expected exported func pureAdd, got %+v", symMap["pureAdd"])
	}
	if s, ok := symMap["render"]; !ok || !s.Exported || s.Kind != "method" {
		t.Errorf("expected exported method render, got %+v", symMap["render"])
	}
}

func TestNimParserExports(t *testing.T) {
	src := `proc public*(x: int): int =
  result = x
proc private(x: int): int =
  result = x
`
	p := NewNimParser()
	pf, err := p.Parse("lib.nim", []byte(src))
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	exportNames := make(map[string]bool)
	for _, exp := range pf.Exports {
		exportNames[exp.Name] = true
	}
	if !exportNames["public"] {
		t.Error("expected public to be exported")
	}
	if exportNames["private"] {
		t.Error("private should not be exported")
	}
}

func TestNimParserEmptyFile(t *testing.T) {
	p := NewNimParser()
	pf, err := p.Parse("empty.nim", []byte(""))
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	if len(pf.Imports) != 0 || len(pf.Exports) != 0 || len(pf.Symbols) != 0 {
		t.Error("expected empty results for empty file")
	}
}

func TestNimParserCommentsSkipped(t *testing.T) {
	src := `# This is a comment
#[ This is a
   block comment ]#
import os
proc realProc*(x: int): int =
  result = x
`
	p := NewNimParser()
	pf, err := p.Parse("main.nim", []byte(src))
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	if len(pf.Imports) != 1 {
		t.Errorf("expected 1 import, got %d", len(pf.Imports))
	}
	if len(pf.Exports) != 1 {
		t.Errorf("expected 1 export, got %d", len(pf.Exports))
	}
}

func TestNimParserTypes(t *testing.T) {
	src := `  Config* = object
    name*: string
  InternalState = enum
    idle, running
`
	p := NewNimParser()
	pf, err := p.Parse("types.nim", []byte(src))
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	symMap := make(map[string]Symbol)
	for _, s := range pf.Symbols {
		symMap[s.Name] = s
	}
	if s, ok := symMap["Config"]; !ok || !s.Exported || s.Kind != "type" {
		t.Errorf("expected exported type Config, got %+v", symMap["Config"])
	}
	if s, ok := symMap["InternalState"]; !ok || s.Exported || s.Kind != "type" {
		t.Errorf("expected private type InternalState, got %+v", symMap["InternalState"])
	}
}

func TestNimParserSampleFile(t *testing.T) {
	content, err := os.ReadFile("../../testdata/nim/sample.nim")
	if err != nil {
		t.Fatalf("reading fixture: %v", err)
	}
	p := NewNimParser()
	pf, err := p.Parse("sample.nim", content)
	if err != nil {
		t.Fatalf("parsing: %v", err)
	}
	if pf.Language != "nim" {
		t.Errorf("expected language nim, got %q", pf.Language)
	}
	if len(pf.Imports) < 3 {
		t.Errorf("expected at least 3 imports, got %d", len(pf.Imports))
	}
	if len(pf.Symbols) < 5 {
		t.Errorf("expected at least 5 symbols, got %d", len(pf.Symbols))
	}
}
