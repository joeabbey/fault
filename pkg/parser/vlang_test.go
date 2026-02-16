package parser

import (
	"os"
	"testing"
)

func TestVlangParserLanguage(t *testing.T) {
	p := NewVlangParser()
	if p.Language() != "vlang" {
		t.Errorf("expected vlang, got %q", p.Language())
	}
}

func TestVlangParserImports(t *testing.T) {
	src := `import os
import json
import net.http { Request, Response }
`
	p := NewVlangParser()
	pf, err := p.Parse("main.v", []byte(src))
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	if len(pf.Imports) != 3 {
		t.Fatalf("expected 3 imports, got %d", len(pf.Imports))
	}
	if pf.Imports[0].Path != "os" || pf.Imports[0].Names[0] != "os" {
		t.Errorf("expected import os, got path=%q names=%v", pf.Imports[0].Path, pf.Imports[0].Names)
	}
	if pf.Imports[2].Path != "net.http" || len(pf.Imports[2].Names) != 2 {
		t.Errorf("expected import net.http with 2 names, got path=%q names=%v", pf.Imports[2].Path, pf.Imports[2].Names)
	}
}

func TestVlangParserFunctions(t *testing.T) {
	src := `pub fn new_config(name string) Config {}
fn internal_helper(x int, y int) int {}
pub fn (c Config) to_string() string {}
`
	p := NewVlangParser()
	pf, err := p.Parse("main.v", []byte(src))
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	symMap := make(map[string]Symbol)
	for _, s := range pf.Symbols {
		symMap[s.Name] = s
	}
	if s, ok := symMap["new_config"]; !ok || !s.Exported || s.Kind != "function" {
		t.Errorf("expected exported function new_config, got %+v", symMap["new_config"])
	}
	if s, ok := symMap["internal_helper"]; !ok || s.Exported {
		t.Errorf("expected private function internal_helper, got %+v", symMap["internal_helper"])
	}
	if s, ok := symMap["to_string"]; !ok || !s.Exported || s.Kind != "method" {
		t.Errorf("expected exported method to_string, got %+v", symMap["to_string"])
	}
}

func TestVlangParserExports(t *testing.T) {
	src := `pub fn public_func() {}
fn private_func() {}
pub struct PublicStruct {}
struct PrivateStruct {}
pub const version = '1.0.0'
const internal_limit = 100
`
	p := NewVlangParser()
	pf, err := p.Parse("lib.v", []byte(src))
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	exportNames := make(map[string]bool)
	for _, exp := range pf.Exports {
		exportNames[exp.Name] = true
	}
	if !exportNames["public_func"] {
		t.Error("expected public_func to be exported")
	}
	if exportNames["private_func"] {
		t.Error("private_func should not be exported")
	}
	if !exportNames["PublicStruct"] {
		t.Error("expected PublicStruct to be exported")
	}
	if exportNames["PrivateStruct"] {
		t.Error("PrivateStruct should not be exported")
	}
}

func TestVlangParserEmptyFile(t *testing.T) {
	p := NewVlangParser()
	pf, err := p.Parse("empty.v", []byte(""))
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	if len(pf.Imports) != 0 || len(pf.Exports) != 0 || len(pf.Symbols) != 0 {
		t.Error("expected empty results for empty file")
	}
}

func TestVlangParserCommentsSkipped(t *testing.T) {
	src := `// This is a comment
/* This is a
   block comment */
import os
pub fn real_func() {}
`
	p := NewVlangParser()
	pf, err := p.Parse("main.v", []byte(src))
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

func TestVlangParserTypesAndInterfaces(t *testing.T) {
	src := `pub struct Config {}
pub enum Status {}
pub interface Handler {}
pub type Callback = fn (string) bool
`
	p := NewVlangParser()
	pf, err := p.Parse("types.v", []byte(src))
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	symMap := make(map[string]Symbol)
	for _, s := range pf.Symbols {
		symMap[s.Name] = s
	}
	if s, ok := symMap["Config"]; !ok || s.Kind != "struct" {
		t.Errorf("expected struct Config, got %+v", symMap["Config"])
	}
	if s, ok := symMap["Status"]; !ok || s.Kind != "type" {
		t.Errorf("expected type Status, got %+v", symMap["Status"])
	}
	if s, ok := symMap["Handler"]; !ok || s.Kind != "interface" {
		t.Errorf("expected interface Handler, got %+v", symMap["Handler"])
	}
	if s, ok := symMap["Callback"]; !ok || s.Kind != "type" {
		t.Errorf("expected type Callback, got %+v", symMap["Callback"])
	}
}

func TestVlangParserSampleFile(t *testing.T) {
	content, err := os.ReadFile("../../testdata/vlang/sample.v")
	if err != nil {
		t.Fatalf("reading fixture: %v", err)
	}
	p := NewVlangParser()
	pf, err := p.Parse("sample.v", content)
	if err != nil {
		t.Fatalf("parsing: %v", err)
	}
	if pf.Language != "vlang" {
		t.Errorf("expected language vlang, got %q", pf.Language)
	}
	if len(pf.Imports) < 3 {
		t.Errorf("expected at least 3 imports, got %d", len(pf.Imports))
	}
	if len(pf.Symbols) < 5 {
		t.Errorf("expected at least 5 symbols, got %d", len(pf.Symbols))
	}
}
