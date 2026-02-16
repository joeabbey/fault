package parser

import (
	"os"
	"testing"
)

func TestCrystalParserLanguage(t *testing.T) {
	p := NewCrystalParser()
	if p.Language() != "crystal" {
		t.Errorf("expected crystal, got %q", p.Language())
	}
}

func TestCrystalParserImports(t *testing.T) {
	src := `require "json"
require "http/server"
require "./models"
`
	p := NewCrystalParser()
	pf, err := p.Parse("main.cr", []byte(src))
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	if len(pf.Imports) != 3 {
		t.Fatalf("expected 3 imports, got %d", len(pf.Imports))
	}
	if pf.Imports[0].Path != "json" {
		t.Errorf("expected import json, got %q", pf.Imports[0].Path)
	}
	if pf.Imports[1].Path != "http/server" {
		t.Errorf("expected import http/server, got %q", pf.Imports[1].Path)
	}
	if pf.Imports[2].Path != "./models" {
		t.Errorf("expected import ./models, got %q", pf.Imports[2].Path)
	}
}

func TestCrystalParserFunctions(t *testing.T) {
	src := `def greet(name : String)
  puts "Hello #{name}"
end

def self.create(data : String)
  # class method
end

private def internal_method
  # private
end
`
	p := NewCrystalParser()
	pf, err := p.Parse("main.cr", []byte(src))
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	symMap := make(map[string]Symbol)
	for _, s := range pf.Symbols {
		symMap[s.Name] = s
	}
	if s, ok := symMap["greet"]; !ok || !s.Exported {
		t.Errorf("expected exported function greet, got %+v", symMap["greet"])
	}
	if s, ok := symMap["self.create"]; !ok || !s.Exported {
		t.Errorf("expected exported class method self.create, got %+v", symMap["self.create"])
	}
	if s, ok := symMap["internal_method"]; !ok || s.Exported {
		t.Errorf("expected private function internal_method, got %+v", symMap["internal_method"])
	}
}

func TestCrystalParserExports(t *testing.T) {
	src := `class PublicClass
end
private class PrivateClass
end
def public_method
end
private def private_method
end
`
	p := NewCrystalParser()
	pf, err := p.Parse("lib.cr", []byte(src))
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	exportNames := make(map[string]bool)
	for _, exp := range pf.Exports {
		exportNames[exp.Name] = true
	}
	if !exportNames["PublicClass"] {
		t.Error("expected PublicClass to be exported")
	}
	if exportNames["PrivateClass"] {
		t.Error("PrivateClass should not be exported")
	}
	if !exportNames["public_method"] {
		t.Error("expected public_method to be exported")
	}
	if exportNames["private_method"] {
		t.Error("private_method should not be exported")
	}
}

func TestCrystalParserEmptyFile(t *testing.T) {
	p := NewCrystalParser()
	pf, err := p.Parse("empty.cr", []byte(""))
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	if len(pf.Imports) != 0 || len(pf.Exports) != 0 || len(pf.Symbols) != 0 {
		t.Error("expected empty results for empty file")
	}
}

func TestCrystalParserCommentsSkipped(t *testing.T) {
	src := `# This is a comment
# def not_a_function
require "json"
def real_method
end
`
	p := NewCrystalParser()
	pf, err := p.Parse("main.cr", []byte(src))
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

func TestCrystalParserClassesAndStructs(t *testing.T) {
	src := `class Config
end
abstract class Base
end
struct Point
end
module MyModule
end
enum Status
end
`
	p := NewCrystalParser()
	pf, err := p.Parse("types.cr", []byte(src))
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	symMap := make(map[string]Symbol)
	for _, s := range pf.Symbols {
		symMap[s.Name] = s
	}
	if _, ok := symMap["Config"]; !ok {
		t.Error("expected symbol Config")
	}
	if _, ok := symMap["Base"]; !ok {
		t.Error("expected symbol Base")
	}
	if _, ok := symMap["Point"]; !ok {
		t.Error("expected symbol Point")
	}
	if _, ok := symMap["MyModule"]; !ok {
		t.Error("expected symbol MyModule")
	}
	if _, ok := symMap["Status"]; !ok {
		t.Error("expected symbol Status")
	}
}

func TestCrystalParserSampleFile(t *testing.T) {
	content, err := os.ReadFile("../../testdata/crystal/sample.cr")
	if err != nil {
		t.Fatalf("reading fixture: %v", err)
	}
	p := NewCrystalParser()
	pf, err := p.Parse("sample.cr", content)
	if err != nil {
		t.Fatalf("parsing: %v", err)
	}
	if pf.Language != "crystal" {
		t.Errorf("expected language crystal, got %q", pf.Language)
	}
	if len(pf.Imports) < 3 {
		t.Errorf("expected at least 3 imports, got %d", len(pf.Imports))
	}
	if len(pf.Symbols) < 5 {
		t.Errorf("expected at least 5 symbols, got %d", len(pf.Symbols))
	}
}
