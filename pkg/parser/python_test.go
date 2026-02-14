package parser

import (
	"os"
	"testing"
)

func TestPythonParserLanguage(t *testing.T) {
	p := NewPythonParser()
	if p.Language() != "python" {
		t.Errorf("expected python, got %q", p.Language())
	}
}

func TestPythonParserFixture(t *testing.T) {
	content, err := os.ReadFile("../../testdata/python/sample.py")
	if err != nil {
		t.Fatalf("reading fixture: %v", err)
	}

	p := NewPythonParser()
	pf, err := p.Parse("sample.py", content)
	if err != nil {
		t.Fatalf("parsing: %v", err)
	}

	if pf.Language != "python" {
		t.Errorf("expected language python, got %q", pf.Language)
	}

	// Check imports
	importPaths := make(map[string]bool)
	for _, imp := range pf.Imports {
		importPaths[imp.Path] = true
	}

	expectedImports := []string{"os", "sys", "json", "pathlib", "typing", "collections.abc", "..models", ".utils"}
	for _, expected := range expectedImports {
		if !importPaths[expected] {
			t.Errorf("expected import %q not found (have: %v)", expected, importPaths)
		}
	}

	// Check aliased import
	for _, imp := range pf.Imports {
		if imp.Path == "json" {
			found := false
			for _, n := range imp.Names {
				if n == "j" {
					found = true
				}
			}
			if !found {
				t.Errorf("expected alias 'j' for json import, got %v", imp.Names)
			}
		}
	}

	// Check from imports have correct names
	for _, imp := range pf.Imports {
		if imp.Path == "typing" {
			nameSet := make(map[string]bool)
			for _, n := range imp.Names {
				nameSet[n] = true
			}
			if !nameSet["Optional"] || !nameSet["List"] {
				t.Errorf("expected Optional and List in typing import, got %v", imp.Names)
			}
		}
	}

	// Check relative imports
	for _, imp := range pf.Imports {
		if imp.Path == "..models" {
			nameSet := make(map[string]bool)
			for _, n := range imp.Names {
				nameSet[n] = true
			}
			if !nameSet["User"] || !nameSet["Profile"] {
				t.Errorf("expected User and Profile in relative import, got %v", imp.Names)
			}
		}
	}

	// Check exports
	exportNames := make(map[string]string)
	for _, exp := range pf.Exports {
		exportNames[exp.Name] = exp.Kind
	}

	expectedExports := map[string]string{
		"UserService":   "class",
		"DataProcessor": "class",
		"format_name":   "function",
		"fetch_data":    "function",
	}

	for name, kind := range expectedExports {
		gotKind, ok := exportNames[name]
		if !ok {
			t.Errorf("expected export %q not found", name)
			continue
		}
		if gotKind != kind {
			t.Errorf("export %q: expected kind %q, got %q", name, kind, gotKind)
		}
	}

	// _private_helper should not be exported
	if _, ok := exportNames["_private_helper"]; ok {
		t.Error("unexpected export _private_helper")
	}

	// Check symbols
	symbolMap := make(map[string]Symbol)
	for _, sym := range pf.Symbols {
		symbolMap[sym.Name] = sym
	}

	// Methods should be detected
	if s, ok := symbolMap["get_user"]; ok {
		if s.Kind != "method" {
			t.Errorf("expected get_user to be method, got %q", s.Kind)
		}
	} else {
		t.Error("expected get_user symbol")
	}

	// Async method
	if s, ok := symbolMap["update_user"]; ok {
		if s.Kind != "method" {
			t.Errorf("expected update_user to be method, got %q", s.Kind)
		}
	} else {
		t.Error("expected update_user symbol")
	}

	// _private_helper should exist but not be exported
	if s, ok := symbolMap["_private_helper"]; ok {
		if s.Exported {
			t.Error("expected _private_helper to not be exported")
		}
	} else {
		t.Error("expected _private_helper symbol")
	}

	// Constants should be detected
	if _, ok := symbolMap["MAX_RETRIES"]; !ok {
		t.Error("expected MAX_RETRIES symbol")
	}
}

func TestPythonParserWithAll(t *testing.T) {
	content, err := os.ReadFile("../../testdata/python/with_all.py")
	if err != nil {
		t.Fatalf("reading fixture: %v", err)
	}

	p := NewPythonParser()
	pf, err := p.Parse("with_all.py", content)
	if err != nil {
		t.Fatalf("parsing: %v", err)
	}

	exportNames := make(map[string]bool)
	for _, exp := range pf.Exports {
		exportNames[exp.Name] = true
	}

	// Only items in __all__ should be exported
	if !exportNames["public_func"] {
		t.Error("expected public_func to be exported (in __all__)")
	}
	if !exportNames["PublicClass"] {
		t.Error("expected PublicClass to be exported (in __all__)")
	}
	if exportNames["another_func"] {
		t.Error("another_func should not be exported (not in __all__)")
	}
	if exportNames["AnotherClass"] {
		t.Error("AnotherClass should not be exported (not in __all__)")
	}
}

func TestPythonParserRelativeImports(t *testing.T) {
	src := `from . import utils
from .. import models
from .helpers import format_date
from ...core.db import Session
`

	p := NewPythonParser()
	pf, err := p.Parse("test.py", []byte(src))
	if err != nil {
		t.Fatal(err)
	}

	if len(pf.Imports) < 3 {
		t.Fatalf("expected at least 3 imports, got %d", len(pf.Imports))
	}

	importPaths := make(map[string]bool)
	for _, imp := range pf.Imports {
		importPaths[imp.Path] = true
	}

	if !importPaths[".helpers"] {
		t.Error("expected relative import .helpers")
	}
	if !importPaths["...core.db"] {
		t.Error("expected relative import ...core.db")
	}
}

func TestPythonParserEmptyFile(t *testing.T) {
	p := NewPythonParser()
	pf, err := p.Parse("empty.py", []byte(""))
	if err != nil {
		t.Fatal(err)
	}

	if len(pf.Imports) != 0 {
		t.Errorf("expected 0 imports, got %d", len(pf.Imports))
	}
	if len(pf.Exports) != 0 {
		t.Errorf("expected 0 exports, got %d", len(pf.Exports))
	}
	if len(pf.Symbols) != 0 {
		t.Errorf("expected 0 symbols, got %d", len(pf.Symbols))
	}
}

func TestPythonParserStarImport(t *testing.T) {
	src := `from module import *
`

	p := NewPythonParser()
	pf, err := p.Parse("test.py", []byte(src))
	if err != nil {
		t.Fatal(err)
	}

	if len(pf.Imports) != 1 {
		t.Fatalf("expected 1 import, got %d", len(pf.Imports))
	}

	imp := pf.Imports[0]
	if imp.Path != "module" {
		t.Errorf("expected path 'module', got %q", imp.Path)
	}
	if len(imp.Names) != 1 || imp.Names[0] != "*" {
		t.Errorf("expected ['*'], got %v", imp.Names)
	}
}
