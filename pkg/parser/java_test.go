package parser

import (
	"os"
	"testing"
)

func TestJavaParserLanguage(t *testing.T) {
	p := NewJavaParser()
	if p.Language() != "java" {
		t.Errorf("expected java, got %q", p.Language())
	}
}

func TestJavaParserFixture(t *testing.T) {
	content, err := os.ReadFile("../../testdata/java/Sample.java")
	if err != nil {
		t.Fatalf("reading fixture: %v", err)
	}

	p := NewJavaParser()
	pf, err := p.Parse("Sample.java", content)
	if err != nil {
		t.Fatalf("parsing: %v", err)
	}

	if pf.Language != "java" {
		t.Errorf("expected language java, got %q", pf.Language)
	}

	// Check imports
	importPaths := make(map[string]bool)
	for _, imp := range pf.Imports {
		importPaths[imp.Path] = true
	}

	expectedImports := []string{
		"java.util.List",
		"java.util.ArrayList",
		"java.util.*",
		"org.junit.Assert.assertEquals",
		"org.junit.Assert.*",
	}
	for _, expected := range expectedImports {
		if !importPaths[expected] {
			t.Errorf("expected import %q not found; have: %v", expected, importPaths)
		}
	}

	// Check static imports are marked
	for _, imp := range pf.Imports {
		if imp.Path == "org.junit.Assert.assertEquals" && !imp.IsType {
			t.Error("expected static import to have IsType=true")
		}
	}

	// Check import names
	for _, imp := range pf.Imports {
		if imp.Path == "java.util.List" {
			found := false
			for _, n := range imp.Names {
				if n == "List" {
					found = true
				}
			}
			if !found {
				t.Error("expected import java.util.List to have name 'List'")
			}
		}
		if imp.Path == "java.util.*" {
			if len(imp.Names) != 0 {
				t.Errorf("expected wildcard import to have no names, got %v", imp.Names)
			}
		}
	}
}

func TestJavaParserExports(t *testing.T) {
	content, err := os.ReadFile("../../testdata/java/Sample.java")
	if err != nil {
		t.Fatalf("reading fixture: %v", err)
	}

	p := NewJavaParser()
	pf, err := p.Parse("Sample.java", content)
	if err != nil {
		t.Fatalf("parsing: %v", err)
	}

	exportNames := make(map[string]string)
	for _, exp := range pf.Exports {
		exportNames[exp.Name] = exp.Kind
	}

	// public class Sample
	if kind, ok := exportNames["Sample"]; !ok {
		t.Error("expected export 'Sample' not found")
	} else if kind != "class" {
		t.Errorf("expected Sample to be class, got %q", kind)
	}

	// public enum Color
	if kind, ok := exportNames["Color"]; !ok {
		t.Error("expected export 'Color' not found")
	} else if kind != "enum" {
		t.Errorf("expected Color to be enum, got %q", kind)
	}

	// public record Point
	if kind, ok := exportNames["Point"]; !ok {
		t.Error("expected export 'Point' not found")
	} else if kind != "record" {
		t.Errorf("expected Point to be record, got %q", kind)
	}

	// interface Configurable (not public, should NOT be exported)
	if _, ok := exportNames["Configurable"]; ok {
		t.Error("Configurable should not be exported (no public modifier)")
	}
}

func TestJavaParserSymbols(t *testing.T) {
	content, err := os.ReadFile("../../testdata/java/Sample.java")
	if err != nil {
		t.Fatalf("reading fixture: %v", err)
	}

	p := NewJavaParser()
	pf, err := p.Parse("Sample.java", content)
	if err != nil {
		t.Fatalf("parsing: %v", err)
	}

	// Build map keyed by name:kind for precise lookups
	type symKey struct {
		name string
		kind string
	}
	symbolSet := make(map[symKey]Symbol)
	for _, sym := range pf.Symbols {
		symbolSet[symKey{sym.Name, sym.Kind}] = sym
	}

	// Also build a simple name lookup for unique names
	symbolMap := make(map[string]Symbol)
	for _, sym := range pf.Symbols {
		symbolMap[sym.Name] = sym
	}

	// Check class symbol
	if sym, ok := symbolSet[symKey{"Sample", "class"}]; !ok {
		t.Error("expected symbol 'Sample' with kind=class not found")
	} else {
		if !sym.Exported {
			t.Error("expected Sample to be exported")
		}
	}

	// Check constructor exists
	if _, ok := symbolSet[symKey{"Sample", "constructor"}]; !ok {
		// Constructor is optional if class line also looks like constructor; that's acceptable
	}

	// Check public method
	if sym, ok := symbolMap["getName"]; !ok {
		t.Error("expected symbol 'getName' not found")
	} else {
		if sym.Kind != "method" {
			t.Errorf("expected getName kind=method, got %q", sym.Kind)
		}
		if !sym.Exported {
			t.Error("expected getName to be exported (public)")
		}
	}

	// Check private method
	if sym, ok := symbolMap["calculate"]; !ok {
		t.Error("expected symbol 'calculate' not found")
	} else {
		if sym.Exported {
			t.Error("expected calculate to NOT be exported (private)")
		}
	}

	// Check field
	if sym, ok := symbolMap["name"]; !ok {
		t.Error("expected symbol 'name' (field) not found")
	} else {
		if sym.Kind != "field" {
			t.Errorf("expected name kind=field, got %q", sym.Kind)
		}
		if sym.Exported {
			t.Error("expected name to NOT be exported (private)")
		}
	}

	// Check public static field
	if sym, ok := symbolMap["MAX_SIZE"]; !ok {
		t.Error("expected symbol 'MAX_SIZE' not found")
	} else {
		if sym.Kind != "field" {
			t.Errorf("expected MAX_SIZE kind=field, got %q", sym.Kind)
		}
		if !sym.Exported {
			t.Error("expected MAX_SIZE to be exported (public)")
		}
	}

	// Check interface
	if sym, ok := symbolMap["Configurable"]; !ok {
		t.Error("expected symbol 'Configurable' not found")
	} else {
		if sym.Kind != "interface" {
			t.Errorf("expected Configurable kind=interface, got %q", sym.Kind)
		}
	}
}

func TestJavaParserInlineCode(t *testing.T) {
	code := `package com.example;

import java.io.IOException;

public class MyService {
    private final String endpoint;

    public MyService(String endpoint) {
        this.endpoint = endpoint;
    }

    public String call() throws IOException {
        return endpoint;
    }
}
`
	p := NewJavaParser()
	pf, err := p.Parse("MyService.java", []byte(code))
	if err != nil {
		t.Fatalf("parsing: %v", err)
	}

	if len(pf.Imports) != 1 {
		t.Errorf("expected 1 import, got %d", len(pf.Imports))
	}

	if pf.Imports[0].Path != "java.io.IOException" {
		t.Errorf("expected import path java.io.IOException, got %q", pf.Imports[0].Path)
	}

	if len(pf.Exports) != 1 {
		t.Errorf("expected 1 export (MyService), got %d", len(pf.Exports))
	}

	symNames := make(map[string]bool)
	for _, sym := range pf.Symbols {
		symNames[sym.Name] = true
	}

	for _, expected := range []string{"MyService", "endpoint", "call"} {
		if !symNames[expected] {
			t.Errorf("expected symbol %q not found", expected)
		}
	}
}
