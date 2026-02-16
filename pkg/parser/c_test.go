package parser

import (
	"os"
	"testing"
)

func TestCParserLanguage(t *testing.T) {
	p := NewCParser()
	if p.Language() != "c" {
		t.Errorf("expected c, got %q", p.Language())
	}
}

func TestCParserImports(t *testing.T) {
	src := `#include <stdio.h>
#include <stdlib.h>
#include "local.h"
#include "config.h"
`

	p := NewCParser()
	pf, err := p.Parse("main.c", []byte(src))
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}

	if len(pf.Imports) != 4 {
		t.Fatalf("expected 4 imports, got %d", len(pf.Imports))
	}

	expected := []string{"stdio.h", "stdlib.h", "local.h", "config.h"}
	for i, exp := range expected {
		if pf.Imports[i].Path != exp {
			t.Errorf("import %d: expected path %q, got %q", i, exp, pf.Imports[i].Path)
		}
	}
}

func TestCParserFunctions(t *testing.T) {
	src := `int add(int a, int b) {
    return a + b;
}

void print_message(const char* msg) {
    printf("%s\n", msg);
}

static int helper(void) {
    return 42;
}

static void another_static(int x) {
    printf("%d\n", x);
}

int main(int argc, char** argv) {
    return 0;
}
`

	p := NewCParser()
	pf, err := p.Parse("main.c", []byte(src))
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}

	symMap := make(map[string]Symbol)
	for _, sym := range pf.Symbols {
		if sym.Kind == "function" {
			symMap[sym.Name] = sym
		}
	}

	if sym, ok := symMap["add"]; !ok {
		t.Error("expected add function not found")
	} else if !sym.Exported {
		t.Error("add should be exported (non-static)")
	}

	if sym, ok := symMap["print_message"]; !ok {
		t.Error("expected print_message function not found")
	} else if !sym.Exported {
		t.Error("print_message should be exported")
	}

	if sym, ok := symMap["helper"]; !ok {
		t.Error("expected helper function not found")
	} else if sym.Exported {
		t.Error("helper should not be exported (static)")
	}

	if sym, ok := symMap["another_static"]; !ok {
		t.Error("expected another_static function not found")
	} else if sym.Exported {
		t.Error("another_static should not be exported (static)")
	}

	if _, ok := symMap["main"]; !ok {
		t.Error("expected main function not found")
	}
}

func TestCParserTypes(t *testing.T) {
	src := `typedef unsigned int uint32;
typedef struct point Point;

struct node {
    int value;
    struct node* next;
};

enum color {
    RED,
    GREEN,
    BLUE
};
`

	p := NewCParser()
	pf, err := p.Parse("types.c", []byte(src))
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}

	symMap := make(map[string]string)
	for _, sym := range pf.Symbols {
		symMap[sym.Name] = sym.Kind
	}

	if kind, ok := symMap["uint32"]; !ok {
		t.Error("expected uint32 typedef not found")
	} else if kind != "type" {
		t.Errorf("uint32: expected kind 'type', got %q", kind)
	}

	if kind, ok := symMap["Point"]; !ok {
		t.Error("expected Point typedef not found")
	} else if kind != "type" {
		t.Errorf("Point: expected kind 'type', got %q", kind)
	}

	if kind, ok := symMap["node"]; !ok {
		t.Error("expected node struct not found")
	} else if kind != "struct" {
		t.Errorf("node: expected kind 'struct', got %q", kind)
	}

	if kind, ok := symMap["color"]; !ok {
		t.Error("expected color enum not found")
	} else if kind != "type" {
		t.Errorf("color: expected kind 'type', got %q", kind)
	}
}

func TestCParserMacros(t *testing.T) {
	src := `#ifndef SAMPLE_H
#define SAMPLE_H

#define MAX_SIZE 1024
#define MIN(a, b) ((a) < (b) ? (a) : (b))
#define VERSION "1.0.0"

#endif
`

	p := NewCParser()
	pf, err := p.Parse("sample.h", []byte(src))
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}

	macroNames := make(map[string]bool)
	for _, sym := range pf.Symbols {
		if sym.Kind == "macro" {
			macroNames[sym.Name] = true
		}
	}

	if !macroNames["MAX_SIZE"] {
		t.Error("expected MAX_SIZE macro not found")
	}
	if !macroNames["MIN"] {
		t.Error("expected MIN macro not found")
	}
	if !macroNames["VERSION"] {
		t.Error("expected VERSION macro not found")
	}
	// Include guards should be excluded
	if macroNames["SAMPLE_H"] {
		t.Error("SAMPLE_H include guard should be excluded")
	}
}

func TestCParserBlockComments(t *testing.T) {
	src := `/* This is a block comment */
#include <stdio.h>
/*
 * Multi-line block comment
 * int fake_function(void) should not be parsed
 */
int real_function(void) {
    return 1;
}
`

	p := NewCParser()
	pf, err := p.Parse("main.c", []byte(src))
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}

	if len(pf.Imports) != 1 {
		t.Errorf("expected 1 import, got %d", len(pf.Imports))
	}

	found := false
	for _, sym := range pf.Symbols {
		if sym.Name == "fake_function" {
			t.Error("fake_function should not be parsed (inside block comment)")
		}
		if sym.Name == "real_function" {
			found = true
		}
	}
	if !found {
		t.Error("expected real_function symbol not found")
	}
}

func TestCParserSampleFile(t *testing.T) {
	content, err := os.ReadFile("../../testdata/c/sample.c")
	if err != nil {
		t.Fatalf("failed to read testdata: %v", err)
	}

	p := NewCParser()
	pf, err := p.Parse("testdata/c/sample.c", content)
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}

	// Verify imports: stdio.h, string.h, stdlib.h, sample.h, config.h
	if len(pf.Imports) != 5 {
		t.Errorf("expected 5 imports, got %d", len(pf.Imports))
		for _, imp := range pf.Imports {
			t.Logf("  import: %s", imp.Path)
		}
	}

	importPaths := make(map[string]bool)
	for _, imp := range pf.Imports {
		importPaths[imp.Path] = true
	}
	for _, expected := range []string{"stdio.h", "string.h", "stdlib.h", "sample.h", "config.h"} {
		if !importPaths[expected] {
			t.Errorf("expected import %q not found", expected)
		}
	}

	// Verify we have some symbols
	if len(pf.Symbols) == 0 {
		t.Error("expected symbols, got none")
	}

	// Check specific symbols exist
	symNames := make(map[string]bool)
	for _, sym := range pf.Symbols {
		symNames[sym.Name] = true
	}

	for _, expected := range []string{"add", "print_point", "format_name", "internal_helper", "main"} {
		if !symNames[expected] {
			t.Errorf("expected symbol %q not found", expected)
		}
	}

	// Verify static functions are not exported
	exportNames := make(map[string]bool)
	for _, exp := range pf.Exports {
		exportNames[exp.Name] = true
	}
	if exportNames["internal_helper"] {
		t.Error("internal_helper should not be exported (static)")
	}
	if exportNames["another_static"] {
		t.Error("another_static should not be exported (static)")
	}
	if !exportNames["add"] {
		t.Error("add should be exported")
	}
}

func TestCParserSampleHeader(t *testing.T) {
	content, err := os.ReadFile("../../testdata/c/sample.h")
	if err != nil {
		t.Fatalf("failed to read testdata: %v", err)
	}

	p := NewCParser()
	pf, err := p.Parse("testdata/c/sample.h", content)
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}

	// Verify imports: stdio.h, stdlib.h, utils.h
	if len(pf.Imports) != 3 {
		t.Errorf("expected 3 imports, got %d", len(pf.Imports))
		for _, imp := range pf.Imports {
			t.Logf("  import: %s", imp.Path)
		}
	}

	// Check that types are found
	symNames := make(map[string]bool)
	for _, sym := range pf.Symbols {
		symNames[sym.Name] = true
	}

	for _, expected := range []string{"uint32", "Point", "Person", "Status"} {
		if !symNames[expected] {
			t.Errorf("expected symbol %q not found", expected)
		}
	}
}
