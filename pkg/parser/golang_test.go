package parser

import (
	"os"
	"testing"
)

func TestGoParserLanguage(t *testing.T) {
	p := NewGoParser()
	if p.Language() != "go" {
		t.Errorf("expected go, got %q", p.Language())
	}
}

func TestGoParserFixture(t *testing.T) {
	content, err := os.ReadFile("../../testdata/go/sample.go")
	if err != nil {
		t.Fatalf("reading fixture: %v", err)
	}

	p := NewGoParser()
	pf, err := p.Parse("testdata/go/sample.go", content)
	if err != nil {
		t.Fatalf("parsing: %v", err)
	}

	if pf.Language != "go" {
		t.Errorf("expected language go, got %q", pf.Language)
	}

	// Check imports
	if len(pf.Imports) != 3 {
		t.Fatalf("expected 3 imports, got %d", len(pf.Imports))
	}

	importPaths := make(map[string]bool)
	for _, imp := range pf.Imports {
		importPaths[imp.Path] = true
	}
	for _, expected := range []string{"fmt", "net/http", "github.com/example/pkg"} {
		if !importPaths[expected] {
			t.Errorf("expected import %q not found", expected)
		}
	}

	// Check aliased import has the alias name
	for _, imp := range pf.Imports {
		if imp.Path == "github.com/example/pkg" {
			if len(imp.Names) == 0 || imp.Names[0] != "myalias" {
				t.Errorf("expected alias myalias for github.com/example/pkg, got %v", imp.Names)
			}
		}
	}

	// Check exports
	exportNames := make(map[string]bool)
	for _, exp := range pf.Exports {
		exportNames[exp.Name] = true
	}
	for _, expected := range []string{"MaxRetries", "Server", "Handler", "NewServer", "Start"} {
		if !exportNames[expected] {
			t.Errorf("expected export %q not found", expected)
		}
	}

	// helperFunc and defaultTimeout should NOT be exported
	for _, unexpected := range []string{"helperFunc", "defaultTimeout"} {
		if exportNames[unexpected] {
			t.Errorf("unexpected export %q", unexpected)
		}
	}

	// Check symbols
	symbolMap := make(map[string]Symbol)
	for _, sym := range pf.Symbols {
		symbolMap[sym.Name] = sym
	}

	// Server should be a struct
	if s, ok := symbolMap["Server"]; ok {
		if s.Kind != "struct" {
			t.Errorf("expected Server to be struct, got %q", s.Kind)
		}
		if !s.Exported {
			t.Error("expected Server to be exported")
		}
	} else {
		t.Error("expected Server symbol")
	}

	// Handler should be an interface
	if s, ok := symbolMap["Handler"]; ok {
		if s.Kind != "interface" {
			t.Errorf("expected Handler to be interface, got %q", s.Kind)
		}
	} else {
		t.Error("expected Handler symbol")
	}

	// NewServer should be a function
	if s, ok := symbolMap["NewServer"]; ok {
		if s.Kind != "function" {
			t.Errorf("expected NewServer to be function, got %q", s.Kind)
		}
		if s.Signature == "" {
			t.Error("expected NewServer to have a signature")
		}
	} else {
		t.Error("expected NewServer symbol")
	}

	// Start should be a method
	if s, ok := symbolMap["Start"]; ok {
		if s.Kind != "method" {
			t.Errorf("expected Start to be method, got %q", s.Kind)
		}
	} else {
		t.Error("expected Start symbol")
	}

	// MaxRetries should be a constant
	if s, ok := symbolMap["MaxRetries"]; ok {
		if s.Kind != "constant" {
			t.Errorf("expected MaxRetries to be constant, got %q", s.Kind)
		}
	} else {
		t.Error("expected MaxRetries symbol")
	}

	// defaultTimeout should be a variable, not exported
	if s, ok := symbolMap["defaultTimeout"]; ok {
		if s.Kind != "variable" {
			t.Errorf("expected defaultTimeout to be variable, got %q", s.Kind)
		}
		if s.Exported {
			t.Error("expected defaultTimeout to not be exported")
		}
	} else {
		t.Error("expected defaultTimeout symbol")
	}
}

func TestGoParserFuncSignature(t *testing.T) {
	src := `package main

func Simple() {}
func WithArgs(x int, y string) {}
func WithReturn(x int) error { return nil }
func MultiReturn(x int) (int, error) { return 0, nil }
`

	p := NewGoParser()
	pf, err := p.Parse("test.go", []byte(src))
	if err != nil {
		t.Fatal(err)
	}

	sigs := make(map[string]string)
	for _, sym := range pf.Symbols {
		sigs[sym.Name] = sym.Signature
	}

	tests := []struct {
		name     string
		contains string
	}{
		{"Simple", "func Simple()"},
		{"WithArgs", "func WithArgs(x int, y string)"},
		{"WithReturn", "func WithReturn(x int) error"},
		{"MultiReturn", "func MultiReturn(x int) (int, error)"},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			sig, ok := sigs[tt.name]
			if !ok {
				t.Fatalf("symbol %s not found", tt.name)
			}
			if sig != tt.contains {
				t.Errorf("expected signature %q, got %q", tt.contains, sig)
			}
		})
	}
}

func TestGoParserInvalidSource(t *testing.T) {
	p := NewGoParser()
	_, err := p.Parse("bad.go", []byte("this is not valid go"))
	if err == nil {
		t.Fatal("expected error for invalid Go source")
	}
}

func TestGoParserEmptyFile(t *testing.T) {
	p := NewGoParser()
	pf, err := p.Parse("empty.go", []byte("package empty\n"))
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
