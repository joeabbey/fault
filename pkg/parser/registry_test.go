package parser

import (
	"testing"
)

func TestDetectLanguage(t *testing.T) {
	tests := []struct {
		filename string
		expected string
	}{
		{"main.go", "go"},
		{"pkg/server.go", "go"},
		{"index.ts", "typescript"},
		{"App.tsx", "typescript"},
		{"script.js", "typescript"},
		{"Component.jsx", "typescript"},
		{"utils.mjs", "typescript"},
		{"config.cjs", "typescript"},
		{"app.py", "python"},
		{"models/user.py", "python"},
		{"README.md", ""},
		{"Dockerfile", ""},
		{"config.yaml", ""},
		{"data.json", ""},
		{".gitignore", ""},
	}

	for _, tt := range tests {
		t.Run(tt.filename, func(t *testing.T) {
			got := DetectLanguage(tt.filename)
			if got != tt.expected {
				t.Errorf("DetectLanguage(%q) = %q, want %q", tt.filename, got, tt.expected)
			}
		})
	}
}

func TestDetectLanguageCaseInsensitive(t *testing.T) {
	// Extensions should be case-insensitive
	tests := []struct {
		filename string
		expected string
	}{
		{"main.GO", "go"},
		{"app.PY", "python"},
		{"index.TS", "typescript"},
	}

	for _, tt := range tests {
		t.Run(tt.filename, func(t *testing.T) {
			got := DetectLanguage(tt.filename)
			if got != tt.expected {
				t.Errorf("DetectLanguage(%q) = %q, want %q", tt.filename, got, tt.expected)
			}
		})
	}
}

// mockParser is a test parser that records calls.
type mockParser struct {
	lang string
}

func (m *mockParser) Language() string { return m.lang }
func (m *mockParser) Parse(filename string, content []byte) (*ParsedFile, error) {
	return &ParsedFile{
		Path:     filename,
		Language: m.lang,
		Imports:  make([]Import, 0),
		Exports:  make([]Export, 0),
		Symbols:  make([]Symbol, 0),
	}, nil
}

func TestRegistryRegisterAndGet(t *testing.T) {
	reg := NewRegistry()

	goParser := &mockParser{lang: "go"}
	tsParser := &mockParser{lang: "typescript"}

	reg.Register(goParser)
	reg.Register(tsParser)

	p, ok := reg.Get("go")
	if !ok {
		t.Fatal("expected go parser to be registered")
	}
	if p.Language() != "go" {
		t.Errorf("expected go, got %q", p.Language())
	}

	p, ok = reg.Get("typescript")
	if !ok {
		t.Fatal("expected typescript parser to be registered")
	}
	if p.Language() != "typescript" {
		t.Errorf("expected typescript, got %q", p.Language())
	}

	_, ok = reg.Get("rust")
	if ok {
		t.Fatal("expected rust parser to not be registered")
	}
}

func TestRegistryParseFile(t *testing.T) {
	reg := NewRegistry()
	reg.Register(&mockParser{lang: "go"})

	// Known language
	pf, err := reg.ParseFile("main.go", []byte("package main"))
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if pf == nil {
		t.Fatal("expected parsed file, got nil")
	}
	if pf.Language != "go" {
		t.Errorf("expected language go, got %q", pf.Language)
	}

	// Unknown language returns nil, nil
	pf, err = reg.ParseFile("README.md", []byte("# Hello"))
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if pf != nil {
		t.Error("expected nil for unsupported language")
	}

	// Known language but no parser registered
	pf, err = reg.ParseFile("app.py", []byte("print('hello')"))
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if pf != nil {
		t.Error("expected nil for unregistered parser")
	}
}

func TestSupportedExtensions(t *testing.T) {
	exts := SupportedExtensions()
	if len(exts) == 0 {
		t.Fatal("expected at least one supported extension")
	}

	// Should include .go, .ts, .py at minimum
	extSet := make(map[string]bool)
	for _, ext := range exts {
		extSet[ext] = true
	}
	for _, required := range []string{".go", ".ts", ".py"} {
		if !extSet[required] {
			t.Errorf("expected %s in supported extensions", required)
		}
	}
}

func TestSupportedLanguages(t *testing.T) {
	langs := SupportedLanguages()
	if len(langs) != 5 {
		t.Errorf("expected 5 languages, got %d: %v", len(langs), langs)
	}

	langSet := make(map[string]bool)
	for _, lang := range langs {
		langSet[lang] = true
	}
	for _, required := range []string{"go", "typescript", "python", "java", "rust"} {
		if !langSet[required] {
			t.Errorf("expected %s in supported languages", required)
		}
	}
}
