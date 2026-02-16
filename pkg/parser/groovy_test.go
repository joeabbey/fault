package parser

import (
	"os"
	"testing"
)

func TestGroovyParserLanguage(t *testing.T) {
	p := NewGroovyParser()
	if p.Language() != "groovy" {
		t.Errorf("expected 'groovy', got %q", p.Language())
	}
}

func TestGroovyImports(t *testing.T) {
	src := "import groovy.json.JsonSlurper\nimport java.util.List\n"
	p := NewGroovyParser()
	pf, err := p.Parse("app.groovy", []byte(src))
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(pf.Imports) != 2 {
		t.Fatalf("expected 2 imports, got %d", len(pf.Imports))
	}
}

func TestGroovyFunctions(t *testing.T) {
	src := "def greet(String name) {\n  println \"Hello, ${name}\"\n}\n\nvoid processItems(List items) {\n  items.each { println it }\n}\n"
	p := NewGroovyParser()
	pf, err := p.Parse("funcs.groovy", []byte(src))
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	names := make(map[string]bool)
	for _, s := range pf.Symbols {
		if s.Kind == "function" {
			names[s.Name] = true
		}
	}
	if !names["greet"] {
		t.Error("expected greet function")
	}
	if !names["processItems"] {
		t.Error("expected processItems function")
	}
}

func TestGroovyEmptyFile(t *testing.T) {
	p := NewGroovyParser()
	pf, err := p.Parse("empty.groovy", []byte(""))
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(pf.Imports) != 0 || len(pf.Symbols) != 0 {
		t.Error("expected empty results")
	}
}

func TestGroovyCommentsSkipped(t *testing.T) {
	src := "// import fake.Module\n// def hidden() { }\n"
	p := NewGroovyParser()
	pf, err := p.Parse("comments.groovy", []byte(src))
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(pf.Imports) != 0 || len(pf.Symbols) != 0 {
		t.Error("expected empty results from comments")
	}
}

func TestGroovySampleFile(t *testing.T) {
	content, err := os.ReadFile("../../testdata/groovy/sample.groovy")
	if err != nil {
		t.Fatalf("failed to read sample: %v", err)
	}
	p := NewGroovyParser()
	pf, err := p.Parse("sample.groovy", content)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(pf.Imports) < 1 {
		t.Errorf("expected at least 1 import, got %d", len(pf.Imports))
	}
	if len(pf.Symbols) < 2 {
		t.Errorf("expected at least 2 symbols, got %d", len(pf.Symbols))
	}
}
