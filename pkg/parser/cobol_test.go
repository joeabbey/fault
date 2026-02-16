package parser

import (
	"os"
	"testing"
)

func TestCobolParserLanguage(t *testing.T) {
	p := NewCobolParser()
	if p.Language() != "cobol" {
		t.Errorf("expected 'cobol', got %q", p.Language())
	}
}

func TestCobolImports(t *testing.T) {
	src := "       COPY CUSTOMER-REC.\n       COPY ACCOUNT-FILE.\n       CALL 'CALCPROG'\n"
	p := NewCobolParser()
	pf, err := p.Parse("app.cob", []byte(src))
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(pf.Imports) != 3 {
		t.Fatalf("expected 3 imports, got %d", len(pf.Imports))
	}
}

func TestCobolProgramID(t *testing.T) {
	src := "       IDENTIFICATION DIVISION.\n       PROGRAM-ID. HELLO-WORLD.\n"
	p := NewCobolParser()
	pf, err := p.Parse("hello.cob", []byte(src))
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	found := false
	for _, s := range pf.Symbols {
		if s.Name == "HELLO-WORLD" && s.Kind == "program" {
			found = true
			break
		}
	}
	if !found {
		t.Error("expected HELLO-WORLD program symbol")
	}
}

func TestCobolEmptyFile(t *testing.T) {
	p := NewCobolParser()
	pf, err := p.Parse("empty.cob", []byte(""))
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(pf.Imports) != 0 || len(pf.Symbols) != 0 {
		t.Error("expected empty results")
	}
}

func TestCobolCommentsSkipped(t *testing.T) {
	// Column 7 asterisk is a comment in fixed-form COBOL
	src := "      * This is a comment\n      *> Free-form comment\n"
	p := NewCobolParser()
	pf, err := p.Parse("comments.cob", []byte(src))
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(pf.Imports) != 0 || len(pf.Symbols) != 0 {
		t.Error("expected empty results from comments")
	}
}

func TestCobolSampleFile(t *testing.T) {
	content, err := os.ReadFile("../../testdata/cobol/sample.cob")
	if err != nil {
		t.Fatalf("failed to read sample: %v", err)
	}
	p := NewCobolParser()
	pf, err := p.Parse("sample.cob", content)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(pf.Symbols) < 2 {
		t.Errorf("expected at least 2 symbols, got %d", len(pf.Symbols))
	}
}
