package parser

import (
	"os"
	"testing"
)

func TestPowershellParserLanguage(t *testing.T) {
	p := NewPowershellParser()
	if p.Language() != "powershell" {
		t.Errorf("expected 'powershell', got %q", p.Language())
	}
}

func TestPowershellImports(t *testing.T) {
	src := "Import-Module ActiveDirectory\nImport-Module 'SqlServer'\n"
	p := NewPowershellParser()
	pf, err := p.Parse("script.ps1", []byte(src))
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(pf.Imports) != 2 {
		t.Fatalf("expected 2 imports, got %d", len(pf.Imports))
	}
}

func TestPowershellFunctions(t *testing.T) {
	src := "function Get-UserInfo {\n  param($Name)\n  return $Name\n}\n\nfunction Set-Config {\n  param($Key, $Value)\n}\n"
	p := NewPowershellParser()
	pf, err := p.Parse("funcs.ps1", []byte(src))
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	names := make(map[string]bool)
	for _, s := range pf.Symbols {
		if s.Kind == "function" {
			names[s.Name] = true
		}
	}
	if !names["Get-UserInfo"] {
		t.Error("expected Get-UserInfo function")
	}
	if !names["Set-Config"] {
		t.Error("expected Set-Config function")
	}
}

func TestPowershellEmptyFile(t *testing.T) {
	p := NewPowershellParser()
	pf, err := p.Parse("empty.ps1", []byte(""))
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(pf.Imports) != 0 || len(pf.Symbols) != 0 {
		t.Error("expected empty results")
	}
}

func TestPowershellCommentsSkipped(t *testing.T) {
	src := "# Import-Module Fake\n# function Hidden { }\n"
	p := NewPowershellParser()
	pf, err := p.Parse("comments.ps1", []byte(src))
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(pf.Imports) != 0 || len(pf.Symbols) != 0 {
		t.Error("expected empty results from comments")
	}
}

func TestPowershellSampleFile(t *testing.T) {
	content, err := os.ReadFile("../../testdata/powershell/sample.ps1")
	if err != nil {
		t.Fatalf("failed to read sample: %v", err)
	}
	p := NewPowershellParser()
	pf, err := p.Parse("sample.ps1", content)
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
