package spec

import (
	"testing"
)

func TestParseMarkdownSpec_Basic(t *testing.T) {
	md := `# My Language Specification

This is the intro.

## 1. Lexical Structure

The language uses UTF-8 encoding.

### 1.1 Keywords

Keywords are reserved words.

### 1.2 Operators

Standard arithmetic and logical operators.

## 2. Grammar

The grammar is defined in EBNF.

## 3. Semantics

Values are dynamically typed.
`

	s, err := ParseMarkdownSpec([]byte(md))
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	if s.Title != "My Language Specification" {
		t.Errorf("title = %q, want %q", s.Title, "My Language Specification")
	}

	if s.Version != 1 {
		t.Errorf("version = %d, want 1", s.Version)
	}

	// Expected: SEC-1 (Lexical Structure), SEC-1-1 (Keywords), SEC-1-2 (Operators),
	//           SEC-2 (Grammar), SEC-3 (Semantics)
	if len(s.Requirements) != 5 {
		t.Fatalf("got %d requirements, want 5", len(s.Requirements))
	}

	expected := []struct {
		id   string
		desc string
	}{
		{"SEC-1", "Lexical Structure:"},
		{"SEC-1-1", "Keywords:"},
		{"SEC-1-2", "Operators:"},
		{"SEC-2", "Grammar:"},
		{"SEC-3", "Semantics:"},
	}

	for i, e := range expected {
		req := s.Requirements[i]
		if req.ID != e.id {
			t.Errorf("req[%d].ID = %q, want %q", i, req.ID, e.id)
		}
		if len(req.Description) < len(e.desc) {
			t.Errorf("req[%d].Description = %q, too short (want prefix %q)", i, req.Description, e.desc)
		}
	}
}

func TestParseMarkdownSpec_NoHeadings(t *testing.T) {
	md := `Just some text without any headings.`

	_, err := ParseMarkdownSpec([]byte(md))
	if err == nil {
		t.Fatal("expected error for document with no headings")
	}
}

func TestParseMarkdownSpec_OnlyH1(t *testing.T) {
	md := `# Title Only

Some body text but no H2 or H3 sections.
`

	_, err := ParseMarkdownSpec([]byte(md))
	if err == nil {
		t.Fatal("expected error for document with only H1")
	}
}

func TestParseMarkdownSpec_CodeBlockIgnored(t *testing.T) {
	md := `# Spec

## Section One

Here is some code:

` + "```" + `
## This is not a heading
# Neither is this
` + "```" + `

More text after code block.

## Section Two

Another section.
`

	s, err := ParseMarkdownSpec([]byte(md))
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	if len(s.Requirements) != 2 {
		t.Fatalf("got %d requirements, want 2 (code block headings should be ignored)", len(s.Requirements))
	}

	if s.Requirements[0].ID != "SEC-1" {
		t.Errorf("req[0].ID = %q, want SEC-1", s.Requirements[0].ID)
	}
	if s.Requirements[1].ID != "SEC-2" {
		t.Errorf("req[1].ID = %q, want SEC-2", s.Requirements[1].ID)
	}
}

func TestParseMarkdownSpec_StripsNumberedPrefix(t *testing.T) {
	md := `# Title

## 1. First Section

Content.

## 2. Second Section

Content.
`

	s, err := ParseMarkdownSpec([]byte(md))
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	if len(s.Requirements) != 2 {
		t.Fatalf("got %d requirements, want 2", len(s.Requirements))
	}

	// Section numbers should be stripped from the title
	if s.Requirements[0].Description != "First Section: Content." {
		t.Errorf("req[0].Description = %q", s.Requirements[0].Description)
	}
}

func TestParseMarkdownSpec_TruncatesLongDescriptions(t *testing.T) {
	long := ""
	for i := 0; i < 100; i++ {
		long += "word "
	}

	md := "# Title\n\n## Section\n\n" + long + "\n"

	s, err := ParseMarkdownSpec([]byte(md))
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	// Description should be truncated to 300 chars + section title prefix
	for _, req := range s.Requirements {
		if len(req.Description) > 320 {
			t.Errorf("description too long (%d chars): %.50s...", len(req.Description), req.Description)
		}
	}
}

func TestParseSpec_FallsBackToMarkdown(t *testing.T) {
	md := `# A Spec

## Feature One

Does something.

## Feature Two

Does something else.
`

	// ParseSpec should try YAML first, fail, then succeed with Markdown
	s, err := ParseSpec([]byte(md))
	if err != nil {
		t.Fatalf("ParseSpec should fall back to Markdown: %v", err)
	}

	if s.Title != "A Spec" {
		t.Errorf("title = %q, want %q", s.Title, "A Spec")
	}

	if len(s.Requirements) != 2 {
		t.Errorf("got %d requirements, want 2", len(s.Requirements))
	}
}

func TestParseYAMLSpec_DoesNotFallBack(t *testing.T) {
	md := `# A Markdown Spec

## Section

Content.
`

	_, err := ParseYAMLSpec([]byte(md))
	if err == nil {
		t.Fatal("ParseYAMLSpec should NOT parse Markdown")
	}
}
