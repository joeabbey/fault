package parser

import (
	"os"
	"testing"
)

func TestTerraformParserLanguage(t *testing.T) {
	p := NewTerraformParser()
	if p.Language() != "terraform" {
		t.Errorf("expected 'terraform', got %q", p.Language())
	}
}

func TestTerraformResources(t *testing.T) {
	src := "provider \"aws\" {\n  region = \"us-east-1\"\n}\n\nresource \"aws_instance\" \"web\" {\n  ami = \"ami-123\"\n}\n\nvariable \"name\" {\n  type = string\n}\n\noutput \"ip\" {\n  value = aws_instance.web.public_ip\n}\n"
	p := NewTerraformParser()
	pf, err := p.Parse("main.tf", []byte(src))
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(pf.Imports) < 1 {
		t.Fatalf("expected at least 1 import (provider), got %d", len(pf.Imports))
	}
	if len(pf.Symbols) < 3 {
		t.Fatalf("expected at least 3 symbols, got %d", len(pf.Symbols))
	}
}

func TestTerraformEmptyFile(t *testing.T) {
	p := NewTerraformParser()
	pf, err := p.Parse("empty.tf", []byte(""))
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(pf.Imports) != 0 || len(pf.Symbols) != 0 {
		t.Error("expected empty results")
	}
}

func TestTerraformCommentsSkipped(t *testing.T) {
	src := "# resource \"aws_instance\" \"hidden\" {}\n// variable \"secret\" {}\n"
	p := NewTerraformParser()
	pf, err := p.Parse("comments.tf", []byte(src))
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(pf.Imports) != 0 || len(pf.Symbols) != 0 {
		t.Error("expected empty results from comments")
	}
}

func TestTerraformSampleFile(t *testing.T) {
	content, err := os.ReadFile("../../testdata/terraform/sample.tf")
	if err != nil {
		t.Fatalf("failed to read sample: %v", err)
	}
	p := NewTerraformParser()
	pf, err := p.Parse("sample.tf", content)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(pf.Symbols) < 3 {
		t.Errorf("expected at least 3 symbols, got %d", len(pf.Symbols))
	}
}
