package parser

import (
	"os"
	"testing"
)

func TestProtobufParserLanguage(t *testing.T) {
	p := NewProtobufParser()
	if p.Language() != "protobuf" {
		t.Errorf("expected 'protobuf', got %q", p.Language())
	}
}

func TestProtobufImports(t *testing.T) {
	src := "syntax = \"proto3\";\nimport \"google/protobuf/timestamp.proto\";\nimport \"common.proto\";\n"
	p := NewProtobufParser()
	pf, err := p.Parse("service.proto", []byte(src))
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(pf.Imports) != 2 {
		t.Fatalf("expected 2 imports, got %d", len(pf.Imports))
	}
}

func TestProtobufMessages(t *testing.T) {
	src := "syntax = \"proto3\";\npackage myapp;\n\nmessage User {\n  string name = 1;\n}\n\nservice UserService {\n  rpc GetUser (UserRequest) returns (User);\n}\n\nenum Status {\n  UNKNOWN = 0;\n}\n"
	p := NewProtobufParser()
	pf, err := p.Parse("user.proto", []byte(src))
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(pf.Symbols) < 3 {
		t.Fatalf("expected at least 3 symbols, got %d", len(pf.Symbols))
	}
}

func TestProtobufEmptyFile(t *testing.T) {
	p := NewProtobufParser()
	pf, err := p.Parse("empty.proto", []byte(""))
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(pf.Imports) != 0 || len(pf.Symbols) != 0 {
		t.Error("expected empty results")
	}
}

func TestProtobufCommentsSkipped(t *testing.T) {
	src := "// import \"fake.proto\";\n// message Hidden {}\n"
	p := NewProtobufParser()
	pf, err := p.Parse("comments.proto", []byte(src))
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(pf.Imports) != 0 || len(pf.Symbols) != 0 {
		t.Error("expected empty results from comments")
	}
}

func TestProtobufSampleFile(t *testing.T) {
	content, err := os.ReadFile("../../testdata/protobuf/sample.proto")
	if err != nil {
		t.Fatalf("failed to read sample: %v", err)
	}
	p := NewProtobufParser()
	pf, err := p.Parse("sample.proto", content)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(pf.Imports) < 1 {
		t.Errorf("expected at least 1 import, got %d", len(pf.Imports))
	}
	if len(pf.Symbols) < 3 {
		t.Errorf("expected at least 3 symbols, got %d", len(pf.Symbols))
	}
}
