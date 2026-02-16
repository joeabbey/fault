package parser

import (
	"os"
	"testing"
)

func TestSolidityParserLanguage(t *testing.T) {
	p := NewSolidityParser()
	if p.Language() != "solidity" {
		t.Errorf("expected 'solidity', got %q", p.Language())
	}
}

func TestSolidityImports(t *testing.T) {
	src := "import \"./IERC20.sol\";\nimport {Ownable} from \"@openzeppelin/contracts/access/Ownable.sol\";\n"
	p := NewSolidityParser()
	pf, err := p.Parse("token.sol", []byte(src))
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(pf.Imports) != 2 {
		t.Fatalf("expected 2 imports, got %d", len(pf.Imports))
	}
}

func TestSolidityFunctions(t *testing.T) {
	src := "contract Token {\n  function transfer(address to, uint256 amount) public returns (bool) {\n    return true;\n  }\n  function _mint(address to, uint256 amount) internal {\n  }\n}\n"
	p := NewSolidityParser()
	pf, err := p.Parse("token.sol", []byte(src))
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	hasContract := false
	hasFunc := false
	for _, s := range pf.Symbols {
		if s.Name == "Token" {
			hasContract = true
		}
		if s.Name == "transfer" {
			hasFunc = true
		}
	}
	if !hasContract {
		t.Error("expected Token contract")
	}
	if !hasFunc {
		t.Error("expected transfer function")
	}
}

func TestSolidityEmptyFile(t *testing.T) {
	p := NewSolidityParser()
	pf, err := p.Parse("empty.sol", []byte(""))
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(pf.Imports) != 0 || len(pf.Symbols) != 0 {
		t.Error("expected empty results")
	}
}

func TestSolidityCommentsSkipped(t *testing.T) {
	src := "// import \"./fake.sol\";\n// contract Hidden {}\n"
	p := NewSolidityParser()
	pf, err := p.Parse("comments.sol", []byte(src))
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(pf.Imports) != 0 || len(pf.Symbols) != 0 {
		t.Error("expected empty results from comments")
	}
}

func TestSoliditySampleFile(t *testing.T) {
	content, err := os.ReadFile("../../testdata/solidity/sample.sol")
	if err != nil {
		t.Fatalf("failed to read sample: %v", err)
	}
	p := NewSolidityParser()
	pf, err := p.Parse("sample.sol", content)
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
