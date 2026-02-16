package parser

import (
	"os"
	"testing"
)

func TestHaskellParserLanguage(t *testing.T) {
	p := NewHaskellParser()
	if p.Language() != "haskell" {
		t.Errorf("expected 'haskell', got %q", p.Language())
	}
}

func TestHaskellImports(t *testing.T) {
	src := `import Data.List
import qualified Data.Map as Map
import Control.Monad (when, unless)
`
	p := NewHaskellParser()
	pf, err := p.Parse("app.hs", []byte(src))
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(pf.Imports) != 3 {
		t.Fatalf("expected 3 imports, got %d", len(pf.Imports))
	}
	expected := []string{"Data.List", "Data.Map", "Control.Monad"}
	for i, exp := range expected {
		if pf.Imports[i].Path != exp {
			t.Errorf("import[%d]: expected %q, got %q", i, exp, pf.Imports[i].Path)
		}
	}
}

func TestHaskellFunctions(t *testing.T) {
	src := `greet :: String -> String
greet name = "Hello, " ++ name

add :: Int -> Int -> Int
add x y = x + y
`
	p := NewHaskellParser()
	pf, err := p.Parse("funcs.hs", []byte(src))
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
	if !names["add"] {
		t.Error("expected add function")
	}
}

func TestHaskellEmptyFile(t *testing.T) {
	p := NewHaskellParser()
	pf, err := p.Parse("empty.hs", []byte(""))
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(pf.Imports) != 0 || len(pf.Exports) != 0 || len(pf.Symbols) != 0 {
		t.Error("expected empty results")
	}
}

func TestHaskellCommentsSkipped(t *testing.T) {
	src := `-- import Data.List
{- import Data.Map -}
-- greet :: String -> String
`
	p := NewHaskellParser()
	pf, err := p.Parse("comments.hs", []byte(src))
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(pf.Imports) != 0 {
		t.Errorf("expected 0 imports, got %d", len(pf.Imports))
	}
}

func TestHaskellSampleFile(t *testing.T) {
	content, err := os.ReadFile("../../testdata/haskell/sample.hs")
	if err != nil {
		t.Fatalf("failed to read sample: %v", err)
	}
	p := NewHaskellParser()
	pf, err := p.Parse("sample.hs", content)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(pf.Imports) != 3 {
		t.Errorf("expected 3 imports, got %d", len(pf.Imports))
	}
	if len(pf.Symbols) < 3 {
		t.Errorf("expected at least 3 symbols, got %d", len(pf.Symbols))
	}
}
