package parser

import (
	"os"
	"testing"
)

func TestRParserLanguage(t *testing.T) {
	p := NewRParser()
	if p.Language() != "r" {
		t.Errorf("expected r, got %q", p.Language())
	}
}

func TestRParserFixture(t *testing.T) {
	content, err := os.ReadFile("../../testdata/r/sample.r")
	if err != nil {
		t.Fatalf("reading fixture: %v", err)
	}

	p := NewRParser()
	pf, err := p.Parse("sample.r", content)
	if err != nil {
		t.Fatalf("parsing: %v", err)
	}

	if pf.Language != "r" {
		t.Errorf("expected language r, got %q", pf.Language)
	}

	// Check imports
	importPaths := make(map[string]bool)
	for _, imp := range pf.Imports {
		importPaths[imp.Path] = true
	}

	expectedImports := []string{"dplyr", "tidyr", "ggplot2", "jsonlite", "readr", "stats"}
	for _, expected := range expectedImports {
		if !importPaths[expected] {
			t.Errorf("expected import %q not found (have: %v)", expected, importPaths)
		}
	}

	// Check exports
	exportNames := make(map[string]string)
	for _, exp := range pf.Exports {
		exportNames[exp.Name] = exp.Kind
	}

	expectedExports := map[string]string{
		"clean_data":        "function",
		"summarize_results": "function",
		"fetch_remote_data": "function",
		"outer_function":    "function",
		"format_output":     "function",
		"SurveyResult":      "class",
		"DataPipeline":      "class",
		"ConfigManager":     "class",
	}

	for name, kind := range expectedExports {
		gotKind, ok := exportNames[name]
		if !ok {
			t.Errorf("expected export %q not found", name)
			continue
		}
		if gotKind != kind {
			t.Errorf("export %q: expected kind %q, got %q", name, kind, gotKind)
		}
	}

	// .validate_input should not be exported (starts with dot)
	if _, ok := exportNames[".validate_input"]; ok {
		t.Error("unexpected export .validate_input (private function)")
	}

	// Check symbols
	symbolMap := make(map[string]Symbol)
	for _, sym := range pf.Symbols {
		symbolMap[sym.Name] = sym
	}

	// Private function should exist but not be exported
	if s, ok := symbolMap[".validate_input"]; ok {
		if s.Exported {
			t.Error("expected .validate_input to not be exported")
		}
		if s.Kind != "function" {
			t.Errorf("expected .validate_input to be function, got %q", s.Kind)
		}
	} else {
		t.Error("expected .validate_input symbol")
	}

	// S4 class
	if s, ok := symbolMap["SurveyResult"]; ok {
		if s.Kind != "class" {
			t.Errorf("expected SurveyResult to be class, got %q", s.Kind)
		}
	} else {
		t.Error("expected SurveyResult symbol")
	}

	// R6 class
	if s, ok := symbolMap["DataPipeline"]; ok {
		if s.Kind != "class" {
			t.Errorf("expected DataPipeline to be class, got %q", s.Kind)
		}
	} else {
		t.Error("expected DataPipeline symbol")
	}

	// setRefClass
	if s, ok := symbolMap["ConfigManager"]; ok {
		if s.Kind != "class" {
			t.Errorf("expected ConfigManager to be class, got %q", s.Kind)
		}
	} else {
		t.Error("expected ConfigManager symbol")
	}

	// Constants
	if _, ok := symbolMap["MAX_RETRIES"]; !ok {
		t.Error("expected MAX_RETRIES symbol")
	}
	if _, ok := symbolMap["DEFAULT_TIMEOUT"]; !ok {
		t.Error("expected DEFAULT_TIMEOUT symbol")
	}
	if _, ok := symbolMap["API_VERSION"]; !ok {
		t.Error("expected API_VERSION symbol")
	}
}

func TestRParserEmptyFile(t *testing.T) {
	p := NewRParser()
	pf, err := p.Parse("empty.r", []byte(""))
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

func TestRParserNamespaceImports(t *testing.T) {
	src := `result <- dplyr::filter(df, x > 1)
plot <- ggplot2::ggplot(df, ggplot2::aes(x, y))
stats::mean(x)
`

	p := NewRParser()
	pf, err := p.Parse("test.r", []byte(src))
	if err != nil {
		t.Fatal(err)
	}

	importPaths := make(map[string]bool)
	for _, imp := range pf.Imports {
		importPaths[imp.Path] = true
	}

	if !importPaths["dplyr"] {
		t.Error("expected namespace import dplyr")
	}
	if !importPaths["ggplot2"] {
		t.Error("expected namespace import ggplot2")
	}
	if !importPaths["stats"] {
		t.Error("expected namespace import stats")
	}

	// ggplot2 should appear only once despite multiple uses
	count := 0
	for _, imp := range pf.Imports {
		if imp.Path == "ggplot2" {
			count++
		}
	}
	if count != 1 {
		t.Errorf("expected ggplot2 import once, got %d times", count)
	}
}

func TestRParserFunctionSignatures(t *testing.T) {
	src := `my_func <- function(x, y = 10) {
  x + y
}
`

	p := NewRParser()
	pf, err := p.Parse("test.r", []byte(src))
	if err != nil {
		t.Fatal(err)
	}

	if len(pf.Symbols) != 1 {
		t.Fatalf("expected 1 symbol, got %d", len(pf.Symbols))
	}

	sym := pf.Symbols[0]
	if sym.Name != "my_func" {
		t.Errorf("expected name my_func, got %q", sym.Name)
	}
	if sym.Signature != "my_func <- function(x, y = 10)" {
		t.Errorf("unexpected signature: %q", sym.Signature)
	}
}
