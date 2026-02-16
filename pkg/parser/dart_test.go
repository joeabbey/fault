package parser

import (
	"os"
	"testing"
)

func TestDartParserLanguage(t *testing.T) {
	p := NewDartParser()
	if p.Language() != "dart" {
		t.Errorf("expected dart, got %q", p.Language())
	}
}

func TestDartParserFixture(t *testing.T) {
	content, err := os.ReadFile("../../testdata/dart/sample.dart")
	if err != nil {
		t.Fatalf("reading fixture: %v", err)
	}

	p := NewDartParser()
	pf, err := p.Parse("sample.dart", content)
	if err != nil {
		t.Fatalf("parsing: %v", err)
	}

	if pf.Language != "dart" {
		t.Errorf("expected language dart, got %q", pf.Language)
	}

	// Check imports — 6 import lines + 2 export directive lines = 8 total
	if len(pf.Imports) != 8 {
		t.Errorf("expected 8 imports, got %d", len(pf.Imports))
		for _, imp := range pf.Imports {
			t.Logf("  import: %s (names=%v)", imp.Path, imp.Names)
		}
	}

	importPaths := make(map[string]bool)
	for _, imp := range pf.Imports {
		importPaths[imp.Path] = true
	}

	expectedImports := []string{
		"dart:async",
		"dart:convert",
		"package:flutter/material.dart",
		"package:http/http.dart",
		"package:provider/provider.dart",
		"../utils/helpers.dart",
		"package:flutter/widgets.dart",
		"src/models.dart",
	}
	for _, expected := range expectedImports {
		if !importPaths[expected] {
			t.Errorf("expected import %q not found; have: %v", expected, importPaths)
		}
	}

	// Check aliased import
	for _, imp := range pf.Imports {
		if imp.Path == "package:http/http.dart" {
			found := false
			for _, n := range imp.Names {
				if n == "http" {
					found = true
				}
			}
			if !found {
				t.Error("expected aliased import package:http/http.dart to have name 'http'")
			}
		}
	}
}

func TestDartParserExports(t *testing.T) {
	content, err := os.ReadFile("../../testdata/dart/sample.dart")
	if err != nil {
		t.Fatalf("reading fixture: %v", err)
	}

	p := NewDartParser()
	pf, err := p.Parse("sample.dart", content)
	if err != nil {
		t.Fatalf("parsing: %v", err)
	}

	exportNames := make(map[string]string)
	for _, exp := range pf.Exports {
		exportNames[exp.Name] = exp.Kind
	}

	// Public classes
	expectedExports := map[string]string{
		"JsonMap":             "typedef",
		"OnPressed":          "typedef",
		"ValidationMixin":    "mixin",
		"StringExtension":    "extension",
		"UserRole":           "enum",
		"Repository":         "class",
		"User":               "class",
		"ApiService":         "class",
		"UserProfileWidget":  "class",
		"initializeApp":      "function",
		"loadConfiguration":  "function",
	}

	for name, kind := range expectedExports {
		if gotKind, ok := exportNames[name]; !ok {
			t.Errorf("expected export %q not found", name)
		} else if gotKind != kind {
			t.Errorf("expected export %q kind=%s, got %s", name, kind, gotKind)
		}
	}

	// Private symbols should NOT be exported
	if _, ok := exportNames["_InternalCache"]; ok {
		t.Error("_InternalCache should not be exported (private)")
	}
	if _, ok := exportNames["_UserProfileWidgetState"]; ok {
		t.Error("_UserProfileWidgetState should not be exported (private)")
	}
	if _, ok := exportNames["_setupLogging"]; ok {
		t.Error("_setupLogging should not be exported (private)")
	}
}

func TestDartParserSymbols(t *testing.T) {
	content, err := os.ReadFile("../../testdata/dart/sample.dart")
	if err != nil {
		t.Fatalf("reading fixture: %v", err)
	}

	p := NewDartParser()
	pf, err := p.Parse("sample.dart", content)
	if err != nil {
		t.Fatalf("parsing: %v", err)
	}

	symbolMap := make(map[string]Symbol)
	for _, sym := range pf.Symbols {
		symbolMap[sym.Name] = sym
	}

	// Check typedef
	if sym, ok := symbolMap["JsonMap"]; !ok {
		t.Error("expected symbol 'JsonMap' not found")
	} else {
		if sym.Kind != "typedef" {
			t.Errorf("expected JsonMap kind=typedef, got %q", sym.Kind)
		}
		if !sym.Exported {
			t.Error("expected JsonMap to be exported")
		}
	}

	// Check mixin
	if sym, ok := symbolMap["ValidationMixin"]; !ok {
		t.Error("expected symbol 'ValidationMixin' not found")
	} else {
		if sym.Kind != "mixin" {
			t.Errorf("expected ValidationMixin kind=mixin, got %q", sym.Kind)
		}
	}

	// Check extension
	if sym, ok := symbolMap["StringExtension"]; !ok {
		t.Error("expected symbol 'StringExtension' not found")
	} else {
		if sym.Kind != "extension" {
			t.Errorf("expected StringExtension kind=extension, got %q", sym.Kind)
		}
	}

	// Check enum
	if sym, ok := symbolMap["UserRole"]; !ok {
		t.Error("expected symbol 'UserRole' not found")
	} else {
		if sym.Kind != "enum" {
			t.Errorf("expected UserRole kind=enum, got %q", sym.Kind)
		}
	}

	// Check abstract class
	if sym, ok := symbolMap["Repository"]; !ok {
		t.Error("expected symbol 'Repository' not found")
	} else {
		if sym.Kind != "class" {
			t.Errorf("expected Repository kind=class, got %q", sym.Kind)
		}
	}

	// Check regular class
	if sym, ok := symbolMap["User"]; !ok {
		t.Error("expected symbol 'User' not found")
	} else {
		if sym.Kind != "class" {
			t.Errorf("expected User kind=class, got %q", sym.Kind)
		}
	}

	// Check top-level variables
	if sym, ok := symbolMap["appName"]; !ok {
		t.Error("expected symbol 'appName' not found")
	} else {
		if sym.Kind != "variable" {
			t.Errorf("expected appName kind=variable, got %q", sym.Kind)
		}
		if !sym.Exported {
			t.Error("expected appName to be exported")
		}
	}

	if sym, ok := symbolMap["defaultPadding"]; !ok {
		t.Error("expected symbol 'defaultPadding' not found")
	} else {
		if sym.Kind != "variable" {
			t.Errorf("expected defaultPadding kind=variable, got %q", sym.Kind)
		}
	}

	// Check top-level functions
	if sym, ok := symbolMap["initializeApp"]; !ok {
		t.Error("expected symbol 'initializeApp' not found")
	} else {
		if sym.Kind != "function" {
			t.Errorf("expected initializeApp kind=function, got %q", sym.Kind)
		}
		if !sym.Exported {
			t.Error("expected initializeApp to be exported")
		}
	}

	// Check private function is not exported
	if sym, ok := symbolMap["_setupLogging"]; !ok {
		t.Error("expected symbol '_setupLogging' not found")
	} else {
		if sym.Exported {
			t.Error("expected _setupLogging to NOT be exported (private)")
		}
	}

	// Check private class is not exported
	if sym, ok := symbolMap["_InternalCache"]; !ok {
		t.Error("expected symbol '_InternalCache' not found")
	} else {
		if sym.Exported {
			t.Error("expected _InternalCache to NOT be exported (private)")
		}
	}
}

func TestDartParserInlineCode(t *testing.T) {
	code := `import 'dart:io';
import 'package:path/path.dart' as p;

class FileManager {
  final String basePath;

  FileManager(this.basePath);

  String resolve(String name) {
    return p.join(basePath, name);
  }
}

void main() {
  final manager = FileManager('/tmp');
}
`
	p := NewDartParser()
	pf, err := p.Parse("file_manager.dart", []byte(code))
	if err != nil {
		t.Fatalf("parsing: %v", err)
	}

	if len(pf.Imports) != 2 {
		t.Errorf("expected 2 imports, got %d", len(pf.Imports))
	}

	// Check aliased import
	for _, imp := range pf.Imports {
		if imp.Path == "package:path/path.dart" {
			found := false
			for _, n := range imp.Names {
				if n == "p" {
					found = true
				}
			}
			if !found {
				t.Error("expected aliased import to have name 'p'")
			}
		}
	}

	symbolNames := make(map[string]bool)
	for _, sym := range pf.Symbols {
		symbolNames[sym.Name] = true
	}

	if !symbolNames["FileManager"] {
		t.Error("expected symbol 'FileManager' not found")
	}
	if !symbolNames["main"] {
		t.Error("expected symbol 'main' not found")
	}
}

func TestDartParserBlockComments(t *testing.T) {
	code := `import 'dart:core';

/*
class InsideComment {
  void doSomething() {}
}
*/

class RealClass {}
`
	p := NewDartParser()
	pf, err := p.Parse("comments.dart", []byte(code))
	if err != nil {
		t.Fatalf("parsing: %v", err)
	}

	symbolMap := make(map[string]Symbol)
	for _, sym := range pf.Symbols {
		symbolMap[sym.Name] = sym
	}

	if _, ok := symbolMap["InsideComment"]; ok {
		t.Error("should not find symbols inside block comments")
	}
	if _, ok := symbolMap["RealClass"]; !ok {
		t.Error("expected symbol 'RealClass' not found")
	}
}
