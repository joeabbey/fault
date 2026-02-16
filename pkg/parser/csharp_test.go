package parser

import (
	"os"
	"testing"
)

func TestCSharpParserLanguage(t *testing.T) {
	p := NewCSharpParser()
	if p.Language() != "csharp" {
		t.Errorf("expected csharp, got %q", p.Language())
	}
}

func TestCSharpParserFixture(t *testing.T) {
	content, err := os.ReadFile("../../testdata/csharp/Sample.cs")
	if err != nil {
		t.Fatalf("reading fixture: %v", err)
	}

	p := NewCSharpParser()
	pf, err := p.Parse("Sample.cs", content)
	if err != nil {
		t.Fatalf("parsing: %v", err)
	}

	if pf.Language != "csharp" {
		t.Errorf("expected language csharp, got %q", pf.Language)
	}

	// Check imports
	importPaths := make(map[string]bool)
	for _, imp := range pf.Imports {
		importPaths[imp.Path] = true
	}

	expectedImports := []string{
		"System",
		"System.Collections.Generic",
		"System.Linq",
		"System.Math",
		"System.String",
		"System.Threading.Tasks",
	}
	for _, expected := range expectedImports {
		if !importPaths[expected] {
			t.Errorf("expected import %q not found; have: %v", expected, importPaths)
		}
	}

	// Check static imports are marked
	for _, imp := range pf.Imports {
		if imp.Path == "System.Math" && !imp.IsType {
			t.Error("expected static using to have IsType=true")
		}
	}

	// Check import names
	for _, imp := range pf.Imports {
		if imp.Path == "System.Collections.Generic" {
			found := false
			for _, n := range imp.Names {
				if n == "Generic" {
					found = true
				}
			}
			if !found {
				t.Error("expected import System.Collections.Generic to have name 'Generic'")
			}
		}
	}
}

func TestCSharpParserExports(t *testing.T) {
	content, err := os.ReadFile("../../testdata/csharp/Sample.cs")
	if err != nil {
		t.Fatalf("reading fixture: %v", err)
	}

	p := NewCSharpParser()
	pf, err := p.Parse("Sample.cs", content)
	if err != nil {
		t.Fatalf("parsing: %v", err)
	}

	exportNames := make(map[string]string)
	for _, exp := range pf.Exports {
		exportNames[exp.Name] = exp.Kind
	}

	// public sealed class Sample
	if kind, ok := exportNames["Sample"]; !ok {
		t.Error("expected export 'Sample' not found")
	} else if kind != "class" {
		t.Errorf("expected Sample to be class, got %q", kind)
	}

	// public enum Color
	if kind, ok := exportNames["Color"]; !ok {
		t.Error("expected export 'Color' not found")
	} else if kind != "enum" {
		t.Errorf("expected Color to be enum, got %q", kind)
	}

	// public record Point
	if kind, ok := exportNames["Point"]; !ok {
		t.Error("expected export 'Point' not found")
	} else if kind != "record" {
		t.Errorf("expected Point to be record, got %q", kind)
	}

	// public abstract class AbstractService
	if kind, ok := exportNames["AbstractService"]; !ok {
		t.Error("expected export 'AbstractService' not found")
	} else if kind != "class" {
		t.Errorf("expected AbstractService to be class, got %q", kind)
	}

	// public partial class PartialWidget
	if kind, ok := exportNames["PartialWidget"]; !ok {
		t.Error("expected export 'PartialWidget' not found")
	} else if kind != "class" {
		t.Errorf("expected PartialWidget to be class, got %q", kind)
	}

	// public struct Coordinate
	if kind, ok := exportNames["Coordinate"]; !ok {
		t.Error("expected export 'Coordinate' not found")
	} else if kind != "struct" {
		t.Errorf("expected Coordinate to be struct, got %q", kind)
	}

	// internal interface IConfigurable (not public, should NOT be exported)
	if _, ok := exportNames["IConfigurable"]; ok {
		t.Error("IConfigurable should not be exported (internal modifier)")
	}
}

func TestCSharpParserSymbols(t *testing.T) {
	content, err := os.ReadFile("../../testdata/csharp/Sample.cs")
	if err != nil {
		t.Fatalf("reading fixture: %v", err)
	}

	p := NewCSharpParser()
	pf, err := p.Parse("Sample.cs", content)
	if err != nil {
		t.Fatalf("parsing: %v", err)
	}

	// Build map keyed by name:kind for precise lookups
	type symKey struct {
		name string
		kind string
	}
	symbolSet := make(map[symKey]Symbol)
	for _, sym := range pf.Symbols {
		symbolSet[symKey{sym.Name, sym.Kind}] = sym
	}

	// Also build a simple name lookup
	symbolMap := make(map[string]Symbol)
	for _, sym := range pf.Symbols {
		symbolMap[sym.Name] = sym
	}

	// Check class symbol
	if sym, ok := symbolSet[symKey{"Sample", "class"}]; !ok {
		t.Error("expected symbol 'Sample' with kind=class not found")
	} else if !sym.Exported {
		t.Error("expected Sample to be exported")
	}

	// Check public method
	if sym, ok := symbolMap["GetName"]; !ok {
		t.Error("expected symbol 'GetName' not found")
	} else {
		if sym.Kind != "method" {
			t.Errorf("expected GetName kind=method, got %q", sym.Kind)
		}
		if !sym.Exported {
			t.Error("expected GetName to be exported (public)")
		}
	}

	// Check private method
	if sym, ok := symbolMap["Calculate"]; !ok {
		t.Error("expected symbol 'Calculate' not found")
	} else {
		if sym.Exported {
			t.Error("expected Calculate to NOT be exported (private)")
		}
	}

	// Check private field
	if sym, ok := symbolMap["_name"]; !ok {
		t.Error("expected symbol '_name' (field) not found")
	} else {
		if sym.Kind != "field" {
			t.Errorf("expected _name kind=field, got %q", sym.Kind)
		}
		if sym.Exported {
			t.Error("expected _name to NOT be exported (private)")
		}
	}

	// Check public static field
	if sym, ok := symbolMap["MaxSize"]; !ok {
		t.Error("expected symbol 'MaxSize' not found")
	} else {
		if sym.Kind != "field" {
			t.Errorf("expected MaxSize kind=field, got %q", sym.Kind)
		}
		if !sym.Exported {
			t.Error("expected MaxSize to be exported (public)")
		}
	}

	// Check interface
	if sym, ok := symbolMap["IConfigurable"]; !ok {
		t.Error("expected symbol 'IConfigurable' not found")
	} else {
		if sym.Kind != "interface" {
			t.Errorf("expected IConfigurable kind=interface, got %q", sym.Kind)
		}
	}

	// Check property
	if sym, ok := symbolMap["Name"]; !ok {
		t.Error("expected symbol 'Name' (property) not found")
	} else {
		if sym.Kind != "property" {
			t.Errorf("expected Name kind=property, got %q", sym.Kind)
		}
		if !sym.Exported {
			t.Error("expected Name property to be exported (public)")
		}
	}

	// Check async method
	if sym, ok := symbolMap["FetchDataAsync"]; !ok {
		t.Error("expected symbol 'FetchDataAsync' not found")
	} else {
		if sym.Kind != "method" {
			t.Errorf("expected FetchDataAsync kind=method, got %q", sym.Kind)
		}
		if !sym.Exported {
			t.Error("expected FetchDataAsync to be exported (public)")
		}
	}
}

func TestCSharpParserInlineCode(t *testing.T) {
	code := `using System.IO;

namespace MyApp;

public class MyService
{
    private readonly string _endpoint;

    public MyService(string endpoint)
    {
        _endpoint = endpoint;
    }

    public string Call()
    {
        return _endpoint;
    }
}
`
	p := NewCSharpParser()
	pf, err := p.Parse("MyService.cs", []byte(code))
	if err != nil {
		t.Fatalf("parsing: %v", err)
	}

	if len(pf.Imports) != 1 {
		t.Errorf("expected 1 import, got %d", len(pf.Imports))
	}

	if pf.Imports[0].Path != "System.IO" {
		t.Errorf("expected import path System.IO, got %q", pf.Imports[0].Path)
	}

	if len(pf.Exports) != 1 {
		t.Errorf("expected 1 export (MyService), got %d", len(pf.Exports))
	}

	symNames := make(map[string]bool)
	for _, sym := range pf.Symbols {
		symNames[sym.Name] = true
	}

	for _, expected := range []string{"MyService", "_endpoint", "Call"} {
		if !symNames[expected] {
			t.Errorf("expected symbol %q not found; have: %v", expected, symNames)
		}
	}
}
