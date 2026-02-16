package parser

import (
	"os"
	"testing"
)

func TestCppParserLanguage(t *testing.T) {
	p := NewCppParser()
	if p.Language() != "cpp" {
		t.Errorf("expected cpp, got %q", p.Language())
	}
}

func TestCppParserImports(t *testing.T) {
	src := `#include <iostream>
#include <string>
#include <vector>
#include "local.hpp"
using namespace std;
using std::shared_ptr;
`

	p := NewCppParser()
	pf, err := p.Parse("main.cpp", []byte(src))
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}

	if len(pf.Imports) != 6 {
		t.Fatalf("expected 6 imports, got %d", len(pf.Imports))
	}

	expected := []string{"iostream", "string", "vector", "local.hpp", "std", "std::shared_ptr"}
	for i, exp := range expected {
		if pf.Imports[i].Path != exp {
			t.Errorf("import %d: expected path %q, got %q", i, exp, pf.Imports[i].Path)
		}
	}
}

func TestCppParserClasses(t *testing.T) {
	src := `class Animal {
public:
    virtual void speak() = 0;
};

class Dog : public Animal {
public:
    void speak() override {}
};

struct Point {
    double x;
    double y;
};
`

	p := NewCppParser()
	pf, err := p.Parse("main.cpp", []byte(src))
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}

	symMap := make(map[string]string)
	for _, sym := range pf.Symbols {
		symMap[sym.Name] = sym.Kind
	}

	if kind, ok := symMap["Animal"]; !ok {
		t.Error("expected Animal class not found")
	} else if kind != "class" {
		t.Errorf("Animal: expected kind 'class', got %q", kind)
	}

	if kind, ok := symMap["Dog"]; !ok {
		t.Error("expected Dog class not found")
	} else if kind != "class" {
		t.Errorf("Dog: expected kind 'class', got %q", kind)
	}

	if kind, ok := symMap["Point"]; !ok {
		t.Error("expected Point struct not found")
	} else if kind != "struct" {
		t.Errorf("Point: expected kind 'struct', got %q", kind)
	}
}

func TestCppParserNamespaces(t *testing.T) {
	src := `namespace geometry {
    class Shape {};
}

namespace utils {
    void log(const string& msg) {}
}
`

	p := NewCppParser()
	pf, err := p.Parse("main.cpp", []byte(src))
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}

	nsNames := make(map[string]bool)
	for _, sym := range pf.Symbols {
		if sym.Kind == "namespace" {
			nsNames[sym.Name] = true
		}
	}

	if !nsNames["geometry"] {
		t.Error("expected geometry namespace not found")
	}
	if !nsNames["utils"] {
		t.Error("expected utils namespace not found")
	}
}

func TestCppParserTemplates(t *testing.T) {
	src := `template <typename T>
class Container {
public:
    void add(const T& item);
};

template <typename T, typename U>
class Pair {
public:
    T first() const;
};

template <typename T>
T max_value(T a, T b) {
    return (a > b) ? a : b;
}
`

	p := NewCppParser()
	pf, err := p.Parse("main.cpp", []byte(src))
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}

	symNames := make(map[string]string)
	for _, sym := range pf.Symbols {
		symNames[sym.Name] = sym.Kind
	}

	if kind, ok := symNames["Container"]; !ok {
		t.Error("expected Container template class not found")
	} else if kind != "class" {
		t.Errorf("Container: expected kind 'class', got %q", kind)
	}

	if kind, ok := symNames["Pair"]; !ok {
		t.Error("expected Pair template class not found")
	} else if kind != "class" {
		t.Errorf("Pair: expected kind 'class', got %q", kind)
	}

	if kind, ok := symNames["max_value"]; !ok {
		t.Error("expected max_value template function not found")
	} else if kind != "function" {
		t.Errorf("max_value: expected kind 'function', got %q", kind)
	}
}

func TestCppParserEnums(t *testing.T) {
	src := `enum class Color {
    Red,
    Green,
    Blue
};

enum OldStyle {
    VALUE_A,
    VALUE_B
};
`

	p := NewCppParser()
	pf, err := p.Parse("main.cpp", []byte(src))
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}

	symNames := make(map[string]bool)
	for _, sym := range pf.Symbols {
		if sym.Kind == "type" {
			symNames[sym.Name] = true
		}
	}

	if !symNames["Color"] {
		t.Error("expected Color enum class not found")
	}
	if !symNames["OldStyle"] {
		t.Error("expected OldStyle enum not found")
	}
}

func TestCppParserFunctions(t *testing.T) {
	src := `void log_message(const string& msg) {
    cout << msg << endl;
}

static void internal_helper() {
    // not exported
}

int main(int argc, char** argv) {
    return 0;
}
`

	p := NewCppParser()
	pf, err := p.Parse("main.cpp", []byte(src))
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}

	symMap := make(map[string]Symbol)
	for _, sym := range pf.Symbols {
		if sym.Kind == "function" {
			symMap[sym.Name] = sym
		}
	}

	if sym, ok := symMap["log_message"]; !ok {
		t.Error("expected log_message function not found")
	} else if !sym.Exported {
		t.Error("log_message should be exported")
	}

	if sym, ok := symMap["internal_helper"]; !ok {
		t.Error("expected internal_helper function not found")
	} else if sym.Exported {
		t.Error("internal_helper should not be exported (static)")
	}

	if _, ok := symMap["main"]; !ok {
		t.Error("expected main function not found")
	}
}

func TestCppParserBlockComments(t *testing.T) {
	src := `/* Block comment
 * class FakeClass {} should be ignored
 */
class RealClass {
};
`

	p := NewCppParser()
	pf, err := p.Parse("main.cpp", []byte(src))
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}

	for _, sym := range pf.Symbols {
		if sym.Name == "FakeClass" {
			t.Error("FakeClass should not be parsed (inside block comment)")
		}
	}

	found := false
	for _, sym := range pf.Symbols {
		if sym.Name == "RealClass" {
			found = true
		}
	}
	if !found {
		t.Error("expected RealClass not found")
	}
}

func TestCppParserSampleFile(t *testing.T) {
	content, err := os.ReadFile("../../testdata/cpp/sample.cpp")
	if err != nil {
		t.Fatalf("failed to read testdata: %v", err)
	}

	p := NewCppParser()
	pf, err := p.Parse("testdata/cpp/sample.cpp", content)
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}

	// Verify imports: iostream, string, vector, algorithm, sample.hpp, utils.hpp,
	//                 std (using namespace), std::shared_ptr, std::make_shared
	if len(pf.Imports) < 8 {
		t.Errorf("expected at least 8 imports, got %d", len(pf.Imports))
		for _, imp := range pf.Imports {
			t.Logf("  import: %s", imp.Path)
		}
	}

	importPaths := make(map[string]bool)
	for _, imp := range pf.Imports {
		importPaths[imp.Path] = true
	}
	for _, expected := range []string{"iostream", "string", "vector", "sample.hpp", "std"} {
		if !importPaths[expected] {
			t.Errorf("expected import %q not found", expected)
		}
	}

	// Check symbols
	symNames := make(map[string]bool)
	for _, sym := range pf.Symbols {
		symNames[sym.Name] = true
	}

	for _, expected := range []string{"geometry", "Circle", "Rectangle", "max_value", "Pair", "Priority", "OldStyle", "utils", "main"} {
		if !symNames[expected] {
			t.Errorf("expected symbol %q not found", expected)
		}
	}
}

func TestCppParserSampleHeader(t *testing.T) {
	content, err := os.ReadFile("../../testdata/cpp/sample.hpp")
	if err != nil {
		t.Fatalf("failed to read testdata: %v", err)
	}

	p := NewCppParser()
	pf, err := p.Parse("testdata/cpp/sample.hpp", content)
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}

	// Verify imports: string, vector, memory, types.h
	if len(pf.Imports) != 4 {
		t.Errorf("expected 4 imports, got %d", len(pf.Imports))
		for _, imp := range pf.Imports {
			t.Logf("  import: %s", imp.Path)
		}
	}

	// Check symbols
	symNames := make(map[string]bool)
	for _, sym := range pf.Symbols {
		symNames[sym.Name] = true
	}

	for _, expected := range []string{"geometry", "Shape", "Point", "Container", "Color"} {
		if !symNames[expected] {
			t.Errorf("expected symbol %q not found", expected)
		}
	}
}
