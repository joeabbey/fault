package parser

import (
	"os"
	"testing"
)

func TestDlangParserLanguage(t *testing.T) {
	p := NewDlangParser()
	if p.Language() != "dlang" {
		t.Errorf("expected dlang, got %q", p.Language())
	}
}

func TestDlangParserImports(t *testing.T) {
	src := `import std.stdio;
import std.string;
import std.json : parseJSON, JSONValue;
static import std.algorithm;
`
	p := NewDlangParser()
	pf, err := p.Parse("main.d", []byte(src))
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	if len(pf.Imports) != 4 {
		t.Fatalf("expected 4 imports, got %d", len(pf.Imports))
	}
	if pf.Imports[0].Path != "std.stdio" || pf.Imports[0].Names[0] != "stdio" {
		t.Errorf("expected import std.stdio, got path=%q names=%v", pf.Imports[0].Path, pf.Imports[0].Names)
	}
	if pf.Imports[2].Path != "std.json" || len(pf.Imports[2].Names) != 2 {
		t.Errorf("expected import std.json with 2 names, got path=%q names=%v", pf.Imports[2].Path, pf.Imports[2].Names)
	}
	if pf.Imports[3].Path != "std.algorithm" {
		t.Errorf("expected static import std.algorithm, got %q", pf.Imports[3].Path)
	}
}

func TestDlangParserClassesAndStructs(t *testing.T) {
	src := `class Config {
}
struct Point {
}
interface Handler {
}
enum Status {
}
private class InternalCache {
}
`
	p := NewDlangParser()
	pf, err := p.Parse("main.d", []byte(src))
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	symMap := make(map[string]Symbol)
	for _, s := range pf.Symbols {
		symMap[s.Name] = s
	}
	if s, ok := symMap["Config"]; !ok || s.Kind != "class" || !s.Exported {
		t.Errorf("expected exported class Config, got %+v", symMap["Config"])
	}
	if s, ok := symMap["Point"]; !ok || s.Kind != "struct" || !s.Exported {
		t.Errorf("expected exported struct Point, got %+v", symMap["Point"])
	}
	if s, ok := symMap["Handler"]; !ok || s.Kind != "interface" || !s.Exported {
		t.Errorf("expected exported interface Handler, got %+v", symMap["Handler"])
	}
	if s, ok := symMap["Status"]; !ok || s.Kind != "type" || !s.Exported {
		t.Errorf("expected exported type Status, got %+v", symMap["Status"])
	}
	if s, ok := symMap["InternalCache"]; !ok || s.Exported {
		t.Errorf("expected private class InternalCache, got %+v", symMap["InternalCache"])
	}
}

func TestDlangParserExports(t *testing.T) {
	src := `class PublicClass {}
private class PrivateClass {}
struct PublicStruct {}
package struct PackageStruct {}
`
	p := NewDlangParser()
	pf, err := p.Parse("lib.d", []byte(src))
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	exportNames := make(map[string]bool)
	for _, exp := range pf.Exports {
		exportNames[exp.Name] = true
	}
	if !exportNames["PublicClass"] {
		t.Error("expected PublicClass to be exported")
	}
	if exportNames["PrivateClass"] {
		t.Error("PrivateClass should not be exported")
	}
	if !exportNames["PublicStruct"] {
		t.Error("expected PublicStruct to be exported")
	}
	if exportNames["PackageStruct"] {
		t.Error("PackageStruct should not be exported")
	}
}

func TestDlangParserEmptyFile(t *testing.T) {
	p := NewDlangParser()
	pf, err := p.Parse("empty.d", []byte(""))
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	if len(pf.Imports) != 0 || len(pf.Exports) != 0 || len(pf.Symbols) != 0 {
		t.Error("expected empty results for empty file")
	}
}

func TestDlangParserCommentsSkipped(t *testing.T) {
	src := `// Line comment
/* Block
   comment */
/+ Nesting
   /+ nested +/
   comment +/
import std.stdio;
class RealClass {}
`
	p := NewDlangParser()
	pf, err := p.Parse("main.d", []byte(src))
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	if len(pf.Imports) != 1 {
		t.Errorf("expected 1 import, got %d", len(pf.Imports))
	}
	if len(pf.Exports) != 1 {
		t.Errorf("expected 1 export, got %d", len(pf.Exports))
	}
}

func TestDlangParserTemplatesAndAliases(t *testing.T) {
	src := `template Singleton(T) {}
alias JsonMap = JSONValue[string];
mixin template Mixin(T) {}
`
	p := NewDlangParser()
	pf, err := p.Parse("types.d", []byte(src))
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	symMap := make(map[string]Symbol)
	for _, s := range pf.Symbols {
		symMap[s.Name] = s
	}
	if _, ok := symMap["Singleton"]; !ok {
		t.Error("expected symbol Singleton")
	}
	if _, ok := symMap["JsonMap"]; !ok {
		t.Error("expected symbol JsonMap")
	}
	if _, ok := symMap["Mixin"]; !ok {
		t.Error("expected symbol Mixin")
	}
}

func TestDlangParserSampleFile(t *testing.T) {
	content, err := os.ReadFile("../../testdata/dlang/sample.d")
	if err != nil {
		t.Fatalf("reading fixture: %v", err)
	}
	p := NewDlangParser()
	pf, err := p.Parse("sample.d", content)
	if err != nil {
		t.Fatalf("parsing: %v", err)
	}
	if pf.Language != "dlang" {
		t.Errorf("expected language dlang, got %q", pf.Language)
	}
	if len(pf.Imports) < 3 {
		t.Errorf("expected at least 3 imports, got %d", len(pf.Imports))
	}
	if len(pf.Symbols) < 5 {
		t.Errorf("expected at least 5 symbols, got %d", len(pf.Symbols))
	}
}
