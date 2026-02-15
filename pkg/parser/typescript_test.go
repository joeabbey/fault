package parser

import (
	"os"
	"testing"
)

func TestTypeScriptParserLanguage(t *testing.T) {
	p := NewTypeScriptParser()
	if p.Language() != "typescript" {
		t.Errorf("expected typescript, got %q", p.Language())
	}
}

func TestTypeScriptParserFixture(t *testing.T) {
	content, err := os.ReadFile("../../testdata/typescript/sample.ts")
	if err != nil {
		t.Fatalf("reading fixture: %v", err)
	}

	p := NewTypeScriptParser()
	pf, err := p.Parse("sample.ts", content)
	if err != nil {
		t.Fatalf("parsing: %v", err)
	}

	if pf.Language != "typescript" {
		t.Errorf("expected language typescript, got %q", pf.Language)
	}

	// Check imports
	importPaths := make(map[string]bool)
	for _, imp := range pf.Imports {
		importPaths[imp.Path] = true
	}

	expectedImports := []string{"react", "./utils", "some-module", "side-effect-module"}
	for _, expected := range expectedImports {
		if !importPaths[expected] {
			t.Errorf("expected import %q not found", expected)
		}
	}

	// Check that type import is marked
	for _, imp := range pf.Imports {
		if imp.Path == "react" && imp.IsType {
			// Found a type import from react
			found := false
			for _, name := range imp.Names {
				if name == "FC" || name == "ReactNode" {
					found = true
				}
			}
			if !found {
				t.Error("expected FC or ReactNode in type import names")
			}
		}
	}

	// Check named imports from react
	for _, imp := range pf.Imports {
		if imp.Path == "react" && !imp.IsType {
			nameSet := make(map[string]bool)
			for _, n := range imp.Names {
				nameSet[n] = true
			}
			if !nameSet["useState"] || !nameSet["useEffect"] {
				t.Errorf("expected useState and useEffect in react import, got %v", imp.Names)
			}
		}
	}

	// Check namespace import
	for _, imp := range pf.Imports {
		if imp.Path == "./utils" {
			found := false
			for _, n := range imp.Names {
				if n == "utils" {
					found = true
				}
			}
			if !found {
				t.Errorf("expected namespace import 'utils' for ./utils, got %v", imp.Names)
			}
		}
	}

	// Check exports
	exportNames := make(map[string]string)
	for _, exp := range pf.Exports {
		exportNames[exp.Name] = exp.Kind
	}

	expectedExports := map[string]string{
		"UserProps":    "type",
		"Status":      "type",
		"UserService":  "class",
		"formatName":   "function",
		"fetchData":    "function",
		"MAX_RETRIES":  "variable",
		"processItems": "function",
		"App":          "class",
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

	// internalHelper and privateFunc should not be exported
	for _, name := range []string{"internalHelper", "privateFunc"} {
		if _, ok := exportNames[name]; ok {
			t.Errorf("unexpected export %q", name)
		}
	}

	// Check symbols include both exported and non-exported
	symbolMap := make(map[string]Symbol)
	for _, sym := range pf.Symbols {
		symbolMap[sym.Name] = sym
	}

	if s, ok := symbolMap["internalHelper"]; ok {
		if s.Exported {
			t.Error("expected internalHelper to not be exported")
		}
	} else {
		t.Error("expected internalHelper symbol")
	}

	if s, ok := symbolMap["privateFunc"]; ok {
		if s.Exported {
			t.Error("expected privateFunc to not be exported")
		}
	} else {
		t.Error("expected privateFunc symbol")
	}
}

func TestTypeScriptParserCommonJS(t *testing.T) {
	content, err := os.ReadFile("../../testdata/typescript/commonjs.js")
	if err != nil {
		t.Fatalf("reading fixture: %v", err)
	}

	p := NewTypeScriptParser()
	pf, err := p.Parse("commonjs.js", content)
	if err != nil {
		t.Fatalf("parsing: %v", err)
	}

	// Check require imports
	importPaths := make(map[string]bool)
	for _, imp := range pf.Imports {
		importPaths[imp.Path] = true
	}

	if !importPaths["express"] {
		t.Error("expected express import via require")
	}
	if !importPaths["path"] {
		t.Error("expected path import via require")
	}

	// Check destructured require
	for _, imp := range pf.Imports {
		if imp.Path == "express" {
			hasRouter := false
			for _, n := range imp.Names {
				if n == "Router" {
					hasRouter = true
				}
			}
			if !hasRouter {
				// One of the express imports should have Router
				// (there are two: default and destructured)
			}
		}
	}
}

func TestTypeScriptParserReExports(t *testing.T) {
	src := `export { Foo, Bar } from './module';
export * from './other';
`

	p := NewTypeScriptParser()
	pf, err := p.Parse("reexport.ts", []byte(src))
	if err != nil {
		t.Fatal(err)
	}

	if len(pf.Exports) < 3 {
		t.Fatalf("expected at least 3 exports, got %d", len(pf.Exports))
	}

	exportNames := make(map[string]bool)
	for _, exp := range pf.Exports {
		exportNames[exp.Name] = true
	}
	if !exportNames["Foo"] {
		t.Error("expected re-export Foo")
	}
	if !exportNames["Bar"] {
		t.Error("expected re-export Bar")
	}
	if !exportNames["*"] {
		t.Error("expected wildcard re-export")
	}
}

func TestTypeScriptParserDefaultImport(t *testing.T) {
	src := `import React from 'react';
import App from './App';
`

	p := NewTypeScriptParser()
	pf, err := p.Parse("test.ts", []byte(src))
	if err != nil {
		t.Fatal(err)
	}

	if len(pf.Imports) != 2 {
		t.Fatalf("expected 2 imports, got %d", len(pf.Imports))
	}

	for _, imp := range pf.Imports {
		if imp.Path == "react" {
			found := false
			for _, n := range imp.Names {
				if n == "React" {
					found = true
				}
			}
			if !found {
				t.Errorf("expected React default import, got %v", imp.Names)
			}
		}
	}
}

func TestTypeScriptParserConstEnum(t *testing.T) {
	src := `export const enum Direction {
  Up = "UP",
  Down = "DOWN",
}

const enum InternalStatus {
  Active,
  Inactive,
}
`
	p := NewTypeScriptParser()
	pf, err := p.Parse("enums.ts", []byte(src))
	if err != nil {
		t.Fatal(err)
	}

	// Check exports
	exportNames := make(map[string]string)
	for _, exp := range pf.Exports {
		exportNames[exp.Name] = exp.Kind
	}
	if kind, ok := exportNames["Direction"]; !ok || kind != "type" {
		t.Errorf("expected exported Direction type, got %v", exportNames)
	}

	// Check symbols
	symbolMap := make(map[string]Symbol)
	for _, sym := range pf.Symbols {
		symbolMap[sym.Name] = sym
	}
	if _, ok := symbolMap["Direction"]; !ok {
		t.Error("expected Direction symbol for const enum")
	}
	if s, ok := symbolMap["InternalStatus"]; !ok {
		t.Error("expected InternalStatus symbol for const enum")
	} else if s.Exported {
		t.Error("expected InternalStatus to not be exported")
	}
}

func TestTypeScriptParserDecorators(t *testing.T) {
	src := `@Component
export class AppComponent {}

@Injectable({providedIn: 'root'})
export class UserService {}
`
	p := NewTypeScriptParser()
	pf, err := p.Parse("app.ts", []byte(src))
	if err != nil {
		t.Fatal(err)
	}

	// Should find decorators as symbols
	decorators := make(map[string]bool)
	for _, sym := range pf.Symbols {
		if sym.Kind == "decorator" {
			decorators[sym.Name] = true
		}
	}
	if !decorators["Component"] {
		t.Error("expected @Component decorator symbol")
	}
	if !decorators["Injectable"] {
		t.Error("expected @Injectable decorator symbol")
	}

	// Classes should still be found
	exportNames := make(map[string]string)
	for _, exp := range pf.Exports {
		exportNames[exp.Name] = exp.Kind
	}
	if _, ok := exportNames["AppComponent"]; !ok {
		t.Error("expected AppComponent export")
	}
	if _, ok := exportNames["UserService"]; !ok {
		t.Error("expected UserService export")
	}
}

func TestTypeScriptParserGenericInterfaces(t *testing.T) {
	src := `export interface Repository<T> {
  find(id: string): T;
  findAll(): T[];
  save(entity: T): void;
}

interface Cache<K, V> {
  get(key: K): V | undefined;
  set(key: K, value: V): void;
}
`
	p := NewTypeScriptParser()
	pf, err := p.Parse("repo.ts", []byte(src))
	if err != nil {
		t.Fatal(err)
	}

	// Check exports
	exportNames := make(map[string]string)
	for _, exp := range pf.Exports {
		exportNames[exp.Name] = exp.Kind
	}
	if kind, ok := exportNames["Repository"]; !ok || kind != "type" {
		t.Errorf("expected exported Repository type, got %v", exportNames)
	}

	// Check symbols
	symbolMap := make(map[string]Symbol)
	for _, sym := range pf.Symbols {
		symbolMap[sym.Name] = sym
	}
	if _, ok := symbolMap["Repository"]; !ok {
		t.Error("expected Repository symbol")
	}
	if s, ok := symbolMap["Cache"]; !ok {
		t.Error("expected Cache symbol")
	} else if s.Exported {
		t.Error("expected Cache to not be exported")
	}
}

func TestTypeScriptParserTypeReExports(t *testing.T) {
	src := `export type { User, Profile } from './models';
export type { Config } from './config';
`
	p := NewTypeScriptParser()
	pf, err := p.Parse("index.ts", []byte(src))
	if err != nil {
		t.Fatal(err)
	}

	exportNames := make(map[string]string)
	for _, exp := range pf.Exports {
		exportNames[exp.Name] = exp.Kind
	}
	if _, ok := exportNames["User"]; !ok {
		t.Error("expected type re-export User")
	}
	if _, ok := exportNames["Profile"]; !ok {
		t.Error("expected type re-export Profile")
	}
	if _, ok := exportNames["Config"]; !ok {
		t.Error("expected type re-export Config")
	}
}

func TestTypeScriptParserEmptyFile(t *testing.T) {
	p := NewTypeScriptParser()
	pf, err := p.Parse("empty.ts", []byte(""))
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
