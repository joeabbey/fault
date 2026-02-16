package parser

import (
	"testing"
)

func TestSwiftParserLanguage(t *testing.T) {
	p := NewSwiftParser()
	if p.Language() != "swift" {
		t.Errorf("expected swift, got %q", p.Language())
	}
}

func TestSwiftParserImports(t *testing.T) {
	src := `import Foundation
import UIKit
@testable import MyModule
import SwiftUI
`

	p := NewSwiftParser()
	pf, err := p.Parse("main.swift", []byte(src))
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}

	if len(pf.Imports) != 4 {
		t.Fatalf("expected 4 imports, got %d", len(pf.Imports))
	}

	expected := []string{"Foundation", "UIKit", "MyModule", "SwiftUI"}
	for i, exp := range expected {
		if pf.Imports[i].Path != exp {
			t.Errorf("import %d: expected path %q, got %q", i, exp, pf.Imports[i].Path)
		}
	}
}

func TestSwiftParserAccessControl(t *testing.T) {
	src := `public class PublicClass {}
open class OpenClass {}
internal class InternalClass {}
fileprivate class FileprivateClass {}
private class PrivateClass {}
class DefaultClass {}
`

	p := NewSwiftParser()
	pf, err := p.Parse("main.swift", []byte(src))
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
	if !exportNames["OpenClass"] {
		t.Error("expected OpenClass to be exported")
	}
	if exportNames["InternalClass"] {
		t.Error("InternalClass should not be exported")
	}
	if exportNames["FileprivateClass"] {
		t.Error("FileprivateClass should not be exported")
	}
	if exportNames["PrivateClass"] {
		t.Error("PrivateClass should not be exported")
	}
	if exportNames["DefaultClass"] {
		t.Error("DefaultClass should not be exported")
	}
}

func TestSwiftParserTypes(t *testing.T) {
	src := `public struct Config {
    let name: String
}
public enum Status: String {
    case active
    case inactive
}
public protocol Processable {
    func process()
}
public actor DataManager {
    var items: [String] = []
}
extension String: Processable {
    func process() {}
}
public typealias Callback = (String) -> Void
`

	p := NewSwiftParser()
	pf, err := p.Parse("main.swift", []byte(src))
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}

	symKinds := make(map[string][]string)
	for _, sym := range pf.Symbols {
		symKinds[sym.Name] = append(symKinds[sym.Name], sym.Kind)
	}

	expectedSymbols := map[string]string{
		"Config":      "struct",
		"Status":      "type",
		"Processable": "interface",
		"DataManager": "type",
		"Callback":    "type",
	}

	for name, kind := range expectedSymbols {
		kinds, ok := symKinds[name]
		if !ok {
			t.Errorf("expected symbol %q not found", name)
			continue
		}
		found := false
		for _, k := range kinds {
			if k == kind {
				found = true
				break
			}
		}
		if !found {
			t.Errorf("symbol %q: expected kind %q in %v", name, kind, kinds)
		}
	}

	// String should appear as extension (type symbol, not exported)
	if kinds, ok := symKinds["String"]; ok {
		found := false
		for _, k := range kinds {
			if k == "type" {
				found = true
				break
			}
		}
		if !found {
			t.Errorf("expected String extension to have kind 'type', got %v", kinds)
		}
	} else {
		t.Error("expected String extension symbol not found")
	}
}

func TestSwiftParserFunctions(t *testing.T) {
	src := `public func processData(input: String) -> Bool {}
private func helper() {}
func defaultAccess() {}
public static func shared() -> Self {}
override func viewDidLoad() {}
public func asyncFetch() async throws -> Data {}
`

	p := NewSwiftParser()
	pf, err := p.Parse("main.swift", []byte(src))
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}

	symMap := make(map[string]Symbol)
	for _, sym := range pf.Symbols {
		if sym.Kind == "function" {
			symMap[sym.Name] = sym
		}
	}

	if sym, ok := symMap["processData"]; !ok {
		t.Error("expected processData function not found")
	} else if !sym.Exported {
		t.Error("processData should be exported")
	}

	if sym, ok := symMap["helper"]; !ok {
		t.Error("expected helper function not found")
	} else if sym.Exported {
		t.Error("helper should not be exported")
	}

	if sym, ok := symMap["defaultAccess"]; !ok {
		t.Error("expected defaultAccess function not found")
	} else if sym.Exported {
		t.Error("defaultAccess should not be exported (internal by default)")
	}

	if _, ok := symMap["shared"]; !ok {
		t.Error("expected static func shared not found")
	}

	if _, ok := symMap["viewDidLoad"]; !ok {
		t.Error("expected override func viewDidLoad not found")
	}

	if _, ok := symMap["asyncFetch"]; !ok {
		t.Error("expected async func asyncFetch not found")
	}
}

func TestSwiftParserProperties(t *testing.T) {
	src := `public let apiKey: String = "abc"
private var count: Int = 0
let defaultProp: Bool = true
public static var shared: Self = Self()
`

	p := NewSwiftParser()
	pf, err := p.Parse("main.swift", []byte(src))
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}

	symMap := make(map[string]Symbol)
	for _, sym := range pf.Symbols {
		if sym.Kind == "variable" {
			symMap[sym.Name] = sym
		}
	}

	if sym, ok := symMap["apiKey"]; !ok {
		t.Error("expected apiKey property not found")
	} else if !sym.Exported {
		t.Error("apiKey should be exported (public)")
	}

	if sym, ok := symMap["count"]; !ok {
		t.Error("expected count property not found")
	} else if sym.Exported {
		t.Error("count should not be exported (private)")
	}

	if sym, ok := symMap["defaultProp"]; !ok {
		t.Error("expected defaultProp property not found")
	} else if sym.Exported {
		t.Error("defaultProp should not be exported (internal by default)")
	}
}

func TestSwiftParserBlockComments(t *testing.T) {
	src := `/* This is a block comment */
import Foundation
/*
 * Multi-line block comment
 * with class declaration inside
 * class ShouldNotParse {}
 */
public class RealClass {}
`

	p := NewSwiftParser()
	pf, err := p.Parse("main.swift", []byte(src))
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}

	if len(pf.Imports) != 1 {
		t.Errorf("expected 1 import, got %d", len(pf.Imports))
	}

	// Should only find RealClass, not ShouldNotParse
	found := false
	for _, sym := range pf.Symbols {
		if sym.Name == "ShouldNotParse" {
			t.Error("ShouldNotParse should not be parsed (inside block comment)")
		}
		if sym.Name == "RealClass" {
			found = true
		}
	}
	if !found {
		t.Error("expected RealClass symbol not found")
	}
}

func TestSwiftParserFinalClass(t *testing.T) {
	src := `public final class Singleton {}
private final class InternalSingleton {}
`

	p := NewSwiftParser()
	pf, err := p.Parse("main.swift", []byte(src))
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}

	symMap := make(map[string]Symbol)
	for _, sym := range pf.Symbols {
		symMap[sym.Name] = sym
	}

	if sym, ok := symMap["Singleton"]; !ok {
		t.Error("expected Singleton not found")
	} else if !sym.Exported {
		t.Error("Singleton should be exported")
	}

	if sym, ok := symMap["InternalSingleton"]; !ok {
		t.Error("expected InternalSingleton not found")
	} else if sym.Exported {
		t.Error("InternalSingleton should not be exported")
	}
}
