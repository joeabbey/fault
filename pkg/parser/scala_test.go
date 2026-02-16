package parser

import (
	"os"
	"testing"
)

func TestScalaParserLanguage(t *testing.T) {
	p := NewScalaParser()
	if p.Language() != "scala" {
		t.Errorf("expected scala, got %q", p.Language())
	}
}

func TestScalaParserFixture(t *testing.T) {
	content, err := os.ReadFile("../../testdata/scala/sample.scala")
	if err != nil {
		t.Fatalf("reading fixture: %v", err)
	}

	p := NewScalaParser()
	pf, err := p.Parse("sample.scala", content)
	if err != nil {
		t.Fatalf("parsing: %v", err)
	}

	if pf.Language != "scala" {
		t.Errorf("expected language scala, got %q", pf.Language)
	}

	// Check imports
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
		"scala.concurrent.Future",
		"scala.concurrent.ExecutionContext.Implicits.global",
		"scala.collection.mutable.{ArrayBuffer, ListBuffer}",
		"scala.util._",
		"akka.actor.{Actor, ActorSystem, Props}",
		"java.time.Instant",
	}
	for _, expected := range expectedImports {
		if !importPaths[expected] {
			t.Errorf("expected import %q not found; have: %v", expected, importPaths)
		}
	}

	// Check selective import names
	for _, imp := range pf.Imports {
		if imp.Path == "scala.collection.mutable.{ArrayBuffer, ListBuffer}" {
			if len(imp.Names) != 2 {
				t.Errorf("expected selective import to have 2 names, got %v", imp.Names)
			}
		}
	}

	// Check wildcard import has no names
	for _, imp := range pf.Imports {
		if imp.Path == "scala.util._" {
			if len(imp.Names) != 0 {
				t.Errorf("expected wildcard import to have no names, got %v", imp.Names)
			}
		}
	}
}

func TestScalaParserExports(t *testing.T) {
	content, err := os.ReadFile("../../testdata/scala/sample.scala")
	if err != nil {
		t.Fatalf("reading fixture: %v", err)
	}

	p := NewScalaParser()
	pf, err := p.Parse("sample.scala", content)
	if err != nil {
		t.Fatalf("parsing: %v", err)
	}

	exportNames := make(map[string]string)
	for _, exp := range pf.Exports {
		exportNames[exp.Name] = exp.Kind
	}

	// Build a set of name:kind pairs for precise checks
	type exportKey struct {
		name string
		kind string
	}
	exportSet := make(map[exportKey]bool)
	for _, exp := range pf.Exports {
		exportSet[exportKey{exp.Name, exp.Kind}] = true
	}

	expectedExports := []exportKey{
		{"JsonMap", "type"},
		{"Priority", "enum"},
		{"AppEvent", "trait"},
		{"UserCreated", "case_class"},
		{"UserDeleted", "case_class"},
		{"MessageSent", "case_class"},
		{"Serializable", "trait"},
		{"Logging", "trait"},
		{"BaseRepository", "class"},
		{"User", "case_class"},
		{"User", "object"},
		{"UserService", "class"},
		{"Result", "class"},
		{"Success", "class"},
		{"Failure", "class"},
		{"MessageActor", "class"},
		{"AppConfig", "object"},
		{"initialize", "function"},
	}

	for _, ek := range expectedExports {
		if !exportSet[ek] {
			t.Errorf("expected export %q with kind=%s not found", ek.name, ek.kind)
		}
	}

	// Private should NOT be exported
	if _, ok := exportNames["setupInternal"]; ok {
		t.Error("setupInternal should not be exported (private)")
	}
}

func TestScalaParserSymbols(t *testing.T) {
	content, err := os.ReadFile("../../testdata/scala/sample.scala")
	if err != nil {
		t.Fatalf("reading fixture: %v", err)
	}

	p := NewScalaParser()
	pf, err := p.Parse("sample.scala", content)
	if err != nil {
		t.Fatalf("parsing: %v", err)
	}

	symbolMap := make(map[string]Symbol)
	for _, sym := range pf.Symbols {
		symbolMap[sym.Name] = sym
	}

	// Check type alias
	if sym, ok := symbolMap["JsonMap"]; !ok {
		t.Error("expected symbol 'JsonMap' not found")
	} else {
		if sym.Kind != "type" {
			t.Errorf("expected JsonMap kind=type, got %q", sym.Kind)
		}
		if !sym.Exported {
			t.Error("expected JsonMap to be exported")
		}
	}

	// Check enum (Scala 3)
	if sym, ok := symbolMap["Priority"]; !ok {
		t.Error("expected symbol 'Priority' not found")
	} else {
		if sym.Kind != "enum" {
			t.Errorf("expected Priority kind=enum, got %q", sym.Kind)
		}
	}

	// Check sealed trait
	if sym, ok := symbolMap["AppEvent"]; !ok {
		t.Error("expected symbol 'AppEvent' not found")
	} else {
		if sym.Kind != "trait" {
			t.Errorf("expected AppEvent kind=trait, got %q", sym.Kind)
		}
	}

	// Check case class User and companion object User both exist
	type symKey struct {
		name string
		kind string
	}
	symbolSet := make(map[symKey]bool)
	for _, sym := range pf.Symbols {
		symbolSet[symKey{sym.Name, sym.Kind}] = true
	}

	if !symbolSet[symKey{"User", "case_class"}] {
		t.Error("expected symbol 'User' with kind=case_class not found")
	}
	if !symbolSet[symKey{"User", "object"}] {
		t.Error("expected symbol 'User' with kind=object not found (companion object)")
	}

	// Check object
	if sym, ok := symbolMap["AppConfig"]; !ok {
		t.Error("expected symbol 'AppConfig' not found")
	} else {
		if sym.Kind != "object" {
			t.Errorf("expected AppConfig kind=object, got %q", sym.Kind)
		}
	}

	// Check top-level val
	if sym, ok := symbolMap["appVersion"]; !ok {
		t.Error("expected symbol 'appVersion' not found")
	} else {
		if sym.Kind != "variable" {
			t.Errorf("expected appVersion kind=variable, got %q", sym.Kind)
		}
		if !sym.Exported {
			t.Error("expected appVersion to be exported")
		}
	}

	// Check top-level var
	if sym, ok := symbolMap["requestCount"]; !ok {
		t.Error("expected symbol 'requestCount' not found")
	} else {
		if sym.Kind != "variable" {
			t.Errorf("expected requestCount kind=variable, got %q", sym.Kind)
		}
	}

	// Check top-level def
	if sym, ok := symbolMap["initialize"]; !ok {
		t.Error("expected symbol 'initialize' not found")
	} else {
		if sym.Kind != "function" {
			t.Errorf("expected initialize kind=function, got %q", sym.Kind)
		}
		if !sym.Exported {
			t.Error("expected initialize to be exported")
		}
	}

	// Check private def
	if sym, ok := symbolMap["setupInternal"]; !ok {
		t.Error("expected symbol 'setupInternal' not found")
	} else {
		if sym.Exported {
			t.Error("expected setupInternal to NOT be exported (private)")
		}
	}

	// Check given instance
	if sym, ok := symbolMap["userOrdering"]; !ok {
		t.Error("expected symbol 'userOrdering' not found")
	} else {
		if sym.Kind != "variable" {
			t.Errorf("expected userOrdering kind=variable, got %q", sym.Kind)
		}
	}
}

func TestScalaParserInlineCode(t *testing.T) {
	code := `package com.example

import scala.concurrent.Future

case class Config(host: String, port: Int)

object Config:
  def default: Config = Config("localhost", 8080)

trait Service:
  def start(): Future[Unit]
  def stop(): Future[Unit]

class HttpService(config: Config) extends Service:
  def start(): Future[Unit] = Future.successful(())
  def stop(): Future[Unit] = Future.successful(())
`
	p := NewScalaParser()
	pf, err := p.Parse("service.scala", []byte(code))
	if err != nil {
		t.Fatalf("parsing: %v", err)
	}

	if len(pf.Imports) != 1 {
		t.Errorf("expected 1 import, got %d", len(pf.Imports))
	}

	symbolNames := make(map[string]bool)
	for _, sym := range pf.Symbols {
		symbolNames[sym.Name] = true
	}

	for _, expected := range []string{"Config", "Service", "HttpService"} {
		if !symbolNames[expected] {
			t.Errorf("expected symbol %q not found", expected)
		}
	}
}

func TestScalaParserBlockComments(t *testing.T) {
	code := `package com.example

/*
class InsideComment
def alsoInsideComment(): Unit = ???
*/

class RealClass
`
	p := NewScalaParser()
	pf, err := p.Parse("comments.scala", []byte(code))
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
	if _, ok := symbolMap["alsoInsideComment"]; ok {
		t.Error("should not find symbols inside block comments")
	}
	if _, ok := symbolMap["RealClass"]; !ok {
		t.Error("expected symbol 'RealClass' not found")
	}
}
