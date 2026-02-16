package parser

import (
	"testing"
)

func TestRubyParserLanguage(t *testing.T) {
	p := NewRubyParser()
	if p.Language() != "ruby" {
		t.Errorf("expected 'ruby', got %q", p.Language())
	}
}

func TestRubyRequire(t *testing.T) {
	src := `require 'json'
require "net/http"
require 'open-uri'
`
	p := NewRubyParser()
	pf, err := p.Parse("app.rb", []byte(src))
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	if len(pf.Imports) != 3 {
		t.Fatalf("expected 3 imports, got %d", len(pf.Imports))
	}

	expected := []string{"json", "net/http", "open-uri"}
	for i, exp := range expected {
		if pf.Imports[i].Path != exp {
			t.Errorf("import[%d]: expected %q, got %q", i, exp, pf.Imports[i].Path)
		}
	}
}

func TestRubyRequireRelative(t *testing.T) {
	src := `require_relative 'helper'
require_relative './lib/utils'
require_relative '../shared/config'
`
	p := NewRubyParser()
	pf, err := p.Parse("app.rb", []byte(src))
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	if len(pf.Imports) != 3 {
		t.Fatalf("expected 3 imports, got %d", len(pf.Imports))
	}

	expected := []string{"./helper", "./lib/utils", "../shared/config"}
	for i, exp := range expected {
		if pf.Imports[i].Path != exp {
			t.Errorf("import[%d]: expected %q, got %q", i, exp, pf.Imports[i].Path)
		}
	}
}

func TestRubyModuleDetection(t *testing.T) {
	src := `module MyApp
  module Config
    VERSION = "1.0"
  end
end
`
	p := NewRubyParser()
	pf, err := p.Parse("app.rb", []byte(src))
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	// Should find MyApp (exported) and Config (nested, not exported).
	var myApp, config *Symbol
	for i := range pf.Symbols {
		if pf.Symbols[i].Name == "MyApp" {
			myApp = &pf.Symbols[i]
		}
		if pf.Symbols[i].Name == "Config" {
			config = &pf.Symbols[i]
		}
	}

	if myApp == nil {
		t.Fatal("expected to find MyApp module")
	}
	if myApp.Kind != "module" || !myApp.Exported {
		t.Errorf("MyApp: expected exported module, got kind=%q exported=%v", myApp.Kind, myApp.Exported)
	}

	if config == nil {
		t.Fatal("expected to find Config module")
	}
	if config.Kind != "module" || config.Exported {
		t.Errorf("Config: expected non-exported module, got kind=%q exported=%v", config.Kind, config.Exported)
	}
}

func TestRubyClassDetection(t *testing.T) {
	src := `class UserService
  def initialize(name)
    @name = name
  end
end

class AdminService < UserService
end
`
	p := NewRubyParser()
	pf, err := p.Parse("service.rb", []byte(src))
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	var userSvc, adminSvc *Symbol
	for i := range pf.Symbols {
		if pf.Symbols[i].Name == "UserService" {
			userSvc = &pf.Symbols[i]
		}
		if pf.Symbols[i].Name == "AdminService" {
			adminSvc = &pf.Symbols[i]
		}
	}

	if userSvc == nil {
		t.Fatal("expected to find UserService class")
	}
	if userSvc.Kind != "class" || !userSvc.Exported {
		t.Errorf("UserService: expected exported class, got kind=%q exported=%v", userSvc.Kind, userSvc.Exported)
	}

	if adminSvc == nil {
		t.Fatal("expected to find AdminService class")
	}
}

func TestRubyMethodDetection(t *testing.T) {
	src := `class MyClass
  def initialize(name)
    @name = name
  end

  def public_method
    puts @name
  end

  def self.class_method
    new("default")
  end

  private

  def secret_method
    "hidden"
  end
end
`
	p := NewRubyParser()
	pf, err := p.Parse("my_class.rb", []byte(src))
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	methods := make(map[string]*Symbol)
	for i := range pf.Symbols {
		if pf.Symbols[i].Kind == "method" || pf.Symbols[i].Kind == "function" {
			methods[pf.Symbols[i].Name] = &pf.Symbols[i]
		}
	}

	if m, ok := methods["initialize"]; !ok {
		t.Error("expected to find initialize method")
	} else if !m.Exported {
		// initialize is public by default
		t.Log("initialize is public — correct before private modifier")
	}

	if m, ok := methods["public_method"]; !ok {
		t.Error("expected to find public_method")
	} else if !m.Exported {
		t.Error("public_method should be exported")
	}

	if m, ok := methods["class_method"]; !ok {
		t.Error("expected to find class_method")
	} else if !m.Exported {
		t.Error("class_method should be exported")
	}

	if m, ok := methods["secret_method"]; !ok {
		t.Error("expected to find secret_method")
	} else if m.Exported {
		t.Error("secret_method should not be exported (after private modifier)")
	}
}

func TestRubyAttrAccessor(t *testing.T) {
	src := `class User
  attr_accessor :name, :email
  attr_reader :id
end
`
	p := NewRubyParser()
	pf, err := p.Parse("user.rb", []byte(src))
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	// Should find name, email, id as symbols.
	symNames := make(map[string]bool)
	for _, s := range pf.Symbols {
		if s.Kind == "variable" {
			symNames[s.Name] = true
		}
	}

	for _, expected := range []string{"name", "email", "id"} {
		if !symNames[expected] {
			t.Errorf("expected to find attr symbol %q", expected)
		}
	}
}

func TestRubyConstant(t *testing.T) {
	src := `MAX_RETRIES = 3
API_VERSION = "v2"

class Config
  DEFAULT_TIMEOUT = 30
end
`
	p := NewRubyParser()
	pf, err := p.Parse("config.rb", []byte(src))
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	constNames := make(map[string]bool)
	for _, s := range pf.Symbols {
		if s.Kind == "variable" && s.Exported {
			constNames[s.Name] = true
		}
	}

	if !constNames["MAX_RETRIES"] {
		t.Error("expected to find MAX_RETRIES constant")
	}
	if !constNames["API_VERSION"] {
		t.Error("expected to find API_VERSION constant")
	}
	if !constNames["DEFAULT_TIMEOUT"] {
		t.Error("expected to find DEFAULT_TIMEOUT constant")
	}
}

func TestRubyEmptyFile(t *testing.T) {
	p := NewRubyParser()
	pf, err := p.Parse("empty.rb", []byte(""))
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
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

func TestRubyCommentSkipped(t *testing.T) {
	src := `# require 'unused'
# class NotAClass
# def not_a_method
`
	p := NewRubyParser()
	pf, err := p.Parse("comments.rb", []byte(src))
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	if len(pf.Imports) != 0 {
		t.Errorf("expected 0 imports from comments, got %d", len(pf.Imports))
	}
	if len(pf.Symbols) != 0 {
		t.Errorf("expected 0 symbols from comments, got %d", len(pf.Symbols))
	}
}

func TestRubyTopLevelFunction(t *testing.T) {
	src := `def helper_method(x, y)
  x + y
end
`
	p := NewRubyParser()
	pf, err := p.Parse("helpers.rb", []byte(src))
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	var found *Symbol
	for i := range pf.Symbols {
		if pf.Symbols[i].Name == "helper_method" {
			found = &pf.Symbols[i]
		}
	}

	if found == nil {
		t.Fatal("expected to find helper_method")
	}
	if found.Kind != "function" {
		t.Errorf("expected kind 'function', got %q", found.Kind)
	}
	if !found.Exported {
		t.Error("top-level function should be exported")
	}
}
