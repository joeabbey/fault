package parser

import (
	"os"
	"testing"
)

func TestLuaParserLanguage(t *testing.T) {
	p := NewLuaParser()
	if p.Language() != "lua" {
		t.Errorf("expected 'lua', got %q", p.Language())
	}
}

func TestLuaRequireQuote(t *testing.T) {
	src := `local json = require 'cjson'
local socket = require "socket"
`
	p := NewLuaParser()
	pf, err := p.Parse("app.lua", []byte(src))
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	if len(pf.Imports) != 2 {
		t.Fatalf("expected 2 imports, got %d", len(pf.Imports))
	}

	expected := []string{"cjson", "socket"}
	for i, exp := range expected {
		if pf.Imports[i].Path != exp {
			t.Errorf("import[%d]: expected %q, got %q", i, exp, pf.Imports[i].Path)
		}
	}
}

func TestLuaRequireCall(t *testing.T) {
	src := `local lfs = require('lfs')
local lpeg = require("lpeg")
`
	p := NewLuaParser()
	pf, err := p.Parse("app.lua", []byte(src))
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	if len(pf.Imports) != 2 {
		t.Fatalf("expected 2 imports, got %d", len(pf.Imports))
	}

	expected := []string{"lfs", "lpeg"}
	for i, exp := range expected {
		if pf.Imports[i].Path != exp {
			t.Errorf("import[%d]: expected %q, got %q", i, exp, pf.Imports[i].Path)
		}
	}
}

func TestLuaLocalFunction(t *testing.T) {
	src := `local function validate_input(data)
    return type(data) == "table"
end

local function internal_log(level, message)
    print(message)
end
`
	p := NewLuaParser()
	pf, err := p.Parse("helpers.lua", []byte(src))
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	funcNames := make(map[string]*Symbol)
	for i := range pf.Symbols {
		if pf.Symbols[i].Kind == "function" {
			funcNames[pf.Symbols[i].Name] = &pf.Symbols[i]
		}
	}

	if f, ok := funcNames["validate_input"]; !ok {
		t.Error("expected to find validate_input function")
	} else if f.Exported {
		t.Error("local function validate_input should not be exported")
	}

	if f, ok := funcNames["internal_log"]; !ok {
		t.Error("expected to find internal_log function")
	} else if f.Exported {
		t.Error("local function internal_log should not be exported")
	}

	// No exports for local functions.
	if len(pf.Exports) != 0 {
		t.Errorf("expected 0 exports, got %d", len(pf.Exports))
	}
}

func TestLuaModuleFunction(t *testing.T) {
	src := `local M = {}

function M.initialize(config)
    M.config = config
end

function M.process(items)
    return items
end
`
	p := NewLuaParser()
	pf, err := p.Parse("module.lua", []byte(src))
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	exportedFuncs := make(map[string]bool)
	for _, e := range pf.Exports {
		if e.Kind == "function" {
			exportedFuncs[e.Name] = true
		}
	}

	if !exportedFuncs["M.initialize"] {
		t.Error("expected to find M.initialize export")
	}
	if !exportedFuncs["M.process"] {
		t.Error("expected to find M.process export")
	}
}

func TestLuaGlobalFunction(t *testing.T) {
	src := `function helper_format(str)
    return string.format("[%s]", str)
end

function create_logger(name)
    return function(msg) print(msg) end
end
`
	p := NewLuaParser()
	pf, err := p.Parse("globals.lua", []byte(src))
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	funcNames := make(map[string]*Symbol)
	for i := range pf.Symbols {
		if pf.Symbols[i].Kind == "function" {
			funcNames[pf.Symbols[i].Name] = &pf.Symbols[i]
		}
	}

	if f, ok := funcNames["helper_format"]; !ok {
		t.Error("expected to find helper_format")
	} else if !f.Exported {
		t.Error("global function helper_format should be exported")
	}

	if f, ok := funcNames["create_logger"]; !ok {
		t.Error("expected to find create_logger")
	} else if !f.Exported {
		t.Error("global function create_logger should be exported")
	}
}

func TestLuaModuleAssign(t *testing.T) {
	src := `local M = {}

M.cleanup = function(self)
    self.data = nil
end
`
	p := NewLuaParser()
	pf, err := p.Parse("module.lua", []byte(src))
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	var found *Symbol
	for i := range pf.Symbols {
		if pf.Symbols[i].Name == "M.cleanup" {
			found = &pf.Symbols[i]
		}
	}

	if found == nil {
		t.Fatal("expected to find M.cleanup")
	}
	if found.Kind != "function" {
		t.Errorf("expected kind 'function', got %q", found.Kind)
	}
	if !found.Exported {
		t.Error("M.cleanup should be exported")
	}
}

func TestLuaEmptyFile(t *testing.T) {
	p := NewLuaParser()
	pf, err := p.Parse("empty.lua", []byte(""))
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

func TestLuaCommentSkipped(t *testing.T) {
	src := `-- local json = require 'cjson'
-- function not_a_func()
-- local x = 10
`
	p := NewLuaParser()
	pf, err := p.Parse("comments.lua", []byte(src))
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

func TestLuaSampleFile(t *testing.T) {
	content, err := os.ReadFile("../../testdata/lua/sample.lua")
	if err != nil {
		t.Fatalf("failed to read sample file: %v", err)
	}

	p := NewLuaParser()
	pf, err := p.Parse("sample.lua", content)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	// Should find 5 imports: cjson, socket, lfs, lpeg, app.utils
	if len(pf.Imports) != 5 {
		t.Errorf("expected 5 imports, got %d", len(pf.Imports))
		for i, imp := range pf.Imports {
			t.Logf("  import[%d]: %q (line %d)", i, imp.Path, imp.Line)
		}
	}

	// Count local vs exported functions.
	localFuncs := 0
	exportedFuncs := 0
	for _, s := range pf.Symbols {
		if s.Kind == "function" {
			if s.Exported {
				exportedFuncs++
			} else {
				localFuncs++
			}
		}
	}

	// Local functions: validate_input, internal_log
	if localFuncs != 2 {
		t.Errorf("expected 2 local functions, got %d", localFuncs)
	}

	// Exported functions: M.initialize, M.process, M.cleanup, helper_format,
	// M.db.connect, create_logger
	if exportedFuncs != 6 {
		t.Errorf("expected 6 exported functions, got %d", exportedFuncs)
		for _, s := range pf.Symbols {
			if s.Kind == "function" && s.Exported {
				t.Logf("  exported func: %s", s.Name)
			}
		}
	}

	// Check specific imports.
	importPaths := make(map[string]bool)
	for _, imp := range pf.Imports {
		importPaths[imp.Path] = true
	}

	for _, exp := range []string{"cjson", "socket", "lfs", "lpeg", "app.utils"} {
		if !importPaths[exp] {
			t.Errorf("expected import %q", exp)
		}
	}
}
