package parser

import (
	"os"
	"testing"
)

func TestZigParserLanguage(t *testing.T) {
	p := NewZigParser()
	if p.Language() != "zig" {
		t.Errorf("expected zig, got %q", p.Language())
	}
}

func TestZigParserImports(t *testing.T) {
	src := `const std = @import("std");
const fs = @import("fs");
pub const json = @import("json");
`
	p := NewZigParser()
	pf, err := p.Parse("main.zig", []byte(src))
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	if len(pf.Imports) != 3 {
		t.Fatalf("expected 3 imports, got %d", len(pf.Imports))
	}
	if pf.Imports[0].Path != "std" || pf.Imports[0].Names[0] != "std" {
		t.Errorf("expected import std, got path=%q names=%v", pf.Imports[0].Path, pf.Imports[0].Names)
	}
	if pf.Imports[1].Path != "fs" || pf.Imports[1].Names[0] != "fs" {
		t.Errorf("expected import fs, got path=%q names=%v", pf.Imports[1].Path, pf.Imports[1].Names)
	}
	if pf.Imports[2].Path != "json" || pf.Imports[2].Names[0] != "json" {
		t.Errorf("expected import json, got path=%q names=%v", pf.Imports[2].Path, pf.Imports[2].Names)
	}
}

func TestZigParserFunctions(t *testing.T) {
	src := `pub fn init(allocator: Allocator) !void {}
fn helper(x: i32, y: i32) i32 {}
pub fn serve(config: Config) !void {}
`
	p := NewZigParser()
	pf, err := p.Parse("main.zig", []byte(src))
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	if len(pf.Symbols) < 3 {
		t.Fatalf("expected at least 3 symbols, got %d", len(pf.Symbols))
	}
	symMap := make(map[string]Symbol)
	for _, s := range pf.Symbols {
		symMap[s.Name] = s
	}
	if s, ok := symMap["init"]; !ok || !s.Exported || s.Kind != "function" {
		t.Errorf("expected exported function init, got %+v", symMap["init"])
	}
	if s, ok := symMap["helper"]; !ok || s.Exported || s.Kind != "function" {
		t.Errorf("expected private function helper, got %+v", symMap["helper"])
	}
	if s, ok := symMap["serve"]; !ok || !s.Exported || s.Kind != "function" {
		t.Errorf("expected exported function serve, got %+v", symMap["serve"])
	}
}

func TestZigParserExports(t *testing.T) {
	src := `pub fn publicFunc() void {}
fn privateFunc() void {}
pub const MyStruct = struct {};
const InternalStruct = struct {};
pub var globalVar: i32 = 0;
var internalVar: i32 = 0;
`
	p := NewZigParser()
	pf, err := p.Parse("lib.zig", []byte(src))
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	exportNames := make(map[string]bool)
	for _, exp := range pf.Exports {
		exportNames[exp.Name] = true
	}
	if !exportNames["publicFunc"] {
		t.Error("expected publicFunc to be exported")
	}
	if exportNames["privateFunc"] {
		t.Error("privateFunc should not be exported")
	}
	if !exportNames["MyStruct"] {
		t.Error("expected MyStruct to be exported")
	}
	if exportNames["InternalStruct"] {
		t.Error("InternalStruct should not be exported")
	}
	if !exportNames["globalVar"] {
		t.Error("expected globalVar to be exported")
	}
	if exportNames["internalVar"] {
		t.Error("internalVar should not be exported")
	}
}

func TestZigParserEmptyFile(t *testing.T) {
	p := NewZigParser()
	pf, err := p.Parse("empty.zig", []byte(""))
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	if len(pf.Imports) != 0 || len(pf.Exports) != 0 || len(pf.Symbols) != 0 {
		t.Error("expected empty results for empty file")
	}
}

func TestZigParserCommentsSkipped(t *testing.T) {
	src := `// This is a comment
// pub fn notAFunction() void {}
const std = @import("std");
pub fn realFunction() void {}
`
	p := NewZigParser()
	pf, err := p.Parse("main.zig", []byte(src))
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

func TestZigParserStructsAndEnums(t *testing.T) {
	src := `pub const Config = struct {
    name: []const u8,
};
const InternalState = enum {
    idle,
    running,
};
pub const Direction = union(enum) {
    up, down,
};
`
	p := NewZigParser()
	pf, err := p.Parse("types.zig", []byte(src))
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	symMap := make(map[string]Symbol)
	for _, s := range pf.Symbols {
		symMap[s.Name] = s
	}
	if s, ok := symMap["Config"]; !ok || s.Kind != "type" || !s.Exported {
		t.Errorf("expected exported type Config, got %+v", symMap["Config"])
	}
	if s, ok := symMap["InternalState"]; !ok || s.Kind != "type" || s.Exported {
		t.Errorf("expected private type InternalState, got %+v", symMap["InternalState"])
	}
	if s, ok := symMap["Direction"]; !ok || s.Kind != "type" || !s.Exported {
		t.Errorf("expected exported type Direction, got %+v", symMap["Direction"])
	}
}

func TestZigParserSampleFile(t *testing.T) {
	content, err := os.ReadFile("../../testdata/zig/sample.zig")
	if err != nil {
		t.Fatalf("reading fixture: %v", err)
	}
	p := NewZigParser()
	pf, err := p.Parse("sample.zig", content)
	if err != nil {
		t.Fatalf("parsing: %v", err)
	}
	if pf.Language != "zig" {
		t.Errorf("expected language zig, got %q", pf.Language)
	}
	if len(pf.Imports) < 3 {
		t.Errorf("expected at least 3 imports, got %d", len(pf.Imports))
	}
	if len(pf.Symbols) < 5 {
		t.Errorf("expected at least 5 symbols, got %d", len(pf.Symbols))
	}
	if len(pf.Exports) < 3 {
		t.Errorf("expected at least 3 exports, got %d", len(pf.Exports))
	}
}
