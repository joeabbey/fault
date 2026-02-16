package parser

import (
	"regexp"
	"strings"
)

// LuaParser extracts structural information from Lua files via regex.
type LuaParser struct{}

// NewLuaParser creates a new Lua parser.
func NewLuaParser() *LuaParser {
	return &LuaParser{}
}

// Language returns "lua".
func (p *LuaParser) Language() string {
	return "lua"
}

// Regex patterns for Lua parsing.
var (
	// require 'module' or require "module"
	luaRequireQuote = regexp.MustCompile(`(?m)^\s*(?:local\s+\w+\s*=\s*)?require\s+['"]([^'"]+)['"]`)
	// require('module') or require("module")
	luaRequireCall = regexp.MustCompile(`(?m)^\s*(?:local\s+\w+\s*=\s*)?require\s*\(\s*['"]([^'"]+)['"]\s*\)`)
	// function ModName.funcName(args) or function ModName.sub.funcName(args)
	luaModuleFunc = regexp.MustCompile(`(?m)^\s*function\s+(\w+(?:\.\w+)+)\s*\(([^)]*)\)`)
	// function name(args) — top-level global function
	luaGlobalFunc = regexp.MustCompile(`(?m)^\s*function\s+(\w+)\s*\(([^)]*)\)`)
	// local function name(args)
	luaLocalFunc = regexp.MustCompile(`(?m)^\s*local\s+function\s+(\w+)\s*\(([^)]*)\)`)
	// M.name = function(args) — module method assigned as variable
	luaModuleAssign = regexp.MustCompile(`(?m)^\s*(\w+\.\w+)\s*=\s*function\s*\(([^)]*)\)`)
	// local name = value (non-function locals, function check done in code)
	luaLocalVar = regexp.MustCompile(`(?m)^\s*local\s+(\w+)\s*=\s*(.+)`)
	// M.NAME = value (module-level constant/variable assignment, function check done in code)
	luaModuleVar = regexp.MustCompile(`(?m)^\s*(\w+)\.(\w+)\s*=\s*(.+)`)
)

// Parse extracts structural info from Lua content.
func (p *LuaParser) Parse(filename string, content []byte) (*ParsedFile, error) {
	pf := &ParsedFile{
		Path:     filename,
		Language: "lua",
		Imports:  make([]Import, 0),
		Exports:  make([]Export, 0),
		Symbols:  make([]Symbol, 0),
	}

	lines := strings.Split(string(content), "\n")
	inBlockComment := false

	for lineNum, line := range lines {
		lineNo := lineNum + 1
		trimmed := strings.TrimSpace(line)

		// Handle block comments (--[[ ... ]]).
		if inBlockComment {
			if strings.Contains(trimmed, "]]") {
				inBlockComment = false
			}
			continue
		}
		if strings.HasPrefix(trimmed, "--[[") {
			if !strings.Contains(trimmed, "]]") {
				inBlockComment = true
			}
			continue
		}

		// Skip empty lines and line comments.
		if trimmed == "" || strings.HasPrefix(trimmed, "--") {
			continue
		}

		// Extract require statements (quote form).
		if matches := luaRequireQuote.FindStringSubmatch(line); matches != nil {
			pf.Imports = append(pf.Imports, Import{
				Path: matches[1],
				Line: lineNo,
			})

			// Also check if this is a local var assignment (local x = require ...).
			// The import is already captured; don't re-capture as local var.
			continue
		}

		// Extract require statements (call form).
		if matches := luaRequireCall.FindStringSubmatch(line); matches != nil {
			pf.Imports = append(pf.Imports, Import{
				Path: matches[1],
				Line: lineNo,
			})
			continue
		}

		// Extract local function declarations.
		if matches := luaLocalFunc.FindStringSubmatch(line); matches != nil {
			name := matches[1]
			params := matches[2]

			sig := "local function " + name + "(" + strings.TrimSpace(params) + ")"

			pf.Symbols = append(pf.Symbols, Symbol{
				Name:      name,
				Kind:      "function",
				Exported:  false,
				Line:      lineNo,
				Signature: sig,
			})
			continue
		}

		// Extract module function declarations (M.func(args)).
		if matches := luaModuleFunc.FindStringSubmatch(line); matches != nil {
			fullName := matches[1]
			params := matches[2]

			sig := "function " + fullName + "(" + strings.TrimSpace(params) + ")"

			pf.Symbols = append(pf.Symbols, Symbol{
				Name:      fullName,
				Kind:      "function",
				Exported:  true,
				Line:      lineNo,
				Signature: sig,
			})

			pf.Exports = append(pf.Exports, Export{
				Name: fullName,
				Kind: "function",
				Line: lineNo,
			})
			continue
		}

		// Extract global function declarations.
		if matches := luaGlobalFunc.FindStringSubmatch(line); matches != nil {
			name := matches[1]
			params := matches[2]

			sig := "function " + name + "(" + strings.TrimSpace(params) + ")"

			pf.Symbols = append(pf.Symbols, Symbol{
				Name:      name,
				Kind:      "function",
				Exported:  true,
				Line:      lineNo,
				Signature: sig,
			})

			pf.Exports = append(pf.Exports, Export{
				Name: name,
				Kind: "function",
				Line: lineNo,
			})
			continue
		}

		// Extract module method assignment (M.name = function(...)).
		if matches := luaModuleAssign.FindStringSubmatch(line); matches != nil {
			fullName := matches[1]
			params := matches[2]

			sig := fullName + " = function(" + strings.TrimSpace(params) + ")"

			pf.Symbols = append(pf.Symbols, Symbol{
				Name:      fullName,
				Kind:      "function",
				Exported:  true,
				Line:      lineNo,
				Signature: sig,
			})

			pf.Exports = append(pf.Exports, Export{
				Name: fullName,
				Kind: "function",
				Line: lineNo,
			})
			continue
		}

		// Extract module variable assignments (M.NAME = value).
		if matches := luaModuleVar.FindStringSubmatch(line); matches != nil {
			tableName := matches[1]
			fieldName := matches[2]
			value := strings.TrimSpace(matches[3])
			fullName := tableName + "." + fieldName

			// Skip function assignments (handled by luaModuleAssign above).
			if strings.HasPrefix(value, "function") {
				continue
			}

			// Skip if it's a local table (like local M = {}).
			if strings.Contains(line, "local") {
				continue
			}

			pf.Symbols = append(pf.Symbols, Symbol{
				Name:     fullName,
				Kind:     "variable",
				Exported: true,
				Line:     lineNo,
			})

			pf.Exports = append(pf.Exports, Export{
				Name: fullName,
				Kind: "variable",
				Line: lineNo,
			})
			continue
		}

		// Extract local variable declarations.
		if matches := luaLocalVar.FindStringSubmatch(line); matches != nil {
			name := matches[1]
			value := strings.TrimSpace(matches[2])

			// Skip function assignments (handled by luaLocalFunc above).
			if strings.HasPrefix(value, "function") {
				continue
			}

			pf.Symbols = append(pf.Symbols, Symbol{
				Name:     name,
				Kind:     "variable",
				Exported: false,
				Line:     lineNo,
			})
			continue
		}
	}

	return pf, nil
}
