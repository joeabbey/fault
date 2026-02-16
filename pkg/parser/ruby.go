package parser

import (
	"regexp"
	"strings"
)

// RubyParser extracts structural information from Ruby files via regex.
type RubyParser struct{}

// NewRubyParser creates a new Ruby parser.
func NewRubyParser() *RubyParser {
	return &RubyParser{}
}

// Language returns "ruby".
func (p *RubyParser) Language() string {
	return "ruby"
}

// Regex patterns for Ruby parsing.
var (
	// require 'module' or require "module"
	rbRequire = regexp.MustCompile(`(?m)^\s*require\s+['"]([^'"]+)['"]`)
	// require_relative './file' or require_relative "file"
	rbRequireRelative = regexp.MustCompile(`(?m)^\s*require_relative\s+['"]([^'"]+)['"]`)
	// module Name
	rbModule = regexp.MustCompile(`(?m)^(\s*)module\s+([A-Z]\w*)`)
	// class Name or class Name < Parent
	rbClass = regexp.MustCompile(`(?m)^(\s*)class\s+([A-Z]\w*)(?:\s*<\s*\S+)?`)
	// def method_name(args) or def self.method_name(args)
	rbMethod = regexp.MustCompile(`(?m)^(\s*)def\s+(self\.)?(\w+[!?=]?)(?:\s*\(([^)]*)\))?`)
	// attr_accessor :name, :other
	rbAttrAccessor = regexp.MustCompile(`(?m)^\s*attr_(accessor|reader|writer)\s+(.+)`)
	// CONSTANT_NAME = value (all uppercase with underscores)
	rbConstant = regexp.MustCompile(`(?m)^(\s*)([A-Z][A-Z0-9_]+)\s*=`)
	// private/protected/public visibility modifiers (standalone on a line)
	rbVisibility = regexp.MustCompile(`(?m)^\s*(private|protected|public)\s*$`)
)

// Parse extracts structural info from Ruby content.
func (p *RubyParser) Parse(filename string, content []byte) (*ParsedFile, error) {
	pf := &ParsedFile{
		Path:     filename,
		Language: "ruby",
		Imports:  make([]Import, 0),
		Exports:  make([]Export, 0),
		Symbols:  make([]Symbol, 0),
	}

	lines := strings.Split(string(content), "\n")
	currentVisibility := "public"
	indentStack := make([]int, 0) // track nesting depth

	for lineNum, line := range lines {
		lineNo := lineNum + 1
		trimmed := strings.TrimSpace(line)

		// Skip empty lines and comments.
		if trimmed == "" || strings.HasPrefix(trimmed, "#") {
			continue
		}

		// Extract require statements.
		if matches := rbRequire.FindStringSubmatch(line); matches != nil {
			pf.Imports = append(pf.Imports, Import{
				Path: matches[1],
				Line: lineNo,
			})
			continue
		}

		if matches := rbRequireRelative.FindStringSubmatch(line); matches != nil {
			path := matches[1]
			if !strings.HasPrefix(path, ".") {
				path = "./" + path
			}
			pf.Imports = append(pf.Imports, Import{
				Path: path,
				Line: lineNo,
			})
			continue
		}

		// Track visibility modifiers.
		if matches := rbVisibility.FindStringSubmatch(line); matches != nil {
			currentVisibility = matches[1]
			continue
		}

		// Extract module definitions.
		if matches := rbModule.FindStringSubmatch(line); matches != nil {
			indent := len(matches[1])
			name := matches[2]
			exported := indent == 0

			pf.Symbols = append(pf.Symbols, Symbol{
				Name:     name,
				Kind:     "module",
				Exported: exported,
				Line:     lineNo,
			})

			if exported {
				pf.Exports = append(pf.Exports, Export{
					Name: name,
					Kind: "module",
					Line: lineNo,
				})
			}

			indentStack = append(indentStack, indent)
			currentVisibility = "public" // reset within new scope
			continue
		}

		// Extract class definitions.
		if matches := rbClass.FindStringSubmatch(line); matches != nil {
			indent := len(matches[1])
			name := matches[2]
			exported := indent == 0

			pf.Symbols = append(pf.Symbols, Symbol{
				Name:     name,
				Kind:     "class",
				Exported: exported,
				Line:     lineNo,
			})

			if exported {
				pf.Exports = append(pf.Exports, Export{
					Name: name,
					Kind: "class",
					Line: lineNo,
				})
			}

			indentStack = append(indentStack, indent)
			currentVisibility = "public" // reset within new scope
			continue
		}

		// Extract method definitions.
		if matches := rbMethod.FindStringSubmatch(line); matches != nil {
			indent := len(matches[1])
			selfPrefix := matches[2]
			name := matches[3]
			params := matches[4]

			kind := "method"
			isClassMethod := selfPrefix != ""

			// Top-level methods (no indentation) are module-level functions.
			if indent == 0 {
				kind = "function"
			}

			exported := currentVisibility == "public" && !strings.HasPrefix(name, "_")
			if isClassMethod {
				exported = true // class methods are always accessible
			}

			sig := "def "
			if isClassMethod {
				sig += "self."
			}
			sig += name
			if params != "" {
				sig += "(" + strings.TrimSpace(params) + ")"
			}

			pf.Symbols = append(pf.Symbols, Symbol{
				Name:      name,
				Kind:      kind,
				Exported:  exported,
				Line:      lineNo,
				Signature: sig,
			})

			if exported && indent == 0 {
				pf.Exports = append(pf.Exports, Export{
					Name: name,
					Kind: kind,
					Line: lineNo,
				})
			}
			continue
		}

		// Extract attr_accessor/reader/writer.
		if matches := rbAttrAccessor.FindStringSubmatch(line); matches != nil {
			attrType := matches[1]
			attrsStr := matches[2]

			attrs := parseRubySymbolList(attrsStr)
			for _, attr := range attrs {
				pf.Symbols = append(pf.Symbols, Symbol{
					Name:     attr,
					Kind:     "variable",
					Exported: currentVisibility == "public",
					Line:     lineNo,
				})

				if currentVisibility == "public" {
					kind := "variable"
					_ = attrType // attr_accessor creates both getter and setter
					pf.Exports = append(pf.Exports, Export{
						Name: attr,
						Kind: kind,
						Line: lineNo,
					})
				}
			}
			continue
		}

		// Extract constants (UPPER_CASE = value).
		if matches := rbConstant.FindStringSubmatch(line); matches != nil {
			indent := len(matches[1])
			name := matches[2]

			pf.Symbols = append(pf.Symbols, Symbol{
				Name:     name,
				Kind:     "variable",
				Exported: true, // Ruby constants are always accessible
				Line:     lineNo,
			})

			if indent == 0 {
				pf.Exports = append(pf.Exports, Export{
					Name: name,
					Kind: "variable",
					Line: lineNo,
				})
			}
			continue
		}
	}

	_ = indentStack // used for future scope tracking

	return pf, nil
}

// parseRubySymbolList parses ":name, :other" into ["name", "other"].
func parseRubySymbolList(s string) []string {
	names := make([]string, 0)
	for _, part := range strings.Split(s, ",") {
		part = strings.TrimSpace(part)
		part = strings.TrimPrefix(part, ":")
		if part != "" {
			names = append(names, part)
		}
	}
	return names
}
