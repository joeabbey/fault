package parser

import (
	"regexp"
	"strings"
)

// RustParser extracts structural information from Rust files via regex.
type RustParser struct{}

// NewRustParser creates a new Rust parser.
func NewRustParser() *RustParser {
	return &RustParser{}
}

// Language returns "rust".
func (p *RustParser) Language() string {
	return "rust"
}

// Regex patterns for Rust parsing.
var (
	// use statements
	// use std::collections::HashMap;
	// use crate::module;
	// use super::*;
	// use std::io as stdio;
	reRustUseSimple = regexp.MustCompile(`(?m)^use\s+([\w:]+(?:::\*)?)\s*(?:as\s+\w+)?;`)

	// Nested/grouped use: use std::{io, fs};
	// use std::{io::Read, fs::File};
	reRustUseGrouped = regexp.MustCompile(`(?m)^use\s+([\w:]+)::\{([^}]+)\};`)

	// pub fn name(...)
	reRustPubFn = regexp.MustCompile(`(?m)^pub(?:\((?:crate|super)\))?\s+(?:async\s+)?fn\s+(\w+)\s*(?:<[^>]*>)?\s*\(([^)]*)\)`)
	// fn name(...)
	reRustFn = regexp.MustCompile(`(?m)^(?:pub(?:\((?:crate|super)\))?\s+)?(?:async\s+)?fn\s+(\w+)\s*(?:<[^>]*>)?\s*\(([^)]*)\)`)

	// pub struct Name
	reRustPubStruct = regexp.MustCompile(`(?m)^pub(?:\((?:crate|super)\))?\s+struct\s+(\w+)`)
	reRustStruct    = regexp.MustCompile(`(?m)^(?:pub(?:\((?:crate|super)\))?\s+)?struct\s+(\w+)`)

	// pub enum Name
	reRustPubEnum = regexp.MustCompile(`(?m)^pub(?:\((?:crate|super)\))?\s+enum\s+(\w+)`)
	reRustEnum    = regexp.MustCompile(`(?m)^(?:pub(?:\((?:crate|super)\))?\s+)?enum\s+(\w+)`)

	// pub trait Name
	reRustPubTrait = regexp.MustCompile(`(?m)^pub(?:\((?:crate|super)\))?\s+trait\s+(\w+)`)
	reRustTrait    = regexp.MustCompile(`(?m)^(?:pub(?:\((?:crate|super)\))?\s+)?trait\s+(\w+)`)

	// impl Name or impl Trait for Name
	reRustImpl = regexp.MustCompile(`(?m)^impl\s+(?:<[^>]*>\s*)?(\w+)(?:\s+for\s+(\w+))?`)

	// pub type Name = ...;
	reRustPubType = regexp.MustCompile(`(?m)^pub(?:\((?:crate|super)\))?\s+type\s+(\w+)`)
	reRustType    = regexp.MustCompile(`(?m)^(?:pub(?:\((?:crate|super)\))?\s+)?type\s+(\w+)\s*(?:<[^>]*>)?\s*=`)

	// pub const NAME: Type = ...;
	reRustPubConst = regexp.MustCompile(`(?m)^pub(?:\((?:crate|super)\))?\s+(?:const|static)\s+(\w+)`)
	reRustConst    = regexp.MustCompile(`(?m)^(?:pub(?:\((?:crate|super)\))?\s+)?(?:const|static)\s+(\w+)`)

	// pub mod name
	reRustPubMod = regexp.MustCompile(`(?m)^pub(?:\((?:crate|super)\))?\s+mod\s+(\w+)`)
	reRustMod    = regexp.MustCompile(`(?m)^(?:pub(?:\((?:crate|super)\))?\s+)?mod\s+(\w+)`)

	// Detect pub prefix (including pub(crate) and pub(super))
	reRustPubPrefix = regexp.MustCompile(`^pub(?:\((?:crate|super)\))?\s+`)
)

// Parse extracts structural info from Rust content.
func (p *RustParser) Parse(filename string, content []byte) (*ParsedFile, error) {
	pf := &ParsedFile{
		Path:     filename,
		Language: "rust",
		Imports:  make([]Import, 0),
		Exports:  make([]Export, 0),
		Symbols:  make([]Symbol, 0),
	}

	lines := strings.Split(string(content), "\n")

	for lineNum, line := range lines {
		lineNo := lineNum + 1
		trimmed := strings.TrimSpace(line)

		// Skip comments and empty lines
		if trimmed == "" || strings.HasPrefix(trimmed, "//") || strings.HasPrefix(trimmed, "/*") || strings.HasPrefix(trimmed, "*") {
			continue
		}

		// Extract imports (use statements)
		p.extractRustImports(pf, trimmed, lineNo)

		// Extract symbols and exports
		p.extractRustSymbols(pf, trimmed, lineNo)
	}

	pf.Symbols = deduplicateSymbols(pf.Symbols)

	return pf, nil
}

// extractRustImports handles Rust use statements.
func (p *RustParser) extractRustImports(pf *ParsedFile, line string, lineNo int) {
	if !strings.HasPrefix(line, "use ") {
		return
	}

	// Grouped use: use std::{io, fs};
	if matches := reRustUseGrouped.FindStringSubmatch(line); matches != nil {
		basePath := matches[1]
		namesStr := matches[2]
		names := parseRustGroupedNames(namesStr)

		pf.Imports = append(pf.Imports, Import{
			Path:  basePath,
			Names: names,
			Line:  lineNo,
		})
		return
	}

	// Simple use: use std::collections::HashMap;
	if matches := reRustUseSimple.FindStringSubmatch(line); matches != nil {
		fullPath := matches[1]

		// Extract the last component as the name
		names := make([]string, 0)
		if strings.HasSuffix(fullPath, "::*") {
			names = append(names, "*")
		} else {
			parts := strings.Split(fullPath, "::")
			if len(parts) > 0 {
				names = append(names, parts[len(parts)-1])
			}
		}

		// Check for 'as' alias
		if idx := strings.Index(line, " as "); idx != -1 {
			rest := line[idx+4:]
			rest = strings.TrimSuffix(rest, ";")
			rest = strings.TrimSpace(rest)
			if rest != "" {
				names = []string{rest}
			}
		}

		pf.Imports = append(pf.Imports, Import{
			Path:  fullPath,
			Names: names,
			Line:  lineNo,
		})
	}
}

// extractRustSymbols handles Rust declarations.
func (p *RustParser) extractRustSymbols(pf *ParsedFile, line string, lineNo int) {
	exported := reRustPubPrefix.MatchString(line)

	// Functions
	if matches := reRustFn.FindStringSubmatch(line); matches != nil {
		name := matches[1]
		params := matches[2]
		sym := Symbol{
			Name:      name,
			Kind:      "function",
			Exported:  exported,
			Line:      lineNo,
			Signature: "fn " + name + "(" + strings.TrimSpace(params) + ")",
		}
		pf.Symbols = append(pf.Symbols, sym)
		if exported {
			pf.Exports = append(pf.Exports, Export{
				Name: name,
				Kind: "function",
				Line: lineNo,
			})
		}
		return
	}

	// Structs
	if matches := reRustStruct.FindStringSubmatch(line); matches != nil {
		name := matches[1]
		pf.Symbols = append(pf.Symbols, Symbol{
			Name:     name,
			Kind:     "struct",
			Exported: exported,
			Line:     lineNo,
		})
		if exported {
			pf.Exports = append(pf.Exports, Export{
				Name: name,
				Kind: "type",
				Line: lineNo,
			})
		}
		return
	}

	// Enums
	if matches := reRustEnum.FindStringSubmatch(line); matches != nil {
		name := matches[1]
		pf.Symbols = append(pf.Symbols, Symbol{
			Name:     name,
			Kind:     "type",
			Exported: exported,
			Line:     lineNo,
		})
		if exported {
			pf.Exports = append(pf.Exports, Export{
				Name: name,
				Kind: "type",
				Line: lineNo,
			})
		}
		return
	}

	// Traits
	if matches := reRustTrait.FindStringSubmatch(line); matches != nil {
		name := matches[1]
		pf.Symbols = append(pf.Symbols, Symbol{
			Name:     name,
			Kind:     "interface",
			Exported: exported,
			Line:     lineNo,
		})
		if exported {
			pf.Exports = append(pf.Exports, Export{
				Name: name,
				Kind: "type",
				Line: lineNo,
			})
		}
		return
	}

	// Impl blocks
	if matches := reRustImpl.FindStringSubmatch(line); matches != nil {
		name := matches[1]
		if matches[2] != "" {
			// impl Trait for Type — use the Type name
			name = matches[2]
		}
		pf.Symbols = append(pf.Symbols, Symbol{
			Name:     name,
			Kind:     "type",
			Exported: false,
			Line:     lineNo,
		})
		return
	}

	// Type aliases
	if matches := reRustType.FindStringSubmatch(line); matches != nil {
		name := matches[1]
		pf.Symbols = append(pf.Symbols, Symbol{
			Name:     name,
			Kind:     "type",
			Exported: exported,
			Line:     lineNo,
		})
		if exported {
			pf.Exports = append(pf.Exports, Export{
				Name: name,
				Kind: "type",
				Line: lineNo,
			})
		}
		return
	}

	// Constants and statics
	if matches := reRustConst.FindStringSubmatch(line); matches != nil {
		name := matches[1]
		pf.Symbols = append(pf.Symbols, Symbol{
			Name:     name,
			Kind:     "variable",
			Exported: exported,
			Line:     lineNo,
		})
		if exported {
			pf.Exports = append(pf.Exports, Export{
				Name: name,
				Kind: "variable",
				Line: lineNo,
			})
		}
		return
	}

	// Modules
	if matches := reRustMod.FindStringSubmatch(line); matches != nil {
		name := matches[1]
		pf.Symbols = append(pf.Symbols, Symbol{
			Name:     name,
			Kind:     "variable",
			Exported: exported,
			Line:     lineNo,
		})
		if exported {
			pf.Exports = append(pf.Exports, Export{
				Name: name,
				Kind: "variable",
				Line: lineNo,
			})
		}
		return
	}
}

// parseRustGroupedNames parses "io, fs::File, collections::HashMap" into names.
func parseRustGroupedNames(s string) []string {
	parts := strings.Split(s, ",")
	names := make([]string, 0, len(parts))
	for _, part := range parts {
		part = strings.TrimSpace(part)
		if part == "" {
			continue
		}
		// Handle "name as alias" — use the local alias
		if idx := strings.Index(part, " as "); idx != -1 {
			names = append(names, strings.TrimSpace(part[idx+4:]))
		} else {
			// For paths like "io::Read", use the last component
			subParts := strings.Split(part, "::")
			names = append(names, subParts[len(subParts)-1])
		}
	}
	return names
}
