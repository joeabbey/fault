package parser

import (
	"regexp"
	"strings"
)

// SwiftParser extracts structural information from Swift files via regex.
type SwiftParser struct{}

// NewSwiftParser creates a new Swift parser.
func NewSwiftParser() *SwiftParser {
	return &SwiftParser{}
}

// Language returns "swift".
func (p *SwiftParser) Language() string {
	return "swift"
}

// Regex patterns for Swift parsing.
var (
	// import Foundation
	// import UIKit
	// @testable import MyModule
	reSwiftImport = regexp.MustCompile(`(?m)^(?:@testable\s+)?import\s+(\w+)`)

	// [open|public|internal|fileprivate|private] [final] class Name [: Parent, Protocol]
	reSwiftClass = regexp.MustCompile(`(?m)^(open|public|internal|fileprivate|private)?\s*(?:final\s+)?class\s+(\w+)`)

	// [access] struct Name [: Protocol]
	reSwiftStruct = regexp.MustCompile(`(?m)^(open|public|internal|fileprivate|private)?\s*struct\s+(\w+)`)

	// [access] enum Name [: RawType]
	reSwiftEnum = regexp.MustCompile(`(?m)^(open|public|internal|fileprivate|private)?\s*enum\s+(\w+)`)

	// [access] protocol Name [: Parent]
	reSwiftProtocol = regexp.MustCompile(`(?m)^(open|public|internal|fileprivate|private)?\s*protocol\s+(\w+)`)

	// [access] actor Name
	reSwiftActor = regexp.MustCompile(`(?m)^(open|public|internal|fileprivate|private)?\s*actor\s+(\w+)`)

	// [access] [static|class] [override] func name(params) [async] [throws] [-> ReturnType]
	reSwiftFunc = regexp.MustCompile(`(?m)^(open|public|internal|fileprivate|private)?\s*(?:static\s+|class\s+)?(?:override\s+)?func\s+(\w+)\s*\(([^)]*)\)`)

	// [access] [static] [let|var] name: Type
	reSwiftProperty = regexp.MustCompile(`(?m)^(open|public|internal|fileprivate|private)?\s*(?:static\s+)?(?:let|var)\s+(\w+)\s*[:\s=]`)

	// extension Type: Protocol
	reSwiftExtension = regexp.MustCompile(`(?m)^extension\s+(\w+)`)

	// typealias Name = Type
	reSwiftTypealias = regexp.MustCompile(`(?m)^(open|public|internal|fileprivate|private)?\s*typealias\s+(\w+)\s*=`)
)

// swiftExportedAccess returns true if the access modifier makes the symbol publicly accessible.
func swiftExportedAccess(access string) bool {
	return access == "open" || access == "public"
}

// Parse extracts structural info from Swift content.
func (p *SwiftParser) Parse(filename string, content []byte) (*ParsedFile, error) {
	pf := &ParsedFile{
		Path:     filename,
		Language: "swift",
		Imports:  make([]Import, 0),
		Exports:  make([]Export, 0),
		Symbols:  make([]Symbol, 0),
	}

	lines := strings.Split(string(content), "\n")
	inBlockComment := false

	for lineNum, line := range lines {
		lineNo := lineNum + 1

		// Handle block comments
		if inBlockComment {
			if strings.Contains(line, "*/") {
				inBlockComment = false
			}
			continue
		}

		trimmed := strings.TrimSpace(line)

		// Skip empty lines and single-line comments
		if trimmed == "" || strings.HasPrefix(trimmed, "//") {
			continue
		}

		// Start of block comment
		if strings.HasPrefix(trimmed, "/*") {
			if !strings.Contains(trimmed, "*/") {
				inBlockComment = true
			}
			continue
		}

		// Extract imports
		p.extractSwiftImports(pf, trimmed, lineNo)

		// Extract symbols and exports
		p.extractSwiftSymbols(pf, trimmed, lineNo)
	}

	pf.Symbols = deduplicateSymbols(pf.Symbols)

	return pf, nil
}

// extractSwiftImports handles Swift import statements.
func (p *SwiftParser) extractSwiftImports(pf *ParsedFile, line string, lineNo int) {
	// Must start with "import" or "@testable import"
	if !strings.HasPrefix(line, "import ") && !strings.HasPrefix(line, "@testable ") {
		return
	}

	if matches := reSwiftImport.FindStringSubmatch(line); matches != nil {
		moduleName := matches[1]
		pf.Imports = append(pf.Imports, Import{
			Path:  moduleName,
			Names: []string{moduleName},
			Line:  lineNo,
		})
	}
}

// extractSwiftSymbols handles Swift declarations.
func (p *SwiftParser) extractSwiftSymbols(pf *ParsedFile, line string, lineNo int) {
	// Functions
	if matches := reSwiftFunc.FindStringSubmatch(line); matches != nil {
		access := matches[1]
		name := matches[2]
		params := matches[3]
		exported := swiftExportedAccess(access)
		sym := Symbol{
			Name:      name,
			Kind:      "function",
			Exported:  exported,
			Line:      lineNo,
			Signature: "func " + name + "(" + strings.TrimSpace(params) + ")",
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

	// Classes
	if matches := reSwiftClass.FindStringSubmatch(line); matches != nil {
		access := matches[1]
		name := matches[2]
		exported := swiftExportedAccess(access)
		pf.Symbols = append(pf.Symbols, Symbol{
			Name:     name,
			Kind:     "class",
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

	// Structs
	if matches := reSwiftStruct.FindStringSubmatch(line); matches != nil {
		access := matches[1]
		name := matches[2]
		exported := swiftExportedAccess(access)
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
	if matches := reSwiftEnum.FindStringSubmatch(line); matches != nil {
		access := matches[1]
		name := matches[2]
		exported := swiftExportedAccess(access)
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

	// Protocols
	if matches := reSwiftProtocol.FindStringSubmatch(line); matches != nil {
		access := matches[1]
		name := matches[2]
		exported := swiftExportedAccess(access)
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

	// Actors
	if matches := reSwiftActor.FindStringSubmatch(line); matches != nil {
		access := matches[1]
		name := matches[2]
		exported := swiftExportedAccess(access)
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

	// Extensions
	if matches := reSwiftExtension.FindStringSubmatch(line); matches != nil {
		name := matches[1]
		pf.Symbols = append(pf.Symbols, Symbol{
			Name:     name,
			Kind:     "type",
			Exported: false,
			Line:     lineNo,
		})
		return
	}

	// Typealiases
	if matches := reSwiftTypealias.FindStringSubmatch(line); matches != nil {
		access := matches[1]
		name := matches[2]
		exported := swiftExportedAccess(access)
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

	// Properties (only top-level, not inside function bodies)
	if matches := reSwiftProperty.FindStringSubmatch(line); matches != nil {
		access := matches[1]
		name := matches[2]
		exported := swiftExportedAccess(access)
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
