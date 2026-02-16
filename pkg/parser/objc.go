package parser

import (
	"regexp"
	"strings"
)

// ObjCParser extracts structural information from Objective-C files via regex.
type ObjCParser struct{}

// NewObjCParser creates a new Objective-C parser.
func NewObjCParser() *ObjCParser {
	return &ObjCParser{}
}

// Language returns "objc".
func (p *ObjCParser) Language() string {
	return "objc"
}

// Regex patterns for Objective-C parsing.
var (
	// #import <Framework/Header.h>
	reObjCImportSystem = regexp.MustCompile(`^#import\s+<([^>]+)>`)

	// #import "local.h"
	reObjCImportLocal = regexp.MustCompile(`^#import\s+"([^"]+)"`)

	// @import Module;
	reObjCModuleImport = regexp.MustCompile(`^@import\s+([\w.]+)\s*;`)

	// @interface ClassName : SuperClass <Protocol>
	reObjCInterface = regexp.MustCompile(`^@interface\s+(\w+)`)

	// @protocol ProtocolName <ParentProtocol>
	reObjCProtocol = regexp.MustCompile(`^@protocol\s+(\w+)`)

	// @implementation ClassName
	reObjCImplementation = regexp.MustCompile(`^@implementation\s+(\w+)`)

	// + (returnType)methodName  or  + (returnType)methodName:(Type)param
	reObjCClassMethod = regexp.MustCompile(`^\+\s*\(([^)]+)\)(\w[\w:]*(?:\s*\([^)]*\)\s*\w+\s*)*)`)

	// - (returnType)methodName  or  - (returnType)methodName:(Type)param
	reObjCInstanceMethod = regexp.MustCompile(`^-\s*\(([^)]+)\)(\w[\w:]*(?:\s*\([^)]*\)\s*\w+\s*)*)`)

	// @property (attributes) Type name;
	reObjCProperty = regexp.MustCompile(`^@property\s+(?:\([^)]*\)\s+)?(\w[\w\s\*]*?)\s+\*?(\w+)\s*;`)

	// #define NAME
	reObjCDefine = regexp.MustCompile(`^#define\s+(\w+)`)
)

// Parse extracts structural info from Objective-C content.
func (p *ObjCParser) Parse(filename string, content []byte) (*ParsedFile, error) {
	pf := &ParsedFile{
		Path:     filename,
		Language: "objc",
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
		p.extractObjCImports(pf, trimmed, lineNo)

		// Extract symbols and exports
		p.extractObjCSymbols(pf, trimmed, lineNo)
	}

	pf.Symbols = deduplicateSymbols(pf.Symbols)

	return pf, nil
}

// extractObjCImports handles Objective-C import statements.
func (p *ObjCParser) extractObjCImports(pf *ParsedFile, line string, lineNo int) {
	if strings.HasPrefix(line, "#import") {
		if matches := reObjCImportSystem.FindStringSubmatch(line); matches != nil {
			pf.Imports = append(pf.Imports, Import{
				Path: matches[1],
				Line: lineNo,
			})
			return
		}
		if matches := reObjCImportLocal.FindStringSubmatch(line); matches != nil {
			pf.Imports = append(pf.Imports, Import{
				Path: matches[1],
				Line: lineNo,
			})
		}
		return
	}

	if strings.HasPrefix(line, "@import") {
		if matches := reObjCModuleImport.FindStringSubmatch(line); matches != nil {
			pf.Imports = append(pf.Imports, Import{
				Path:  matches[1],
				Names: []string{matches[1]},
				Line:  lineNo,
			})
		}
	}
}

// extractObjCSymbols handles Objective-C declarations.
func (p *ObjCParser) extractObjCSymbols(pf *ParsedFile, line string, lineNo int) {
	// Macros
	if strings.HasPrefix(line, "#define") {
		if matches := reObjCDefine.FindStringSubmatch(line); matches != nil {
			name := matches[1]
			pf.Symbols = append(pf.Symbols, Symbol{
				Name:     name,
				Kind:     "macro",
				Exported: true,
				Line:     lineNo,
			})
		}
		return
	}

	// @interface
	if strings.HasPrefix(line, "@interface") {
		if matches := reObjCInterface.FindStringSubmatch(line); matches != nil {
			name := matches[1]
			pf.Symbols = append(pf.Symbols, Symbol{
				Name:     name,
				Kind:     "class",
				Exported: true,
				Line:     lineNo,
			})
			pf.Exports = append(pf.Exports, Export{
				Name: name,
				Kind: "type",
				Line: lineNo,
			})
		}
		return
	}

	// @protocol
	if strings.HasPrefix(line, "@protocol") {
		if matches := reObjCProtocol.FindStringSubmatch(line); matches != nil {
			name := matches[1]
			pf.Symbols = append(pf.Symbols, Symbol{
				Name:     name,
				Kind:     "interface",
				Exported: true,
				Line:     lineNo,
			})
			pf.Exports = append(pf.Exports, Export{
				Name: name,
				Kind: "type",
				Line: lineNo,
			})
		}
		return
	}

	// @implementation
	if strings.HasPrefix(line, "@implementation") {
		if matches := reObjCImplementation.FindStringSubmatch(line); matches != nil {
			name := matches[1]
			pf.Symbols = append(pf.Symbols, Symbol{
				Name:     name,
				Kind:     "class",
				Exported: false,
				Line:     lineNo,
			})
		}
		return
	}

	// @property
	if strings.HasPrefix(line, "@property") {
		if matches := reObjCProperty.FindStringSubmatch(line); matches != nil {
			name := matches[2]
			pf.Symbols = append(pf.Symbols, Symbol{
				Name:     name,
				Kind:     "variable",
				Exported: true,
				Line:     lineNo,
			})
		}
		return
	}

	// Class methods (+)
	if strings.HasPrefix(line, "+") {
		if matches := reObjCClassMethod.FindStringSubmatch(line); matches != nil {
			rawSel := matches[2]
			name := objcSelectorName(rawSel)
			pf.Symbols = append(pf.Symbols, Symbol{
				Name:      name,
				Kind:      "method",
				Exported:  true,
				Line:      lineNo,
				Signature: "+" + name,
			})
		}
		return
	}

	// Instance methods (-)
	if strings.HasPrefix(line, "-") {
		if matches := reObjCInstanceMethod.FindStringSubmatch(line); matches != nil {
			rawSel := matches[2]
			name := objcSelectorName(rawSel)
			pf.Symbols = append(pf.Symbols, Symbol{
				Name:      name,
				Kind:      "method",
				Exported:  true,
				Line:      lineNo,
				Signature: "-" + name,
			})
		}
		return
	}
}

// objcSelectorName extracts the selector name from a method signature.
// E.g., "fetchDataWithCompletion:(void (^)(NSData *, NSError *))completion" -> "fetchDataWithCompletion:"
// E.g., "sharedManager" -> "sharedManager"
func objcSelectorName(raw string) string {
	// Trim whitespace
	raw = strings.TrimSpace(raw)

	// If it contains ':', extract all the keyword parts to form the selector
	if strings.Contains(raw, ":") {
		// Split by whitespace to get tokens, then find keyword parts ending in ':'
		var parts []string
		tokens := strings.Fields(raw)
		for _, tok := range tokens {
			// Remove any leading/trailing parens from cast types
			if strings.HasPrefix(tok, "(") {
				continue
			}
			if strings.HasSuffix(tok, ":") {
				parts = append(parts, tok)
			}
			// Handle tokens with embedded colons like "name:value"
			if idx := strings.Index(tok, ":"); idx > 0 && !strings.HasSuffix(tok, ":") {
				parts = append(parts, tok[:idx+1])
			}
		}
		if len(parts) > 0 {
			return strings.Join(parts, "")
		}
	}

	// Simple selector without params
	return strings.Fields(raw)[0]
}
