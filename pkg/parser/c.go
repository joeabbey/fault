package parser

import (
	"regexp"
	"strings"
)

// CParser extracts structural information from C files via regex.
type CParser struct{}

// NewCParser creates a new C parser.
func NewCParser() *CParser {
	return &CParser{}
}

// Language returns "c".
func (p *CParser) Language() string {
	return "c"
}

// Regex patterns for C parsing.
var (
	// #include <header.h>
	reCIncludeSystem = regexp.MustCompile(`^#include\s+<([^>]+)>`)

	// #include "local.h"
	reCIncludeLocal = regexp.MustCompile(`^#include\s+"([^"]+)"`)

	// typedef ... name;  (simple typedefs like typedef unsigned int uint32)
	reCTypedef = regexp.MustCompile(`^typedef\s+.*\s+(\w+)\s*;`)

	// typedef struct { ... } Name;  (one-line)
	reCTypedefStruct = regexp.MustCompile(`^typedef\s+struct\s*\{.*\}\s*(\w+)\s*;`)

	// typedef struct name Name;
	reCTypedefStructFwd = regexp.MustCompile(`^typedef\s+struct\s+\w+\s+(\w+)\s*;`)

	// typedef enum { ... } Name;  (one-line)
	reCTypedefEnum = regexp.MustCompile(`^typedef\s+enum\s*\{.*\}\s*(\w+)\s*;`)

	// } Name;  (closing a multi-line typedef struct/enum block)
	reCTypedefClose = regexp.MustCompile(`^\}\s*(\w+)\s*;`)

	// struct name {
	reCStruct = regexp.MustCompile(`^struct\s+(\w+)\s*\{`)

	// enum name {
	reCEnum = regexp.MustCompile(`^enum\s+(\w+)\s*\{`)

	// #define NAME ...
	reCDefine = regexp.MustCompile(`^#define\s+(\w+)`)

	// Function: return_type name(params) {  or  return_type name(params);
	// Also handles pointer returns: char* name(...)
	reCFunc = regexp.MustCompile(`^(static\s+)?(?:(?:const|unsigned|signed|long|short|inline|extern|volatile)\s+)*(\w[\w\s\*]*?)\s+(\*?\w+)\s*\(([^)]*)\)\s*[{;]`)
)

// Parse extracts structural info from C content.
func (p *CParser) Parse(filename string, content []byte) (*ParsedFile, error) {
	pf := &ParsedFile{
		Path:     filename,
		Language: "c",
		Imports:  make([]Import, 0),
		Exports:  make([]Export, 0),
		Symbols:  make([]Symbol, 0),
	}

	lines := strings.Split(string(content), "\n")
	inBlockComment := false
	inTypedefBlock := false
	typedefStartLine := 0

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

		// If inside a multi-line typedef block, look for closing } Name;
		if inTypedefBlock {
			if matches := reCTypedefClose.FindStringSubmatch(trimmed); matches != nil {
				name := matches[1]
				pf.Symbols = append(pf.Symbols, Symbol{
					Name:     name,
					Kind:     "type",
					Exported: true,
					Line:     typedefStartLine,
				})
				pf.Exports = append(pf.Exports, Export{
					Name: name,
					Kind: "type",
					Line: typedefStartLine,
				})
				inTypedefBlock = false
			}
			continue
		}

		// Detect start of multi-line typedef struct/enum
		if (strings.HasPrefix(trimmed, "typedef struct") || strings.HasPrefix(trimmed, "typedef enum")) &&
			strings.Contains(trimmed, "{") && !strings.Contains(trimmed, "}") {
			inTypedefBlock = true
			typedefStartLine = lineNo
			continue
		}

		// Extract imports
		p.extractCImports(pf, trimmed, lineNo)

		// Extract symbols and exports
		p.extractCSymbols(pf, trimmed, lineNo)
	}

	pf.Symbols = deduplicateSymbols(pf.Symbols)

	return pf, nil
}

// extractCImports handles C #include statements.
func (p *CParser) extractCImports(pf *ParsedFile, line string, lineNo int) {
	if !strings.HasPrefix(line, "#include") {
		return
	}

	if matches := reCIncludeSystem.FindStringSubmatch(line); matches != nil {
		pf.Imports = append(pf.Imports, Import{
			Path: matches[1],
			Line: lineNo,
		})
		return
	}

	if matches := reCIncludeLocal.FindStringSubmatch(line); matches != nil {
		pf.Imports = append(pf.Imports, Import{
			Path: matches[1],
			Line: lineNo,
		})
	}
}

// extractCSymbols handles C declarations.
func (p *CParser) extractCSymbols(pf *ParsedFile, line string, lineNo int) {
	// Macros
	if strings.HasPrefix(line, "#define") {
		if matches := reCDefine.FindStringSubmatch(line); matches != nil {
			name := matches[1]
			// Skip include guards
			if strings.HasSuffix(name, "_H") || strings.HasSuffix(name, "_H_") {
				return
			}
			pf.Symbols = append(pf.Symbols, Symbol{
				Name:     name,
				Kind:     "macro",
				Exported: true,
				Line:     lineNo,
			})
		}
		return
	}

	// Typedef enum (one-line)
	if strings.HasPrefix(line, "typedef enum") {
		if matches := reCTypedefEnum.FindStringSubmatch(line); matches != nil {
			name := matches[1]
			pf.Symbols = append(pf.Symbols, Symbol{
				Name:     name,
				Kind:     "type",
				Exported: true,
				Line:     lineNo,
			})
			pf.Exports = append(pf.Exports, Export{
				Name: name,
				Kind: "type",
				Line: lineNo,
			})
			return
		}
	}

	// Typedef struct (one-line or forward)
	if strings.HasPrefix(line, "typedef struct") {
		if matches := reCTypedefStruct.FindStringSubmatch(line); matches != nil {
			name := matches[1]
			pf.Symbols = append(pf.Symbols, Symbol{
				Name:     name,
				Kind:     "type",
				Exported: true,
				Line:     lineNo,
			})
			pf.Exports = append(pf.Exports, Export{
				Name: name,
				Kind: "type",
				Line: lineNo,
			})
			return
		}
		if matches := reCTypedefStructFwd.FindStringSubmatch(line); matches != nil {
			name := matches[1]
			pf.Symbols = append(pf.Symbols, Symbol{
				Name:     name,
				Kind:     "type",
				Exported: true,
				Line:     lineNo,
			})
			pf.Exports = append(pf.Exports, Export{
				Name: name,
				Kind: "type",
				Line: lineNo,
			})
			return
		}
	}

	// Typedef (simple)
	if strings.HasPrefix(line, "typedef") {
		if matches := reCTypedef.FindStringSubmatch(line); matches != nil {
			name := matches[1]
			pf.Symbols = append(pf.Symbols, Symbol{
				Name:     name,
				Kind:     "type",
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

	// Struct definition
	if strings.HasPrefix(line, "struct") {
		if matches := reCStruct.FindStringSubmatch(line); matches != nil {
			name := matches[1]
			pf.Symbols = append(pf.Symbols, Symbol{
				Name:     name,
				Kind:     "struct",
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

	// Enum definition
	if strings.HasPrefix(line, "enum") {
		if matches := reCEnum.FindStringSubmatch(line); matches != nil {
			name := matches[1]
			pf.Symbols = append(pf.Symbols, Symbol{
				Name:     name,
				Kind:     "type",
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

	// Functions
	if matches := reCFunc.FindStringSubmatch(line); matches != nil {
		isStatic := strings.TrimSpace(matches[1]) == "static"
		name := strings.TrimLeft(matches[3], "*")
		params := matches[4]

		sym := Symbol{
			Name:      name,
			Kind:      "function",
			Exported:  !isStatic,
			Line:      lineNo,
			Signature: name + "(" + strings.TrimSpace(params) + ")",
		}
		pf.Symbols = append(pf.Symbols, sym)
		if !isStatic {
			pf.Exports = append(pf.Exports, Export{
				Name: name,
				Kind: "function",
				Line: lineNo,
			})
		}
	}
}
