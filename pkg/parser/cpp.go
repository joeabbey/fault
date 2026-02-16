package parser

import (
	"regexp"
	"strings"
)

// CppParser extracts structural information from C++ files via regex.
type CppParser struct{}

// NewCppParser creates a new C++ parser.
func NewCppParser() *CppParser {
	return &CppParser{}
}

// Language returns "cpp".
func (p *CppParser) Language() string {
	return "cpp"
}

// Regex patterns for C++ parsing.
var (
	// #include <header>
	reCppIncludeSystem = regexp.MustCompile(`^#include\s+<([^>]+)>`)

	// #include "local.h"
	reCppIncludeLocal = regexp.MustCompile(`^#include\s+"([^"]+)"`)

	// using namespace std;
	reCppUsingNamespace = regexp.MustCompile(`^using\s+namespace\s+([\w:]+)\s*;`)

	// using std::vector;
	reCppUsingDecl = regexp.MustCompile(`^using\s+([\w:]+::\w+)\s*;`)

	// class Name [: bases] {
	reCppClass = regexp.MustCompile(`^(?:template\s*<[^>]*>\s*)?class\s+(\w+)(?:\s*:\s*.*?)?\s*\{`)

	// struct Name [: bases] {
	reCppStruct = regexp.MustCompile(`^(?:template\s*<[^>]*>\s*)?struct\s+(\w+)(?:\s*:\s*.*?)?\s*\{`)

	// namespace name {
	reCppNamespace = regexp.MustCompile(`^namespace\s+(\w+)\s*\{`)

	// template <typename T> / template <class T>
	reCppTemplate = regexp.MustCompile(`^template\s*<([^>]*)>`)

	// template <typename T> class/struct Name
	reCppTemplateClass = regexp.MustCompile(`^template\s*<[^>]*>\s*class\s+(\w+)`)
	reCppTemplateStruct = regexp.MustCompile(`^template\s*<[^>]*>\s*struct\s+(\w+)`)

	// template <typename T> T func_name(...)
	reCppTemplateFunc = regexp.MustCompile(`^template\s*<[^>]*>\s*(?:[\w:*&\s]+)\s+(\w+)\s*\(([^)]*)\)\s*[{;]`)

	// enum class Name {
	reCppEnumClass = regexp.MustCompile(`^enum\s+class\s+(\w+)`)

	// enum Name {
	reCppEnum = regexp.MustCompile(`^enum\s+(\w+)\s*\{`)

	// #define NAME
	reCppDefine = regexp.MustCompile(`^#define\s+(\w+)`)

	// Function: [static] return_type name(params) [const] {  or  ;
	reCppFunc = regexp.MustCompile(`^(static\s+)?(?:(?:const|unsigned|signed|long|short|inline|extern|volatile|virtual)\s+)*(\w[\w\s\*&:<>]*?)\s+(\*?\w+)\s*\(([^)]*)\)\s*(?:const\s*)?(?:override\s*)?[{;]`)
)

// Parse extracts structural info from C++ content.
func (p *CppParser) Parse(filename string, content []byte) (*ParsedFile, error) {
	pf := &ParsedFile{
		Path:     filename,
		Language: "cpp",
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
		p.extractCppImports(pf, trimmed, lineNo)

		// Extract symbols and exports
		p.extractCppSymbols(pf, trimmed, lineNo)
	}

	pf.Symbols = deduplicateSymbols(pf.Symbols)

	return pf, nil
}

// extractCppImports handles C++ include and using statements.
func (p *CppParser) extractCppImports(pf *ParsedFile, line string, lineNo int) {
	if strings.HasPrefix(line, "#include") {
		if matches := reCppIncludeSystem.FindStringSubmatch(line); matches != nil {
			pf.Imports = append(pf.Imports, Import{
				Path: matches[1],
				Line: lineNo,
			})
			return
		}
		if matches := reCppIncludeLocal.FindStringSubmatch(line); matches != nil {
			pf.Imports = append(pf.Imports, Import{
				Path: matches[1],
				Line: lineNo,
			})
		}
		return
	}

	if strings.HasPrefix(line, "using ") {
		if matches := reCppUsingNamespace.FindStringSubmatch(line); matches != nil {
			pf.Imports = append(pf.Imports, Import{
				Path:  matches[1],
				Names: []string{matches[1]},
				Line:  lineNo,
			})
			return
		}
		if matches := reCppUsingDecl.FindStringSubmatch(line); matches != nil {
			fullPath := matches[1]
			// Extract the short name (last component)
			parts := strings.Split(fullPath, "::")
			shortName := parts[len(parts)-1]
			pf.Imports = append(pf.Imports, Import{
				Path:  fullPath,
				Names: []string{shortName},
				Line:  lineNo,
			})
		}
	}
}

// extractCppSymbols handles C++ declarations.
func (p *CppParser) extractCppSymbols(pf *ParsedFile, line string, lineNo int) {
	// Macros
	if strings.HasPrefix(line, "#define") {
		if matches := reCppDefine.FindStringSubmatch(line); matches != nil {
			name := matches[1]
			if strings.HasSuffix(name, "_H") || strings.HasSuffix(name, "_H_") ||
				strings.HasSuffix(name, "_HPP") || strings.HasSuffix(name, "_HPP_") {
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

	// Namespaces
	if strings.HasPrefix(line, "namespace") {
		if matches := reCppNamespace.FindStringSubmatch(line); matches != nil {
			name := matches[1]
			pf.Symbols = append(pf.Symbols, Symbol{
				Name:     name,
				Kind:     "namespace",
				Exported: true,
				Line:     lineNo,
			})
			pf.Exports = append(pf.Exports, Export{
				Name: name,
				Kind: "namespace",
				Line: lineNo,
			})
		}
		return
	}

	// Template declarations (class, struct, or function)
	if strings.HasPrefix(line, "template") {
		if matches := reCppTemplateClass.FindStringSubmatch(line); matches != nil {
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
			return
		}
		if matches := reCppTemplateStruct.FindStringSubmatch(line); matches != nil {
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
			return
		}
		if matches := reCppTemplateFunc.FindStringSubmatch(line); matches != nil {
			name := matches[1]
			params := matches[2]
			pf.Symbols = append(pf.Symbols, Symbol{
				Name:      name,
				Kind:      "function",
				Exported:  true,
				Line:      lineNo,
				Signature: name + "(" + strings.TrimSpace(params) + ")",
			})
			pf.Exports = append(pf.Exports, Export{
				Name: name,
				Kind: "function",
				Line: lineNo,
			})
			return
		}
		// Standalone template declaration (template line only) - skip
		return
	}

	// Enum class
	if strings.HasPrefix(line, "enum class") {
		if matches := reCppEnumClass.FindStringSubmatch(line); matches != nil {
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

	// Enum (old style)
	if strings.HasPrefix(line, "enum ") && !strings.HasPrefix(line, "enum class") {
		if matches := reCppEnum.FindStringSubmatch(line); matches != nil {
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

	// Classes
	if strings.Contains(line, "class ") && !strings.HasPrefix(line, "enum class") {
		if matches := reCppClass.FindStringSubmatch(line); matches != nil {
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
			return
		}
	}

	// Structs
	if strings.HasPrefix(line, "struct") {
		if matches := reCppStruct.FindStringSubmatch(line); matches != nil {
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

	// Functions (non-template)
	if matches := reCppFunc.FindStringSubmatch(line); matches != nil {
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
