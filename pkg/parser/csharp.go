package parser

import (
	"regexp"
	"strings"
)

// CSharpParser extracts structural information from C# files via regex.
type CSharpParser struct{}

// NewCSharpParser creates a new C# parser.
func NewCSharpParser() *CSharpParser {
	return &CSharpParser{}
}

// Language returns "csharp".
func (p *CSharpParser) Language() string {
	return "csharp"
}

// Regex patterns for C# parsing.
var (
	// Using statements:
	// using System.Collections.Generic;
	// using static System.Math;
	// using Alias = System.String;
	// global using System.Linq;
	reCSharpUsing = regexp.MustCompile(`^\s*(?:global\s+)?using\s+(?:static\s+)?(?:\w+\s*=\s*)?([\w.]+)\s*;`)

	// Static using (to mark as type import)
	reCSharpStaticUsing = regexp.MustCompile(`^\s*(?:global\s+)?using\s+static\s+`)

	// Namespace declarations:
	// namespace Foo.Bar { }  (block-scoped)
	// namespace Foo.Bar;     (file-scoped)
	reCSharpNamespace = regexp.MustCompile(`^\s*namespace\s+([\w.]+)\s*[;{]`)

	// Class declarations:
	// [public|internal|protected|private] [abstract|sealed|static|partial] class Name [: Base, IInterface]
	reCSharpClass = regexp.MustCompile(`(?:^|\s)(public|internal|protected|private)?\s*(?:abstract\s+|sealed\s+|static\s+|partial\s+)*class\s+(\w+)`)

	// Interface declarations:
	// [access] [partial] interface IName [: IBase]
	reCSharpInterface = regexp.MustCompile(`(?:^|\s)(public|internal|protected|private)?\s*(?:partial\s+)?interface\s+(\w+)`)

	// Record declarations:
	// [access] [sealed|abstract] record Name(params);
	reCSharpRecord = regexp.MustCompile(`(?:^|\s)(public|internal|protected|private)?\s*(?:sealed\s+|abstract\s+)*record\s+(?:struct\s+|class\s+)?(\w+)`)

	// Struct declarations:
	// [access] [readonly|partial] struct Name
	reCSharpStruct = regexp.MustCompile(`(?:^|\s)(public|internal|protected|private)?\s*(?:readonly\s+|partial\s+)*struct\s+(\w+)`)

	// Enum declarations:
	// [access] enum Name
	reCSharpEnum = regexp.MustCompile(`(?:^|\s)(public|internal|protected|private)?\s*enum\s+(\w+)`)

	// Method declarations:
	// [access] [static|async|virtual|override|abstract] ReturnType MethodName(params)
	reCSharpMethod = regexp.MustCompile(`^\s*(public|protected|private|internal)?\s*(?:static\s+)?(?:async\s+)?(?:virtual\s+)?(?:override\s+)?(?:abstract\s+)?(?:new\s+)?(\w[\w<>\[\],\s?]*?)\s+(\w+)\s*\(([^)]*)\)`)

	// Constructor declarations:
	// [access] ClassName(params)
	reCSharpConstructor = regexp.MustCompile(`^\s*(public|protected|private|internal)?\s*(\w+)\s*\(([^)]*)\)\s*(?::\s*(?:base|this)\s*\([^)]*\)\s*)?\{`)

	// Property declarations:
	// [access] [static] Type Name { get; set; }
	reCSharpProperty = regexp.MustCompile(`^\s*(public|protected|private|internal)\s+(?:static\s+)?(?:virtual\s+)?(?:override\s+)?(?:abstract\s+)?(?:new\s+)?(\w[\w<>\[\],\s?]*?)\s+(\w+)\s*\{`)

	// Field declarations:
	// [access] [static] [readonly] Type name;
	// [access] [static] [readonly] Type name = value;
	reCSharpField = regexp.MustCompile(`^\s*(public|protected|private|internal)\s+(?:static\s+)?(?:readonly\s+)?(?:volatile\s+)?(?:const\s+)?(\w[\w<>\[\],\s?]*?)\s+(\w+)\s*[;=]`)

	// Attribute (annotation-like)
	reCSharpAttribute = regexp.MustCompile(`^\s*\[[\w.]+`)
)

// Parse extracts structural info from C# content.
func (p *CSharpParser) Parse(filename string, content []byte) (*ParsedFile, error) {
	pf := &ParsedFile{
		Path:     filename,
		Language: "csharp",
		Imports:  make([]Import, 0),
		Exports:  make([]Export, 0),
		Symbols:  make([]Symbol, 0),
	}

	lines := strings.Split(string(content), "\n")
	inBlockComment := false

	for lineNum, line := range lines {
		lineNo := lineNum + 1

		// Track block comments
		trimmed := strings.TrimSpace(line)
		if inBlockComment {
			if strings.Contains(trimmed, "*/") {
				inBlockComment = false
			}
			continue
		}
		if strings.HasPrefix(trimmed, "/*") {
			if !strings.Contains(trimmed, "*/") {
				inBlockComment = true
			}
			continue
		}

		// Skip empty lines, single-line comments
		if trimmed == "" || strings.HasPrefix(trimmed, "//") {
			continue
		}

		// Skip attribute-only lines (the declaration follows on next line)
		if reCSharpAttribute.MatchString(trimmed) && !strings.Contains(trimmed, "class ") && !strings.Contains(trimmed, "interface ") {
			continue
		}

		// Extract using statements
		p.extractCSharpUsings(pf, trimmed, lineNo)

		// Extract type declarations
		p.extractCSharpTypes(pf, trimmed, lineNo)

		// Extract methods
		p.extractCSharpMethods(pf, trimmed, lineNo)

		// Extract properties and fields
		p.extractCSharpMembers(pf, trimmed, lineNo)
	}

	// Deduplicate symbols
	pf.Symbols = deduplicateSymbols(pf.Symbols)

	return pf, nil
}

// extractCSharpUsings finds using statements on a line.
func (p *CSharpParser) extractCSharpUsings(pf *ParsedFile, line string, lineNo int) {
	matches := reCSharpUsing.FindStringSubmatch(line)
	if matches == nil {
		return
	}

	namespacePath := matches[1]
	isStatic := reCSharpStaticUsing.MatchString(line)

	imp := Import{
		Path:  namespacePath,
		Names: make([]string, 0),
		Line:  lineNo,
	}

	// Extract the last segment as the imported name
	parts := strings.Split(namespacePath, ".")
	name := parts[len(parts)-1]
	imp.Names = append(imp.Names, name)

	if isStatic {
		imp.IsType = true
	}

	pf.Imports = append(pf.Imports, imp)
}

// extractCSharpTypes finds class, interface, record, struct, and enum declarations.
func (p *CSharpParser) extractCSharpTypes(pf *ParsedFile, line string, lineNo int) {
	type typeMatch struct {
		re   *regexp.Regexp
		kind string
	}
	typeMatches := []typeMatch{
		{reCSharpClass, "class"},
		{reCSharpInterface, "interface"},
		{reCSharpRecord, "record"},
		{reCSharpStruct, "struct"},
		{reCSharpEnum, "enum"},
	}

	for _, tm := range typeMatches {
		matches := tm.re.FindStringSubmatch(line)
		if matches == nil {
			continue
		}

		access := matches[1]
		name := matches[2]
		exported := access == "public"

		pf.Symbols = append(pf.Symbols, Symbol{
			Name:     name,
			Kind:     tm.kind,
			Exported: exported,
			Line:     lineNo,
		})

		if exported {
			pf.Exports = append(pf.Exports, Export{
				Name: name,
				Kind: tm.kind,
				Line: lineNo,
			})
		}
		return // Only match one type per line
	}
}

// extractCSharpMethods finds method and constructor declarations.
func (p *CSharpParser) extractCSharpMethods(pf *ParsedFile, line string, lineNo int) {
	// Skip lines that are type declarations (already handled)
	if reCSharpClass.MatchString(line) || reCSharpInterface.MatchString(line) ||
		reCSharpRecord.MatchString(line) || reCSharpStruct.MatchString(line) ||
		reCSharpEnum.MatchString(line) {
		return
	}

	// Skip lines that contain type declaration keywords
	trimmed := strings.TrimSpace(line)
	for _, kw := range []string{"class ", "interface ", "enum ", "struct ", "record "} {
		if strings.Contains(trimmed, kw) {
			return
		}
	}

	// Try constructor first
	if matches := reCSharpConstructor.FindStringSubmatch(line); matches != nil {
		access := matches[1]
		name := matches[2]
		params := strings.TrimSpace(matches[3])

		// Skip if the name is a C# keyword
		if isCSharpKeyword(name) {
			return
		}

		pf.Symbols = append(pf.Symbols, Symbol{
			Name:      name,
			Kind:      "constructor",
			Exported:  access == "public",
			Line:      lineNo,
			Signature: name + "(" + params + ")",
		})
		return
	}

	// Try regular method
	if matches := reCSharpMethod.FindStringSubmatch(line); matches != nil {
		access := matches[1]
		returnType := strings.TrimSpace(matches[2])
		name := matches[3]
		params := strings.TrimSpace(matches[4])

		// Skip if the return type or name is a keyword
		if isCSharpKeyword(returnType) || isCSharpKeyword(name) {
			return
		}

		// Skip property-like patterns (get/set)
		if name == "get" || name == "set" || name == "init" || name == "add" || name == "remove" {
			return
		}

		pf.Symbols = append(pf.Symbols, Symbol{
			Name:      name,
			Kind:      "method",
			Exported:  access == "public",
			Line:      lineNo,
			Signature: returnType + " " + name + "(" + params + ")",
		})
	}
}

// extractCSharpMembers finds property and field declarations.
func (p *CSharpParser) extractCSharpMembers(pf *ParsedFile, line string, lineNo int) {
	// Skip lines that are type or method declarations
	if reCSharpClass.MatchString(line) || reCSharpInterface.MatchString(line) ||
		reCSharpRecord.MatchString(line) || reCSharpStruct.MatchString(line) ||
		reCSharpEnum.MatchString(line) {
		return
	}
	if reCSharpMethod.MatchString(line) || reCSharpConstructor.MatchString(line) {
		return
	}

	// Try property first (has { get; set; })
	if matches := reCSharpProperty.FindStringSubmatch(line); matches != nil {
		access := matches[1]
		name := matches[3]

		if isCSharpKeyword(name) {
			return
		}

		pf.Symbols = append(pf.Symbols, Symbol{
			Name:     name,
			Kind:     "property",
			Exported: access == "public",
			Line:     lineNo,
		})
		return
	}

	// Try field
	if matches := reCSharpField.FindStringSubmatch(line); matches != nil {
		access := matches[1]
		name := matches[3]

		if isCSharpKeyword(name) {
			return
		}

		pf.Symbols = append(pf.Symbols, Symbol{
			Name:     name,
			Kind:     "field",
			Exported: access == "public",
			Line:     lineNo,
		})
	}
}

// isCSharpKeyword returns true if the string is a C# keyword that could be
// confused with an identifier by our regex patterns.
func isCSharpKeyword(s string) bool {
	keywords := map[string]bool{
		"if": true, "else": true, "for": true, "while": true, "do": true,
		"switch": true, "case": true, "default": true, "break": true, "continue": true,
		"return": true, "throw": true, "try": true, "catch": true,
		"finally": true, "new": true, "this": true, "base": true, "class": true,
		"interface": true, "enum": true, "struct": true, "record": true,
		"using": true, "namespace": true, "void": true, "null": true, "true": true,
		"false": true, "is": true, "as": true, "in": true, "out": true,
		"ref": true, "typeof": true, "sizeof": true, "lock": true,
		"foreach": true, "yield": true, "await": true, "async": true,
		"where": true, "select": true, "from": true, "var": true,
	}
	return keywords[s]
}
