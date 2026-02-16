package parser

import (
	"regexp"
	"strings"
)

// KotlinParser extracts structural information from Kotlin files via regex.
type KotlinParser struct{}

// NewKotlinParser creates a new Kotlin parser.
func NewKotlinParser() *KotlinParser {
	return &KotlinParser{}
}

// Language returns "kotlin".
func (p *KotlinParser) Language() string {
	return "kotlin"
}

// Regex patterns for Kotlin parsing.
var (
	// Package declaration: package com.example.app
	reKotlinPackage = regexp.MustCompile(`^\s*package\s+([\w.]+)`)

	// Import statements:
	// import java.util.List
	// import kotlinx.coroutines.*
	// import java.util.List as JList
	reKotlinImport = regexp.MustCompile(`^\s*import\s+([\w]+(?:\.[\w]+)*(?:\.\*)?)\s*(?:as\s+(\w+))?`)

	// Class declarations with optional modifiers:
	// class Foo
	// data class Foo(val x: Int)
	// sealed class Foo
	// enum class Color
	// abstract class Foo
	// open class Foo
	// inner class Foo
	// annotation class Foo
	// value class Foo
	reKotlinClass = regexp.MustCompile(`(?:^|\s)(?:public|protected|private|internal)?\s*(?:abstract\s+|open\s+|sealed\s+|inner\s+|data\s+|value\s+|annotation\s+|actual\s+|expect\s+)*class\s+(\w+)`)

	// Enum class
	reKotlinEnumClass = regexp.MustCompile(`(?:^|\s)(?:public|protected|private|internal)?\s*enum\s+class\s+(\w+)`)

	// Object declarations: object Foo, companion object
	reKotlinObject = regexp.MustCompile(`(?:^|\s)(?:public|protected|private|internal)?\s*(?:companion\s+)?object\s+(\w+)`)

	// Interface declarations: interface Foo, fun interface Foo
	reKotlinInterface = regexp.MustCompile(`(?:^|\s)(?:public|protected|private|internal)?\s*(?:fun\s+|sealed\s+)?interface\s+(\w+)`)

	// Function declarations:
	// fun doSomething(arg: String): Int
	// suspend fun fetchData(): Result
	// inline fun <T> execute(block: () -> T): T
	// private fun helper()
	// override fun toString(): String
	// infix fun Int.shl(x: Int): Int   (extension function)
	reKotlinFunction = regexp.MustCompile(`^\s*(?:(?:public|protected|private|internal)\s+)?(?:override\s+)?(?:actual\s+|expect\s+)?(?:inline\s+|noinline\s+|crossinline\s+)?(?:suspend\s+)?(?:operator\s+)?(?:infix\s+)?(?:tailrec\s+)?fun\s+(?:<[^>]*>\s+)?(?:[\w<>,\s?*]+\.)?\s*(\w+)\s*\(`)

	// Property declarations:
	// val name: String
	// var count: Int = 0
	// const val MAX = 100
	// lateinit var service: Service
	reKotlinProperty = regexp.MustCompile(`^\s*(?:(?:public|protected|private|internal)\s+)?(?:override\s+)?(?:const\s+)?(?:lateinit\s+)?(?:val|var)\s+(\w+)\s*(?::\s*([\w<>\[\],\s?.*]+))?`)

	// Type alias: typealias Name = Type
	reKotlinTypeAlias = regexp.MustCompile(`^\s*(?:(?:public|protected|private|internal)\s+)?typealias\s+(\w+)`)

	// Annotation line
	reKotlinAnnotation = regexp.MustCompile(`^\s*@\w+`)

	// Visibility detection — Kotlin defaults to public, so we only check for restrictive modifiers
	reKotlinPrivate   = regexp.MustCompile(`(?:^|\s)private\s`)
	reKotlinProtected = regexp.MustCompile(`(?:^|\s)protected\s`)
	reKotlinInternal  = regexp.MustCompile(`(?:^|\s)internal\s`)
)

// Parse extracts structural info from Kotlin content.
func (p *KotlinParser) Parse(filename string, content []byte) (*ParsedFile, error) {
	pf := &ParsedFile{
		Path:     filename,
		Language: "kotlin",
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

		// Skip empty lines and single-line comments
		if trimmed == "" || strings.HasPrefix(trimmed, "//") {
			continue
		}

		// Skip annotation-only lines
		if reKotlinAnnotation.MatchString(trimmed) && !strings.Contains(trimmed, "class ") &&
			!strings.Contains(trimmed, "interface ") && !strings.Contains(trimmed, "fun ") &&
			!strings.Contains(trimmed, "object ") {
			continue
		}

		// Extract imports
		p.extractKotlinImports(pf, trimmed, lineNo)

		// Extract type declarations
		p.extractKotlinTypes(pf, trimmed, lineNo)

		// Extract functions
		p.extractKotlinFunctions(pf, trimmed, lineNo)

		// Extract properties
		p.extractKotlinProperties(pf, trimmed, lineNo)
	}

	// Deduplicate symbols
	pf.Symbols = deduplicateSymbols(pf.Symbols)

	return pf, nil
}

// extractKotlinImports finds import statements on a line.
func (p *KotlinParser) extractKotlinImports(pf *ParsedFile, line string, lineNo int) {
	matches := reKotlinImport.FindStringSubmatch(line)
	if matches == nil {
		return
	}

	importPath := matches[1]
	alias := ""
	if len(matches) > 2 {
		alias = matches[2]
	}

	imp := Import{
		Path:  importPath,
		Names: make([]string, 0),
		Line:  lineNo,
	}

	if strings.HasSuffix(importPath, ".*") {
		// Wildcard import — no specific names
	} else if alias != "" {
		// Aliased import: import foo.Bar as Baz -> name is "Baz"
		imp.Names = append(imp.Names, alias)
	} else {
		// Specific import: extract the last component
		parts := strings.Split(importPath, ".")
		name := parts[len(parts)-1]
		imp.Names = append(imp.Names, name)
	}

	pf.Imports = append(pf.Imports, imp)
}

// extractKotlinTypes finds class, interface, object, and enum class declarations.
func (p *KotlinParser) extractKotlinTypes(pf *ParsedFile, line string, lineNo int) {
	// Check enum class first (before generic class match)
	if matches := reKotlinEnumClass.FindStringSubmatch(line); matches != nil {
		name := matches[1]
		exported := isKotlinExported(line)
		pf.Symbols = append(pf.Symbols, Symbol{
			Name:     name,
			Kind:     "enum",
			Exported: exported,
			Line:     lineNo,
		})
		if exported {
			pf.Exports = append(pf.Exports, Export{
				Name: name,
				Kind: "enum",
				Line: lineNo,
			})
		}
		return
	}

	// Interface
	if matches := reKotlinInterface.FindStringSubmatch(line); matches != nil {
		name := matches[1]
		exported := isKotlinExported(line)
		pf.Symbols = append(pf.Symbols, Symbol{
			Name:     name,
			Kind:     "interface",
			Exported: exported,
			Line:     lineNo,
		})
		if exported {
			pf.Exports = append(pf.Exports, Export{
				Name: name,
				Kind: "interface",
				Line: lineNo,
			})
		}
		return
	}

	// Object
	if matches := reKotlinObject.FindStringSubmatch(line); matches != nil {
		name := matches[1]
		exported := isKotlinExported(line)
		pf.Symbols = append(pf.Symbols, Symbol{
			Name:     name,
			Kind:     "object",
			Exported: exported,
			Line:     lineNo,
		})
		if exported {
			pf.Exports = append(pf.Exports, Export{
				Name: name,
				Kind: "object",
				Line: lineNo,
			})
		}
		return
	}

	// Class (data class, sealed class, annotation class, etc.)
	if matches := reKotlinClass.FindStringSubmatch(line); matches != nil {
		name := matches[1]
		exported := isKotlinExported(line)

		kind := "class"
		if strings.Contains(line, "data class") {
			kind = "data_class"
		} else if strings.Contains(line, "sealed class") {
			kind = "class"
		} else if strings.Contains(line, "annotation class") {
			kind = "annotation"
		}

		pf.Symbols = append(pf.Symbols, Symbol{
			Name:     name,
			Kind:     kind,
			Exported: exported,
			Line:     lineNo,
		})
		if exported {
			pf.Exports = append(pf.Exports, Export{
				Name: name,
				Kind: kind,
				Line: lineNo,
			})
		}
		return
	}

	// Type alias
	if matches := reKotlinTypeAlias.FindStringSubmatch(line); matches != nil {
		name := matches[1]
		exported := isKotlinExported(line)
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
	}
}

// extractKotlinFunctions finds function declarations.
func (p *KotlinParser) extractKotlinFunctions(pf *ParsedFile, line string, lineNo int) {
	// Skip lines that are type declarations
	if reKotlinClass.MatchString(line) || reKotlinInterface.MatchString(line) ||
		reKotlinObject.MatchString(line) || reKotlinEnumClass.MatchString(line) {
		return
	}

	// Skip lines with type declaration keywords that could confuse the function regex
	trimmed := strings.TrimSpace(line)
	for _, kw := range []string{"class ", "interface ", "object "} {
		if strings.Contains(trimmed, kw) && !strings.Contains(trimmed, "fun ") {
			return
		}
	}

	if matches := reKotlinFunction.FindStringSubmatch(line); matches != nil {
		name := matches[1]

		if isKotlinKeyword(name) {
			return
		}

		exported := isKotlinExported(line)
		pf.Symbols = append(pf.Symbols, Symbol{
			Name:     name,
			Kind:     "function",
			Exported: exported,
			Line:     lineNo,
		})
		if exported {
			pf.Exports = append(pf.Exports, Export{
				Name: name,
				Kind: "function",
				Line: lineNo,
			})
		}
	}
}

// extractKotlinProperties finds val/var declarations.
func (p *KotlinParser) extractKotlinProperties(pf *ParsedFile, line string, lineNo int) {
	// Skip lines that are type or function declarations
	if reKotlinClass.MatchString(line) || reKotlinInterface.MatchString(line) ||
		reKotlinObject.MatchString(line) || reKotlinEnumClass.MatchString(line) ||
		reKotlinFunction.MatchString(line) || reKotlinTypeAlias.MatchString(line) {
		return
	}

	if matches := reKotlinProperty.FindStringSubmatch(line); matches != nil {
		name := matches[1]

		if isKotlinKeyword(name) {
			return
		}

		exported := isKotlinExported(line)
		pf.Symbols = append(pf.Symbols, Symbol{
			Name:     name,
			Kind:     "variable",
			Exported: exported,
			Line:     lineNo,
		})
	}
}

// isKotlinExported checks if a declaration is exported (public or internal).
// In Kotlin, declarations without explicit visibility are public by default.
func isKotlinExported(line string) bool {
	if reKotlinPrivate.MatchString(line) {
		return false
	}
	if reKotlinProtected.MatchString(line) {
		return false
	}
	// public, internal, and no modifier are all "exported"
	return true
}

// isKotlinKeyword returns true if the string is a Kotlin keyword.
func isKotlinKeyword(s string) bool {
	keywords := map[string]bool{
		"if": true, "else": true, "for": true, "while": true, "do": true,
		"when": true, "try": true, "catch": true, "finally": true,
		"throw": true, "return": true, "break": true, "continue": true,
		"is": true, "in": true, "as": true, "this": true, "super": true,
		"class": true, "interface": true, "object": true, "fun": true,
		"val": true, "var": true, "null": true, "true": true, "false": true,
		"package": true, "import": true, "typeof": true, "yield": true,
	}
	return keywords[s]
}
