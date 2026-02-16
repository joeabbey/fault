package parser

import (
	"regexp"
	"strings"
)

// ScalaParser extracts structural information from Scala files via regex.
type ScalaParser struct{}

// NewScalaParser creates a new Scala parser.
func NewScalaParser() *ScalaParser {
	return &ScalaParser{}
}

// Language returns "scala".
func (p *ScalaParser) Language() string {
	return "scala"
}

// Regex patterns for Scala parsing.
var (
	// Package declaration: package com.example.app
	reScalaPackage = regexp.MustCompile(`^\s*package\s+([\w.]+)`)

	// Import statements:
	// import scala.concurrent.Future
	// import scala.collection.mutable.{ArrayBuffer, ListBuffer}
	// import scala.util._
	// import given scala.math.Ordering
	reScalaImport = regexp.MustCompile(`^\s*import\s+(?:given\s+)?([\w]+(?:\.[\w]+)*(?:\.\{[^}]+\}|\.\*|\._)?)`)

	// Class declarations:
	// class Foo
	// sealed class Foo
	// abstract class Foo
	reScalaClass = regexp.MustCompile(`(?:^|\s)(?:sealed\s+|abstract\s+|final\s+)*class\s+(\w+)`)

	// Case class declarations: case class Foo(...)
	reScalaCaseClass = regexp.MustCompile(`(?:^|\s)case\s+class\s+(\w+)`)

	// Object declarations: object Foo
	reScalaObject = regexp.MustCompile(`(?:^|\s)(?:case\s+)?object\s+(\w+)`)

	// Trait declarations: trait Foo, sealed trait Foo
	reScalaTrait = regexp.MustCompile(`(?:^|\s)(?:sealed\s+)?trait\s+(\w+)`)

	// Enum declarations (Scala 3): enum Foo
	reScalaEnum = regexp.MustCompile(`(?:^|\s)enum\s+(\w+)`)

	// Def declarations: def name(...): Type
	reScalaDef = regexp.MustCompile(`^\s*(?:(?:override|private|protected|final|lazy|implicit|inline|transparent)\s+)*def\s+(\w+)`)

	// Val declarations: val name: Type = ...
	reScalaVal = regexp.MustCompile(`^\s*(?:(?:override|private|protected|final|lazy|implicit|inline)\s+)*val\s+(\w+)`)

	// Var declarations: var name: Type = ...
	reScalaVar = regexp.MustCompile(`^\s*(?:(?:override|private|protected|final|lazy)\s+)*var\s+(\w+)`)

	// Type alias: type Name = Type
	reScalaType = regexp.MustCompile(`^\s*(?:(?:override|private|protected|opaque)\s+)*type\s+(\w+)`)

	// Given instance (Scala 3): given name: Type = ...
	reScalaGiven = regexp.MustCompile(`^\s*given\s+(\w+)\s*:`)

	// Visibility detection
	reScalaPrivate   = regexp.MustCompile(`(?:^|\s)private(?:\[\w+\])?\s`)
	reScalaProtected = regexp.MustCompile(`(?:^|\s)protected(?:\[\w+\])?\s`)
)

// Scala keywords to skip when matched as symbol names.
var scalaKeywords = map[string]bool{
	"if": true, "else": true, "for": true, "while": true, "do": true,
	"match": true, "case": true, "try": true, "catch": true, "finally": true,
	"throw": true, "return": true, "yield": true, "class": true, "object": true,
	"trait": true, "extends": true, "with": true, "new": true, "this": true,
	"super": true, "val": true, "var": true, "def": true, "type": true,
	"import": true, "package": true, "null": true, "true": true, "false": true,
	"abstract": true, "sealed": true, "final": true, "implicit": true,
	"lazy": true, "override": true, "private": true, "protected": true,
	"given": true, "using": true, "enum": true, "then": true, "end": true,
}

// Parse extracts structural info from Scala content.
func (p *ScalaParser) Parse(filename string, content []byte) (*ParsedFile, error) {
	pf := &ParsedFile{
		Path:     filename,
		Language: "scala",
		Imports:  make([]Import, 0),
		Exports:  make([]Export, 0),
		Symbols:  make([]Symbol, 0),
	}

	lines := strings.Split(string(content), "\n")
	inBlockComment := false
	braceDepth := 0
	// Track indentation-based nesting for Scala 3 syntax
	// We consider depth 0 as top-level

	for lineNum, line := range lines {
		lineNo := lineNum + 1

		trimmed := strings.TrimSpace(line)

		// Track block comments
		if inBlockComment {
			if strings.Contains(trimmed, "*/") {
				inBlockComment = false
			}
			continue
		}
		if strings.HasPrefix(trimmed, "/*") || strings.HasPrefix(trimmed, "/**") {
			if !strings.Contains(trimmed, "*/") {
				inBlockComment = true
			}
			continue
		}

		// Skip empty lines and single-line comments
		if trimmed == "" || strings.HasPrefix(trimmed, "//") {
			continue
		}

		// Extract imports (at any level)
		p.extractScalaImports(pf, trimmed, lineNo)

		// Only extract top-level declarations
		if braceDepth == 0 {
			p.extractScalaTypes(pf, trimmed, lineNo)
			p.extractScalaMembers(pf, trimmed, lineNo)
		}

		// Track brace depth
		braceDepth += strings.Count(trimmed, "{") - strings.Count(trimmed, "}")
		if braceDepth < 0 {
			braceDepth = 0
		}
	}

	pf.Symbols = deduplicateSymbols(pf.Symbols)

	return pf, nil
}

// extractScalaImports finds import statements.
func (p *ScalaParser) extractScalaImports(pf *ParsedFile, line string, lineNo int) {
	if !strings.HasPrefix(strings.TrimSpace(line), "import") {
		return
	}

	matches := reScalaImport.FindStringSubmatch(line)
	if matches == nil {
		return
	}

	importPath := matches[1]
	imp := Import{
		Path:  importPath,
		Names: make([]string, 0),
		Line:  lineNo,
	}

	// Parse selective imports: import package.{A, B}
	if strings.Contains(importPath, "{") {
		// Extract the base path and the names
		braceIdx := strings.Index(importPath, ".{")
		if braceIdx >= 0 {
			basePath := importPath[:braceIdx]
			namesPart := importPath[braceIdx+2 : len(importPath)-1] // remove .{ and }
			names := strings.Split(namesPart, ",")
			for i, n := range names {
				names[i] = strings.TrimSpace(n)
			}
			imp.Path = importPath
			imp.Names = names
			_ = basePath
		}
	} else if strings.HasSuffix(importPath, "._") || strings.HasSuffix(importPath, ".*") {
		// Wildcard import — no specific names
	} else {
		// Specific import: extract the last component
		parts := strings.Split(importPath, ".")
		name := parts[len(parts)-1]
		imp.Names = append(imp.Names, name)
	}

	pf.Imports = append(pf.Imports, imp)
}

// extractScalaTypes finds class, trait, object, case class, enum declarations.
func (p *ScalaParser) extractScalaTypes(pf *ParsedFile, line string, lineNo int) {
	// Case class (check before generic class)
	if matches := reScalaCaseClass.FindStringSubmatch(line); matches != nil {
		name := matches[1]
		exported := isScalaExported(line)
		pf.Symbols = append(pf.Symbols, Symbol{
			Name:     name,
			Kind:     "case_class",
			Exported: exported,
			Line:     lineNo,
		})
		if exported {
			pf.Exports = append(pf.Exports, Export{
				Name: name,
				Kind: "case_class",
				Line: lineNo,
			})
		}
		return
	}

	// Enum (Scala 3)
	if matches := reScalaEnum.FindStringSubmatch(line); matches != nil {
		name := matches[1]
		exported := isScalaExported(line)
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

	// Trait
	if matches := reScalaTrait.FindStringSubmatch(line); matches != nil {
		name := matches[1]
		exported := isScalaExported(line)
		pf.Symbols = append(pf.Symbols, Symbol{
			Name:     name,
			Kind:     "trait",
			Exported: exported,
			Line:     lineNo,
		})
		if exported {
			pf.Exports = append(pf.Exports, Export{
				Name: name,
				Kind: "trait",
				Line: lineNo,
			})
		}
		return
	}

	// Object (including case object)
	if matches := reScalaObject.FindStringSubmatch(line); matches != nil {
		name := matches[1]
		exported := isScalaExported(line)
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

	// Class (including sealed class, abstract class)
	if matches := reScalaClass.FindStringSubmatch(line); matches != nil {
		name := matches[1]
		exported := isScalaExported(line)
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
		return
	}
}

// extractScalaMembers finds def, val, var, type, and given declarations at top level.
func (p *ScalaParser) extractScalaMembers(pf *ParsedFile, line string, lineNo int) {
	// Skip lines that are type declarations
	if reScalaCaseClass.MatchString(line) || reScalaClass.MatchString(line) ||
		reScalaTrait.MatchString(line) || reScalaObject.MatchString(line) ||
		reScalaEnum.MatchString(line) {
		return
	}

	// Skip import lines
	if strings.HasPrefix(strings.TrimSpace(line), "import") {
		return
	}

	// Skip package lines
	if reScalaPackage.MatchString(line) {
		return
	}

	// Type alias
	if matches := reScalaType.FindStringSubmatch(line); matches != nil {
		name := matches[1]
		if !scalaKeywords[name] {
			exported := isScalaExported(line)
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
	}

	// Given (Scala 3)
	if matches := reScalaGiven.FindStringSubmatch(line); matches != nil {
		name := matches[1]
		if !scalaKeywords[name] {
			exported := isScalaExported(line)
			pf.Symbols = append(pf.Symbols, Symbol{
				Name:     name,
				Kind:     "variable",
				Exported: exported,
				Line:     lineNo,
			})
			return
		}
	}

	// Def
	if matches := reScalaDef.FindStringSubmatch(line); matches != nil {
		name := matches[1]
		if !scalaKeywords[name] {
			exported := isScalaExported(line)
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
			return
		}
	}

	// Val
	if matches := reScalaVal.FindStringSubmatch(line); matches != nil {
		name := matches[1]
		if !scalaKeywords[name] {
			exported := isScalaExported(line)
			pf.Symbols = append(pf.Symbols, Symbol{
				Name:     name,
				Kind:     "variable",
				Exported: exported,
				Line:     lineNo,
			})
			return
		}
	}

	// Var
	if matches := reScalaVar.FindStringSubmatch(line); matches != nil {
		name := matches[1]
		if !scalaKeywords[name] {
			exported := isScalaExported(line)
			pf.Symbols = append(pf.Symbols, Symbol{
				Name:     name,
				Kind:     "variable",
				Exported: exported,
				Line:     lineNo,
			})
			return
		}
	}
}

// isScalaExported checks if a declaration is exported.
// In Scala, declarations without explicit visibility are public by default.
func isScalaExported(line string) bool {
	if reScalaPrivate.MatchString(line) {
		return false
	}
	if reScalaProtected.MatchString(line) {
		return false
	}
	return true
}
