package parser

import (
	"regexp"
	"strings"
)

// DartParser extracts structural information from Dart files via regex.
type DartParser struct{}

// NewDartParser creates a new Dart parser.
func NewDartParser() *DartParser {
	return &DartParser{}
}

// Language returns "dart".
func (p *DartParser) Language() string {
	return "dart"
}

// Regex patterns for Dart parsing.
var (
	// Import statements:
	// import 'dart:async';
	// import 'package:flutter/material.dart';
	// import '../utils/helpers.dart';
	// import 'package:http/http.dart' as http;
	// import 'package:provider/provider.dart' show Provider, ChangeNotifierProvider;
	reDartImport = regexp.MustCompile(`^\s*import\s+'([^']+)'\s*(?:as\s+(\w+))?\s*(?:(?:show|hide)\s+[\w\s,]+)?;`)

	// Export re-exports:
	// export 'package:flutter/widgets.dart';
	// export 'src/models.dart' show User, Profile;
	reDartExportDirective = regexp.MustCompile(`^\s*export\s+'([^']+)'\s*(?:(?:show|hide)\s+[\w\s,]+)?;`)

	// Class declarations:
	// class Foo
	// abstract class Foo
	// class Foo extends Bar
	// class Foo with Mixin
	reDartClass = regexp.MustCompile(`^\s*(?:abstract\s+)?class\s+(\w+)`)

	// Mixin declarations: mixin Foo
	reDartMixin = regexp.MustCompile(`^\s*mixin\s+(\w+)`)

	// Extension declarations: extension Foo on Bar
	reDartExtension = regexp.MustCompile(`^\s*extension\s+(\w+)\s+on\s+`)

	// Enum declarations: enum Foo
	reDartEnum = regexp.MustCompile(`^\s*enum\s+(\w+)`)

	// Typedef declarations:
	// typedef JsonMap = Map<String, dynamic>;
	// typedef OnPressed = void Function();
	reDartTypedef = regexp.MustCompile(`^\s*typedef\s+(\w+)`)

	// Top-level function declarations:
	// void doSomething() {
	// Future<void> fetchData(String url) async {
	// String getName() => 'name';
	// T parse<T>(String data) {
	reDartFunction = regexp.MustCompile(`^\s*(?:(?:static|Future|Stream|void|int|double|String|bool|dynamic|List|Map|Set|Iterable|FutureOr)\S*\s+|(\w+(?:<[^>]*>)?)\s+)(\w+)\s*(?:<[^>]*>)?\s*\(`)

	// Simpler top-level function: word followed by word followed by (
	reDartSimpleFunction = regexp.MustCompile(`^(\w+(?:<[^>]*>)?)\s+(\w+)\s*\(`)

	// Top-level variable declarations:
	// const String appName = 'MyApp';
	// final double defaultPadding = 16.0;
	// var int requestCount = 0;
	reDartTopLevelVar = regexp.MustCompile(`^\s*(?:const|final|var|late)\s+`)

	// Annotation line
	reDartAnnotation = regexp.MustCompile(`^\s*@\w+`)
)

// Dart keywords to skip when matched as function names.
var dartKeywords = map[string]bool{
	"if": true, "else": true, "for": true, "while": true, "do": true,
	"switch": true, "case": true, "default": true, "try": true, "catch": true,
	"finally": true, "throw": true, "return": true, "break": true, "continue": true,
	"new": true, "const": true, "final": true, "var": true, "void": true,
	"class": true, "extends": true, "implements": true, "with": true, "mixin": true,
	"enum": true, "typedef": true, "import": true, "export": true, "library": true,
	"part": true, "abstract": true, "static": true, "super": true, "this": true,
	"null": true, "true": true, "false": true, "is": true, "as": true, "in": true,
	"assert": true, "async": true, "await": true, "yield": true, "late": true,
	"required": true, "extension": true, "on": true,
}

// Parse extracts structural info from Dart content.
func (p *DartParser) Parse(filename string, content []byte) (*ParsedFile, error) {
	pf := &ParsedFile{
		Path:     filename,
		Language: "dart",
		Imports:  make([]Import, 0),
		Exports:  make([]Export, 0),
		Symbols:  make([]Symbol, 0),
	}

	lines := strings.Split(string(content), "\n")
	inBlockComment := false
	braceDepth := 0

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
		if trimmed == "" || strings.HasPrefix(trimmed, "//") || strings.HasPrefix(trimmed, "///") {
			continue
		}

		// Track brace depth for top-level detection
		// Skip annotation-only lines
		if reDartAnnotation.MatchString(trimmed) && !strings.Contains(trimmed, "class ") &&
			!strings.Contains(trimmed, "enum ") && !strings.Contains(trimmed, "mixin ") {
			continue
		}

		// Extract imports
		p.extractDartImports(pf, trimmed, lineNo)

		// Extract export directives (re-exports)
		p.extractDartExportDirectives(pf, trimmed, lineNo)

		// Only extract declarations at top level (braceDepth == 0)
		if braceDepth == 0 {
			p.extractDartTypes(pf, trimmed, lineNo)
			p.extractDartFunctions(pf, trimmed, lineNo)
			p.extractDartVariables(pf, trimmed, lineNo)
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

// extractDartImports finds import statements.
func (p *DartParser) extractDartImports(pf *ParsedFile, line string, lineNo int) {
	matches := reDartImport.FindStringSubmatch(line)
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

	if alias != "" {
		imp.Names = append(imp.Names, alias)
	}

	pf.Imports = append(pf.Imports, imp)
}

// extractDartExportDirectives finds export re-export directives.
func (p *DartParser) extractDartExportDirectives(pf *ParsedFile, line string, lineNo int) {
	matches := reDartExportDirective.FindStringSubmatch(line)
	if matches == nil {
		return
	}

	// Re-exports are treated as imports with a special note
	importPath := matches[1]
	pf.Imports = append(pf.Imports, Import{
		Path:  importPath,
		Names: make([]string, 0),
		Line:  lineNo,
	})
}

// extractDartTypes finds class, mixin, extension, enum, and typedef declarations.
func (p *DartParser) extractDartTypes(pf *ParsedFile, line string, lineNo int) {
	// Typedef
	if matches := reDartTypedef.FindStringSubmatch(line); matches != nil {
		name := matches[1]
		exported := !strings.HasPrefix(name, "_")
		pf.Symbols = append(pf.Symbols, Symbol{
			Name:     name,
			Kind:     "typedef",
			Exported: exported,
			Line:     lineNo,
		})
		if exported {
			pf.Exports = append(pf.Exports, Export{
				Name: name,
				Kind: "typedef",
				Line: lineNo,
			})
		}
		return
	}

	// Enum (check before class since enums are simpler)
	if matches := reDartEnum.FindStringSubmatch(line); matches != nil {
		name := matches[1]
		exported := !strings.HasPrefix(name, "_")
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

	// Mixin (check before class)
	if matches := reDartMixin.FindStringSubmatch(line); matches != nil {
		name := matches[1]
		exported := !strings.HasPrefix(name, "_")
		pf.Symbols = append(pf.Symbols, Symbol{
			Name:     name,
			Kind:     "mixin",
			Exported: exported,
			Line:     lineNo,
		})
		if exported {
			pf.Exports = append(pf.Exports, Export{
				Name: name,
				Kind: "mixin",
				Line: lineNo,
			})
		}
		return
	}

	// Extension
	if matches := reDartExtension.FindStringSubmatch(line); matches != nil {
		name := matches[1]
		exported := !strings.HasPrefix(name, "_")
		pf.Symbols = append(pf.Symbols, Symbol{
			Name:     name,
			Kind:     "extension",
			Exported: exported,
			Line:     lineNo,
		})
		if exported {
			pf.Exports = append(pf.Exports, Export{
				Name: name,
				Kind: "extension",
				Line: lineNo,
			})
		}
		return
	}

	// Class (including abstract class)
	if matches := reDartClass.FindStringSubmatch(line); matches != nil {
		name := matches[1]
		exported := !strings.HasPrefix(name, "_")

		kind := "class"
		if strings.Contains(line, "abstract class") {
			kind = "class" // still "class" kind but abstract
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
}

// extractDartFunctions finds top-level function declarations.
func (p *DartParser) extractDartFunctions(pf *ParsedFile, line string, lineNo int) {
	// Skip lines that are type declarations
	if reDartClass.MatchString(line) || reDartEnum.MatchString(line) ||
		reDartMixin.MatchString(line) || reDartExtension.MatchString(line) ||
		reDartTypedef.MatchString(line) {
		return
	}

	// Skip import/export lines
	if reDartImport.MatchString(line) || reDartExportDirective.MatchString(line) {
		return
	}

	// Skip variable declarations
	if reDartTopLevelVar.MatchString(line) {
		return
	}

	matches := reDartSimpleFunction.FindStringSubmatch(line)
	if matches == nil {
		return
	}

	returnType := matches[1]
	name := matches[2]

	// Skip if the "return type" is a keyword that indicates something else
	if dartKeywords[returnType] && returnType != "void" {
		return
	}

	if dartKeywords[name] {
		return
	}

	exported := !strings.HasPrefix(name, "_")
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

// extractDartVariables finds top-level const/final/var declarations.
func (p *DartParser) extractDartVariables(pf *ParsedFile, line string, lineNo int) {
	// Skip lines that are type declarations
	if reDartClass.MatchString(line) || reDartEnum.MatchString(line) ||
		reDartMixin.MatchString(line) || reDartExtension.MatchString(line) ||
		reDartTypedef.MatchString(line) {
		return
	}

	// Skip import/export lines
	if reDartImport.MatchString(line) || reDartExportDirective.MatchString(line) {
		return
	}

	if !reDartTopLevelVar.MatchString(line) {
		return
	}

	// Parse variable name from const/final/var declarations
	// const String appName = 'MyApp';
	// final double defaultPadding = 16.0;
	// var int requestCount = 0;
	trimmed := strings.TrimSpace(line)

	// Remove the leading keyword (const/final/var/late)
	for _, prefix := range []string{"late final ", "late var ", "const ", "final ", "var "} {
		if strings.HasPrefix(trimmed, prefix) {
			trimmed = strings.TrimPrefix(trimmed, prefix)
			break
		}
	}

	// Now trimmed is like "String appName = 'MyApp';" or "appName = 'MyApp';"
	// Try to extract: Type name or just name
	reVarName := regexp.MustCompile(`^(?:\w+(?:<[^>]*>)?\s+)?(\w+)`)
	varMatches := reVarName.FindStringSubmatch(trimmed)
	if varMatches == nil {
		return
	}

	name := varMatches[1]
	if dartKeywords[name] {
		return
	}

	exported := !strings.HasPrefix(name, "_")
	pf.Symbols = append(pf.Symbols, Symbol{
		Name:     name,
		Kind:     "variable",
		Exported: exported,
		Line:     lineNo,
	})
}
