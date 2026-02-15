package parser

import (
	"regexp"
	"strings"
)

// JavaParser extracts structural information from Java files via regex.
type JavaParser struct{}

// NewJavaParser creates a new Java parser.
func NewJavaParser() *JavaParser {
	return &JavaParser{}
}

// Language returns "java".
func (p *JavaParser) Language() string {
	return "java"
}

// Regex patterns for Java parsing.
var (
	// Package declaration: package com.example.foo;
	reJavaPackage = regexp.MustCompile(`^\s*package\s+([\w.]+)\s*;`)

	// Import statements:
	// import java.util.List;
	// import java.util.*;
	// import static org.junit.Assert.*;
	// import static org.junit.Assert.assertEquals;
	reJavaImport = regexp.MustCompile(`^\s*import\s+(static\s+)?([\w.]+(?:\.\*)?)\s*;`)

	// Class/interface/enum/record declarations with optional modifiers:
	// public class Foo { ... }
	// public abstract class Foo extends Bar implements Baz { ... }
	// public interface Foo<T> { ... }
	// public enum Color { ... }
	// public record Point(int x, int y) { ... }
	reJavaClass     = regexp.MustCompile(`(?:^|\s)(public|protected|private)?\s*(?:abstract\s+|final\s+|static\s+)*class\s+(\w+)`)
	reJavaInterface = regexp.MustCompile(`(?:^|\s)(public|protected|private)?\s*(?:abstract\s+|static\s+)*interface\s+(\w+)`)
	reJavaEnum      = regexp.MustCompile(`(?:^|\s)(public|protected|private)?\s*(?:static\s+)*enum\s+(\w+)`)
	reJavaRecord    = regexp.MustCompile(`(?:^|\s)(public|protected|private)?\s*(?:static\s+)*record\s+(\w+)`)

	// Method declarations:
	// public void doSomething(String arg) {
	// private static int calculate(int a, int b) {
	// protected List<String> getItems() throws Exception {
	// @Override public String toString() {
	reJavaMethod = regexp.MustCompile(`^\s*(public|protected|private)?\s*(?:static\s+)?(?:final\s+)?(?:synchronized\s+)?(?:abstract\s+)?(?:<[\w,\s?]+>\s+)?(\w[\w<>\[\],\s?]*?)\s+(\w+)\s*\(([^)]*)\)`)

	// Constructor declarations:
	// public ClassName(args) {
	reJavaConstructor = regexp.MustCompile(`^\s*(public|protected|private)?\s+(\w+)\s*\(([^)]*)\)\s*(?:throws\s+[\w,\s]+)?\s*\{`)

	// Field declarations:
	// private String name;
	// public static final int MAX = 100;
	// private final List<String> items = new ArrayList<>();
	reJavaField = regexp.MustCompile(`^\s*(public|protected|private)\s+(?:static\s+)?(?:final\s+)?(?:volatile\s+)?(?:transient\s+)?(\w[\w<>\[\],\s?]*?)\s+(\w+)\s*[;=]`)

	// Annotation
	reJavaAnnotation = regexp.MustCompile(`^\s*@\w+`)
)

// Parse extracts structural info from Java content.
func (p *JavaParser) Parse(filename string, content []byte) (*ParsedFile, error) {
	pf := &ParsedFile{
		Path:     filename,
		Language: "java",
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

		// Skip empty lines, single-line comments, and annotations (handled via symbols)
		if trimmed == "" || strings.HasPrefix(trimmed, "//") {
			continue
		}

		// Skip annotation-only lines (the declaration follows on next line)
		if reJavaAnnotation.MatchString(trimmed) && !strings.Contains(trimmed, "class ") && !strings.Contains(trimmed, "interface ") {
			continue
		}

		// Extract imports
		p.extractJavaImports(pf, trimmed, lineNo)

		// Extract class/interface/enum/record declarations
		p.extractJavaTypes(pf, trimmed, lineNo)

		// Extract methods
		p.extractJavaMethods(pf, trimmed, lineNo)

		// Extract fields
		p.extractJavaFields(pf, trimmed, lineNo)
	}

	// Deduplicate symbols
	pf.Symbols = deduplicateSymbols(pf.Symbols)

	return pf, nil
}

// extractJavaImports finds import statements on a line.
func (p *JavaParser) extractJavaImports(pf *ParsedFile, line string, lineNo int) {
	matches := reJavaImport.FindStringSubmatch(line)
	if matches == nil {
		return
	}

	isStatic := strings.TrimSpace(matches[1]) != ""
	importPath := matches[2]

	imp := Import{
		Path:  importPath,
		Names: make([]string, 0),
		Line:  lineNo,
	}

	// Extract the class name from the import path
	if strings.HasSuffix(importPath, ".*") {
		// Wildcard import: java.util.* -> names stays empty, path is java.util.*
	} else {
		// Specific class: java.util.List -> name is "List"
		parts := strings.Split(importPath, ".")
		name := parts[len(parts)-1]
		imp.Names = append(imp.Names, name)
	}

	// Mark static imports as type imports (closest semantic equivalent)
	if isStatic {
		imp.IsType = true
	}

	pf.Imports = append(pf.Imports, imp)
}

// extractJavaTypes finds class, interface, enum, and record declarations.
func (p *JavaParser) extractJavaTypes(pf *ParsedFile, line string, lineNo int) {
	type typeMatch struct {
		re   *regexp.Regexp
		kind string
	}
	typeMatches := []typeMatch{
		{reJavaClass, "class"},
		{reJavaInterface, "interface"},
		{reJavaEnum, "enum"},
		{reJavaRecord, "record"},
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

// extractJavaMethods finds method and constructor declarations.
func (p *JavaParser) extractJavaMethods(pf *ParsedFile, line string, lineNo int) {
	// Skip lines that are type declarations (already handled)
	if reJavaClass.MatchString(line) || reJavaInterface.MatchString(line) ||
		reJavaEnum.MatchString(line) || reJavaRecord.MatchString(line) {
		return
	}

	// Skip lines that contain type declaration keywords (prevents matching
	// class/interface declarations as constructors)
	trimmed := strings.TrimSpace(line)
	for _, kw := range []string{"class ", "interface ", "enum ", "record "} {
		if strings.Contains(trimmed, kw) {
			return
		}
	}

	// Try constructor first
	if matches := reJavaConstructor.FindStringSubmatch(line); matches != nil {
		access := matches[1]
		name := matches[2]
		params := strings.TrimSpace(matches[3])

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
	if matches := reJavaMethod.FindStringSubmatch(line); matches != nil {
		access := matches[1]
		returnType := strings.TrimSpace(matches[2])
		name := matches[3]
		params := strings.TrimSpace(matches[4])

		// Skip if the return type is a Java keyword that indicates this is not a method
		if isJavaKeyword(returnType) || isJavaKeyword(name) {
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

// extractJavaFields finds field declarations.
func (p *JavaParser) extractJavaFields(pf *ParsedFile, line string, lineNo int) {
	// Skip lines that are type or method declarations
	if reJavaClass.MatchString(line) || reJavaInterface.MatchString(line) ||
		reJavaEnum.MatchString(line) || reJavaRecord.MatchString(line) {
		return
	}
	if reJavaMethod.MatchString(line) || reJavaConstructor.MatchString(line) {
		return
	}

	if matches := reJavaField.FindStringSubmatch(line); matches != nil {
		access := matches[1]
		name := matches[3]

		// Skip if name is a keyword
		if isJavaKeyword(name) {
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

// isJavaKeyword returns true if the string is a Java keyword that could be
// confused with an identifier by our regex patterns.
func isJavaKeyword(s string) bool {
	keywords := map[string]bool{
		"if": true, "else": true, "for": true, "while": true, "do": true,
		"switch": true, "case": true, "default": true, "break": true, "continue": true,
		"return": true, "throw": true, "throws": true, "try": true, "catch": true,
		"finally": true, "new": true, "this": true, "super": true, "class": true,
		"interface": true, "enum": true, "extends": true, "implements": true,
		"import": true, "package": true, "void": true, "null": true, "true": true,
		"false": true, "instanceof": true, "assert": true, "yield": true,
	}
	return keywords[s]
}
