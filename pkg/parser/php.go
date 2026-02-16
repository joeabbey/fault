package parser

import (
	"regexp"
	"strings"
)

// PHPParser extracts structural information from PHP files via regex.
type PHPParser struct{}

// NewPHPParser creates a new PHP parser.
func NewPHPParser() *PHPParser {
	return &PHPParser{}
}

// Language returns "php".
func (p *PHPParser) Language() string {
	return "php"
}

// Regex patterns for PHP parsing.
var (
	// Namespace: namespace App\Controllers;
	rePHPNamespace = regexp.MustCompile(`^\s*namespace\s+([\w\\]+)\s*;`)

	// Use statements:
	// use Vendor\Package\Class;
	// use Vendor\Package\Class as Alias;
	// use function Vendor\Package\func_name;
	// use const Vendor\Package\CONST_NAME;
	rePHPUse = regexp.MustCompile(`^\s*use\s+(function\s+|const\s+)?([\w\\]+)(?:\s+as\s+(\w+))?\s*;`)

	// Grouped use statements:
	// use Vendor\Package\{Foo, Bar, Baz};
	// use Vendor\Package\{Foo as F, Bar};
	rePHPGroupUse = regexp.MustCompile(`^\s*use\s+(function\s+|const\s+)?([\w\\]+)\\\{([^}]+)\}\s*;`)

	// Class declarations:
	// class Foo { ... }
	// abstract class Foo extends Bar implements Baz, Qux { ... }
	// final class Foo { ... }
	rePHPClass = regexp.MustCompile(`(?:^|\s)(?:abstract\s+|final\s+)?(class)\s+(\w+)`)

	// Interface declarations:
	// interface Foo { ... }
	// interface Foo extends OtherInterface { ... }
	rePHPInterface = regexp.MustCompile(`(?:^|\s)(interface)\s+(\w+)`)

	// Trait declarations:
	// trait Foo { ... }
	rePHPTrait = regexp.MustCompile(`(?:^|\s)(trait)\s+(\w+)`)

	// Enum declarations (PHP 8.1+):
	// enum Suit { ... }
	// enum Suit: string { ... }
	rePHPEnum = regexp.MustCompile(`(?:^|\s)(enum)\s+(\w+)`)

	// Method declarations:
	// public function doSomething(Type $param): ReturnType
	// protected static function calculate(int $a, int $b): int
	// public function __construct(private readonly string $name)
	rePHPMethod = regexp.MustCompile(`^\s*(public|protected|private)\s+(?:static\s+)?function\s+(\w+)\s*\(([^)]*)\)(?:\s*:\s*([\w\\?|]+))?`)

	// Standalone function (no visibility modifier, at top level):
	// function helper_func($arg): string
	rePHPFunction = regexp.MustCompile(`^\s*function\s+(\w+)\s*\(([^)]*)\)(?:\s*:\s*([\w\\?|]+))?`)

	// Property declarations:
	// public string $name;
	// protected static readonly int $count = 0;
	// private ?string $value;
	rePHPProperty = regexp.MustCompile(`^\s*(public|protected|private)\s+(?:static\s+)?(?:readonly\s+)?(?:([\w\\?|]+)\s+)?(\$\w+)`)

	// Constant declarations:
	// const MAX = 100;
	// public const STATUS_ACTIVE = 'active';
	rePHPClassConst = regexp.MustCompile(`^\s*(public|protected|private)?\s*const\s+(\w+)\s*=`)
)

// Parse extracts structural info from PHP content.
func (p *PHPParser) Parse(filename string, content []byte) (*ParsedFile, error) {
	pf := &ParsedFile{
		Path:     filename,
		Language: "php",
		Imports:  make([]Import, 0),
		Exports:  make([]Export, 0),
		Symbols:  make([]Symbol, 0),
	}

	lines := strings.Split(string(content), "\n")
	inBlockComment := false

	for lineNum, line := range lines {
		lineNo := lineNum + 1

		trimmed := strings.TrimSpace(line)

		// Track block comments.
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

		// Skip empty lines, single-line comments, PHP tags.
		if trimmed == "" || strings.HasPrefix(trimmed, "//") || strings.HasPrefix(trimmed, "#") {
			continue
		}
		if trimmed == "<?php" || trimmed == "?>" || trimmed == "<?php declare(strict_types=1);" {
			continue
		}
		// Also handle inline opening tag: <?php at start of line with more content
		if strings.HasPrefix(trimmed, "<?php") {
			trimmed = strings.TrimSpace(strings.TrimPrefix(trimmed, "<?php"))
			if trimmed == "" {
				continue
			}
		}

		// Extract namespace
		if matches := rePHPNamespace.FindStringSubmatch(trimmed); matches != nil {
			pf.Symbols = append(pf.Symbols, Symbol{
				Name:     matches[1],
				Kind:     "namespace",
				Exported: true,
				Line:     lineNo,
			})
			continue
		}

		// Extract use statements (imports)
		p.extractPHPImports(pf, trimmed, lineNo)

		// Extract type declarations
		p.extractPHPTypes(pf, trimmed, lineNo)

		// Extract methods and functions
		p.extractPHPMethods(pf, trimmed, lineNo)

		// Extract properties
		p.extractPHPProperties(pf, trimmed, lineNo)

		// Extract constants
		p.extractPHPConstants(pf, trimmed, lineNo)
	}

	pf.Symbols = deduplicateSymbols(pf.Symbols)

	return pf, nil
}

// extractPHPImports handles PHP use statements.
func (p *PHPParser) extractPHPImports(pf *ParsedFile, line string, lineNo int) {
	// Grouped use: use Vendor\Package\{Foo, Bar, Baz};
	if matches := rePHPGroupUse.FindStringSubmatch(line); matches != nil {
		prefix := matches[2]
		namesStr := matches[3]

		names := parsePHPGroupNames(namesStr)
		for _, name := range names {
			fullPath := prefix + `\` + name.path
			imp := Import{
				Path:  fullPath,
				Names: make([]string, 0),
				Line:  lineNo,
			}
			if name.alias != "" {
				imp.Names = append(imp.Names, name.alias)
			} else {
				// Extract the class name (last segment)
				parts := strings.Split(name.path, `\`)
				imp.Names = append(imp.Names, parts[len(parts)-1])
			}
			pf.Imports = append(pf.Imports, imp)
		}
		return
	}

	// Single use: use Vendor\Package\Class;
	if matches := rePHPUse.FindStringSubmatch(line); matches != nil {
		importPath := matches[2]
		alias := matches[3]

		// Skip trait use inside classes (no backslash = local trait name)
		if !strings.Contains(importPath, `\`) {
			return
		}

		imp := Import{
			Path:  importPath,
			Names: make([]string, 0),
			Line:  lineNo,
		}

		if alias != "" {
			imp.Names = append(imp.Names, alias)
		} else {
			// Extract the class name (last segment after \)
			parts := strings.Split(importPath, `\`)
			imp.Names = append(imp.Names, parts[len(parts)-1])
		}

		pf.Imports = append(pf.Imports, imp)
	}
}

type phpGroupName struct {
	path  string
	alias string
}

// parsePHPGroupNames parses "Foo, Bar as B, Baz" from a grouped use statement.
func parsePHPGroupNames(s string) []phpGroupName {
	parts := strings.Split(s, ",")
	names := make([]phpGroupName, 0, len(parts))
	for _, part := range parts {
		part = strings.TrimSpace(part)
		if part == "" {
			continue
		}
		n := phpGroupName{}
		if idx := strings.Index(part, " as "); idx != -1 {
			n.path = strings.TrimSpace(part[:idx])
			n.alias = strings.TrimSpace(part[idx+4:])
		} else {
			n.path = part
		}
		names = append(names, n)
	}
	return names
}

// extractPHPTypes finds class, interface, trait, and enum declarations.
func (p *PHPParser) extractPHPTypes(pf *ParsedFile, line string, lineNo int) {
	type typeMatch struct {
		re   *regexp.Regexp
		kind string
	}
	typeMatches := []typeMatch{
		{rePHPClass, "class"},
		{rePHPInterface, "interface"},
		{rePHPTrait, "trait"},
		{rePHPEnum, "enum"},
	}

	for _, tm := range typeMatches {
		matches := tm.re.FindStringSubmatch(line)
		if matches == nil {
			continue
		}

		name := matches[2]

		// In PHP, all top-level types are effectively exported (public by default).
		pf.Symbols = append(pf.Symbols, Symbol{
			Name:     name,
			Kind:     tm.kind,
			Exported: true,
			Line:     lineNo,
		})

		pf.Exports = append(pf.Exports, Export{
			Name: name,
			Kind: tm.kind,
			Line: lineNo,
		})
		return // Only match one type per line
	}
}

// extractPHPMethods finds method and function declarations.
func (p *PHPParser) extractPHPMethods(pf *ParsedFile, line string, lineNo int) {
	// Skip lines that are type declarations.
	if rePHPClass.MatchString(line) || rePHPInterface.MatchString(line) ||
		rePHPTrait.MatchString(line) || rePHPEnum.MatchString(line) {
		return
	}

	// Try method with visibility modifier first.
	if matches := rePHPMethod.FindStringSubmatch(line); matches != nil {
		access := matches[1]
		name := matches[2]
		params := strings.TrimSpace(matches[3])
		returnType := strings.TrimSpace(matches[4])

		kind := "method"
		if name == "__construct" {
			kind = "constructor"
		}

		sig := "function " + name + "(" + params + ")"
		if returnType != "" {
			sig += ": " + returnType
		}

		pf.Symbols = append(pf.Symbols, Symbol{
			Name:      name,
			Kind:      kind,
			Exported:  access == "public",
			Line:      lineNo,
			Signature: sig,
		})
		return
	}

	// Try standalone function (no visibility modifier).
	if matches := rePHPFunction.FindStringSubmatch(line); matches != nil {
		name := matches[1]
		params := strings.TrimSpace(matches[2])
		returnType := strings.TrimSpace(matches[3])

		sig := "function " + name + "(" + params + ")"
		if returnType != "" {
			sig += ": " + returnType
		}

		pf.Symbols = append(pf.Symbols, Symbol{
			Name:      name,
			Kind:      "function",
			Exported:  true,
			Line:      lineNo,
			Signature: sig,
		})

		pf.Exports = append(pf.Exports, Export{
			Name: name,
			Kind: "function",
			Line: lineNo,
		})
	}
}

// extractPHPProperties finds property declarations.
func (p *PHPParser) extractPHPProperties(pf *ParsedFile, line string, lineNo int) {
	// Skip lines that are type, method, or function declarations.
	if rePHPClass.MatchString(line) || rePHPInterface.MatchString(line) ||
		rePHPTrait.MatchString(line) || rePHPEnum.MatchString(line) {
		return
	}
	if rePHPMethod.MatchString(line) || rePHPFunction.MatchString(line) {
		return
	}

	if matches := rePHPProperty.FindStringSubmatch(line); matches != nil {
		access := matches[1]
		propName := matches[3] // includes $

		pf.Symbols = append(pf.Symbols, Symbol{
			Name:     propName,
			Kind:     "property",
			Exported: access == "public",
			Line:     lineNo,
		})
	}
}

// extractPHPConstants finds constant declarations.
func (p *PHPParser) extractPHPConstants(pf *ParsedFile, line string, lineNo int) {
	// Skip lines that match other declarations.
	if rePHPClass.MatchString(line) || rePHPInterface.MatchString(line) ||
		rePHPTrait.MatchString(line) || rePHPEnum.MatchString(line) {
		return
	}
	if rePHPMethod.MatchString(line) || rePHPFunction.MatchString(line) || rePHPProperty.MatchString(line) {
		return
	}

	if matches := rePHPClassConst.FindStringSubmatch(line); matches != nil {
		access := matches[1]
		name := matches[2]

		// If no access modifier, it's public by default in PHP.
		exported := access == "" || access == "public"

		pf.Symbols = append(pf.Symbols, Symbol{
			Name:     name,
			Kind:     "constant",
			Exported: exported,
			Line:     lineNo,
		})
	}
}
