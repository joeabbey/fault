package parser

import (
	"regexp"
	"strings"
)

// TypeScriptParser extracts structural information from TS/JS files via regex.
type TypeScriptParser struct{}

// NewTypeScriptParser creates a new TypeScript/JavaScript parser.
func NewTypeScriptParser() *TypeScriptParser {
	return &TypeScriptParser{}
}

// Language returns "typescript".
func (p *TypeScriptParser) Language() string {
	return "typescript"
}

// Regex patterns for TypeScript/JavaScript parsing.
var (
	// ES module imports
	// import { Foo, Bar } from 'module'
	// import { Foo as F } from 'module'
	// import Foo from 'module'
	// import * as Foo from 'module'
	// import 'module'
	reImportFrom = regexp.MustCompile(
		`(?m)^import\s+(?:type\s+)?` +
			`(?:` +
			`(\{[^}]+\})` + // named imports { Foo, Bar }
			`|(\*\s+as\s+\w+)` + // namespace import * as Foo
			`|(\w+)` + // default import
			`)?\s*` +
			`(?:,\s*(?:` +
			`(\{[^}]+\})` + // additional named imports after default
			`|(\*\s+as\s+\w+)` + // additional namespace after default
			`))?\s*` +
			`(?:from\s+)?` +
			`['"]([^'"]+)['"]`)

	// Side-effect import: import 'module'
	reImportSideEffect = regexp.MustCompile(`(?m)^import\s+['"]([^'"]+)['"]`)

	// Type-only import: import type { Foo } from 'module'
	reImportType = regexp.MustCompile(`(?m)^import\s+type\s+`)

	// CommonJS require: const Foo = require('module')
	reRequire = regexp.MustCompile(`(?m)(?:const|let|var)\s+(\w+|\{[^}]+\})\s*=\s*require\s*\(\s*['"]([^'"]+)['"]\s*\)`)

	// Dynamic import: import('module')  — tracked as import but no names
	reDynamicImport = regexp.MustCompile(`(?m)import\s*\(\s*['"]([^'"]+)['"]\s*\)`)

	// Export patterns
	reExportFunction = regexp.MustCompile(`(?m)^export\s+(?:default\s+)?(?:async\s+)?function\s+(\w+)`)
	reExportClass    = regexp.MustCompile(`(?m)^export\s+(?:default\s+)?class\s+(\w+)`)
	reExportConst    = regexp.MustCompile(`(?m)^export\s+(?:default\s+)?(?:const|let|var)\s+(\w+)`)
	reExportType     = regexp.MustCompile(`(?m)^export\s+(?:type|interface|(?:const\s+)?enum)\s+(\w+)`)
	reExportDefault  = regexp.MustCompile(`(?m)^export\s+default\s+`)

	// Re-exports: export { Foo, Bar } from 'module'
	reReExport = regexp.MustCompile(`(?m)^export\s+\{([^}]+)\}\s+from\s+['"]([^'"]+)['"]`)
	// export * from 'module'
	reReExportAll = regexp.MustCompile(`(?m)^export\s+\*\s+from\s+['"]([^'"]+)['"]`)

	// Symbol patterns (declarations)
	reFuncDecl     = regexp.MustCompile(`(?m)^(?:export\s+)?(?:default\s+)?(?:async\s+)?function\s+(\w+)\s*\(([^)]*)\)`)
	reClassDecl    = regexp.MustCompile(`(?m)^(?:export\s+)?(?:default\s+)?class\s+(\w+)`)
	reConstDecl    = regexp.MustCompile(`(?m)^(?:export\s+)?(?:const|let|var)\s+(\w+)`)
	reTypeDecl     = regexp.MustCompile(`(?m)^(?:export\s+)?(?:type|interface)\s+(\w+)`)
	reEnumDecl     = regexp.MustCompile(`(?m)^(?:export\s+)?(?:const\s+)?enum\s+(\w+)`)
	reArrowFunc    = regexp.MustCompile(`(?m)^(?:export\s+)?const\s+(\w+)\s*=\s*(?:async\s+)?(?:\([^)]*\)|[a-zA-Z_]\w*)\s*=>`)
	reMethodInline = regexp.MustCompile(`(?m)^(?:export\s+)?const\s+(\w+)\s*=\s*(?:async\s+)?function`)

	// Decorator: @Name or @Name(args)
	reTSDecorator = regexp.MustCompile(`^@(\w+)`)

	// Generic interface: interface Foo<T> { ... }
	reGenericInterfaceDecl = regexp.MustCompile(`(?m)^(?:export\s+)?interface\s+(\w+)\s*<[^>]+>`)

	// Type-only re-export: export type { Foo } from './bar'
	reTypeReExport = regexp.MustCompile(`(?m)^export\s+type\s+\{([^}]+)\}\s+from\s+['"]([^'"]+)['"]`)
)

// Parse extracts structural info from TypeScript/JavaScript content.
func (p *TypeScriptParser) Parse(filename string, content []byte) (*ParsedFile, error) {
	pf := &ParsedFile{
		Path:     filename,
		Language: "typescript",
		Imports:  make([]Import, 0),
		Exports:  make([]Export, 0),
		Symbols:  make([]Symbol, 0),
	}

	lines := strings.Split(string(content), "\n")

	// Process line by line for accurate line numbers
	for lineNum, line := range lines {
		lineNo := lineNum + 1 // 1-indexed
		trimmed := strings.TrimSpace(line)

		// Skip comments and empty lines
		if trimmed == "" || strings.HasPrefix(trimmed, "//") || strings.HasPrefix(trimmed, "/*") || strings.HasPrefix(trimmed, "*") {
			continue
		}

		// Extract imports
		p.extractImports(pf, trimmed, lineNo)

		// Extract exports
		p.extractExports(pf, trimmed, lineNo)

		// Extract symbols
		p.extractSymbols(pf, trimmed, lineNo)
	}

	// Deduplicate symbols (exports + declarations can overlap)
	pf.Symbols = deduplicateSymbols(pf.Symbols)

	return pf, nil
}

// extractImports finds import statements on a line.
func (p *TypeScriptParser) extractImports(pf *ParsedFile, line string, lineNo int) {
	// ES module imports
	if matches := reImportFrom.FindStringSubmatch(line); matches != nil {
		imp := Import{
			Path:   matches[6],
			Names:  make([]string, 0),
			IsType: reImportType.MatchString(line),
			Line:   lineNo,
		}

		// Named imports: { Foo, Bar }
		if matches[1] != "" {
			imp.Names = parseNamedImports(matches[1])
		}
		// Namespace: * as Foo
		if matches[2] != "" {
			name := strings.TrimSpace(strings.TrimPrefix(matches[2], "* as "))
			imp.Names = append(imp.Names, name)
		}
		// Default import
		if matches[3] != "" {
			imp.Names = append(imp.Names, matches[3])
		}
		// Additional named imports after default
		if matches[4] != "" {
			imp.Names = append(imp.Names, parseNamedImports(matches[4])...)
		}

		pf.Imports = append(pf.Imports, imp)
		return
	}

	// Side-effect imports: import 'module'
	if matches := reImportSideEffect.FindStringSubmatch(line); matches != nil {
		// Only if it's truly a side-effect import (no other import matched)
		if !strings.Contains(line, "from") && !strings.Contains(line, "{") {
			pf.Imports = append(pf.Imports, Import{
				Path:  matches[1],
				Names: make([]string, 0),
				Line:  lineNo,
			})
		}
		return
	}

	// CommonJS require
	if matches := reRequire.FindStringSubmatch(line); matches != nil {
		imp := Import{
			Path:  matches[2],
			Names: make([]string, 0),
			Line:  lineNo,
		}
		name := strings.TrimSpace(matches[1])
		if strings.HasPrefix(name, "{") {
			imp.Names = parseNamedImports(name)
		} else {
			imp.Names = append(imp.Names, name)
		}
		pf.Imports = append(pf.Imports, imp)
	}
}

// extractExports finds export statements on a line.
func (p *TypeScriptParser) extractExports(pf *ParsedFile, line string, lineNo int) {
	if !strings.HasPrefix(line, "export") {
		return
	}

	// Type-only re-exports: export type { Foo } from './bar'
	if matches := reTypeReExport.FindStringSubmatch(line); matches != nil {
		names := parseNamedImports("{" + matches[1] + "}")
		for _, name := range names {
			pf.Exports = append(pf.Exports, Export{
				Name: name,
				Kind: "type",
				Line: lineNo,
			})
		}
		return
	}

	// Re-exports
	if matches := reReExport.FindStringSubmatch(line); matches != nil {
		names := parseNamedImports("{" + matches[1] + "}")
		for _, name := range names {
			pf.Exports = append(pf.Exports, Export{
				Name: name,
				Kind: "re-export",
				Line: lineNo,
			})
		}
		return
	}
	if reReExportAll.MatchString(line) {
		pf.Exports = append(pf.Exports, Export{
			Name: "*",
			Kind: "re-export",
			Line: lineNo,
		})
		return
	}

	// Functions
	if matches := reExportFunction.FindStringSubmatch(line); matches != nil {
		pf.Exports = append(pf.Exports, Export{
			Name: matches[1],
			Kind: "function",
			Line: lineNo,
		})
		return
	}

	// Classes
	if matches := reExportClass.FindStringSubmatch(line); matches != nil {
		pf.Exports = append(pf.Exports, Export{
			Name: matches[1],
			Kind: "class",
			Line: lineNo,
		})
		return
	}

	// Generic interfaces: export interface Foo<T> { ... }
	if matches := reGenericInterfaceDecl.FindStringSubmatch(line); matches != nil {
		pf.Exports = append(pf.Exports, Export{
			Name: matches[1],
			Kind: "type",
			Line: lineNo,
		})
		return
	}

	// Types / interfaces / enums
	if matches := reExportType.FindStringSubmatch(line); matches != nil {
		pf.Exports = append(pf.Exports, Export{
			Name: matches[1],
			Kind: "type",
			Line: lineNo,
		})
		return
	}

	// Arrow functions and function expressions assigned to const
	if matches := reArrowFunc.FindStringSubmatch(line); matches != nil {
		pf.Exports = append(pf.Exports, Export{
			Name: matches[1],
			Kind: "function",
			Line: lineNo,
		})
		return
	}

	// Const/let/var
	if matches := reExportConst.FindStringSubmatch(line); matches != nil {
		pf.Exports = append(pf.Exports, Export{
			Name: matches[1],
			Kind: "variable",
			Line: lineNo,
		})
		return
	}

	// Default export (anonymous)
	if reExportDefault.MatchString(line) && !reExportFunction.MatchString(line) && !reExportClass.MatchString(line) {
		pf.Exports = append(pf.Exports, Export{
			Name: "default",
			Kind: "default",
			Line: lineNo,
		})
	}
}

// extractSymbols finds symbol declarations on a line.
func (p *TypeScriptParser) extractSymbols(pf *ParsedFile, line string, lineNo int) {
	// Track decorators as metadata on the next symbol.
	if matches := reTSDecorator.FindStringSubmatch(line); matches != nil {
		pf.Symbols = append(pf.Symbols, Symbol{
			Name: matches[1],
			Kind: "decorator",
			Line: lineNo,
		})
		return
	}

	exported := strings.HasPrefix(line, "export")

	// Generic interfaces: interface Foo<T> { ... }
	if matches := reGenericInterfaceDecl.FindStringSubmatch(line); matches != nil {
		pf.Symbols = append(pf.Symbols, Symbol{
			Name:     matches[1],
			Kind:     "type",
			Exported: exported,
			Line:     lineNo,
		})
		return
	}

	// Functions
	if matches := reFuncDecl.FindStringSubmatch(line); matches != nil {
		pf.Symbols = append(pf.Symbols, Symbol{
			Name:      matches[1],
			Kind:      "function",
			Exported:  exported,
			Line:      lineNo,
			Signature: "function " + matches[1] + "(" + matches[2] + ")",
		})
		return
	}

	// Classes
	if matches := reClassDecl.FindStringSubmatch(line); matches != nil {
		pf.Symbols = append(pf.Symbols, Symbol{
			Name:     matches[1],
			Kind:     "class",
			Exported: exported,
			Line:     lineNo,
		})
		return
	}

	// Enums
	if matches := reEnumDecl.FindStringSubmatch(line); matches != nil {
		pf.Symbols = append(pf.Symbols, Symbol{
			Name:     matches[1],
			Kind:     "type",
			Exported: exported,
			Line:     lineNo,
		})
		return
	}

	// Types and interfaces
	if matches := reTypeDecl.FindStringSubmatch(line); matches != nil {
		pf.Symbols = append(pf.Symbols, Symbol{
			Name:     matches[1],
			Kind:     "type",
			Exported: exported,
			Line:     lineNo,
		})
		return
	}

	// Arrow functions
	if matches := reArrowFunc.FindStringSubmatch(line); matches != nil {
		pf.Symbols = append(pf.Symbols, Symbol{
			Name:     matches[1],
			Kind:     "function",
			Exported: exported,
			Line:     lineNo,
		})
		return
	}

	// Function expressions: const foo = function
	if matches := reMethodInline.FindStringSubmatch(line); matches != nil {
		pf.Symbols = append(pf.Symbols, Symbol{
			Name:     matches[1],
			Kind:     "function",
			Exported: exported,
			Line:     lineNo,
		})
		return
	}

	// Const/let/var (only at top level — simple heuristic: line starts with keyword)
	if matches := reConstDecl.FindStringSubmatch(line); matches != nil {
		pf.Symbols = append(pf.Symbols, Symbol{
			Name:     matches[1],
			Kind:     "variable",
			Exported: exported,
			Line:     lineNo,
		})
		return
	}
}

// parseNamedImports parses "{ Foo, Bar as B, Baz }" into a slice of names.
func parseNamedImports(s string) []string {
	s = strings.Trim(s, "{ }")
	parts := strings.Split(s, ",")
	names := make([]string, 0, len(parts))
	for _, part := range parts {
		part = strings.TrimSpace(part)
		if part == "" {
			continue
		}
		// Handle "Foo as Bar" — use the local name (Bar)
		if idx := strings.Index(part, " as "); idx != -1 {
			names = append(names, strings.TrimSpace(part[idx+4:]))
		} else {
			names = append(names, part)
		}
	}
	return names
}

// deduplicateSymbols removes duplicate symbols (same name and line).
func deduplicateSymbols(symbols []Symbol) []Symbol {
	seen := make(map[string]bool)
	result := make([]Symbol, 0, len(symbols))
	for _, s := range symbols {
		key := s.Name + ":" + s.Kind
		if !seen[key] {
			seen[key] = true
			result = append(result, s)
		}
	}
	return result
}
