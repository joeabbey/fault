package parser

import (
	"regexp"
	"strings"
)

// PythonParser extracts structural information from Python files via regex.
type PythonParser struct{}

// NewPythonParser creates a new Python parser.
func NewPythonParser() *PythonParser {
	return &PythonParser{}
}

// Language returns "python".
func (p *PythonParser) Language() string {
	return "python"
}

// Regex patterns for Python parsing.
var (
	// import module
	// import module as alias
	// import module1, module2
	reImportModule = regexp.MustCompile(`(?m)^import\s+(.+)`)

	// from module import name1, name2
	// from module import (name1, name2)
	// from .module import name
	// from ..module import name
	// from . import utils
	reFromImport = regexp.MustCompile(`(?m)^from\s+(\.{1,3}[\w.]*|\w[\w.]*)\s+import\s+(.+)`)

	// from module import *
	reFromImportStar = regexp.MustCompile(`(?m)^from\s+(\.{1,3}[\w.]*|\w[\w.]*)\s+import\s+\*`)

	// Function definitions
	// def function_name(args):
	// async def function_name(args):
	reFuncDef = regexp.MustCompile(`(?m)^(\s*)(?:async\s+)?def\s+(\w+)\s*\(([^)]*)\)`)

	// Class definitions
	// class ClassName:
	// class ClassName(BaseClass):
	reClassDef = regexp.MustCompile(`(?m)^(\s*)class\s+(\w+)(?:\s*\([^)]*\))?\s*:`)

	// __all__ = ['name1', 'name2']
	// __all__ = ["name1", "name2"]
	reAllDecl = regexp.MustCompile(`(?m)^__all__\s*=\s*\[([^\]]+)\]`)

	// Variable assignment at module level (no indentation)
	reModuleVar = regexp.MustCompile(`(?m)^([A-Z_][A-Z0-9_]*)\s*[=:]`)

	// Decorator
	reDecorator = regexp.MustCompile(`(?m)^(\s*)@(\w+)`)
)

// Parse extracts structural info from Python content.
func (p *PythonParser) Parse(filename string, content []byte) (*ParsedFile, error) {
	pf := &ParsedFile{
		Path:     filename,
		Language: "python",
		Imports:  make([]Import, 0),
		Exports:  make([]Export, 0),
		Symbols:  make([]Symbol, 0),
	}

	lines := strings.Split(string(content), "\n")
	allNames := make(map[string]bool)

	// First pass: look for __all__ to determine explicit exports
	for _, line := range lines {
		trimmed := strings.TrimSpace(line)
		if matches := reAllDecl.FindStringSubmatch(trimmed); matches != nil {
			allNames = parseAllDecl(matches[1])
		}
	}

	// Second pass: extract everything
	for lineNum, line := range lines {
		lineNo := lineNum + 1

		// Skip comments and empty lines
		trimmed := strings.TrimSpace(line)
		if trimmed == "" || strings.HasPrefix(trimmed, "#") {
			continue
		}

		// Skip decorators (the decorated function/class will be found)
		if reDecorator.MatchString(line) {
			continue
		}

		// Extract imports
		p.extractPythonImports(pf, line, trimmed, lineNo)

		// Extract function definitions
		if matches := reFuncDef.FindStringSubmatch(line); matches != nil {
			indent := matches[1]
			name := matches[2]
			params := matches[3]

			kind := "function"
			if len(indent) > 0 {
				kind = "method"
			}

			exported := isPublicPython(name)
			// If __all__ is defined, only those names are exported
			if len(allNames) > 0 && kind == "function" {
				exported = allNames[name]
			}

			sym := Symbol{
				Name:      name,
				Kind:      kind,
				Exported:  exported,
				Line:      lineNo,
				Signature: "def " + name + "(" + strings.TrimSpace(params) + ")",
			}
			pf.Symbols = append(pf.Symbols, sym)

			if exported && kind == "function" {
				pf.Exports = append(pf.Exports, Export{
					Name: name,
					Kind: "function",
					Line: lineNo,
				})
			}
			continue
		}

		// Extract class definitions
		if matches := reClassDef.FindStringSubmatch(line); matches != nil {
			indent := matches[1]
			name := matches[2]

			if len(indent) > 0 {
				// Nested class — treat as a symbol but not top-level export
				pf.Symbols = append(pf.Symbols, Symbol{
					Name:     name,
					Kind:     "class",
					Exported: false,
					Line:     lineNo,
				})
				continue
			}

			exported := isPublicPython(name)
			if len(allNames) > 0 {
				exported = allNames[name]
			}

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
			continue
		}

		// Module-level constants (UPPER_CASE)
		if matches := reModuleVar.FindStringSubmatch(trimmed); matches != nil {
			// Only if at module level (no indentation)
			if line == trimmed || !strings.HasPrefix(line, " ") && !strings.HasPrefix(line, "\t") {
				name := matches[1]
				pf.Symbols = append(pf.Symbols, Symbol{
					Name:     name,
					Kind:     "variable",
					Exported: isPublicPython(name),
					Line:     lineNo,
				})
			}
		}
	}

	return pf, nil
}

// extractPythonImports handles Python import statements.
func (p *PythonParser) extractPythonImports(pf *ParsedFile, line, trimmed string, lineNo int) {
	// from module import *
	if matches := reFromImportStar.FindStringSubmatch(trimmed); matches != nil {
		pf.Imports = append(pf.Imports, Import{
			Path:  matches[1],
			Names: []string{"*"},
			Line:  lineNo,
		})
		return
	}

	// from module import name1, name2
	if matches := reFromImport.FindStringSubmatch(trimmed); matches != nil {
		module := matches[1]
		namesStr := matches[2]
		// Remove parentheses if present
		namesStr = strings.Trim(namesStr, "()")
		names := parsePythonImportNames(namesStr)

		pf.Imports = append(pf.Imports, Import{
			Path:  module,
			Names: names,
			Line:  lineNo,
		})
		return
	}

	// import module, import module as alias
	if matches := reImportModule.FindStringSubmatch(trimmed); matches != nil {
		// Don't match "from ... import ..." (already handled)
		if strings.HasPrefix(trimmed, "from ") {
			return
		}
		modules := parseModuleList(matches[1])
		for _, mod := range modules {
			pf.Imports = append(pf.Imports, Import{
				Path:  mod.path,
				Names: mod.names,
				Line:  lineNo,
			})
		}
	}
}

type moduleRef struct {
	path  string
	names []string
}

// parseModuleList parses "module1, module2 as m2" into module references.
func parseModuleList(s string) []moduleRef {
	parts := strings.Split(s, ",")
	refs := make([]moduleRef, 0, len(parts))
	for _, part := range parts {
		part = strings.TrimSpace(part)
		if part == "" {
			continue
		}
		ref := moduleRef{
			names: make([]string, 0),
		}
		if idx := strings.Index(part, " as "); idx != -1 {
			ref.path = strings.TrimSpace(part[:idx])
			ref.names = []string{strings.TrimSpace(part[idx+4:])}
		} else {
			ref.path = part
		}
		refs = append(refs, ref)
	}
	return refs
}

// parsePythonImportNames parses "name1, name2 as n2, name3".
func parsePythonImportNames(s string) []string {
	parts := strings.Split(s, ",")
	names := make([]string, 0, len(parts))
	for _, part := range parts {
		part = strings.TrimSpace(part)
		if part == "" {
			continue
		}
		// Handle "name as alias" — use the local alias
		if idx := strings.Index(part, " as "); idx != -1 {
			names = append(names, strings.TrimSpace(part[idx+4:]))
		} else {
			names = append(names, part)
		}
	}
	return names
}

// parseAllDecl parses the content of __all__ = [...].
func parseAllDecl(s string) map[string]bool {
	names := make(map[string]bool)
	// Remove quotes and split
	s = strings.ReplaceAll(s, "'", "")
	s = strings.ReplaceAll(s, "\"", "")
	for _, part := range strings.Split(s, ",") {
		name := strings.TrimSpace(part)
		if name != "" {
			names[name] = true
		}
	}
	return names
}

// isPublicPython returns true if the name doesn't start with underscore.
func isPublicPython(name string) bool {
	return !strings.HasPrefix(name, "_")
}
