package parser

import (
	"regexp"
	"strings"
)

// RParser extracts structural information from R files via regex.
type RParser struct{}

// NewRParser creates a new R parser.
func NewRParser() *RParser {
	return &RParser{}
}

// Language returns "r".
func (p *RParser) Language() string {
	return "r"
}

// Regex patterns for R parsing.
var (
	// library(package) or library("package")
	reRLibrary = regexp.MustCompile(`^(?:library|require)\s*\(\s*"?([a-zA-Z][\w.]*)"?\s*\)`)

	// package::function() — namespace-qualified call
	reRNamespace = regexp.MustCompile(`([a-zA-Z][\w.]+)::(\w+)`)

	// name <- function(...) or name = function(...)
	// R allows dots and leading dots in names: .validate_input <- function(x)
	reRFuncAssign = regexp.MustCompile(`^(\s*)(\.?\w[\w.]*)\s*(?:<-|=)\s*function\s*\(([^)]*)\)`)

	// setClass("ClassName", ...)
	reRSetClass = regexp.MustCompile(`^setClass\s*\(\s*"([^"]+)"`)

	// ClassName <- R6Class(...) or ClassName = R6Class(...)
	reRR6Class = regexp.MustCompile(`^(\w+)\s*(?:<-|=)\s*R6Class\s*\(`)

	// setRefClass("ClassName", ...)
	reRSetRefClass = regexp.MustCompile(`^(\w[\w.]*)\s*(?:<-|=)\s*setRefClass\s*\(\s*"([^"]+)"`)

	// Variable assignment at top level: NAME <- value or NAME = value (not function)
	reRVarAssign = regexp.MustCompile(`^([A-Z_][A-Z0-9_]*)\s*(?:<-|=)\s*(.+)`)
)

// Parse extracts structural info from R content.
func (p *RParser) Parse(filename string, content []byte) (*ParsedFile, error) {
	pf := &ParsedFile{
		Path:     filename,
		Language: "r",
		Imports:  make([]Import, 0),
		Exports:  make([]Export, 0),
		Symbols:  make([]Symbol, 0),
	}

	lines := strings.Split(string(content), "\n")
	seenImports := make(map[string]bool)

	for lineNum, line := range lines {
		lineNo := lineNum + 1

		trimmed := strings.TrimSpace(line)
		if trimmed == "" || strings.HasPrefix(trimmed, "#") {
			continue
		}

		// library() / require() imports
		if matches := reRLibrary.FindStringSubmatch(trimmed); matches != nil {
			pkg := matches[1]
			if !seenImports[pkg] {
				seenImports[pkg] = true
				pf.Imports = append(pf.Imports, Import{
					Path: pkg,
					Line: lineNo,
				})
			}
			continue
		}

		// Namespace-qualified calls: package::function
		if nsMatches := reRNamespace.FindAllStringSubmatch(trimmed, -1); nsMatches != nil {
			for _, m := range nsMatches {
				pkg := m[1]
				if !seenImports[pkg] {
					seenImports[pkg] = true
					pf.Imports = append(pf.Imports, Import{
						Path: pkg,
						Line: lineNo,
					})
				}
			}
		}

		// setClass("ClassName", ...)
		if matches := reRSetClass.FindStringSubmatch(trimmed); matches != nil {
			name := matches[1]
			pf.Symbols = append(pf.Symbols, Symbol{
				Name:     name,
				Kind:     "class",
				Exported: true,
				Line:     lineNo,
			})
			pf.Exports = append(pf.Exports, Export{
				Name: name,
				Kind: "class",
				Line: lineNo,
			})
			continue
		}

		// setRefClass("ClassName", ...)
		if matches := reRSetRefClass.FindStringSubmatch(trimmed); matches != nil {
			varName := matches[1]
			className := matches[2]
			// Use the variable name as the symbol name
			pf.Symbols = append(pf.Symbols, Symbol{
				Name:     varName,
				Kind:     "class",
				Exported: true,
				Line:     lineNo,
			})
			pf.Exports = append(pf.Exports, Export{
				Name: varName,
				Kind: "class",
				Line: lineNo,
			})
			_ = className
			continue
		}

		// R6Class
		if matches := reRR6Class.FindStringSubmatch(trimmed); matches != nil {
			name := matches[1]
			pf.Symbols = append(pf.Symbols, Symbol{
				Name:     name,
				Kind:     "class",
				Exported: true,
				Line:     lineNo,
			})
			pf.Exports = append(pf.Exports, Export{
				Name: name,
				Kind: "class",
				Line: lineNo,
			})
			continue
		}

		// Function assignments: name <- function(...) or name = function(...)
		if matches := reRFuncAssign.FindStringSubmatch(line); matches != nil {
			indent := matches[1]
			name := matches[2]
			params := matches[3]

			// Only top-level functions (no indentation)
			if len(indent) > 0 {
				continue
			}

			exported := isPublicR(name)
			sig := name + " <- function(" + strings.TrimSpace(params) + ")"

			pf.Symbols = append(pf.Symbols, Symbol{
				Name:      name,
				Kind:      "function",
				Exported:  exported,
				Line:      lineNo,
				Signature: sig,
			})

			if exported {
				pf.Exports = append(pf.Exports, Export{
					Name: name,
					Kind: "function",
					Line: lineNo,
				})
			}
			continue
		}

		// Top-level constant variables (UPPER_CASE)
		if matches := reRVarAssign.FindStringSubmatch(trimmed); matches != nil {
			// Only if at top level (no indentation in original line)
			if line == trimmed || (!strings.HasPrefix(line, " ") && !strings.HasPrefix(line, "\t")) {
				name := matches[1]
				rhs := strings.TrimSpace(matches[2])
				// Skip if the RHS is a function definition (already handled above)
				if strings.HasPrefix(rhs, "function") {
					continue
				}
				pf.Symbols = append(pf.Symbols, Symbol{
					Name:     name,
					Kind:     "variable",
					Exported: true,
					Line:     lineNo,
				})
			}
		}
	}

	return pf, nil
}

// isPublicR returns true if the name doesn't start with a dot (R convention for private).
func isPublicR(name string) bool {
	return !strings.HasPrefix(name, ".")
}
