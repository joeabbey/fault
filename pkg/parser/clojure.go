package parser

import (
	"regexp"
	"strings"
)

type ClojureParser struct{}

func NewClojureParser() *ClojureParser { return &ClojureParser{} }

func (p *ClojureParser) Language() string { return "clojure" }

var (
	clojureNsRequire   = regexp.MustCompile(`\(:require\s+\[([^\]\s]+)`)
	clojureNsImport    = regexp.MustCompile(`\(:import\s+\(([^\)]+)\)`)
	clojureRequire     = regexp.MustCompile(`\(require\s+'\[([^\]\s]+)`)
	clojureImport      = regexp.MustCompile(`\(import\s+'([^\s)]+)`)
	clojureDefn        = regexp.MustCompile(`(?m)^\s*\(defn\s+(\S+)`)
	clojureDefnPrivate = regexp.MustCompile(`(?m)^\s*\(defn-\s+(\S+)`)
	clojureDef         = regexp.MustCompile(`(?m)^\s*\(def\s+(\S+)`)
)

func (p *ClojureParser) Parse(filename string, content []byte) (*ParsedFile, error) {
	pf := &ParsedFile{
		Path:     filename,
		Language: "clojure",
		Imports:  make([]Import, 0),
		Exports:  make([]Export, 0),
		Symbols:  make([]Symbol, 0),
	}

	lines := strings.Split(string(content), "\n")

	for lineNum, line := range lines {
		lineNo := lineNum + 1
		trimmed := strings.TrimSpace(line)

		if trimmed == "" || strings.HasPrefix(trimmed, ";") {
			continue
		}

		// NS require forms.
		for _, m := range clojureNsRequire.FindAllStringSubmatch(line, -1) {
			pf.Imports = append(pf.Imports, Import{Path: m[1], Line: lineNo})
		}

		// NS import forms.
		for _, m := range clojureNsImport.FindAllStringSubmatch(line, -1) {
			parts := strings.Fields(m[1])
			if len(parts) > 0 {
				pf.Imports = append(pf.Imports, Import{Path: parts[0], Line: lineNo})
			}
		}

		// Standalone require.
		if m := clojureRequire.FindStringSubmatch(line); m != nil {
			pf.Imports = append(pf.Imports, Import{Path: m[1], Line: lineNo})
		}

		// Standalone import.
		if m := clojureImport.FindStringSubmatch(line); m != nil {
			pf.Imports = append(pf.Imports, Import{Path: m[1], Line: lineNo})
		}

		// Private functions (defn-).
		if m := clojureDefnPrivate.FindStringSubmatch(line); m != nil {
			name := m[1]
			pf.Symbols = append(pf.Symbols, Symbol{
				Name:     name,
				Kind:     "function",
				Exported: false,
				Line:     lineNo,
			})
			continue
		}

		// Public functions (defn).
		if m := clojureDefn.FindStringSubmatch(line); m != nil {
			name := m[1]
			pf.Symbols = append(pf.Symbols, Symbol{
				Name:     name,
				Kind:     "function",
				Exported: true,
				Line:     lineNo,
			})
			pf.Exports = append(pf.Exports, Export{Name: name, Kind: "function", Line: lineNo})
			continue
		}

		// Defs (public vars).
		if m := clojureDef.FindStringSubmatch(line); m != nil {
			name := m[1]
			if name == "defn" || name == "defn-" || strings.HasPrefix(name, "^") {
				continue
			}
			pf.Symbols = append(pf.Symbols, Symbol{
				Name:     name,
				Kind:     "variable",
				Exported: true,
				Line:     lineNo,
			})
			pf.Exports = append(pf.Exports, Export{Name: name, Kind: "variable", Line: lineNo})
		}
	}

	return pf, nil
}
