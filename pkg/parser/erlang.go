package parser

import (
	"regexp"
	"strings"
)

type ErlangParser struct{}

func NewErlangParser() *ErlangParser { return &ErlangParser{} }

func (p *ErlangParser) Language() string { return "erlang" }

var (
	erlangInclude    = regexp.MustCompile(`(?m)^\s*-include\("([^"]+)"\)`)
	erlangIncludeLib = regexp.MustCompile(`(?m)^\s*-include_lib\("([^"]+)"\)`)
	erlangExport     = regexp.MustCompile(`(?m)^\s*-export\(\[([^\]]+)\]\)`)
	erlangExportAll  = regexp.MustCompile(`(?m)^\s*-compile\(export_all\)`)
	erlangFunc       = regexp.MustCompile(`(?m)^([a-z]\w*)\s*\(([^)]*)\)\s*->`)
	erlangModule     = regexp.MustCompile(`(?m)^\s*-module\((\w+)\)`)
)

func (p *ErlangParser) Parse(filename string, content []byte) (*ParsedFile, error) {
	pf := &ParsedFile{
		Path:     filename,
		Language: "erlang",
		Imports:  make([]Import, 0),
		Exports:  make([]Export, 0),
		Symbols:  make([]Symbol, 0),
	}

	lines := strings.Split(string(content), "\n")
	exportedFuncs := make(map[string]bool)
	exportAll := false

	fullText := string(content)

	// Check for export_all.
	if erlangExportAll.MatchString(fullText) {
		exportAll = true
	}

	// Gather exported function names.
	for _, m := range erlangExport.FindAllStringSubmatch(fullText, -1) {
		exports := m[1]
		for _, entry := range strings.Split(exports, ",") {
			entry = strings.TrimSpace(entry)
			// Format: func_name/arity
			if idx := strings.Index(entry, "/"); idx != -1 {
				exportedFuncs[entry[:idx]] = true
			}
		}
	}

	for lineNum, line := range lines {
		lineNo := lineNum + 1
		trimmed := strings.TrimSpace(line)

		if trimmed == "" || strings.HasPrefix(trimmed, "%") {
			continue
		}

		// Includes.
		if m := erlangInclude.FindStringSubmatch(line); m != nil {
			pf.Imports = append(pf.Imports, Import{Path: m[1], Line: lineNo})
			continue
		}
		if m := erlangIncludeLib.FindStringSubmatch(line); m != nil {
			pf.Imports = append(pf.Imports, Import{Path: m[1], Line: lineNo})
			continue
		}

		// Module declaration.
		if m := erlangModule.FindStringSubmatch(line); m != nil {
			pf.Symbols = append(pf.Symbols, Symbol{
				Name:     m[1],
				Kind:     "module",
				Exported: true,
				Line:     lineNo,
			})
			continue
		}

		// Function declarations.
		if m := erlangFunc.FindStringSubmatch(line); m != nil {
			name := m[1]
			params := m[2]
			exported := exportAll || exportedFuncs[name]
			sig := name + "(" + strings.TrimSpace(params) + ")"

			pf.Symbols = append(pf.Symbols, Symbol{
				Name:      name,
				Kind:      "function",
				Exported:  exported,
				Line:      lineNo,
				Signature: sig,
			})
			if exported {
				pf.Exports = append(pf.Exports, Export{Name: name, Kind: "function", Line: lineNo})
			}
		}
	}

	return pf, nil
}
