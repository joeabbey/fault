package parser

import (
	"regexp"
	"strings"
)

type JuliaParser struct{}

func NewJuliaParser() *JuliaParser { return &JuliaParser{} }

func (p *JuliaParser) Language() string { return "julia" }

var (
	juliaUsing    = regexp.MustCompile(`(?m)^\s*using\s+([\w.]+(?:\s*,\s*[\w.]+)*)`)
	juliaImport   = regexp.MustCompile(`(?m)^\s*import\s+([\w.]+(?:\s*,\s*[\w.]+)*)`)
	juliaInclude  = regexp.MustCompile(`(?m)^\s*include\s*\(\s*"([^"]+)"\s*\)`)
	juliaFunction = regexp.MustCompile(`(?m)^\s*(?:export\s+)?function\s+(\w+)`)
	juliaMacro    = regexp.MustCompile(`(?m)^\s*macro\s+(\w+)`)
	juliaStruct   = regexp.MustCompile(`(?m)^\s*(?:mutable\s+)?struct\s+(\w+)`)
	juliaExport   = regexp.MustCompile(`(?m)^\s*export\s+([\w,\s]+)`)
)

func (p *JuliaParser) Parse(filename string, content []byte) (*ParsedFile, error) {
	pf := &ParsedFile{
		Path:     filename,
		Language: "julia",
		Imports:  make([]Import, 0),
		Exports:  make([]Export, 0),
		Symbols:  make([]Symbol, 0),
	}

	lines := strings.Split(string(content), "\n")
	exportNames := make(map[string]bool)
	inBlockComment := false

	// First pass: collect exports
	for _, line := range lines {
		if m := juliaExport.FindStringSubmatch(line); m != nil {
			for _, name := range strings.Split(m[1], ",") {
				name = strings.TrimSpace(name)
				if name != "" {
					exportNames[name] = true
				}
			}
		}
	}

	for lineNum, line := range lines {
		lineNo := lineNum + 1
		trimmed := strings.TrimSpace(line)

		// Handle block comments #= ... =#
		if inBlockComment {
			if strings.Contains(trimmed, "=#") {
				inBlockComment = false
			}
			continue
		}
		if strings.HasPrefix(trimmed, "#=") {
			if !strings.Contains(trimmed, "=#") {
				inBlockComment = true
			}
			continue
		}
		if trimmed == "" || strings.HasPrefix(trimmed, "#") {
			continue
		}

		// using
		if m := juliaUsing.FindStringSubmatch(line); m != nil {
			for _, mod := range strings.Split(m[1], ",") {
				mod = strings.TrimSpace(mod)
				if mod != "" {
					pf.Imports = append(pf.Imports, Import{Path: mod, Line: lineNo})
				}
			}
			continue
		}
		// import
		if m := juliaImport.FindStringSubmatch(line); m != nil {
			for _, mod := range strings.Split(m[1], ",") {
				mod = strings.TrimSpace(mod)
				if mod != "" {
					pf.Imports = append(pf.Imports, Import{Path: mod, Line: lineNo})
				}
			}
			continue
		}
		// include
		if m := juliaInclude.FindStringSubmatch(line); m != nil {
			pf.Imports = append(pf.Imports, Import{Path: m[1], Line: lineNo})
			continue
		}

		// function
		if m := juliaFunction.FindStringSubmatch(line); m != nil {
			name := m[1]
			exported := exportNames[name]
			pf.Symbols = append(pf.Symbols, Symbol{Name: name, Kind: "function", Exported: exported, Line: lineNo})
			if exported {
				pf.Exports = append(pf.Exports, Export{Name: name, Kind: "function", Line: lineNo})
			}
			continue
		}
		// macro
		if m := juliaMacro.FindStringSubmatch(line); m != nil {
			pf.Symbols = append(pf.Symbols, Symbol{Name: m[1], Kind: "macro", Exported: exportNames[m[1]], Line: lineNo})
			continue
		}
		// struct
		if m := juliaStruct.FindStringSubmatch(line); m != nil {
			name := m[1]
			exported := exportNames[name]
			pf.Symbols = append(pf.Symbols, Symbol{Name: name, Kind: "type", Exported: exported, Line: lineNo})
			if exported {
				pf.Exports = append(pf.Exports, Export{Name: name, Kind: "type", Line: lineNo})
			}
			continue
		}
	}

	return pf, nil
}
