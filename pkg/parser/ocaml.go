package parser

import (
	"regexp"
	"strings"
)

type OcamlParser struct{}

func NewOcamlParser() *OcamlParser { return &OcamlParser{} }

func (p *OcamlParser) Language() string { return "ocaml" }

var (
	ocamlOpen    = regexp.MustCompile(`(?m)^\s*open\s+([A-Z]\w+)`)
	ocamlInclude = regexp.MustCompile(`(?m)^\s*include\s+([A-Z]\w+)`)
	ocamlLet     = regexp.MustCompile(`(?m)^\s*let\s+(?:rec\s+)?(\w+)`)
	ocamlVal     = regexp.MustCompile(`(?m)^\s*val\s+(\w+)\s*:\s*(.+)`)
	ocamlType    = regexp.MustCompile(`(?m)^\s*type\s+(\w+)`)
	ocamlModule  = regexp.MustCompile(`(?m)^\s*module\s+(\w+)`)
)

func (p *OcamlParser) Parse(filename string, content []byte) (*ParsedFile, error) {
	pf := &ParsedFile{
		Path:     filename,
		Language: "ocaml",
		Imports:  make([]Import, 0),
		Exports:  make([]Export, 0),
		Symbols:  make([]Symbol, 0),
	}

	lines := strings.Split(string(content), "\n")
	inBlockComment := 0

	isMli := strings.HasSuffix(filename, ".mli")

	for lineNum, line := range lines {
		lineNo := lineNum + 1
		trimmed := strings.TrimSpace(line)

		// Handle nested block comments (* ... *).
		if inBlockComment > 0 {
			inBlockComment += strings.Count(trimmed, "(*")
			inBlockComment -= strings.Count(trimmed, "*)")
			if inBlockComment < 0 {
				inBlockComment = 0
			}
			continue
		}
		if strings.Contains(trimmed, "(*") && !strings.Contains(trimmed, "*)") {
			inBlockComment = 1
			continue
		}
		if strings.HasPrefix(trimmed, "(*") && strings.Contains(trimmed, "*)") {
			continue
		}

		if trimmed == "" {
			continue
		}

		// Open.
		if m := ocamlOpen.FindStringSubmatch(line); m != nil {
			pf.Imports = append(pf.Imports, Import{Path: m[1], Line: lineNo})
			continue
		}

		// Include.
		if m := ocamlInclude.FindStringSubmatch(line); m != nil {
			pf.Imports = append(pf.Imports, Import{Path: m[1], Line: lineNo})
			continue
		}

		// Val declarations (in .mli files, these are public).
		if m := ocamlVal.FindStringSubmatch(line); m != nil {
			name := m[1]
			sig := strings.TrimSpace(m[2])
			pf.Symbols = append(pf.Symbols, Symbol{
				Name:      name,
				Kind:      "function",
				Exported:  true,
				Line:      lineNo,
				Signature: "val " + name + " : " + sig,
			})
			pf.Exports = append(pf.Exports, Export{Name: name, Kind: "function", Line: lineNo})
			continue
		}

		// Let bindings.
		if m := ocamlLet.FindStringSubmatch(line); m != nil {
			name := m[1]
			if name == "_" || name == "open" || name == "in" {
				continue
			}
			exported := !isMli // In .ml, everything is exported; in .mli, val declarations are exports
			if isMli {
				exported = false
			}
			pf.Symbols = append(pf.Symbols, Symbol{
				Name:     name,
				Kind:     "function",
				Exported: exported,
				Line:     lineNo,
			})
			if exported {
				pf.Exports = append(pf.Exports, Export{Name: name, Kind: "function", Line: lineNo})
			}
			continue
		}

		// Type declarations.
		if m := ocamlType.FindStringSubmatch(line); m != nil {
			name := m[1]
			pf.Symbols = append(pf.Symbols, Symbol{
				Name:     name,
				Kind:     "type",
				Exported: true,
				Line:     lineNo,
			})
			continue
		}

		// Module declarations.
		if m := ocamlModule.FindStringSubmatch(line); m != nil {
			name := m[1]
			pf.Symbols = append(pf.Symbols, Symbol{
				Name:     name,
				Kind:     "module",
				Exported: true,
				Line:     lineNo,
			})
		}
	}

	return pf, nil
}
