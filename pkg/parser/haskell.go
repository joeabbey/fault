package parser

import (
	"regexp"
	"strings"
)

// HaskellParser extracts structural information from Haskell files via regex.
type HaskellParser struct{}

func NewHaskellParser() *HaskellParser { return &HaskellParser{} }

func (p *HaskellParser) Language() string { return "haskell" }

var (
	haskellImport         = regexp.MustCompile(`(?m)^\s*import\s+(?:qualified\s+)?([A-Z][\w.]+)`)
	haskellFuncSig        = regexp.MustCompile(`(?m)^(\w+)\s*::\s*(.+)`)
	haskellFuncDef        = regexp.MustCompile(`(?m)^(\w+)\s+[^:=]*=`)
	haskellModuleExports  = regexp.MustCompile(`(?m)^module\s+\S+\s*\(([^)]+)\)`)
)

func (p *HaskellParser) Parse(filename string, content []byte) (*ParsedFile, error) {
	pf := &ParsedFile{
		Path:     filename,
		Language: "haskell",
		Imports:  make([]Import, 0),
		Exports:  make([]Export, 0),
		Symbols:  make([]Symbol, 0),
	}

	lines := strings.Split(string(content), "\n")
	inBlockComment := false
	exportNames := make(map[string]bool)

	// First pass: extract module exports list.
	fullText := string(content)
	if m := haskellModuleExports.FindStringSubmatch(fullText); m != nil {
		for _, name := range strings.Split(m[1], ",") {
			name = strings.TrimSpace(name)
			if name != "" {
				exportNames[name] = true
			}
		}
	}

	signatures := make(map[string]string)

	for lineNum, line := range lines {
		lineNo := lineNum + 1
		trimmed := strings.TrimSpace(line)

		// Handle block comments {- ... -}.
		if inBlockComment {
			if strings.Contains(trimmed, "-}") {
				inBlockComment = false
			}
			continue
		}
		if strings.HasPrefix(trimmed, "{-") {
			if !strings.Contains(trimmed, "-}") {
				inBlockComment = true
			}
			continue
		}

		if trimmed == "" || strings.HasPrefix(trimmed, "--") {
			continue
		}

		// Extract imports.
		if m := haskellImport.FindStringSubmatch(line); m != nil {
			pf.Imports = append(pf.Imports, Import{Path: m[1], Line: lineNo})
			continue
		}

		// Extract type signatures.
		if m := haskellFuncSig.FindStringSubmatch(line); m != nil {
			name := m[1]
			sig := strings.TrimSpace(m[2])
			signatures[name] = sig

			exported := exportNames[name] || len(exportNames) == 0
			pf.Symbols = append(pf.Symbols, Symbol{
				Name:      name,
				Kind:      "function",
				Exported:  exported,
				Line:      lineNo,
				Signature: name + " :: " + sig,
			})
			if exported {
				pf.Exports = append(pf.Exports, Export{Name: name, Kind: "function", Line: lineNo})
			}
			continue
		}

		// Extract function definitions (only if no type sig already captured).
		if m := haskellFuncDef.FindStringSubmatch(line); m != nil {
			name := m[1]
			if _, hasSig := signatures[name]; hasSig {
				continue
			}
			// Skip keywords.
			switch name {
			case "module", "import", "data", "type", "newtype", "class", "instance", "where", "let", "in", "if", "then", "else", "case", "of", "do":
				continue
			}
			exported := exportNames[name] || len(exportNames) == 0
			pf.Symbols = append(pf.Symbols, Symbol{
				Name:     name,
				Kind:     "function",
				Exported: exported,
				Line:     lineNo,
			})
			if exported {
				pf.Exports = append(pf.Exports, Export{Name: name, Kind: "function", Line: lineNo})
			}
		}
	}

	return pf, nil
}
