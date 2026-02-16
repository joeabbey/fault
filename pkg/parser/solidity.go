package parser

import (
	"regexp"
	"strings"
)

type SolidityParser struct{}

func NewSolidityParser() *SolidityParser { return &SolidityParser{} }

func (p *SolidityParser) Language() string { return "solidity" }

var (
	solImport    = regexp.MustCompile(`(?m)^\s*import\s+(?:"([^"]+)"|{[^}]+}\s+from\s+"([^"]+)")`)
	solContract  = regexp.MustCompile(`(?m)^\s*(?:abstract\s+)?contract\s+(\w+)`)
	solInterface = regexp.MustCompile(`(?m)^\s*interface\s+(\w+)`)
	solLibrary   = regexp.MustCompile(`(?m)^\s*library\s+(\w+)`)
	solFunction  = regexp.MustCompile(`(?m)^\s*function\s+(\w+)\s*\(`)
	solEvent     = regexp.MustCompile(`(?m)^\s*event\s+(\w+)\s*\(`)
	solModifier  = regexp.MustCompile(`(?m)^\s*modifier\s+(\w+)`)
)

func (p *SolidityParser) Parse(filename string, content []byte) (*ParsedFile, error) {
	pf := &ParsedFile{
		Path:     filename,
		Language: "solidity",
		Imports:  make([]Import, 0),
		Exports:  make([]Export, 0),
		Symbols:  make([]Symbol, 0),
	}

	lines := strings.Split(string(content), "\n")
	inBlockComment := false

	for lineNum, line := range lines {
		lineNo := lineNum + 1
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
		if trimmed == "" || strings.HasPrefix(trimmed, "//") {
			continue
		}

		// Imports
		if m := solImport.FindStringSubmatch(line); m != nil {
			path := m[1]
			if path == "" {
				path = m[2]
			}
			pf.Imports = append(pf.Imports, Import{Path: path, Line: lineNo})
			continue
		}

		// Contract
		if m := solContract.FindStringSubmatch(line); m != nil {
			pf.Symbols = append(pf.Symbols, Symbol{Name: m[1], Kind: "class", Exported: true, Line: lineNo})
			pf.Exports = append(pf.Exports, Export{Name: m[1], Kind: "class", Line: lineNo})
			continue
		}
		// Interface
		if m := solInterface.FindStringSubmatch(line); m != nil {
			pf.Symbols = append(pf.Symbols, Symbol{Name: m[1], Kind: "interface", Exported: true, Line: lineNo})
			pf.Exports = append(pf.Exports, Export{Name: m[1], Kind: "interface", Line: lineNo})
			continue
		}
		// Library
		if m := solLibrary.FindStringSubmatch(line); m != nil {
			pf.Symbols = append(pf.Symbols, Symbol{Name: m[1], Kind: "class", Exported: true, Line: lineNo})
			pf.Exports = append(pf.Exports, Export{Name: m[1], Kind: "class", Line: lineNo})
			continue
		}

		// Functions
		if m := solFunction.FindStringSubmatch(line); m != nil {
			name := m[1]
			exported := strings.Contains(line, "public") || strings.Contains(line, "external")
			pf.Symbols = append(pf.Symbols, Symbol{Name: name, Kind: "function", Exported: exported, Line: lineNo})
			if exported {
				pf.Exports = append(pf.Exports, Export{Name: name, Kind: "function", Line: lineNo})
			}
			continue
		}
		// Events
		if m := solEvent.FindStringSubmatch(line); m != nil {
			pf.Symbols = append(pf.Symbols, Symbol{Name: m[1], Kind: "event", Exported: true, Line: lineNo})
			continue
		}
		// Modifiers
		if m := solModifier.FindStringSubmatch(line); m != nil {
			pf.Symbols = append(pf.Symbols, Symbol{Name: m[1], Kind: "modifier", Exported: true, Line: lineNo})
			continue
		}
	}

	return pf, nil
}
