package parser

import (
	"regexp"
	"strings"
)

type PascalParser struct{}

func NewPascalParser() *PascalParser { return &PascalParser{} }

func (p *PascalParser) Language() string { return "pascal" }

var (
	pascalUses      = regexp.MustCompile(`(?mi)^\s*uses\s+(.+)`)
	pascalUnit      = regexp.MustCompile(`(?mi)^\s*unit\s+(\w+)`)
	pascalProgram   = regexp.MustCompile(`(?mi)^\s*program\s+(\w+)`)
	pascalProcedure = regexp.MustCompile(`(?mi)^\s*(?:class\s+)?procedure\s+(?:(\w+)\.)?(\w+)`)
	pascalFunction  = regexp.MustCompile(`(?mi)^\s*(?:class\s+)?function\s+(?:(\w+)\.)?(\w+)`)
	pascalType      = regexp.MustCompile(`(?mi)^\s*(\w+)\s*=\s*(?:class|record|interface|object|packed\s+record)`)
	pascalConst     = regexp.MustCompile(`(?mi)^\s*(\w+)\s*(?::\s*\w+)?\s*=\s*`)
)

func (p *PascalParser) Parse(filename string, content []byte) (*ParsedFile, error) {
	pf := &ParsedFile{
		Path:     filename,
		Language: "pascal",
		Imports:  make([]Import, 0),
		Exports:  make([]Export, 0),
		Symbols:  make([]Symbol, 0),
	}

	lines := strings.Split(string(content), "\n")
	inBlockComment := false
	inInterface := false
	inImplementation := false

	for lineNum, line := range lines {
		lineNo := lineNum + 1
		trimmed := strings.TrimSpace(line)

		// Handle block comments { } and (* *)
		if inBlockComment {
			if strings.Contains(trimmed, "}") || strings.Contains(trimmed, "*)") {
				inBlockComment = false
			}
			continue
		}
		if strings.HasPrefix(trimmed, "{") && !strings.HasPrefix(trimmed, "{$") {
			if !strings.Contains(trimmed, "}") {
				inBlockComment = true
			}
			continue
		}
		if strings.HasPrefix(trimmed, "(*") {
			if !strings.Contains(trimmed, "*)") {
				inBlockComment = true
			}
			continue
		}
		if trimmed == "" || strings.HasPrefix(trimmed, "//") {
			continue
		}

		upper := strings.ToUpper(trimmed)
		if upper == "INTERFACE" {
			inInterface = true
			inImplementation = false
			continue
		}
		if upper == "IMPLEMENTATION" {
			inInterface = false
			inImplementation = true
			continue
		}

		// uses (import)
		if m := pascalUses.FindStringSubmatch(line); m != nil {
			unitList := m[1]
			// Handle multi-line uses by taking what we have on this line
			unitList = strings.TrimSuffix(strings.TrimSpace(unitList), ";")
			for _, u := range strings.Split(unitList, ",") {
				u = strings.TrimSpace(u)
				if u != "" && !strings.HasPrefix(u, "{") {
					pf.Imports = append(pf.Imports, Import{Path: u, Line: lineNo})
				}
			}
			continue
		}

		// unit
		if m := pascalUnit.FindStringSubmatch(line); m != nil {
			pf.Symbols = append(pf.Symbols, Symbol{Name: m[1], Kind: "module", Exported: true, Line: lineNo})
			pf.Exports = append(pf.Exports, Export{Name: m[1], Kind: "module", Line: lineNo})
			continue
		}
		// program
		if m := pascalProgram.FindStringSubmatch(line); m != nil {
			pf.Symbols = append(pf.Symbols, Symbol{Name: m[1], Kind: "program", Exported: true, Line: lineNo})
			continue
		}

		exported := inInterface && !inImplementation

		// type definition (only in type section, heuristic)
		if m := pascalType.FindStringSubmatch(line); m != nil {
			name := m[1]
			pf.Symbols = append(pf.Symbols, Symbol{Name: name, Kind: "type", Exported: exported, Line: lineNo})
			if exported {
				pf.Exports = append(pf.Exports, Export{Name: name, Kind: "type", Line: lineNo})
			}
			continue
		}
		// procedure
		if m := pascalProcedure.FindStringSubmatch(line); m != nil {
			name := m[2]
			pf.Symbols = append(pf.Symbols, Symbol{Name: name, Kind: "function", Exported: exported, Line: lineNo})
			if exported {
				pf.Exports = append(pf.Exports, Export{Name: name, Kind: "function", Line: lineNo})
			}
			continue
		}
		// function
		if m := pascalFunction.FindStringSubmatch(line); m != nil {
			name := m[2]
			pf.Symbols = append(pf.Symbols, Symbol{Name: name, Kind: "function", Exported: exported, Line: lineNo})
			if exported {
				pf.Exports = append(pf.Exports, Export{Name: name, Kind: "function", Line: lineNo})
			}
			continue
		}

		_ = pascalConst // available for future use
	}

	return pf, nil
}
