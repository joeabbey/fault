package parser

import (
	"regexp"
	"strings"
)

type FortranParser struct{}

func NewFortranParser() *FortranParser { return &FortranParser{} }

func (p *FortranParser) Language() string { return "fortran" }

var (
	fortranUse        = regexp.MustCompile(`(?mi)^\s*use\s+(\w+)`)
	fortranInclude    = regexp.MustCompile(`(?mi)^\s*include\s+['"]([^'"]+)['"]`)
	fortranSubroutine = regexp.MustCompile(`(?mi)^\s*(?:pure\s+|elemental\s+|recursive\s+)*subroutine\s+(\w+)`)
	fortranFunction   = regexp.MustCompile(`(?mi)^\s*(?:pure\s+|elemental\s+|recursive\s+)*(?:integer|real|double\s+precision|complex|logical|character|type\s*\([^)]+\))?\s*function\s+(\w+)`)
	fortranModule     = regexp.MustCompile(`(?mi)^\s*module\s+(\w+)\s*$`)
	fortranProgram    = regexp.MustCompile(`(?mi)^\s*program\s+(\w+)`)
)

func (p *FortranParser) Parse(filename string, content []byte) (*ParsedFile, error) {
	pf := &ParsedFile{
		Path:     filename,
		Language: "fortran",
		Imports:  make([]Import, 0),
		Exports:  make([]Export, 0),
		Symbols:  make([]Symbol, 0),
	}

	lines := strings.Split(string(content), "\n")

	for lineNum, line := range lines {
		lineNo := lineNum + 1
		trimmed := strings.TrimSpace(line)

		if trimmed == "" {
			continue
		}
		// Skip comments: ! or C/c in column 1 for fixed-form
		if strings.HasPrefix(trimmed, "!") {
			continue
		}
		if len(line) > 0 && (line[0] == 'C' || line[0] == 'c' || line[0] == '*') {
			continue
		}

		// use module
		if m := fortranUse.FindStringSubmatch(line); m != nil {
			pf.Imports = append(pf.Imports, Import{Path: m[1], Line: lineNo})
			continue
		}
		// include
		if m := fortranInclude.FindStringSubmatch(line); m != nil {
			pf.Imports = append(pf.Imports, Import{Path: m[1], Line: lineNo})
			continue
		}
		// program
		if m := fortranProgram.FindStringSubmatch(line); m != nil {
			pf.Symbols = append(pf.Symbols, Symbol{Name: m[1], Kind: "program", Exported: true, Line: lineNo})
			continue
		}
		// module
		if m := fortranModule.FindStringSubmatch(line); m != nil {
			name := m[1]
			if strings.EqualFold(name, "procedure") {
				continue
			}
			pf.Symbols = append(pf.Symbols, Symbol{Name: name, Kind: "module", Exported: true, Line: lineNo})
			pf.Exports = append(pf.Exports, Export{Name: name, Kind: "module", Line: lineNo})
			continue
		}
		// subroutine
		if m := fortranSubroutine.FindStringSubmatch(line); m != nil {
			pf.Symbols = append(pf.Symbols, Symbol{Name: m[1], Kind: "function", Exported: true, Line: lineNo})
			pf.Exports = append(pf.Exports, Export{Name: m[1], Kind: "function", Line: lineNo})
			continue
		}
		// function
		if m := fortranFunction.FindStringSubmatch(line); m != nil {
			pf.Symbols = append(pf.Symbols, Symbol{Name: m[1], Kind: "function", Exported: true, Line: lineNo})
			pf.Exports = append(pf.Exports, Export{Name: m[1], Kind: "function", Line: lineNo})
			continue
		}
	}

	return pf, nil
}
