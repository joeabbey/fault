package parser

import (
	"regexp"
	"strings"
)

type TerraformParser struct{}

func NewTerraformParser() *TerraformParser { return &TerraformParser{} }

func (p *TerraformParser) Language() string { return "terraform" }

var (
	tfModule   = regexp.MustCompile(`(?m)^\s*module\s+"([^"]+)"`)
	tfResource = regexp.MustCompile(`(?m)^\s*resource\s+"(\w+)"\s+"(\w+)"`)
	tfData     = regexp.MustCompile(`(?m)^\s*data\s+"(\w+)"\s+"(\w+)"`)
	tfVariable = regexp.MustCompile(`(?m)^\s*variable\s+"(\w+)"`)
	tfOutput   = regexp.MustCompile(`(?m)^\s*output\s+"(\w+)"`)
	tfProvider = regexp.MustCompile(`(?m)^\s*provider\s+"(\w+)"`)
	tfSource   = regexp.MustCompile(`(?m)^\s*source\s*=\s*"([^"]+)"`)
)

func (p *TerraformParser) Parse(filename string, content []byte) (*ParsedFile, error) {
	pf := &ParsedFile{
		Path:     filename,
		Language: "terraform",
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
		if trimmed == "" || strings.HasPrefix(trimmed, "#") || strings.HasPrefix(trimmed, "//") {
			continue
		}

		// Provider (treated as import)
		if m := tfProvider.FindStringSubmatch(line); m != nil {
			pf.Imports = append(pf.Imports, Import{Path: m[1], Line: lineNo})
			continue
		}
		// Module source (treated as import)
		if m := tfModule.FindStringSubmatch(line); m != nil {
			pf.Symbols = append(pf.Symbols, Symbol{Name: m[1], Kind: "module", Exported: true, Line: lineNo})
			continue
		}
		// Source attribute in module
		if m := tfSource.FindStringSubmatch(line); m != nil {
			pf.Imports = append(pf.Imports, Import{Path: m[1], Line: lineNo})
			continue
		}
		// Resource
		if m := tfResource.FindStringSubmatch(line); m != nil {
			name := m[1] + "." + m[2]
			pf.Symbols = append(pf.Symbols, Symbol{Name: name, Kind: "resource", Exported: true, Line: lineNo})
			continue
		}
		// Data source
		if m := tfData.FindStringSubmatch(line); m != nil {
			name := m[1] + "." + m[2]
			pf.Symbols = append(pf.Symbols, Symbol{Name: name, Kind: "data", Exported: true, Line: lineNo})
			continue
		}
		// Variable
		if m := tfVariable.FindStringSubmatch(line); m != nil {
			pf.Symbols = append(pf.Symbols, Symbol{Name: m[1], Kind: "variable", Exported: true, Line: lineNo})
			continue
		}
		// Output
		if m := tfOutput.FindStringSubmatch(line); m != nil {
			pf.Symbols = append(pf.Symbols, Symbol{Name: m[1], Kind: "output", Exported: true, Line: lineNo})
			pf.Exports = append(pf.Exports, Export{Name: m[1], Kind: "output", Line: lineNo})
			continue
		}
	}

	return pf, nil
}
