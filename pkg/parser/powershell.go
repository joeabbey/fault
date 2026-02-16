package parser

import (
	"regexp"
	"strings"
)

type PowershellParser struct{}

func NewPowershellParser() *PowershellParser { return &PowershellParser{} }

func (p *PowershellParser) Language() string { return "powershell" }

var (
	psImportModule = regexp.MustCompile(`(?mi)^\s*Import-Module\s+['\"]?(\S+?)['\"]?\s*$`)
	psUsingModule  = regexp.MustCompile(`(?mi)^\s*using\s+module\s+(\S+)`)
	psDotSource    = regexp.MustCompile(`(?m)^\s*\.\s+"?([^"]+\.ps1)"?`)
	psFunction     = regexp.MustCompile(`(?mi)^\s*function\s+([\w-]+)`)
	psCmdlet       = regexp.MustCompile(`(?mi)^\s*(?:filter|workflow)\s+([\w-]+)`)
)

func (p *PowershellParser) Parse(filename string, content []byte) (*ParsedFile, error) {
	pf := &ParsedFile{
		Path:     filename,
		Language: "powershell",
		Imports:  make([]Import, 0),
		Exports:  make([]Export, 0),
		Symbols:  make([]Symbol, 0),
	}

	lines := strings.Split(string(content), "\n")
	inBlockComment := false

	for lineNum, line := range lines {
		lineNo := lineNum + 1
		trimmed := strings.TrimSpace(line)

		// Handle block comments <# ... #>
		if inBlockComment {
			if strings.Contains(trimmed, "#>") {
				inBlockComment = false
			}
			continue
		}
		if strings.HasPrefix(trimmed, "<#") {
			if !strings.Contains(trimmed, "#>") {
				inBlockComment = true
			}
			continue
		}

		if trimmed == "" || strings.HasPrefix(trimmed, "#") {
			continue
		}

		// Import-Module
		if m := psImportModule.FindStringSubmatch(line); m != nil {
			pf.Imports = append(pf.Imports, Import{Path: m[1], Line: lineNo})
			continue
		}
		// using module
		if m := psUsingModule.FindStringSubmatch(line); m != nil {
			pf.Imports = append(pf.Imports, Import{Path: m[1], Line: lineNo})
			continue
		}
		// Dot-sourcing
		if m := psDotSource.FindStringSubmatch(line); m != nil {
			pf.Imports = append(pf.Imports, Import{Path: m[1], Line: lineNo})
			continue
		}

		// Function definitions
		if m := psFunction.FindStringSubmatch(line); m != nil {
			name := m[1]
			pf.Symbols = append(pf.Symbols, Symbol{Name: name, Kind: "function", Exported: true, Line: lineNo})
			pf.Exports = append(pf.Exports, Export{Name: name, Kind: "function", Line: lineNo})
			continue
		}
		// Filters/workflows
		if m := psCmdlet.FindStringSubmatch(line); m != nil {
			name := m[1]
			pf.Symbols = append(pf.Symbols, Symbol{Name: name, Kind: "function", Exported: true, Line: lineNo})
			pf.Exports = append(pf.Exports, Export{Name: name, Kind: "function", Line: lineNo})
		}
	}

	return pf, nil
}
