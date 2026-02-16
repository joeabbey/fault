package parser

import (
	"regexp"
	"strings"
)

type PerlParser struct{}

func NewPerlParser() *PerlParser { return &PerlParser{} }

func (p *PerlParser) Language() string { return "perl" }

var (
	perlUseImport     = regexp.MustCompile(`(?m)^\s*use\s+([a-zA-Z][\w:]+)`)
	perlRequireImport = regexp.MustCompile(`(?m)^\s*require\s+([a-zA-Z][\w:]+)`)
	perlSubDef        = regexp.MustCompile(`(?m)^\s*sub\s+(\w+)`)
	perlPackageDecl   = regexp.MustCompile(`(?m)^\s*package\s+([\w:]+)`)
)

func (p *PerlParser) Parse(filename string, content []byte) (*ParsedFile, error) {
	pf := &ParsedFile{
		Path:     filename,
		Language: "perl",
		Imports:  make([]Import, 0),
		Exports:  make([]Export, 0),
		Symbols:  make([]Symbol, 0),
	}

	lines := strings.Split(string(content), "\n")
	inPod := false

	for lineNum, line := range lines {
		lineNo := lineNum + 1
		trimmed := strings.TrimSpace(line)

		// Handle POD documentation blocks.
		if strings.HasPrefix(trimmed, "=") {
			if strings.HasPrefix(trimmed, "=cut") {
				inPod = false
			} else if strings.HasPrefix(trimmed, "=head") || strings.HasPrefix(trimmed, "=pod") || strings.HasPrefix(trimmed, "=over") || strings.HasPrefix(trimmed, "=item") || strings.HasPrefix(trimmed, "=begin") || strings.HasPrefix(trimmed, "=for") || strings.HasPrefix(trimmed, "=encoding") || strings.HasPrefix(trimmed, "=back") || strings.HasPrefix(trimmed, "=end") {
				inPod = true
			}
			continue
		}
		if inPod {
			continue
		}

		if trimmed == "" || strings.HasPrefix(trimmed, "#") {
			continue
		}

		// Imports: use Module::Name
		if m := perlUseImport.FindStringSubmatch(line); m != nil {
			pf.Imports = append(pf.Imports, Import{Path: m[1], Line: lineNo})
			continue
		}
		// Imports: require Module::Name
		if m := perlRequireImport.FindStringSubmatch(line); m != nil {
			pf.Imports = append(pf.Imports, Import{Path: m[1], Line: lineNo})
			continue
		}

		// Package declaration
		if m := perlPackageDecl.FindStringSubmatch(line); m != nil {
			pf.Symbols = append(pf.Symbols, Symbol{Name: m[1], Kind: "module", Exported: true, Line: lineNo})
			pf.Exports = append(pf.Exports, Export{Name: m[1], Kind: "module", Line: lineNo})
			continue
		}

		// Subroutine definitions
		if m := perlSubDef.FindStringSubmatch(line); m != nil {
			name := m[1]
			exported := !strings.HasPrefix(name, "_")
			pf.Symbols = append(pf.Symbols, Symbol{Name: name, Kind: "function", Exported: exported, Line: lineNo})
			if exported {
				pf.Exports = append(pf.Exports, Export{Name: name, Kind: "function", Line: lineNo})
			}
		}
	}

	return pf, nil
}
