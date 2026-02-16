package parser

import (
	"regexp"
	"strings"
)

type FsharpParser struct{}

func NewFsharpParser() *FsharpParser { return &FsharpParser{} }

func (p *FsharpParser) Language() string { return "fsharp" }

var (
	fsharpOpen       = regexp.MustCompile(`(?m)^\s*open\s+([A-Z][\w.]+)`)
	fsharpLetFunc    = regexp.MustCompile(`(?m)^\s*let\s+((?:rec\s+)?(?:private\s+)?(?:inline\s+)?)(\w+)\s+([^=]*)\s*=`)
	fsharpMember     = regexp.MustCompile(`(?m)^\s*member\s+\w+\.(\w+)\s*\(([^)]*)\)`)
	fsharpStaticMem  = regexp.MustCompile(`(?m)^\s*static\s+member\s+(\w+)\s*\(([^)]*)\)`)
)

func (p *FsharpParser) Parse(filename string, content []byte) (*ParsedFile, error) {
	pf := &ParsedFile{
		Path:     filename,
		Language: "fsharp",
		Imports:  make([]Import, 0),
		Exports:  make([]Export, 0),
		Symbols:  make([]Symbol, 0),
	}

	lines := strings.Split(string(content), "\n")
	inBlockComment := false

	for lineNum, line := range lines {
		lineNo := lineNum + 1
		trimmed := strings.TrimSpace(line)

		// Handle block comments (* ... *).
		if inBlockComment {
			if strings.Contains(trimmed, "*)") {
				inBlockComment = false
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

		// Open statements.
		if m := fsharpOpen.FindStringSubmatch(line); m != nil {
			pf.Imports = append(pf.Imports, Import{Path: m[1], Line: lineNo})
			continue
		}

		// Let bindings / functions.
		if m := fsharpLetFunc.FindStringSubmatch(line); m != nil {
			modifiers := m[1]
			name := m[2]
			params := strings.TrimSpace(m[3])
			isPrivate := strings.Contains(modifiers, "private")

			kind := "variable"
			if params != "" {
				kind = "function"
			}

			pf.Symbols = append(pf.Symbols, Symbol{
				Name:     name,
				Kind:     kind,
				Exported: !isPrivate,
				Line:     lineNo,
			})
			if !isPrivate {
				pf.Exports = append(pf.Exports, Export{Name: name, Kind: kind, Line: lineNo})
			}
			continue
		}

		// Instance members.
		if m := fsharpMember.FindStringSubmatch(line); m != nil {
			name := m[1]
			pf.Symbols = append(pf.Symbols, Symbol{
				Name:     name,
				Kind:     "method",
				Exported: true,
				Line:     lineNo,
			})
			pf.Exports = append(pf.Exports, Export{Name: name, Kind: "method", Line: lineNo})
			continue
		}

		// Static members.
		if m := fsharpStaticMem.FindStringSubmatch(line); m != nil {
			name := m[1]
			pf.Symbols = append(pf.Symbols, Symbol{
				Name:     name,
				Kind:     "method",
				Exported: true,
				Line:     lineNo,
			})
			pf.Exports = append(pf.Exports, Export{Name: name, Kind: "method", Line: lineNo})
		}
	}

	return pf, nil
}
