package parser

import (
	"regexp"
	"strings"
)

type AdaParser struct{}

func NewAdaParser() *AdaParser { return &AdaParser{} }

func (p *AdaParser) Language() string { return "ada" }

var (
	adaWith      = regexp.MustCompile(`(?mi)^\s*with\s+([\w.]+)`)
	adaUse       = regexp.MustCompile(`(?mi)^\s*use\s+([\w.]+)`)
	adaPackage   = regexp.MustCompile(`(?mi)^\s*package\s+(?:body\s+)?(\w[\w.]*)\s+is`)
	adaProcedure = regexp.MustCompile(`(?mi)^\s*(?:overriding\s+)?procedure\s+(\w+)`)
	adaFunction  = regexp.MustCompile(`(?mi)^\s*(?:overriding\s+)?function\s+(\w+)`)
	adaType      = regexp.MustCompile(`(?mi)^\s*type\s+(\w+)\s+is`)
	adaSubtype   = regexp.MustCompile(`(?mi)^\s*subtype\s+(\w+)\s+is`)
	adaTask      = regexp.MustCompile(`(?mi)^\s*task\s+(?:type\s+|body\s+)?(\w+)`)
	adaProtected = regexp.MustCompile(`(?mi)^\s*protected\s+(?:type\s+|body\s+)?(\w+)`)
	adaGeneric   = regexp.MustCompile(`(?mi)^\s*generic\b`)
)

func (p *AdaParser) Parse(filename string, content []byte) (*ParsedFile, error) {
	pf := &ParsedFile{
		Path:     filename,
		Language: "ada",
		Imports:  make([]Import, 0),
		Exports:  make([]Export, 0),
		Symbols:  make([]Symbol, 0),
	}

	lines := strings.Split(string(content), "\n")

	for lineNum, line := range lines {
		lineNo := lineNum + 1
		trimmed := strings.TrimSpace(line)

		if trimmed == "" || strings.HasPrefix(trimmed, "--") {
			continue
		}

		// with (import)
		if m := adaWith.FindStringSubmatch(line); m != nil {
			pf.Imports = append(pf.Imports, Import{Path: m[1], Line: lineNo})
			continue
		}
		// use (also an import form)
		if m := adaUse.FindStringSubmatch(line); m != nil {
			if !adaUse.MatchString(line) {
				continue
			}
			pf.Imports = append(pf.Imports, Import{Path: m[1], Line: lineNo})
			continue
		}
		// generic (skip, the next declaration is the generic itself)
		if adaGeneric.MatchString(line) {
			continue
		}
		// package
		if m := adaPackage.FindStringSubmatch(line); m != nil {
			pf.Symbols = append(pf.Symbols, Symbol{Name: m[1], Kind: "module", Exported: true, Line: lineNo})
			pf.Exports = append(pf.Exports, Export{Name: m[1], Kind: "module", Line: lineNo})
			continue
		}
		// procedure
		if m := adaProcedure.FindStringSubmatch(line); m != nil {
			pf.Symbols = append(pf.Symbols, Symbol{Name: m[1], Kind: "function", Exported: true, Line: lineNo})
			pf.Exports = append(pf.Exports, Export{Name: m[1], Kind: "function", Line: lineNo})
			continue
		}
		// function
		if m := adaFunction.FindStringSubmatch(line); m != nil {
			pf.Symbols = append(pf.Symbols, Symbol{Name: m[1], Kind: "function", Exported: true, Line: lineNo})
			pf.Exports = append(pf.Exports, Export{Name: m[1], Kind: "function", Line: lineNo})
			continue
		}
		// type
		if m := adaType.FindStringSubmatch(line); m != nil {
			pf.Symbols = append(pf.Symbols, Symbol{Name: m[1], Kind: "type", Exported: true, Line: lineNo})
			pf.Exports = append(pf.Exports, Export{Name: m[1], Kind: "type", Line: lineNo})
			continue
		}
		// subtype
		if m := adaSubtype.FindStringSubmatch(line); m != nil {
			pf.Symbols = append(pf.Symbols, Symbol{Name: m[1], Kind: "type", Exported: true, Line: lineNo})
			continue
		}
		// task
		if m := adaTask.FindStringSubmatch(line); m != nil {
			pf.Symbols = append(pf.Symbols, Symbol{Name: m[1], Kind: "type", Exported: true, Line: lineNo})
			continue
		}
		// protected
		if m := adaProtected.FindStringSubmatch(line); m != nil {
			pf.Symbols = append(pf.Symbols, Symbol{Name: m[1], Kind: "type", Exported: true, Line: lineNo})
			continue
		}
	}

	return pf, nil
}
