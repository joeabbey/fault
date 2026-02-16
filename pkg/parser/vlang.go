package parser

import (
	"regexp"
	"strings"
)

// VlangParser extracts structural information from V language files via regex.
type VlangParser struct{}

func NewVlangParser() *VlangParser { return &VlangParser{} }

func (p *VlangParser) Language() string { return "vlang" }

var (
	reVlangImport    = regexp.MustCompile(`(?m)^import\s+([\w.]+)(?:\s*\{([^}]+)\})?`)
	reVlangMethod    = regexp.MustCompile(`(?m)^(?:pub\s+)?fn\s+\(\w+\s+\w+\)\s+(\w+)\s*\(([^)]*)\)`)
	reVlangFn        = regexp.MustCompile(`(?m)^(?:pub\s+)?fn\s+(\w+)\s*\(([^)]*)\)`)
	reVlangStruct    = regexp.MustCompile(`(?m)^(?:pub\s+)?struct\s+(\w+)`)
	reVlangEnum      = regexp.MustCompile(`(?m)^(?:pub\s+)?enum\s+(\w+)`)
	reVlangInterface = regexp.MustCompile(`(?m)^(?:pub\s+)?interface\s+(\w+)`)
	reVlangType      = regexp.MustCompile(`(?m)^(?:pub\s+)?type\s+(\w+)\s*=`)
	reVlangConst     = regexp.MustCompile(`(?m)^(?:pub\s+)?const\s+(\w+)\s*=`)
	reVlangPubPrefix = regexp.MustCompile(`^pub\s+`)
)

func (p *VlangParser) Parse(filename string, content []byte) (*ParsedFile, error) {
	pf := &ParsedFile{
		Path:     filename,
		Language: "vlang",
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
		if trimmed == "" || strings.HasPrefix(trimmed, "//") {
			continue
		}
		if strings.HasPrefix(trimmed, "/*") {
			if !strings.Contains(trimmed, "*/") {
				inBlockComment = true
			}
			continue
		}

		p.extractVlangImports(pf, trimmed, lineNo)
		p.extractVlangSymbols(pf, trimmed, lineNo)
	}

	pf.Symbols = deduplicateSymbols(pf.Symbols)
	return pf, nil
}

func (p *VlangParser) extractVlangImports(pf *ParsedFile, line string, lineNo int) {
	if !strings.HasPrefix(line, "import ") {
		return
	}
	if m := reVlangImport.FindStringSubmatch(line); m != nil {
		names := make([]string, 0)
		if m[2] != "" {
			for _, n := range strings.Split(m[2], ",") {
				n = strings.TrimSpace(n)
				if n != "" {
					names = append(names, n)
				}
			}
		} else {
			parts := strings.Split(m[1], ".")
			names = append(names, parts[len(parts)-1])
		}
		pf.Imports = append(pf.Imports, Import{Path: m[1], Names: names, Line: lineNo})
	}
}

func (p *VlangParser) extractVlangSymbols(pf *ParsedFile, line string, lineNo int) {
	if strings.HasPrefix(line, "import ") || strings.HasPrefix(line, "module ") {
		return
	}

	exported := reVlangPubPrefix.MatchString(line)

	// Methods
	if m := reVlangMethod.FindStringSubmatch(line); m != nil {
		pf.Symbols = append(pf.Symbols, Symbol{Name: m[1], Kind: "method", Exported: exported, Line: lineNo, Signature: "fn " + m[1] + "(" + strings.TrimSpace(m[2]) + ")"})
		if exported {
			pf.Exports = append(pf.Exports, Export{Name: m[1], Kind: "function", Line: lineNo})
		}
		return
	}
	// Functions
	if m := reVlangFn.FindStringSubmatch(line); m != nil {
		pf.Symbols = append(pf.Symbols, Symbol{Name: m[1], Kind: "function", Exported: exported, Line: lineNo, Signature: "fn " + m[1] + "(" + strings.TrimSpace(m[2]) + ")"})
		if exported {
			pf.Exports = append(pf.Exports, Export{Name: m[1], Kind: "function", Line: lineNo})
		}
		return
	}
	// Structs
	if m := reVlangStruct.FindStringSubmatch(line); m != nil {
		pf.Symbols = append(pf.Symbols, Symbol{Name: m[1], Kind: "struct", Exported: exported, Line: lineNo})
		if exported {
			pf.Exports = append(pf.Exports, Export{Name: m[1], Kind: "type", Line: lineNo})
		}
		return
	}
	// Enums
	if m := reVlangEnum.FindStringSubmatch(line); m != nil {
		pf.Symbols = append(pf.Symbols, Symbol{Name: m[1], Kind: "type", Exported: exported, Line: lineNo})
		if exported {
			pf.Exports = append(pf.Exports, Export{Name: m[1], Kind: "type", Line: lineNo})
		}
		return
	}
	// Interfaces
	if m := reVlangInterface.FindStringSubmatch(line); m != nil {
		pf.Symbols = append(pf.Symbols, Symbol{Name: m[1], Kind: "interface", Exported: exported, Line: lineNo})
		if exported {
			pf.Exports = append(pf.Exports, Export{Name: m[1], Kind: "type", Line: lineNo})
		}
		return
	}
	// Type aliases
	if m := reVlangType.FindStringSubmatch(line); m != nil {
		pf.Symbols = append(pf.Symbols, Symbol{Name: m[1], Kind: "type", Exported: exported, Line: lineNo})
		if exported {
			pf.Exports = append(pf.Exports, Export{Name: m[1], Kind: "type", Line: lineNo})
		}
		return
	}
	// Constants
	if m := reVlangConst.FindStringSubmatch(line); m != nil {
		pf.Symbols = append(pf.Symbols, Symbol{Name: m[1], Kind: "variable", Exported: exported, Line: lineNo})
		if exported {
			pf.Exports = append(pf.Exports, Export{Name: m[1], Kind: "variable", Line: lineNo})
		}
		return
	}
}
