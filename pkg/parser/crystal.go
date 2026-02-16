package parser

import (
	"regexp"
	"strings"
)

// CrystalParser extracts structural information from Crystal files via regex.
type CrystalParser struct{}

func NewCrystalParser() *CrystalParser { return &CrystalParser{} }

func (p *CrystalParser) Language() string { return "crystal" }

var (
	reCrystalRequire   = regexp.MustCompile(`(?m)^require\s+"([^"]+)"`)
	reCrystalSelfDef   = regexp.MustCompile(`(?m)^\s*(?:private\s+|protected\s+)?def\s+self\.(\w+[?!]?)\s*(?:\(([^)]*)\))?`)
	reCrystalDef       = regexp.MustCompile(`(?m)^\s*(?:private\s+|protected\s+)?def\s+(\w+[?!]?)\s*(?:\(([^)]*)\))?`)
	reCrystalClass     = regexp.MustCompile(`(?m)^\s*(?:private\s+|protected\s+)?(?:abstract\s+)?class\s+(\w+)`)
	reCrystalStruct    = regexp.MustCompile(`(?m)^\s*(?:private\s+|protected\s+)?(?:abstract\s+)?struct\s+(\w+)`)
	reCrystalModule    = regexp.MustCompile(`(?m)^\s*module\s+(\w+)`)
	reCrystalEnum      = regexp.MustCompile(`(?m)^\s*enum\s+(\w+)`)
	reCrystalConstant  = regexp.MustCompile(`(?m)^\s*([A-Z][A-Z0-9_]+)\s*=`)
	reCrystalPrivate   = regexp.MustCompile(`(?m)^\s*(?:private|protected)\s+`)
	reCrystalAlias     = regexp.MustCompile(`(?m)^\s*alias\s+(\w+)\s*=`)
)

func (p *CrystalParser) Parse(filename string, content []byte) (*ParsedFile, error) {
	pf := &ParsedFile{
		Path:     filename,
		Language: "crystal",
		Imports:  make([]Import, 0),
		Exports:  make([]Export, 0),
		Symbols:  make([]Symbol, 0),
	}

	lines := strings.Split(string(content), "\n")
	for lineNum, line := range lines {
		lineNo := lineNum + 1
		trimmed := strings.TrimSpace(line)
		if trimmed == "" || strings.HasPrefix(trimmed, "#") {
			continue
		}
		p.extractCrystalImports(pf, trimmed, lineNo)
		p.extractCrystalSymbols(pf, trimmed, lineNo)
	}

	pf.Symbols = deduplicateSymbols(pf.Symbols)
	return pf, nil
}

func (p *CrystalParser) extractCrystalImports(pf *ParsedFile, line string, lineNo int) {
	if !strings.HasPrefix(line, "require ") {
		return
	}
	if m := reCrystalRequire.FindStringSubmatch(line); m != nil {
		pf.Imports = append(pf.Imports, Import{Path: m[1], Names: []string{m[1]}, Line: lineNo})
	}
}

func (p *CrystalParser) extractCrystalSymbols(pf *ParsedFile, line string, lineNo int) {
	if strings.HasPrefix(line, "require ") {
		return
	}

	exported := !reCrystalPrivate.MatchString(line)

	// Self methods
	if m := reCrystalSelfDef.FindStringSubmatch(line); m != nil {
		params := ""
		if len(m) > 2 {
			params = m[2]
		}
		name := "self." + m[1]
		pf.Symbols = append(pf.Symbols, Symbol{Name: name, Kind: "function", Exported: exported, Line: lineNo, Signature: "def " + name + "(" + strings.TrimSpace(params) + ")"})
		if exported {
			pf.Exports = append(pf.Exports, Export{Name: name, Kind: "function", Line: lineNo})
		}
		return
	}
	// Instance methods
	if m := reCrystalDef.FindStringSubmatch(line); m != nil {
		params := ""
		if len(m) > 2 {
			params = m[2]
		}
		pf.Symbols = append(pf.Symbols, Symbol{Name: m[1], Kind: "function", Exported: exported, Line: lineNo, Signature: "def " + m[1] + "(" + strings.TrimSpace(params) + ")"})
		if exported {
			pf.Exports = append(pf.Exports, Export{Name: m[1], Kind: "function", Line: lineNo})
		}
		return
	}
	// Classes
	if m := reCrystalClass.FindStringSubmatch(line); m != nil {
		pf.Symbols = append(pf.Symbols, Symbol{Name: m[1], Kind: "class", Exported: exported, Line: lineNo})
		if exported {
			pf.Exports = append(pf.Exports, Export{Name: m[1], Kind: "type", Line: lineNo})
		}
		return
	}
	// Structs
	if m := reCrystalStruct.FindStringSubmatch(line); m != nil {
		pf.Symbols = append(pf.Symbols, Symbol{Name: m[1], Kind: "struct", Exported: exported, Line: lineNo})
		if exported {
			pf.Exports = append(pf.Exports, Export{Name: m[1], Kind: "type", Line: lineNo})
		}
		return
	}
	// Modules
	if m := reCrystalModule.FindStringSubmatch(line); m != nil {
		pf.Symbols = append(pf.Symbols, Symbol{Name: m[1], Kind: "type", Exported: exported, Line: lineNo})
		if exported {
			pf.Exports = append(pf.Exports, Export{Name: m[1], Kind: "type", Line: lineNo})
		}
		return
	}
	// Enums
	if m := reCrystalEnum.FindStringSubmatch(line); m != nil {
		pf.Symbols = append(pf.Symbols, Symbol{Name: m[1], Kind: "type", Exported: exported, Line: lineNo})
		if exported {
			pf.Exports = append(pf.Exports, Export{Name: m[1], Kind: "type", Line: lineNo})
		}
		return
	}
	// Aliases
	if m := reCrystalAlias.FindStringSubmatch(line); m != nil {
		pf.Symbols = append(pf.Symbols, Symbol{Name: m[1], Kind: "type", Exported: exported, Line: lineNo})
		if exported {
			pf.Exports = append(pf.Exports, Export{Name: m[1], Kind: "type", Line: lineNo})
		}
		return
	}
	// Constants
	if m := reCrystalConstant.FindStringSubmatch(line); m != nil {
		pf.Symbols = append(pf.Symbols, Symbol{Name: m[1], Kind: "variable", Exported: exported, Line: lineNo})
		if exported {
			pf.Exports = append(pf.Exports, Export{Name: m[1], Kind: "variable", Line: lineNo})
		}
		return
	}
}
