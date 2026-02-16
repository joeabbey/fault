package parser

import (
	"regexp"
	"strings"
)

// DlangParser extracts structural information from D language files via regex.
type DlangParser struct{}

func NewDlangParser() *DlangParser { return &DlangParser{} }

func (p *DlangParser) Language() string { return "dlang" }

var (
	reDlangImport    = regexp.MustCompile(`(?m)^(?:static\s+)?import\s+([\w.]+)(?:\s*:\s*(.+?))?;`)
	reDlangClass     = regexp.MustCompile(`(?m)^(?:(private|package|protected|public|export)\s+)?(?:abstract\s+|final\s+)*class\s+(\w+)`)
	reDlangStruct    = regexp.MustCompile(`(?m)^(?:(private|package|protected|public|export)\s+)?struct\s+(\w+)`)
	reDlangInterface = regexp.MustCompile(`(?m)^(?:(private|package|protected|public|export)\s+)?interface\s+(\w+)`)
	reDlangEnum      = regexp.MustCompile(`(?m)^(?:(private|package|protected|public|export)\s+)?enum\s+(\w+)`)
	reDlangAlias     = regexp.MustCompile(`(?m)^(?:(private|package|protected|public|export)\s+)?alias\s+(\w+)\s*=`)
	reDlangTemplate  = regexp.MustCompile(`(?m)^(?:(private|package|protected|public|export)\s+)?(?:mixin\s+)?template\s+(\w+)`)
	reDlangFunc      = regexp.MustCompile(`(?m)^(?:(private|package|protected|public|export)\s+)?(?:static\s+)?(?:pure\s+|nothrow\s+|@\w+\s+)*(?:auto|void|int|uint|long|ulong|float|double|real|bool|char|wchar|dchar|string|wstring|dstring|size_t|[\w.]+(?:\[[\w.,\s]*\])?)\s+(\w+)\s*(?:\([^)]*\))?\s*\(([^)]*)\)`)
)

func dlangExported(access string) bool {
	return access != "private" && access != "package"
}

func (p *DlangParser) Parse(filename string, content []byte) (*ParsedFile, error) {
	pf := &ParsedFile{
		Path:     filename,
		Language: "dlang",
		Imports:  make([]Import, 0),
		Exports:  make([]Export, 0),
		Symbols:  make([]Symbol, 0),
	}

	lines := strings.Split(string(content), "\n")
	inBlockComment := false
	inNestingComment := false
	nestingLevel := 0

	for lineNum, line := range lines {
		lineNo := lineNum + 1
		trimmed := strings.TrimSpace(line)

		if inNestingComment {
			nestingLevel += strings.Count(trimmed, "/+")
			nestingLevel -= strings.Count(trimmed, "+/")
			if nestingLevel <= 0 {
				inNestingComment = false
				nestingLevel = 0
			}
			continue
		}
		if inBlockComment {
			if strings.Contains(trimmed, "*/") {
				inBlockComment = false
			}
			continue
		}
		if trimmed == "" || strings.HasPrefix(trimmed, "//") {
			continue
		}
		if strings.HasPrefix(trimmed, "/+") {
			nestingLevel = 1 + strings.Count(trimmed[2:], "/+") - strings.Count(trimmed[2:], "+/")
			if nestingLevel > 0 {
				inNestingComment = true
			}
			continue
		}
		if strings.HasPrefix(trimmed, "/*") {
			if !strings.Contains(trimmed, "*/") {
				inBlockComment = true
			}
			continue
		}

		p.extractDlangImports(pf, trimmed, lineNo)
		p.extractDlangSymbols(pf, trimmed, lineNo)
	}

	pf.Symbols = deduplicateSymbols(pf.Symbols)
	return pf, nil
}

func (p *DlangParser) extractDlangImports(pf *ParsedFile, line string, lineNo int) {
	if !strings.Contains(line, "import ") {
		return
	}
	if strings.HasPrefix(line, "module ") {
		return
	}
	if m := reDlangImport.FindStringSubmatch(line); m != nil {
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

func (p *DlangParser) extractDlangSymbols(pf *ParsedFile, line string, lineNo int) {
	if strings.Contains(line, "import ") || strings.HasPrefix(line, "module ") {
		return
	}
	// Classes
	if m := reDlangClass.FindStringSubmatch(line); m != nil {
		exp := dlangExported(m[1])
		pf.Symbols = append(pf.Symbols, Symbol{Name: m[2], Kind: "class", Exported: exp, Line: lineNo})
		if exp {
			pf.Exports = append(pf.Exports, Export{Name: m[2], Kind: "type", Line: lineNo})
		}
		return
	}
	// Structs
	if m := reDlangStruct.FindStringSubmatch(line); m != nil {
		exp := dlangExported(m[1])
		pf.Symbols = append(pf.Symbols, Symbol{Name: m[2], Kind: "struct", Exported: exp, Line: lineNo})
		if exp {
			pf.Exports = append(pf.Exports, Export{Name: m[2], Kind: "type", Line: lineNo})
		}
		return
	}
	// Interfaces
	if m := reDlangInterface.FindStringSubmatch(line); m != nil {
		exp := dlangExported(m[1])
		pf.Symbols = append(pf.Symbols, Symbol{Name: m[2], Kind: "interface", Exported: exp, Line: lineNo})
		if exp {
			pf.Exports = append(pf.Exports, Export{Name: m[2], Kind: "type", Line: lineNo})
		}
		return
	}
	// Enums
	if m := reDlangEnum.FindStringSubmatch(line); m != nil {
		exp := dlangExported(m[1])
		pf.Symbols = append(pf.Symbols, Symbol{Name: m[2], Kind: "type", Exported: exp, Line: lineNo})
		if exp {
			pf.Exports = append(pf.Exports, Export{Name: m[2], Kind: "type", Line: lineNo})
		}
		return
	}
	// Templates
	if m := reDlangTemplate.FindStringSubmatch(line); m != nil {
		exp := dlangExported(m[1])
		pf.Symbols = append(pf.Symbols, Symbol{Name: m[2], Kind: "function", Exported: exp, Line: lineNo})
		if exp {
			pf.Exports = append(pf.Exports, Export{Name: m[2], Kind: "function", Line: lineNo})
		}
		return
	}
	// Aliases
	if m := reDlangAlias.FindStringSubmatch(line); m != nil {
		exp := dlangExported(m[1])
		pf.Symbols = append(pf.Symbols, Symbol{Name: m[2], Kind: "type", Exported: exp, Line: lineNo})
		if exp {
			pf.Exports = append(pf.Exports, Export{Name: m[2], Kind: "type", Line: lineNo})
		}
		return
	}
	// Functions
	if m := reDlangFunc.FindStringSubmatch(line); m != nil {
		exp := dlangExported(m[1])
		pf.Symbols = append(pf.Symbols, Symbol{Name: m[2], Kind: "function", Exported: exp, Line: lineNo, Signature: m[2] + "(" + strings.TrimSpace(m[3]) + ")"})
		if exp {
			pf.Exports = append(pf.Exports, Export{Name: m[2], Kind: "function", Line: lineNo})
		}
		return
	}
}
