package parser

import (
	"regexp"
	"strings"
)

// NimParser extracts structural information from Nim files via regex.
type NimParser struct{}

func NewNimParser() *NimParser { return &NimParser{} }

func (p *NimParser) Language() string { return "nim" }

var (
	reNimImport     = regexp.MustCompile(`(?m)^import\s+(\w+)`)
	reNimFromImport = regexp.MustCompile(`(?m)^from\s+(\w+)\s+import\s+(.+)`)
	reNimInclude    = regexp.MustCompile(`(?m)^include\s+(\w+)`)
	reNimProc       = regexp.MustCompile(`(?m)^proc\s+(\w+)(\*?)\s*\(([^)]*)\)`)
	reNimFunc       = regexp.MustCompile(`(?m)^func\s+(\w+)(\*?)\s*\(([^)]*)\)`)
	reNimMethod     = regexp.MustCompile(`(?m)^method\s+(\w+)(\*?)\s*\(([^)]*)\)`)
	reNimType       = regexp.MustCompile(`(?m)^\s*(\w+)(\*?)\s*=\s*(?:ref\s+)?(?:object|enum|distinct|tuple|concept)`)
	reNimTemplate   = regexp.MustCompile(`(?m)^template\s+(\w+)(\*?)`)
	reNimMacro      = regexp.MustCompile(`(?m)^macro\s+(\w+)(\*?)`)
	reNimConst      = regexp.MustCompile(`(?m)^(?:const|let|var)\s+(\w+)(\*?)\s*(?::\s*\w+)?\s*=`)
)

func (p *NimParser) Parse(filename string, content []byte) (*ParsedFile, error) {
	pf := &ParsedFile{
		Path:     filename,
		Language: "nim",
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
			if strings.Contains(trimmed, "]#") {
				inBlockComment = false
			}
			continue
		}
		if trimmed == "" {
			continue
		}
		if strings.HasPrefix(trimmed, "#[") {
			if !strings.Contains(trimmed, "]#") {
				inBlockComment = true
			}
			continue
		}
		if strings.HasPrefix(trimmed, "#") {
			continue
		}

		p.extractNimImports(pf, trimmed, lineNo)
		p.extractNimSymbols(pf, trimmed, lineNo)
	}

	pf.Symbols = deduplicateSymbols(pf.Symbols)
	return pf, nil
}

func (p *NimParser) extractNimImports(pf *ParsedFile, line string, lineNo int) {
	if matches := reNimFromImport.FindStringSubmatch(line); matches != nil {
		names := make([]string, 0)
		for _, n := range strings.Split(matches[2], ",") {
			n = strings.TrimSpace(n)
			if n != "" {
				names = append(names, n)
			}
		}
		pf.Imports = append(pf.Imports, Import{Path: matches[1], Names: names, Line: lineNo})
		return
	}
	if matches := reNimImport.FindStringSubmatch(line); matches != nil {
		pf.Imports = append(pf.Imports, Import{Path: matches[1], Names: []string{matches[1]}, Line: lineNo})
		return
	}
	if matches := reNimInclude.FindStringSubmatch(line); matches != nil {
		pf.Imports = append(pf.Imports, Import{Path: matches[1], Names: []string{matches[1]}, Line: lineNo})
		return
	}
}

func (p *NimParser) extractNimSymbols(pf *ParsedFile, line string, lineNo int) {
	if strings.HasPrefix(line, "import ") || strings.HasPrefix(line, "from ") || strings.HasPrefix(line, "include ") {
		return
	}

	// Procs
	if m := reNimProc.FindStringSubmatch(line); m != nil {
		exp := m[2] == "*"
		pf.Symbols = append(pf.Symbols, Symbol{Name: m[1], Kind: "function", Exported: exp, Line: lineNo, Signature: "proc " + m[1] + "(" + strings.TrimSpace(m[3]) + ")"})
		if exp {
			pf.Exports = append(pf.Exports, Export{Name: m[1], Kind: "function", Line: lineNo})
		}
		return
	}
	// Funcs
	if m := reNimFunc.FindStringSubmatch(line); m != nil {
		exp := m[2] == "*"
		pf.Symbols = append(pf.Symbols, Symbol{Name: m[1], Kind: "function", Exported: exp, Line: lineNo, Signature: "func " + m[1] + "(" + strings.TrimSpace(m[3]) + ")"})
		if exp {
			pf.Exports = append(pf.Exports, Export{Name: m[1], Kind: "function", Line: lineNo})
		}
		return
	}
	// Methods
	if m := reNimMethod.FindStringSubmatch(line); m != nil {
		exp := m[2] == "*"
		pf.Symbols = append(pf.Symbols, Symbol{Name: m[1], Kind: "method", Exported: exp, Line: lineNo, Signature: "method " + m[1] + "(" + strings.TrimSpace(m[3]) + ")"})
		if exp {
			pf.Exports = append(pf.Exports, Export{Name: m[1], Kind: "function", Line: lineNo})
		}
		return
	}
	// Templates
	if m := reNimTemplate.FindStringSubmatch(line); m != nil {
		exp := m[2] == "*"
		pf.Symbols = append(pf.Symbols, Symbol{Name: m[1], Kind: "function", Exported: exp, Line: lineNo})
		if exp {
			pf.Exports = append(pf.Exports, Export{Name: m[1], Kind: "function", Line: lineNo})
		}
		return
	}
	// Macros
	if m := reNimMacro.FindStringSubmatch(line); m != nil {
		exp := m[2] == "*"
		pf.Symbols = append(pf.Symbols, Symbol{Name: m[1], Kind: "function", Exported: exp, Line: lineNo})
		if exp {
			pf.Exports = append(pf.Exports, Export{Name: m[1], Kind: "function", Line: lineNo})
		}
		return
	}
	// Types
	if m := reNimType.FindStringSubmatch(line); m != nil {
		exp := m[2] == "*"
		pf.Symbols = append(pf.Symbols, Symbol{Name: m[1], Kind: "type", Exported: exp, Line: lineNo})
		if exp {
			pf.Exports = append(pf.Exports, Export{Name: m[1], Kind: "type", Line: lineNo})
		}
		return
	}
	// Consts/vars
	if m := reNimConst.FindStringSubmatch(line); m != nil {
		exp := m[2] == "*"
		pf.Symbols = append(pf.Symbols, Symbol{Name: m[1], Kind: "variable", Exported: exp, Line: lineNo})
		if exp {
			pf.Exports = append(pf.Exports, Export{Name: m[1], Kind: "variable", Line: lineNo})
		}
		return
	}
}
