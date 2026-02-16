package parser

import (
	"regexp"
	"strings"
)

type GroovyParser struct{}

func NewGroovyParser() *GroovyParser { return &GroovyParser{} }

func (p *GroovyParser) Language() string { return "groovy" }

var (
	groovyImport  = regexp.MustCompile(`(?m)^\s*import\s+([\w.]+)`)
	groovyClass   = regexp.MustCompile(`(?m)^\s*(?:public\s+|private\s+|protected\s+)?(?:abstract\s+)?class\s+(\w+)`)
	groovyDef     = regexp.MustCompile(`(?m)^\s*(?:public\s+|private\s+|protected\s+|static\s+)*def\s+(\w+)\s*\(`)
	groovyTypedFn = regexp.MustCompile(`(?m)^\s*(?:public\s+|private\s+|protected\s+|static\s+)*(?:void|int|String|boolean|def|Object|List|Map|long|double|float)\s+(\w+)\s*\(`)
)

func (p *GroovyParser) Parse(filename string, content []byte) (*ParsedFile, error) {
	pf := &ParsedFile{
		Path:     filename,
		Language: "groovy",
		Imports:  make([]Import, 0),
		Exports:  make([]Export, 0),
		Symbols:  make([]Symbol, 0),
	}

	lines := strings.Split(string(content), "\n")
	inBlockComment := false

	for lineNum, line := range lines {
		lineNo := lineNum + 1
		trimmed := strings.TrimSpace(line)

		// Handle block comments /* ... */
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

		if trimmed == "" || strings.HasPrefix(trimmed, "//") {
			continue
		}

		// Imports
		if m := groovyImport.FindStringSubmatch(line); m != nil {
			pf.Imports = append(pf.Imports, Import{Path: m[1], Line: lineNo})
			continue
		}

		// Class declarations
		if m := groovyClass.FindStringSubmatch(line); m != nil {
			name := m[1]
			exported := !strings.Contains(line, "private")
			pf.Symbols = append(pf.Symbols, Symbol{Name: name, Kind: "class", Exported: exported, Line: lineNo})
			if exported {
				pf.Exports = append(pf.Exports, Export{Name: name, Kind: "class", Line: lineNo})
			}
			continue
		}

		// Methods with def keyword
		if m := groovyDef.FindStringSubmatch(line); m != nil {
			name := m[1]
			exported := !strings.Contains(line, "private")
			pf.Symbols = append(pf.Symbols, Symbol{Name: name, Kind: "function", Exported: exported, Line: lineNo})
			if exported {
				pf.Exports = append(pf.Exports, Export{Name: name, Kind: "function", Line: lineNo})
			}
			continue
		}

		// Methods with typed return
		if m := groovyTypedFn.FindStringSubmatch(line); m != nil {
			name := m[1]
			exported := !strings.Contains(line, "private")
			pf.Symbols = append(pf.Symbols, Symbol{Name: name, Kind: "function", Exported: exported, Line: lineNo})
			if exported {
				pf.Exports = append(pf.Exports, Export{Name: name, Kind: "function", Line: lineNo})
			}
		}
	}

	return pf, nil
}
