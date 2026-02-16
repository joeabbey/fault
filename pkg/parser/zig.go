package parser

import (
	"regexp"
	"strings"
)

// ZigParser extracts structural information from Zig files via regex.
type ZigParser struct{}

// NewZigParser creates a new Zig parser.
func NewZigParser() *ZigParser {
	return &ZigParser{}
}

// Language returns "zig".
func (p *ZigParser) Language() string {
	return "zig"
}

// Regex patterns for Zig parsing.
var (
	reZigImport    = regexp.MustCompile(`(?m)^(?:pub\s+)?const\s+(\w+)\s*=\s*@import\(\s*"([^"]+)"\s*\)`)
	reZigFn        = regexp.MustCompile(`(?m)^(?:pub\s+)?fn\s+(\w+)\s*\(([^)]*)\)`)
	reZigConst     = regexp.MustCompile(`(?m)^(?:pub\s+)?const\s+(\w+)\s*=`)
	reZigVar       = regexp.MustCompile(`(?m)^(?:pub\s+)?var\s+(\w+)`)
	reZigPubPrefix = regexp.MustCompile(`^pub\s+`)
)

// Parse extracts structural info from Zig content.
func (p *ZigParser) Parse(filename string, content []byte) (*ParsedFile, error) {
	pf := &ParsedFile{
		Path:     filename,
		Language: "zig",
		Imports:  make([]Import, 0),
		Exports:  make([]Export, 0),
		Symbols:  make([]Symbol, 0),
	}

	lines := strings.Split(string(content), "\n")

	for lineNum, line := range lines {
		lineNo := lineNum + 1
		trimmed := strings.TrimSpace(line)

		// Skip empty lines and comments
		if trimmed == "" || strings.HasPrefix(trimmed, "//") {
			continue
		}

		p.extractZigImports(pf, trimmed, lineNo)
		p.extractZigSymbols(pf, trimmed, lineNo)
	}

	pf.Symbols = deduplicateSymbols(pf.Symbols)
	return pf, nil
}

func (p *ZigParser) extractZigImports(pf *ParsedFile, line string, lineNo int) {
	if !strings.Contains(line, "@import") {
		return
	}
	if matches := reZigImport.FindStringSubmatch(line); matches != nil {
		pf.Imports = append(pf.Imports, Import{
			Path:  matches[2],
			Names: []string{matches[1]},
			Line:  lineNo,
		})
	}
}

func (p *ZigParser) extractZigSymbols(pf *ParsedFile, line string, lineNo int) {
	if strings.Contains(line, "@import") {
		return
	}

	exported := reZigPubPrefix.MatchString(line)

	// Functions
	if matches := reZigFn.FindStringSubmatch(line); matches != nil {
		name := matches[1]
		params := matches[2]
		pf.Symbols = append(pf.Symbols, Symbol{
			Name:      name,
			Kind:      "function",
			Exported:  exported,
			Line:      lineNo,
			Signature: "fn " + name + "(" + strings.TrimSpace(params) + ")",
		})
		if exported {
			pf.Exports = append(pf.Exports, Export{Name: name, Kind: "function", Line: lineNo})
		}
		return
	}

	// Constants
	if matches := reZigConst.FindStringSubmatch(line); matches != nil {
		name := matches[1]
		kind := "variable"
		if strings.Contains(line, "struct") || strings.Contains(line, "enum") || strings.Contains(line, "union") {
			kind = "type"
		}
		pf.Symbols = append(pf.Symbols, Symbol{Name: name, Kind: kind, Exported: exported, Line: lineNo})
		if exported {
			pf.Exports = append(pf.Exports, Export{Name: name, Kind: kind, Line: lineNo})
		}
		return
	}

	// Variables
	if matches := reZigVar.FindStringSubmatch(line); matches != nil {
		name := matches[1]
		pf.Symbols = append(pf.Symbols, Symbol{Name: name, Kind: "variable", Exported: exported, Line: lineNo})
		if exported {
			pf.Exports = append(pf.Exports, Export{Name: name, Kind: "variable", Line: lineNo})
		}
		return
	}
}
