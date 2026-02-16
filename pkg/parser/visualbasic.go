package parser

import (
	"regexp"
	"strings"
)

type VisualBasicParser struct{}

func NewVisualBasicParser() *VisualBasicParser { return &VisualBasicParser{} }

func (p *VisualBasicParser) Language() string { return "visualbasic" }

var (
	vbImports   = regexp.MustCompile(`(?mi)^\s*Imports\s+([\w.]+)`)
	vbClass     = regexp.MustCompile(`(?mi)^\s*(?:Public\s+|Private\s+|Friend\s+)?(?:MustInherit\s+|NotInheritable\s+)?Class\s+(\w+)`)
	vbModule    = regexp.MustCompile(`(?mi)^\s*(?:Public\s+|Friend\s+)?Module\s+(\w+)`)
	vbInterface = regexp.MustCompile(`(?mi)^\s*(?:Public\s+|Private\s+|Friend\s+)?Interface\s+(\w+)`)
	vbFunction  = regexp.MustCompile(`(?mi)^\s*(?:Public\s+|Private\s+|Protected\s+|Friend\s+)?(?:Shared\s+)?(?:Overrides\s+|Overridable\s+|MustOverride\s+)?(?:Async\s+)?Function\s+(\w+)`)
	vbSub       = regexp.MustCompile(`(?mi)^\s*(?:Public\s+|Private\s+|Protected\s+|Friend\s+)?(?:Shared\s+)?(?:Overrides\s+|Overridable\s+|MustOverride\s+)?(?:Async\s+)?Sub\s+(\w+)`)
	vbProperty  = regexp.MustCompile(`(?mi)^\s*(?:Public\s+|Private\s+|Protected\s+|Friend\s+)?(?:Shared\s+)?(?:ReadOnly\s+|WriteOnly\s+)?Property\s+(\w+)`)
	vbEnum      = regexp.MustCompile(`(?mi)^\s*(?:Public\s+|Private\s+|Friend\s+)?Enum\s+(\w+)`)
	vbStruct    = regexp.MustCompile(`(?mi)^\s*(?:Public\s+|Private\s+|Friend\s+)?Structure\s+(\w+)`)
)

func (p *VisualBasicParser) Parse(filename string, content []byte) (*ParsedFile, error) {
	pf := &ParsedFile{
		Path:     filename,
		Language: "visualbasic",
		Imports:  make([]Import, 0),
		Exports:  make([]Export, 0),
		Symbols:  make([]Symbol, 0),
	}

	lines := strings.Split(string(content), "\n")

	for lineNum, line := range lines {
		lineNo := lineNum + 1
		trimmed := strings.TrimSpace(line)

		if trimmed == "" || strings.HasPrefix(trimmed, "'") || strings.HasPrefix(strings.ToUpper(trimmed), "REM ") {
			continue
		}

		// Imports
		if m := vbImports.FindStringSubmatch(line); m != nil {
			pf.Imports = append(pf.Imports, Import{Path: m[1], Line: lineNo})
			continue
		}

		isPublic := !strings.Contains(strings.ToLower(line), "private")

		// Class
		if m := vbClass.FindStringSubmatch(line); m != nil {
			pf.Symbols = append(pf.Symbols, Symbol{Name: m[1], Kind: "class", Exported: isPublic, Line: lineNo})
			if isPublic {
				pf.Exports = append(pf.Exports, Export{Name: m[1], Kind: "class", Line: lineNo})
			}
			continue
		}
		// Module
		if m := vbModule.FindStringSubmatch(line); m != nil {
			pf.Symbols = append(pf.Symbols, Symbol{Name: m[1], Kind: "module", Exported: isPublic, Line: lineNo})
			if isPublic {
				pf.Exports = append(pf.Exports, Export{Name: m[1], Kind: "module", Line: lineNo})
			}
			continue
		}
		// Interface
		if m := vbInterface.FindStringSubmatch(line); m != nil {
			pf.Symbols = append(pf.Symbols, Symbol{Name: m[1], Kind: "interface", Exported: isPublic, Line: lineNo})
			if isPublic {
				pf.Exports = append(pf.Exports, Export{Name: m[1], Kind: "interface", Line: lineNo})
			}
			continue
		}
		// Structure
		if m := vbStruct.FindStringSubmatch(line); m != nil {
			pf.Symbols = append(pf.Symbols, Symbol{Name: m[1], Kind: "type", Exported: isPublic, Line: lineNo})
			if isPublic {
				pf.Exports = append(pf.Exports, Export{Name: m[1], Kind: "type", Line: lineNo})
			}
			continue
		}
		// Enum
		if m := vbEnum.FindStringSubmatch(line); m != nil {
			pf.Symbols = append(pf.Symbols, Symbol{Name: m[1], Kind: "type", Exported: isPublic, Line: lineNo})
			if isPublic {
				pf.Exports = append(pf.Exports, Export{Name: m[1], Kind: "type", Line: lineNo})
			}
			continue
		}
		// Function
		if m := vbFunction.FindStringSubmatch(line); m != nil {
			pf.Symbols = append(pf.Symbols, Symbol{Name: m[1], Kind: "function", Exported: isPublic, Line: lineNo})
			if isPublic {
				pf.Exports = append(pf.Exports, Export{Name: m[1], Kind: "function", Line: lineNo})
			}
			continue
		}
		// Sub
		if m := vbSub.FindStringSubmatch(line); m != nil {
			pf.Symbols = append(pf.Symbols, Symbol{Name: m[1], Kind: "function", Exported: isPublic, Line: lineNo})
			if isPublic {
				pf.Exports = append(pf.Exports, Export{Name: m[1], Kind: "function", Line: lineNo})
			}
			continue
		}
		// Property
		if m := vbProperty.FindStringSubmatch(line); m != nil {
			pf.Symbols = append(pf.Symbols, Symbol{Name: m[1], Kind: "property", Exported: isPublic, Line: lineNo})
			if isPublic {
				pf.Exports = append(pf.Exports, Export{Name: m[1], Kind: "property", Line: lineNo})
			}
			continue
		}
	}

	return pf, nil
}
