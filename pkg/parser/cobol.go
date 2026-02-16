package parser

import (
	"regexp"
	"strings"
)

type CobolParser struct{}

func NewCobolParser() *CobolParser { return &CobolParser{} }

func (p *CobolParser) Language() string { return "cobol" }

var (
	cobolCopy       = regexp.MustCompile(`(?mi)^\s*COPY\s+(\S+)`)
	cobolProgram    = regexp.MustCompile(`(?mi)^\s*PROGRAM-ID\.\s*(\S+)`)
	cobolSection    = regexp.MustCompile(`(?mi)^\s*(\w[\w-]+)\s+SECTION\s*\.`)
	cobolParagraph  = regexp.MustCompile(`(?m)^\s{0,6}\s(\w[\w-]+)\s*\.\s*$`)
	cobolPerform    = regexp.MustCompile(`(?mi)^\s*PERFORM\s+(\w[\w-]+)`)
	cobolCall       = regexp.MustCompile(`(?mi)^\s*CALL\s+['"](\w+)['"]`)
	cobolDataItem   = regexp.MustCompile(`(?mi)^\s*(?:01|77)\s+(\w[\w-]+)`)
)

func (p *CobolParser) Parse(filename string, content []byte) (*ParsedFile, error) {
	pf := &ParsedFile{
		Path:     filename,
		Language: "cobol",
		Imports:  make([]Import, 0),
		Exports:  make([]Export, 0),
		Symbols:  make([]Symbol, 0),
	}

	lines := strings.Split(string(content), "\n")

	for lineNum, line := range lines {
		lineNo := lineNum + 1

		// Skip comment lines: * in column 7 (index 6) for fixed-form
		if len(line) > 6 && (line[6] == '*' || line[6] == '/') {
			continue
		}
		trimmed := strings.TrimSpace(line)
		if trimmed == "" {
			continue
		}
		// Free-form comments
		if strings.HasPrefix(trimmed, "*>") {
			continue
		}

		upper := strings.ToUpper(trimmed)

		// COPY (import)
		if m := cobolCopy.FindStringSubmatch(line); m != nil {
			name := strings.TrimSuffix(strings.TrimSuffix(m[1], "."), ",")
			pf.Imports = append(pf.Imports, Import{Path: name, Line: lineNo})
			continue
		}
		// CALL (external program reference, treated as import)
		if m := cobolCall.FindStringSubmatch(line); m != nil {
			pf.Imports = append(pf.Imports, Import{Path: m[1], Line: lineNo})
			continue
		}
		// PROGRAM-ID
		if m := cobolProgram.FindStringSubmatch(line); m != nil {
			name := strings.TrimSuffix(m[1], ".")
			pf.Symbols = append(pf.Symbols, Symbol{Name: name, Kind: "program", Exported: true, Line: lineNo})
			pf.Exports = append(pf.Exports, Export{Name: name, Kind: "program", Line: lineNo})
			continue
		}
		// SECTION
		if m := cobolSection.FindStringSubmatch(line); m != nil {
			name := m[1]
			// Skip standard division sections
			nameUp := strings.ToUpper(name)
			if nameUp == "ENVIRONMENT" || nameUp == "DATA" || nameUp == "PROCEDURE" ||
				nameUp == "IDENTIFICATION" || nameUp == "CONFIGURATION" ||
				nameUp == "INPUT-OUTPUT" || nameUp == "FILE" || nameUp == "WORKING-STORAGE" ||
				nameUp == "LOCAL-STORAGE" || nameUp == "LINKAGE" {
				continue
			}
			pf.Symbols = append(pf.Symbols, Symbol{Name: name, Kind: "function", Exported: true, Line: lineNo})
			continue
		}
		// 01 / 77 level data items
		if m := cobolDataItem.FindStringSubmatch(line); m != nil {
			name := m[1]
			nameUp := strings.ToUpper(name)
			if nameUp == "FILLER" {
				continue
			}
			pf.Symbols = append(pf.Symbols, Symbol{Name: name, Kind: "variable", Exported: true, Line: lineNo})
			continue
		}
		// Paragraph (only in procedure division context - heuristic)
		if strings.Contains(upper, "PERFORM") || strings.Contains(upper, "CALL") {
			continue // already handled above
		}
		_ = cobolParagraph // available for future use
		_ = cobolPerform
	}

	return pf, nil
}
