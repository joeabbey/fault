package parser

import (
	"regexp"
	"strings"
)

// SQLParser extracts structural information from SQL files via regex.
type SQLParser struct{}

// NewSQLParser creates a new SQL parser.
func NewSQLParser() *SQLParser {
	return &SQLParser{}
}

// Language returns "sql".
func (p *SQLParser) Language() string {
	return "sql"
}

// Regex patterns for SQL parsing.
var (
	// CREATE [OR REPLACE] TABLE [IF NOT EXISTS] name
	sqlCreateTable = regexp.MustCompile(`(?i)^\s*CREATE\s+TABLE\s+(?:IF\s+NOT\s+EXISTS\s+)?(\w+)`)
	// CREATE [OR REPLACE] VIEW name
	sqlCreateView = regexp.MustCompile(`(?i)^\s*CREATE\s+(?:OR\s+REPLACE\s+)?VIEW\s+(\w+)`)
	// CREATE [OR REPLACE] FUNCTION name
	sqlCreateFunction = regexp.MustCompile(`(?i)^\s*CREATE\s+(?:OR\s+REPLACE\s+)?FUNCTION\s+(\w+)`)
	// CREATE [OR REPLACE] PROCEDURE name
	sqlCreateProcedure = regexp.MustCompile(`(?i)^\s*CREATE\s+(?:OR\s+REPLACE\s+)?PROCEDURE\s+(\w+)`)
	// CREATE TRIGGER name
	sqlCreateTrigger = regexp.MustCompile(`(?i)^\s*CREATE\s+TRIGGER\s+(\w+)`)
	// CREATE [UNIQUE] INDEX [IF NOT EXISTS] name
	sqlCreateIndex = regexp.MustCompile(`(?i)^\s*CREATE\s+(?:UNIQUE\s+)?INDEX\s+(?:IF\s+NOT\s+EXISTS\s+)?(\w+)`)
)

// Parse extracts structural info from SQL content.
func (p *SQLParser) Parse(filename string, content []byte) (*ParsedFile, error) {
	pf := &ParsedFile{
		Path:     filename,
		Language: "sql",
		Imports:  make([]Import, 0),
		Exports:  make([]Export, 0),
		Symbols:  make([]Symbol, 0),
	}

	lines := strings.Split(string(content), "\n")
	inBlockComment := false

	for lineNum, line := range lines {
		lineNo := lineNum + 1
		trimmed := strings.TrimSpace(line)

		// Handle block comments.
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

		// Skip empty lines and line comments.
		if trimmed == "" || strings.HasPrefix(trimmed, "--") {
			continue
		}

		// Extract CREATE TABLE.
		if matches := sqlCreateTable.FindStringSubmatch(line); matches != nil {
			name := matches[1]
			pf.Symbols = append(pf.Symbols, Symbol{
				Name:     name,
				Kind:     "table",
				Exported: true,
				Line:     lineNo,
			})
			pf.Exports = append(pf.Exports, Export{
				Name: name,
				Kind: "table",
				Line: lineNo,
			})
			continue
		}

		// Extract CREATE VIEW.
		if matches := sqlCreateView.FindStringSubmatch(line); matches != nil {
			name := matches[1]
			pf.Symbols = append(pf.Symbols, Symbol{
				Name:     name,
				Kind:     "view",
				Exported: true,
				Line:     lineNo,
			})
			pf.Exports = append(pf.Exports, Export{
				Name: name,
				Kind: "view",
				Line: lineNo,
			})
			continue
		}

		// Extract CREATE FUNCTION.
		if matches := sqlCreateFunction.FindStringSubmatch(line); matches != nil {
			name := matches[1]
			pf.Symbols = append(pf.Symbols, Symbol{
				Name:     name,
				Kind:     "function",
				Exported: true,
				Line:     lineNo,
			})
			pf.Exports = append(pf.Exports, Export{
				Name: name,
				Kind: "function",
				Line: lineNo,
			})
			continue
		}

		// Extract CREATE PROCEDURE.
		if matches := sqlCreateProcedure.FindStringSubmatch(line); matches != nil {
			name := matches[1]
			pf.Symbols = append(pf.Symbols, Symbol{
				Name:     name,
				Kind:     "procedure",
				Exported: true,
				Line:     lineNo,
			})
			pf.Exports = append(pf.Exports, Export{
				Name: name,
				Kind: "procedure",
				Line: lineNo,
			})
			continue
		}

		// Extract CREATE TRIGGER.
		if matches := sqlCreateTrigger.FindStringSubmatch(line); matches != nil {
			name := matches[1]
			pf.Symbols = append(pf.Symbols, Symbol{
				Name:     name,
				Kind:     "trigger",
				Exported: true,
				Line:     lineNo,
			})
			pf.Exports = append(pf.Exports, Export{
				Name: name,
				Kind: "trigger",
				Line: lineNo,
			})
			continue
		}

		// Extract CREATE INDEX.
		if matches := sqlCreateIndex.FindStringSubmatch(line); matches != nil {
			name := matches[1]
			pf.Symbols = append(pf.Symbols, Symbol{
				Name:     name,
				Kind:     "index",
				Exported: true,
				Line:     lineNo,
			})
			pf.Exports = append(pf.Exports, Export{
				Name: name,
				Kind: "index",
				Line: lineNo,
			})
			continue
		}
	}

	return pf, nil
}
