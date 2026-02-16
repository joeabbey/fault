package parser

import (
	"regexp"
	"strings"
)

// BashParser extracts structural information from Bash/shell script files via regex.
type BashParser struct{}

// NewBashParser creates a new Bash parser.
func NewBashParser() *BashParser {
	return &BashParser{}
}

// Language returns "bash".
func (p *BashParser) Language() string {
	return "bash"
}

// Regex patterns for Bash parsing.
var (
	// source script.sh or source ./path/script.sh
	bashSource = regexp.MustCompile(`(?m)^\s*source\s+(.+)`)
	// . ./script.sh or . /path/script.sh (dot-source)
	bashDotSource = regexp.MustCompile(`(?m)^\s*\.\s+(.+)`)
	// function_name() { or function_name () {
	bashFuncParens = regexp.MustCompile(`(?m)^(\s*)(\w+)\s*\(\)\s*\{?`)
	// function name { or function name() {
	bashFuncKeyword = regexp.MustCompile(`(?m)^(\s*)function\s+(\w+)(?:\s*\(\))?\s*\{?`)
	// export VAR=value or export VAR="value"
	bashExportVar = regexp.MustCompile(`(?m)^\s*export\s+(\w+)=`)
	// export -f function_name
	bashExportFunc = regexp.MustCompile(`(?m)^\s*export\s+-f\s+(\w+)`)
	// declare [-flags] VAR=value
	bashDeclare = regexp.MustCompile(`(?m)^\s*declare\s+(?:-\w+\s+)?(\w+)=`)
	// local VAR=value (only inside functions, but we detect it anyway)
	bashLocal = regexp.MustCompile(`(?m)^\s*local\s+(\w+)=`)
)

// Parse extracts structural info from Bash content.
func (p *BashParser) Parse(filename string, content []byte) (*ParsedFile, error) {
	pf := &ParsedFile{
		Path:     filename,
		Language: "bash",
		Imports:  make([]Import, 0),
		Exports:  make([]Export, 0),
		Symbols:  make([]Symbol, 0),
	}

	lines := strings.Split(string(content), "\n")

	for lineNum, line := range lines {
		lineNo := lineNum + 1
		trimmed := strings.TrimSpace(line)

		// Skip empty lines and comments.
		if trimmed == "" || strings.HasPrefix(trimmed, "#") {
			continue
		}

		// Extract source statements.
		if matches := bashSource.FindStringSubmatch(line); matches != nil {
			path := cleanBashPath(matches[1])
			pf.Imports = append(pf.Imports, Import{
				Path: path,
				Line: lineNo,
			})
			continue
		}

		// Extract dot-source statements (. ./script.sh).
		if matches := bashDotSource.FindStringSubmatch(line); matches != nil {
			path := cleanBashPath(matches[1])
			pf.Imports = append(pf.Imports, Import{
				Path: path,
				Line: lineNo,
			})
			continue
		}

		// Extract export -f (function export, not a new symbol).
		if bashExportFunc.MatchString(line) {
			continue
		}

		// Extract function declarations with 'function' keyword (check before parens pattern).
		if matches := bashFuncKeyword.FindStringSubmatch(line); matches != nil {
			indent := len(matches[1])
			name := matches[2]

			pf.Symbols = append(pf.Symbols, Symbol{
				Name:      name,
				Kind:      "function",
				Exported:  true,
				Line:      lineNo,
				Signature: "function " + name,
			})

			if indent == 0 {
				pf.Exports = append(pf.Exports, Export{
					Name: name,
					Kind: "function",
					Line: lineNo,
				})
			}
			continue
		}

		// Extract function declarations with name() { pattern.
		if matches := bashFuncParens.FindStringSubmatch(line); matches != nil {
			indent := len(matches[1])
			name := matches[2]

			// Skip keywords that look like functions.
			if isBashKeyword(name) {
				continue
			}

			pf.Symbols = append(pf.Symbols, Symbol{
				Name:      name,
				Kind:      "function",
				Exported:  true,
				Line:      lineNo,
				Signature: name + "()",
			})

			if indent == 0 {
				pf.Exports = append(pf.Exports, Export{
					Name: name,
					Kind: "function",
					Line: lineNo,
				})
			}
			continue
		}

		// Extract export VAR=value.
		if matches := bashExportVar.FindStringSubmatch(line); matches != nil {
			name := matches[1]

			pf.Symbols = append(pf.Symbols, Symbol{
				Name:     name,
				Kind:     "variable",
				Exported: true,
				Line:     lineNo,
			})

			pf.Exports = append(pf.Exports, Export{
				Name: name,
				Kind: "variable",
				Line: lineNo,
			})
			continue
		}

		// Extract declare statements.
		if matches := bashDeclare.FindStringSubmatch(line); matches != nil {
			name := matches[1]

			pf.Symbols = append(pf.Symbols, Symbol{
				Name:     name,
				Kind:     "variable",
				Exported: false,
				Line:     lineNo,
			})
			continue
		}

		// Extract local variable declarations.
		if matches := bashLocal.FindStringSubmatch(line); matches != nil {
			name := matches[1]

			pf.Symbols = append(pf.Symbols, Symbol{
				Name:     name,
				Kind:     "variable",
				Exported: false,
				Line:     lineNo,
			})
			continue
		}
	}

	return pf, nil
}

// cleanBashPath removes quotes and trailing comments from a sourced path.
func cleanBashPath(s string) string {
	s = strings.TrimSpace(s)
	// Remove surrounding quotes.
	if len(s) >= 2 && (s[0] == '"' || s[0] == '\'') {
		s = s[1 : len(s)-1]
	}
	// Remove trailing comments.
	if idx := strings.Index(s, " #"); idx >= 0 {
		s = s[:idx]
	}
	return strings.TrimSpace(s)
}

// isBashKeyword checks if a name is a Bash keyword that could be mistaken for a function.
func isBashKeyword(name string) bool {
	keywords := map[string]bool{
		"if": true, "then": true, "else": true, "elif": true, "fi": true,
		"for": true, "while": true, "do": true, "done": true, "case": true,
		"esac": true, "in": true, "select": true, "until": true, "time": true,
	}
	return keywords[name]
}
