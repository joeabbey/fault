package parser

import (
	"regexp"
	"strings"
)

// ElixirParser extracts structural information from Elixir files via regex.
type ElixirParser struct{}

// NewElixirParser creates a new Elixir parser.
func NewElixirParser() *ElixirParser {
	return &ElixirParser{}
}

// Language returns "elixir".
func (p *ElixirParser) Language() string {
	return "elixir"
}

// Regex patterns for Elixir parsing.
var (
	// defmodule Module.Name do
	reElixirModule = regexp.MustCompile(`^\s*defmodule\s+([\w.]+)\s+do`)

	// def name(args) do / def name(args),
	reElixirDef = regexp.MustCompile(`^\s*def\s+(\w+)(?:\(([^)]*)\))?`)

	// defp name(args) do / defp name(args),
	reElixirDefp = regexp.MustCompile(`^\s*defp\s+(\w+)(?:\(([^)]*)\))?`)

	// defmacro name(args)
	reElixirDefmacro = regexp.MustCompile(`^\s*defmacro\s+(\w+)(?:\(([^)]*)\))?`)

	// defstruct [...]
	reElixirDefstruct = regexp.MustCompile(`^\s*defstruct\s+`)

	// @callback name(args) :: return
	reElixirCallback = regexp.MustCompile(`^\s*@callback\s+(\w+)\(([^)]*)\)`)

	// import Module
	// import Module, only: [...]
	reElixirImport = regexp.MustCompile(`^\s*import\s+([\w.]+)`)

	// alias Module.SubModule
	// alias Module.SubModule, as: Name
	reElixirAlias = regexp.MustCompile(`^\s*alias\s+([\w.]+)(?:\s*,\s*as:\s*(\w+))?`)

	// use Module
	// use Module, opts
	reElixirUse = regexp.MustCompile(`^\s*use\s+([\w.]+)`)

	// require Module
	reElixirRequire = regexp.MustCompile(`^\s*require\s+([\w.]+)`)
)

// Parse extracts structural info from Elixir content.
func (p *ElixirParser) Parse(filename string, content []byte) (*ParsedFile, error) {
	pf := &ParsedFile{
		Path:     filename,
		Language: "elixir",
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

		// Skip @moduledoc and @doc strings
		if strings.HasPrefix(trimmed, "@moduledoc") || strings.HasPrefix(trimmed, "@doc") {
			continue
		}

		// import Module
		if matches := reElixirImport.FindStringSubmatch(trimmed); matches != nil {
			mod := matches[1]
			pf.Imports = append(pf.Imports, Import{
				Path: mod,
				Line: lineNo,
			})
			continue
		}

		// alias Module.SubModule, as: Name
		if matches := reElixirAlias.FindStringSubmatch(trimmed); matches != nil {
			mod := matches[1]
			names := make([]string, 0)
			if matches[2] != "" {
				names = append(names, matches[2])
			}
			pf.Imports = append(pf.Imports, Import{
				Path:  mod,
				Names: names,
				Line:  lineNo,
			})
			continue
		}

		// use Module
		if matches := reElixirUse.FindStringSubmatch(trimmed); matches != nil {
			mod := matches[1]
			pf.Imports = append(pf.Imports, Import{
				Path: mod,
				Line: lineNo,
			})
			continue
		}

		// require Module
		if matches := reElixirRequire.FindStringSubmatch(trimmed); matches != nil {
			mod := matches[1]
			pf.Imports = append(pf.Imports, Import{
				Path: mod,
				Line: lineNo,
			})
			continue
		}

		// defmodule
		if matches := reElixirModule.FindStringSubmatch(trimmed); matches != nil {
			name := matches[1]
			pf.Symbols = append(pf.Symbols, Symbol{
				Name:     name,
				Kind:     "module",
				Exported: true,
				Line:     lineNo,
			})
			pf.Exports = append(pf.Exports, Export{
				Name: name,
				Kind: "module",
				Line: lineNo,
			})
			continue
		}

		// defmacro (check before def to avoid prefix match)
		if matches := reElixirDefmacro.FindStringSubmatch(trimmed); matches != nil {
			name := matches[1]
			params := matches[2]
			sig := "defmacro " + name
			if params != "" {
				sig += "(" + strings.TrimSpace(params) + ")"
			}
			pf.Symbols = append(pf.Symbols, Symbol{
				Name:      name,
				Kind:      "macro",
				Exported:  true,
				Line:      lineNo,
				Signature: sig,
			})
			pf.Exports = append(pf.Exports, Export{
				Name: name,
				Kind: "macro",
				Line: lineNo,
			})
			continue
		}

		// defp (check before def to avoid prefix match)
		if matches := reElixirDefp.FindStringSubmatch(trimmed); matches != nil {
			name := matches[1]
			params := matches[2]
			sig := "defp " + name
			if params != "" {
				sig += "(" + strings.TrimSpace(params) + ")"
			}
			pf.Symbols = append(pf.Symbols, Symbol{
				Name:      name,
				Kind:      "function",
				Exported:  false,
				Line:      lineNo,
				Signature: sig,
			})
			continue
		}

		// def (public function)
		if matches := reElixirDef.FindStringSubmatch(trimmed); matches != nil {
			name := matches[1]
			params := matches[2]
			sig := "def " + name
			if params != "" {
				sig += "(" + strings.TrimSpace(params) + ")"
			}
			pf.Symbols = append(pf.Symbols, Symbol{
				Name:      name,
				Kind:      "function",
				Exported:  true,
				Line:      lineNo,
				Signature: sig,
			})
			pf.Exports = append(pf.Exports, Export{
				Name: name,
				Kind: "function",
				Line: lineNo,
			})
			continue
		}

		// defstruct
		if reElixirDefstruct.MatchString(trimmed) {
			pf.Symbols = append(pf.Symbols, Symbol{
				Name:     "__struct__",
				Kind:     "type",
				Exported: true,
				Line:     lineNo,
			})
			continue
		}

		// @callback
		if matches := reElixirCallback.FindStringSubmatch(trimmed); matches != nil {
			name := matches[1]
			params := matches[2]
			sig := "@callback " + name + "(" + strings.TrimSpace(params) + ")"
			pf.Symbols = append(pf.Symbols, Symbol{
				Name:      name,
				Kind:      "callback",
				Exported:  true,
				Line:      lineNo,
				Signature: sig,
			})
			continue
		}
	}

	return pf, nil
}
