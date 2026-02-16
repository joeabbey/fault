package parser

import (
	"fmt"
	"path/filepath"
	"strings"
	"sync"
)

// extensionToLanguage maps file extensions to language names.
var extensionToLanguage = map[string]string{
	".go":   "go",
	".ts":   "typescript",
	".tsx":  "typescript",
	".js":   "typescript",
	".jsx":  "typescript",
	".mjs":  "typescript",
	".cjs":  "typescript",
	".py":   "python",
	".java": "java",
	".rs":   "rust",
	".rb":    "ruby",
	".rake":  "ruby",
	".kt":    "kotlin",
	".kts":   "kotlin",
	".cs":    "csharp",
	".php":   "php",
	".swift": "swift",
	// v3 languages
	".c":     "c",
	".h":     "c",
	".cpp":   "cpp",
	".cc":    "cpp",
	".cxx":   "cpp",
	".hpp":   "cpp",
	".hxx":   "cpp",
	".m":     "objc",
	".mm":    "objc",
	".sh":    "bash",
	".bash":  "bash",
	".sql":   "sql",
	".dart":  "dart",
	".scala": "scala",
	".sc":    "scala",
	".r":     "r",
	".ex":    "elixir",
	".exs":   "elixir",
	".lua":   "lua",
	// v4 domain languages
	".jl":    "julia",
	".f90":   "fortran",
	".f95":   "fortran",
	".f03":   "fortran",
	".f08":   "fortran",
	".f":     "fortran",
	".for":   "fortran",
	".sol":   "solidity",
	".tf":    "terraform",
	".proto": "protobuf",
	// v4 scripting languages
	".pl":      "perl",
	".pm":      "perl",
	".ps1":     "powershell",
	".psm1":    "powershell",
	".groovy":  "groovy",
	".gvy":     "groovy",
	// v4 modern systems languages
	".zig":     "zig",
	".nim":     "nim",
	".cr":      "crystal",
	".v":       "vlang",
	".d":       "dlang",
	// v4 functional languages
	".hs":      "haskell",
	".lhs":     "haskell",
	".clj":     "clojure",
	".cljs":    "clojure",
	".cljc":    "clojure",
	".edn":     "clojure",
	".erl":     "erlang",
	".hrl":     "erlang",
	".fs":      "fsharp",
	".fsx":     "fsharp",
	".fsi":     "fsharp",
	".ml":      "ocaml",
	".mli":     "ocaml",
	// v4 legacy languages
	".vb":      "visualbasic",
	".vbs":     "visualbasic",
	".cob":     "cobol",
	".cbl":     "cobol",
	".cpy":     "cobol",
	".adb":     "ada",
	".ads":     "ada",
	".pas":     "pascal",
	".pp":      "pascal",
	".dpr":     "pascal",
	".lpr":     "pascal",
}

// Registry manages parsers for different languages.
type Registry struct {
	mu      sync.RWMutex
	parsers map[string]Parser
}

// NewRegistry creates an empty parser registry.
func NewRegistry() *Registry {
	return &Registry{
		parsers: make(map[string]Parser),
	}
}

// Register adds a parser for a language.
func (r *Registry) Register(parser Parser) {
	r.mu.Lock()
	defer r.mu.Unlock()
	r.parsers[parser.Language()] = parser
}

// Get returns the parser for a language, if registered.
func (r *Registry) Get(language string) (Parser, bool) {
	r.mu.RLock()
	defer r.mu.RUnlock()
	p, ok := r.parsers[language]
	return p, ok
}

// DetectLanguage returns the language for a file based on its extension.
// Returns empty string if the language is not supported.
func DetectLanguage(filename string) string {
	ext := strings.ToLower(filepath.Ext(filename))
	lang, ok := extensionToLanguage[ext]
	if !ok {
		return ""
	}
	return lang
}

// ParseFile detects language and parses a single file.
// Returns nil, nil if the language is not supported (not an error).
func (r *Registry) ParseFile(filename string, content []byte) (*ParsedFile, error) {
	lang := DetectLanguage(filename)
	if lang == "" {
		return nil, nil
	}

	p, ok := r.Get(lang)
	if !ok {
		return nil, nil
	}

	parsed, err := p.Parse(filename, content)
	if err != nil {
		return nil, fmt.Errorf("parsing %s: %w", filename, err)
	}

	return parsed, nil
}

// SupportedExtensions returns all supported file extensions.
func SupportedExtensions() []string {
	exts := make([]string, 0, len(extensionToLanguage))
	for ext := range extensionToLanguage {
		exts = append(exts, ext)
	}
	return exts
}

// SupportedLanguages returns all unique supported language names.
func SupportedLanguages() []string {
	seen := make(map[string]bool)
	langs := make([]string, 0)
	for _, lang := range extensionToLanguage {
		if !seen[lang] {
			seen[lang] = true
			langs = append(langs, lang)
		}
	}
	return langs
}
