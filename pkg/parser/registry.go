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
	".rb":   "ruby",
	".rake": "ruby",
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
