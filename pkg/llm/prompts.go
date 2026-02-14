package llm

import (
	"embed"
	"strings"
)

//go:embed prompts/*.md
var promptFS embed.FS

// promptCache stores loaded prompts to avoid repeated FS reads.
var promptCache = make(map[string]string)

// LoadPrompt loads an embedded prompt template by name.
// The name should not include the directory or extension (e.g., "confidence" not "prompts/confidence.md").
func LoadPrompt(name string) string {
	if cached, ok := promptCache[name]; ok {
		return cached
	}

	path := "prompts/" + name + ".md"
	data, err := promptFS.ReadFile(path)
	if err != nil {
		return ""
	}

	content := string(data)
	promptCache[name] = content
	return content
}

// FormatPrompt performs simple {{var}} template replacement.
// Variables in the template like {{diff}} are replaced with values from the vars map.
func FormatPrompt(template string, vars map[string]string) string {
	result := template
	for key, value := range vars {
		placeholder := "{{" + key + "}}"
		result = strings.ReplaceAll(result, placeholder, value)
	}
	return result
}

// AvailablePrompts returns the names of all embedded prompt templates.
func AvailablePrompts() []string {
	entries, err := promptFS.ReadDir("prompts")
	if err != nil {
		return nil
	}

	names := make([]string, 0, len(entries))
	for _, entry := range entries {
		if entry.IsDir() {
			continue
		}
		name := entry.Name()
		if strings.HasSuffix(name, ".md") {
			names = append(names, strings.TrimSuffix(name, ".md"))
		}
	}
	return names
}
