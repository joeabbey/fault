package fixer

import (
	"os"
	"path/filepath"
	"regexp"
	"strings"
	"unicode"

	"github.com/joeabbey/fault/pkg/analyzer"
)

// SecurityFixer handles hardcoded secrets and insecure random usage.
type SecurityFixer struct{}

func NewSecurityFixer() *SecurityFixer { return &SecurityFixer{} }

func (f *SecurityFixer) FixIDs() []string {
	return []string{"sec-hardcoded-secret", "sec-insecure-random"}
}

func (f *SecurityFixer) GenerateFix(issue analyzer.Issue, repoRoot string) *Fix {
	if issue.Line == 0 || issue.File == "" {
		return nil
	}

	filePath := filepath.Join(repoRoot, issue.File)
	data, err := os.ReadFile(filePath)
	if err != nil {
		return nil
	}
	lines := strings.Split(string(data), "\n")

	if issue.Line < 1 || issue.Line > len(lines) {
		return nil
	}

	line := lines[issue.Line-1]
	lang := detectLanguage(issue.File)

	switch issue.FixID {
	case "sec-hardcoded-secret":
		return f.fixHardcodedSecret(issue, line, lang)
	case "sec-insecure-random":
		return f.fixInsecureRandom(issue, line, lang)
	}

	return nil
}

// fixHardcodedSecret replaces a hardcoded string with an env var lookup.
func (f *SecurityFixer) fixHardcodedSecret(issue analyzer.Issue, line string, lang string) *Fix {
	varName := inferVarName(line)
	if varName == "" {
		varName = "SECRET"
	}
	envName := toEnvVarName(varName)

	var newLine string
	switch lang {
	case "go":
		newLine = replaceSecretGo(line, envName)
	case "typescript":
		newLine = replaceSecretTS(line, envName)
	case "python":
		newLine = replaceSecretPython(line, envName)
	default:
		return nil
	}

	if newLine == "" || newLine == line {
		return nil
	}

	return &Fix{
		IssueID:     issue.ID,
		FixID:       issue.FixID,
		File:        issue.File,
		Description: "Replace hardcoded secret with environment variable " + envName,
		Edits:       []Edit{{Line: issue.Line, OldText: line, NewText: newLine}},
	}
}

// fixInsecureRandom replaces insecure random usage with crypto alternatives.
func (f *SecurityFixer) fixInsecureRandom(issue analyzer.Issue, line string, lang string) *Fix {
	var newLine string

	switch lang {
	case "typescript":
		// Math.random() → crypto.randomUUID()
		newLine = strings.Replace(line, "Math.random()", "crypto.randomUUID()", 1)
	case "python":
		// random.random() → secrets.token_hex(16)
		re := regexp.MustCompile(`random\.random\(\)`)
		newLine = re.ReplaceAllString(line, "secrets.token_hex(16)")
	default:
		return nil
	}

	if newLine == "" || newLine == line {
		return nil
	}

	return &Fix{
		IssueID:     issue.ID,
		FixID:       issue.FixID,
		File:        issue.File,
		Description: "Replace insecure random with cryptographic alternative",
		Edits:       []Edit{{Line: issue.Line, OldText: line, NewText: newLine}},
	}
}

// inferVarName tries to extract a variable name from an assignment line.
// Handles patterns like: apiKey := "...", api_key = "...", const TOKEN = "..."
func inferVarName(line string) string {
	trimmed := strings.TrimSpace(line)

	// Go short assignment: varName := "value"
	if idx := strings.Index(trimmed, ":="); idx > 0 {
		return strings.TrimSpace(trimmed[:idx])
	}

	// Assignment: varName = "value"
	if idx := strings.Index(trimmed, "="); idx > 0 {
		lhs := strings.TrimSpace(trimmed[:idx])
		// Remove var/const/let keywords
		for _, kw := range []string{"var ", "const ", "let ", "export const ", "export let "} {
			lhs = strings.TrimPrefix(lhs, kw)
		}
		// Remove type annotations (TypeScript): varName: string
		if colonIdx := strings.Index(lhs, ":"); colonIdx > 0 {
			lhs = lhs[:colonIdx]
		}
		return strings.TrimSpace(lhs)
	}

	// Key-value: "key": "value" or key: "value"
	re := regexp.MustCompile(`["']?(\w+)["']?\s*:`)
	if m := re.FindStringSubmatch(trimmed); m != nil {
		return m[1]
	}

	return ""
}

// toEnvVarName converts a variable name to UPPER_SNAKE_CASE for use as an env var.
func toEnvVarName(name string) string {
	// Handle already-uppercase names or names with underscores
	if name == strings.ToUpper(name) {
		return name
	}

	runes := []rune(name)
	var result strings.Builder
	for i, ch := range runes {
		if unicode.IsUpper(ch) && i > 0 {
			prev := runes[i-1]
			if unicode.IsLower(prev) || unicode.IsDigit(prev) {
				result.WriteRune('_')
			} else if unicode.IsUpper(prev) && i+1 < len(runes) && unicode.IsLower(runes[i+1]) {
				// Handle transitions like "APIKey" -> "API_KEY": insert _ before K
				result.WriteRune('_')
			}
		}
		if ch == '-' || ch == '.' {
			result.WriteRune('_')
		} else if ch == '_' {
			result.WriteRune('_')
		} else {
			result.WriteRune(unicode.ToUpper(ch))
		}
	}
	return result.String()
}

// replaceSecretGo replaces a hardcoded string value with os.Getenv() in Go.
func replaceSecretGo(line, envName string) string {
	// Match the string literal value
	re := regexp.MustCompile(`"[^"]{8,}"`)
	return re.ReplaceAllString(line, `os.Getenv("`+envName+`")`)
}

// replaceSecretTS replaces a hardcoded string value with process.env in TypeScript.
func replaceSecretTS(line, envName string) string {
	// Match single or double quoted string literals
	re := regexp.MustCompile(`["'][^"']{8,}["']`)
	return re.ReplaceAllString(line, "process.env."+envName)
}

// replaceSecretPython replaces a hardcoded string value with os.environ.get() in Python.
func replaceSecretPython(line, envName string) string {
	re := regexp.MustCompile(`["'][^"']{8,}["']`)
	return re.ReplaceAllString(line, `os.environ.get("`+envName+`")`)
}
