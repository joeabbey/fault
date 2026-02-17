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
	case "java":
		newLine = replaceSecretJava(line, envName)
	case "kotlin":
		newLine = replaceSecretKotlin(line, envName)
	case "ruby":
		newLine = replaceSecretRuby(line, envName)
	case "php":
		newLine = replaceSecretPHP(line, envName)
	case "csharp":
		newLine = replaceSecretCSharp(line, envName)
	case "swift":
		newLine = replaceSecretSwift(line, envName)
	case "c":
		newLine = replaceSecretC(line, envName)
	case "cpp":
		newLine = replaceSecretCpp(line, envName)
	case "rust":
		newLine = replaceSecretRust(line, envName)
	case "perl":
		newLine = f.replaceSecretPerl(line, envName)
	case "powershell":
		newLine = f.replaceSecretPowerShell(line, envName)
	case "groovy":
		newLine = f.replaceSecretGroovy(line, envName)
	case "scala":
		newLine = f.replaceSecretScala(line, envName)
	case "r":
		newLine = f.replaceSecretR(line, envName)
	case "objc":
		newLine = f.replaceSecretObjC(line, envName)
	case "dart":
		newLine = f.replaceSecretDart(line, envName)
	case "bash":
		newLine = f.replaceSecretBash(line, envName)
	case "elixir":
		newLine = f.replaceSecretElixir(line, envName)
	case "lua":
		newLine = f.replaceSecretLua(line, envName)
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

// replaceSecretJava replaces a hardcoded string value with System.getenv() in Java.
func replaceSecretJava(line, envName string) string {
	re := regexp.MustCompile(`"[^"]{8,}"`)
	return re.ReplaceAllString(line, `System.getenv("`+envName+`")`)
}

// replaceSecretKotlin replaces a hardcoded string value with System.getenv() in Kotlin.
func replaceSecretKotlin(line, envName string) string {
	re := regexp.MustCompile(`"[^"]{8,}"`)
	return re.ReplaceAllString(line, `System.getenv("`+envName+`")`)
}

// replaceSecretRuby replaces a hardcoded string value with ENV[] in Ruby.
func replaceSecretRuby(line, envName string) string {
	re := regexp.MustCompile(`["'][^"']{8,}["']`)
	return re.ReplaceAllString(line, `ENV["`+envName+`"]`)
}

// replaceSecretPHP replaces a hardcoded string value with getenv() in PHP.
func replaceSecretPHP(line, envName string) string {
	re := regexp.MustCompile(`["'][^"']{8,}["']`)
	return re.ReplaceAllString(line, `getenv('`+envName+`')`)
}

// replaceSecretCSharp replaces a hardcoded string value with Environment.GetEnvironmentVariable() in C#.
func replaceSecretCSharp(line, envName string) string {
	re := regexp.MustCompile(`"[^"]{8,}"`)
	return re.ReplaceAllString(line, `Environment.GetEnvironmentVariable("`+envName+`")`)
}

// replaceSecretSwift replaces a hardcoded string value with ProcessInfo.processInfo.environment[] in Swift.
func replaceSecretSwift(line, envName string) string {
	re := regexp.MustCompile(`"[^"]{8,}"`)
	return re.ReplaceAllString(line, `ProcessInfo.processInfo.environment["`+envName+`"]`)
}

// replaceSecretC replaces a hardcoded string value with getenv() in C.
func replaceSecretC(line, envName string) string {
	re := regexp.MustCompile(`"[^"]{8,}"`)
	return re.ReplaceAllString(line, `getenv("`+envName+`")`)
}

// replaceSecretCpp replaces a hardcoded string value with std::getenv() in C++.
func replaceSecretCpp(line, envName string) string {
	re := regexp.MustCompile(`"[^"]{8,}"`)
	return re.ReplaceAllString(line, `std::getenv("`+envName+`")`)
}

// replaceSecretRust replaces a hardcoded string value with std::env::var().unwrap_or_default() in Rust.
func replaceSecretRust(line, envName string) string {
	re := regexp.MustCompile(`"[^"]{8,}"`)
	return re.ReplaceAllString(line, `std::env::var("`+envName+`").unwrap_or_default()`)
}

// replaceSecretPerl replaces a hardcoded string value with $ENV{KEY} in Perl.
func (f *SecurityFixer) replaceSecretPerl(line, envName string) string {
	re := regexp.MustCompile(`["'][^"']{8,}["']`)
	return re.ReplaceAllString(line, `$ENV{`+envName+`}`)
}

// replaceSecretPowerShell replaces a hardcoded string value with $env:KEY in PowerShell.
func (f *SecurityFixer) replaceSecretPowerShell(line, envName string) string {
	re := regexp.MustCompile(`["'][^"']{8,}["']`)
	return re.ReplaceAllString(line, `$env:`+envName)
}

// replaceSecretGroovy replaces a hardcoded string value with System.getenv("KEY") in Groovy.
func (f *SecurityFixer) replaceSecretGroovy(line, envName string) string {
	re := regexp.MustCompile(`["'][^"']{8,}["']`)
	return re.ReplaceAllString(line, `System.getenv("`+envName+`")`)
}

// replaceSecretScala replaces a hardcoded string value with sys.env.getOrElse("KEY", "") in Scala.
func (f *SecurityFixer) replaceSecretScala(line, envName string) string {
	re := regexp.MustCompile(`"[^"]{8,}"`)
	return re.ReplaceAllString(line, `sys.env.getOrElse("`+envName+`", "")`)
}

// replaceSecretR replaces a hardcoded string value with Sys.getenv("KEY") in R.
func (f *SecurityFixer) replaceSecretR(line, envName string) string {
	re := regexp.MustCompile(`["'][^"']{8,}["']`)
	return re.ReplaceAllString(line, `Sys.getenv("`+envName+`")`)
}

// replaceSecretObjC replaces a hardcoded string value with [[NSProcessInfo processInfo] environment] in Objective-C.
func (f *SecurityFixer) replaceSecretObjC(line, envName string) string {
	re := regexp.MustCompile(`@"[^"]{8,}"`)
	result := re.ReplaceAllString(line, `[[[NSProcessInfo processInfo] environment] objectForKey:@"`+envName+`"]`)
	if result == line {
		re2 := regexp.MustCompile(`"[^"]{8,}"`)
		result = re2.ReplaceAllString(line, `[[[NSProcessInfo processInfo] environment] objectForKey:@"`+envName+`"]`)
	}
	return result
}

// replaceSecretDart replaces a hardcoded string value with Platform.environment[] in Dart.
func (f *SecurityFixer) replaceSecretDart(line, envName string) string {
	re := regexp.MustCompile(`["'][^"']{8,}["']`)
	return re.ReplaceAllString(line, `Platform.environment['`+envName+`'] ?? ''`)
}

// replaceSecretBash replaces a hardcoded string value with an env var reference in Bash.
func (f *SecurityFixer) replaceSecretBash(line, envName string) string {
	re := regexp.MustCompile(`["'][^"']{8,}["']`)
	return re.ReplaceAllString(line, `"${`+envName+`}"`)
}

// replaceSecretElixir replaces a hardcoded string value with System.get_env() in Elixir.
func (f *SecurityFixer) replaceSecretElixir(line, envName string) string {
	re := regexp.MustCompile(`"[^"]{8,}"`)
	return re.ReplaceAllString(line, `System.get_env("`+envName+`")`)
}

// replaceSecretLua replaces a hardcoded string value with os.getenv() in Lua.
func (f *SecurityFixer) replaceSecretLua(line, envName string) string {
	re := regexp.MustCompile(`"[^"]{8,}"`)
	return re.ReplaceAllString(line, `os.getenv("`+envName+`")`)
}
