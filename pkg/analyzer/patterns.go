package analyzer

import (
	"regexp"
	"strings"

	"github.com/joeabbey/fault/pkg/git"
)

// AntiPatternAnalyzer detects common anti-patterns in added code.
type AntiPatternAnalyzer struct{}

// NewAntiPatternAnalyzer creates a new anti-pattern analyzer.
func NewAntiPatternAnalyzer() *AntiPatternAnalyzer {
	return &AntiPatternAnalyzer{}
}

// Name returns the analyzer name.
func (a *AntiPatternAnalyzer) Name() string {
	return "patterns"
}

// Analyze scans diff content for anti-patterns in added lines.
func (a *AntiPatternAnalyzer) Analyze(ctx *AnalysisContext) ([]Issue, error) {
	issues := make([]Issue, 0)

	if ctx.Diff == nil || len(ctx.Diff.Files) == 0 {
		return issues, nil
	}

	for _, fileDiff := range ctx.Diff.Files {
		if fileDiff.Status == "deleted" || fileDiff.IsBinary {
			continue
		}

		issues = append(issues, checkTODOPlaceholders(fileDiff)...)
		issues = append(issues, checkHardcodedCredentials(fileDiff)...)
		issues = append(issues, checkConsoleDebug(fileDiff)...)
		issues = append(issues, checkCommentedOutCode(fileDiff)...)
		issues = append(issues, checkUnreachableCode(fileDiff)...)
	}

	return issues, nil
}

// --- TODO / Placeholder detection ---

var todoPatterns = []*regexp.Regexp{
	regexp.MustCompile(`(?i)//\s*TODO\b`),
	regexp.MustCompile(`(?i)#\s*TODO\b`),
	regexp.MustCompile(`(?i)raise\s+NotImplementedError`),
	regexp.MustCompile(`(?i)throw\s+new\s+Error\(\s*['"]not\s+implemented`),
	regexp.MustCompile(`panic\(\s*"not implemented"`),
}

// standalonePythonPass matches "pass" as a standalone statement (the only non-whitespace on the line).
var standalonePythonPass = regexp.MustCompile(`^\s*pass\s*$`)

func checkTODOPlaceholders(fileDiff git.FileDiff) []Issue {
	issues := make([]Issue, 0)

	for _, hunk := range fileDiff.Hunks {
		for _, line := range hunk.Lines {
			if line.Type != "added" {
				continue
			}

			content := line.Content

			for _, pat := range todoPatterns {
				if pat.MatchString(content) {
					issues = append(issues, Issue{
						ID:         "patterns/todo-placeholder",
						Severity:   SeverityWarning,
						Category:   "patterns",
						File:       fileDiff.Path,
						Line:       line.NewNum,
						Message:    "TODO or placeholder left in code: " + strings.TrimSpace(content),
						Suggestion: "Implement the functionality or remove the placeholder before committing",
					})
					break // Only report once per line even if multiple patterns match.
				}
			}

			// Check for standalone Python pass (only for .py files).
			if isPythonFile(fileDiff.Path) && standalonePythonPass.MatchString(content) {
				issues = append(issues, Issue{
					ID:         "patterns/todo-placeholder",
					Severity:   SeverityWarning,
					Category:   "patterns",
					File:       fileDiff.Path,
					Line:       line.NewNum,
					Message:    "Standalone 'pass' statement may be a placeholder",
					Suggestion: "Implement the function body or add a comment explaining why it's empty",
				})
			}
		}
	}

	return issues
}

// --- Hardcoded credential detection ---

var credentialPatterns = []*regexp.Regexp{
	regexp.MustCompile(`(?i)["']sk-[a-zA-Z0-9]{20,}["']`),
	regexp.MustCompile(`(?i)["']AKIA[A-Z0-9]{16}["']`),
	regexp.MustCompile(`(?i)["']ghp_[a-zA-Z0-9]{36,}["']`),
	regexp.MustCompile(`(?i)password\s*=\s*"[^"]+"`),
	regexp.MustCompile(`(?i)secret\s*=\s*"[^"]+"`),
}

// credentialExclusions filters out common false positives.
var credentialExclusions = []*regexp.Regexp{
	regexp.MustCompile(`(?i)password\s*=\s*""\s*$`),                  // empty string
	regexp.MustCompile(`(?i)secret\s*=\s*""\s*$`),                    // empty string
	regexp.MustCompile(`(?i)os\.Getenv\(.*password`),                 // env var lookup
	regexp.MustCompile(`(?i)os\.Getenv\(.*secret`),                   // env var lookup
	regexp.MustCompile(`(?i)password\s*=\s*"\$\{`),                   // template variable
	regexp.MustCompile(`(?i)secret\s*=\s*"\$\{`),                     // template variable
	regexp.MustCompile(`(?i)password\s*=\s*"<[^>]*>"`),               // placeholder like <password>
	regexp.MustCompile(`(?i)secret\s*=\s*"<[^>]*>"`),                 // placeholder like <secret>
	regexp.MustCompile(`(?i)password\s*=\s*"(test|example|dummy)"`),   // obvious test values
}

func checkHardcodedCredentials(fileDiff git.FileDiff) []Issue {
	issues := make([]Issue, 0)

	for _, hunk := range fileDiff.Hunks {
		for _, line := range hunk.Lines {
			if line.Type != "added" {
				continue
			}

			content := line.Content

			for _, pat := range credentialPatterns {
				if pat.MatchString(content) {
					// Check exclusions.
					excluded := false
					for _, excl := range credentialExclusions {
						if excl.MatchString(content) {
							excluded = true
							break
						}
					}
					if excluded {
						continue
					}

					issues = append(issues, Issue{
						ID:         "patterns/hardcoded-credential",
						Severity:   SeverityError,
						Category:   "patterns",
						File:       fileDiff.Path,
						Line:       line.NewNum,
						Message:    "Possible hardcoded credential detected",
						Suggestion: "Use environment variables or a secrets manager instead of hardcoding credentials",
					})
					break // Only report once per line.
				}
			}
		}
	}

	return issues
}

// --- Console/debug artifact detection ---

var consolePatterns = []*regexp.Regexp{
	regexp.MustCompile(`\bconsole\.log\(`),
	regexp.MustCompile(`\bfmt\.Println\(`),
	regexp.MustCompile(`\bprint\(`),
}

func checkConsoleDebug(fileDiff git.FileDiff) []Issue {
	issues := make([]Issue, 0)

	for _, hunk := range fileDiff.Hunks {
		for _, line := range hunk.Lines {
			if line.Type != "added" {
				continue
			}

			content := line.Content

			// Skip comments — they're not debug artifacts.
			trimmed := strings.TrimSpace(content)
			if strings.HasPrefix(trimmed, "//") || strings.HasPrefix(trimmed, "#") || strings.HasPrefix(trimmed, "/*") {
				continue
			}

			for _, pat := range consolePatterns {
				if pat.MatchString(content) {
					issues = append(issues, Issue{
						ID:         "patterns/console-debug",
						Severity:   SeverityInfo,
						Category:   "patterns",
						File:       fileDiff.Path,
						Line:       line.NewNum,
						Message:    "Debug/console output left in code: " + strings.TrimSpace(content),
						Suggestion: "Remove debug output or replace with proper logging",
					})
					break
				}
			}
		}
	}

	return issues
}

// --- Commented-out code detection ---

// commentLinePattern matches lines that are just comments (for Go, JS/TS, Python).
var commentLinePatterns = []*regexp.Regexp{
	regexp.MustCompile(`^\s*//`),  // Go, JS, TS
	regexp.MustCompile(`^\s*#`),   // Python, shell
	regexp.MustCompile(`^\s*/\*`), // Block comment start
	regexp.MustCompile(`^\s*\*`),  // Block comment continuation
}

func isCommentLine(content string) bool {
	for _, pat := range commentLinePatterns {
		if pat.MatchString(content) {
			return true
		}
	}
	return false
}

func checkCommentedOutCode(fileDiff git.FileDiff) []Issue {
	issues := make([]Issue, 0)

	for _, hunk := range fileDiff.Hunks {
		consecutiveComments := 0
		blockStartLine := 0

		for _, line := range hunk.Lines {
			if line.Type != "added" {
				// Reset streak on non-added lines.
				if consecutiveComments >= 3 {
					issues = append(issues, Issue{
						ID:       "patterns/commented-code",
						Severity: SeverityWarning,
						Category: "patterns",
						File:     fileDiff.Path,
						Line:     blockStartLine,
						EndLine:  line.NewNum - 1,
						Message:  "Large block of commented-out code detected",
						Suggestion: "Remove commented-out code; use version control to recover it if needed",
					})
				}
				consecutiveComments = 0
				continue
			}

			if isCommentLine(line.Content) {
				if consecutiveComments == 0 {
					blockStartLine = line.NewNum
				}
				consecutiveComments++
			} else {
				if consecutiveComments >= 3 {
					issues = append(issues, Issue{
						ID:       "patterns/commented-code",
						Severity: SeverityWarning,
						Category: "patterns",
						File:     fileDiff.Path,
						Line:     blockStartLine,
						EndLine:  line.NewNum - 1,
						Message:  "Large block of commented-out code detected",
						Suggestion: "Remove commented-out code; use version control to recover it if needed",
					})
				}
				consecutiveComments = 0
			}
		}

		// Check at end of hunk.
		if consecutiveComments >= 3 {
			lastLine := hunk.Lines[len(hunk.Lines)-1]
			issues = append(issues, Issue{
				ID:       "patterns/commented-code",
				Severity: SeverityWarning,
				Category: "patterns",
				File:     fileDiff.Path,
				Line:     blockStartLine,
				EndLine:  lastLine.NewNum,
				Message:  "Large block of commented-out code detected",
				Suggestion: "Remove commented-out code; use version control to recover it if needed",
			})
		}
	}

	return issues
}

// --- Unreachable code detection ---

// returnPatterns match lines that are return/throw/panic statements.
var returnPatterns = []*regexp.Regexp{
	regexp.MustCompile(`^\s*return\b`),
	regexp.MustCompile(`^\s*throw\b`),
	regexp.MustCompile(`^\s*panic\(`),
}

func isReturnLike(content string) bool {
	for _, pat := range returnPatterns {
		if pat.MatchString(content) {
			return true
		}
	}
	return false
}

// isBlockEnd matches closing braces/brackets or blank lines that end a block scope.
func isBlockEnd(content string) bool {
	trimmed := strings.TrimSpace(content)
	return trimmed == "}" || trimmed == "})" || trimmed == ""
}

func checkUnreachableCode(fileDiff git.FileDiff) []Issue {
	issues := make([]Issue, 0)

	for _, hunk := range fileDiff.Hunks {
		addedLines := make([]git.Line, 0)
		for _, line := range hunk.Lines {
			if line.Type == "added" {
				addedLines = append(addedLines, line)
			}
		}

		for i := 0; i < len(addedLines)-1; i++ {
			if isReturnLike(addedLines[i].Content) {
				// Look at the next added line.
				next := addedLines[i+1]
				nextTrimmed := strings.TrimSpace(next.Content)

				// Skip if next line is a block-end or empty.
				if isBlockEnd(next.Content) {
					continue
				}
				// Skip if next line is a comment.
				if isCommentLine(next.Content) {
					continue
				}
				// Skip if next line is a case/default label.
				if strings.HasPrefix(nextTrimmed, "case ") || strings.HasPrefix(nextTrimmed, "default:") {
					continue
				}

				issues = append(issues, Issue{
					ID:         "patterns/unreachable-code",
					Severity:   SeverityWarning,
					Category:   "patterns",
					File:       fileDiff.Path,
					Line:       next.NewNum,
					Message:    "Potentially unreachable code after return/throw/panic",
					Suggestion: "Remove unreachable code or restructure the control flow",
				})
			}
		}
	}

	return issues
}

// --- Helpers ---

func isPythonFile(path string) bool {
	return strings.HasSuffix(path, ".py")
}
