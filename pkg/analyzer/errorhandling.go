package analyzer

import (
	"path/filepath"
	"regexp"
	"strings"

	"github.com/joeabbey/fault/pkg/git"
)

// ErrorHandlingAnalyzer detects poor error handling patterns in added code.
type ErrorHandlingAnalyzer struct{}

// NewErrorHandlingAnalyzer creates a new error handling analyzer.
func NewErrorHandlingAnalyzer() *ErrorHandlingAnalyzer {
	return &ErrorHandlingAnalyzer{}
}

// Name returns the analyzer name.
func (a *ErrorHandlingAnalyzer) Name() string {
	return "errorhandling"
}

// Analyze scans diff content for error handling anti-patterns in added lines.
func (a *ErrorHandlingAnalyzer) Analyze(ctx *AnalysisContext) ([]Issue, error) {
	issues := make([]Issue, 0)

	if ctx.Diff == nil || len(ctx.Diff.Files) == 0 {
		return issues, nil
	}

	for _, fileDiff := range ctx.Diff.Files {
		if fileDiff.Status == "deleted" || fileDiff.IsBinary {
			continue
		}

		if isErrorHandlingSkippedPath(fileDiff.Path) {
			continue
		}

		if isTestFile(fileDiff.Path) {
			continue
		}

		ext := strings.ToLower(filepath.Ext(fileDiff.Path))

		switch ext {
		case ".go":
			issues = append(issues, checkGoErrorHandling(fileDiff)...)
		case ".ts", ".tsx", ".js", ".jsx", ".mjs":
			issues = append(issues, checkTSErrorHandling(fileDiff)...)
		case ".py":
			issues = append(issues, checkPythonErrorHandling(fileDiff)...)
		case ".java":
			issues = append(issues, checkJavaErrorHandling(fileDiff)...)
		case ".rs":
			issues = append(issues, checkRustErrorHandling(fileDiff)...)
		case ".c", ".h", ".cpp", ".cc", ".cxx", ".hpp", ".hxx":
			issues = append(issues, checkCErrorHandling(fileDiff)...)
		case ".dart":
			issues = append(issues, checkDartErrorHandling(fileDiff)...)
		case ".ex", ".exs":
			issues = append(issues, checkElixirErrorHandling(fileDiff)...)
		case ".scala", ".sc":
			issues = append(issues, checkScalaErrorHandling(fileDiff)...)
		}
	}

	return issues, nil
}

// isErrorHandlingSkippedPath returns true for vendored/generated directories.
func isErrorHandlingSkippedPath(path string) bool {
	normalized := filepath.ToSlash(path)
	parts := strings.Split(normalized, "/")
	for _, p := range parts {
		switch p {
		case "vendor", "node_modules", "testdata", "__tests__":
			return true
		}
	}
	return false
}

// --- Go error handling detection ---

var (
	// Matches `_ = someFunc(` or `_, _ = someFunc(`
	goDiscardedError = regexp.MustCompile(`_\s*=\s*\w+[\w.]*\(`)
	// Matches `_ , _ =` style (with spaces around comma)
	goDiscardedErrorMulti = regexp.MustCompile(`_\s*,\s*_\s*=\s*\w+[\w.]*\(`)
)

func checkGoErrorHandling(fileDiff git.FileDiff) []Issue {
	issues := make([]Issue, 0)

	for _, hunk := range fileDiff.Hunks {
		for _, line := range hunk.Lines {
			if line.Type != "added" {
				continue
			}

			content := line.Content
			trimmed := strings.TrimSpace(content)

			// Skip comments.
			if strings.HasPrefix(trimmed, "//") || strings.HasPrefix(trimmed, "/*") || strings.HasPrefix(trimmed, "*") {
				continue
			}

			// Check for discarded errors: `_ = someFunc(` or `_, _ = someFunc(`
			if goDiscardedErrorMulti.MatchString(trimmed) || goDiscardedError.MatchString(trimmed) {
				issues = append(issues, Issue{
					ID:         "errorhandling/go-discarded-error",
					Severity:   SeverityWarning,
					Category:   "errorhandling-go",
					File:       fileDiff.Path,
					Line:       line.NewNum,
					Message:    "Discarded error return value",
					Suggestion: "Handle the error instead of assigning to _; at minimum log it or add a comment explaining why it's safe to ignore",
				})
			}
		}
	}

	return issues
}

// --- TypeScript/JavaScript error handling detection ---

var (
	// Matches `.then(` — indicates a promise chain
	tsThenCall = regexp.MustCompile(`\.then\s*\(`)
	// Matches `.catch(` — indicates error handling in promise chain
	tsCatchCall = regexp.MustCompile(`\.catch\s*\(`)
	// Matches `await ` expression
	tsAwaitExpr = regexp.MustCompile(`\bawait\s+`)
)

func checkTSErrorHandling(fileDiff git.FileDiff) []Issue {
	issues := make([]Issue, 0)

	for _, hunk := range fileDiff.Hunks {
		lines := hunk.Lines

		for i, line := range lines {
			if line.Type != "added" {
				continue
			}

			content := line.Content
			trimmed := strings.TrimSpace(content)

			// Skip comments.
			if strings.HasPrefix(trimmed, "//") || strings.HasPrefix(trimmed, "/*") || strings.HasPrefix(trimmed, "*") {
				continue
			}

			// Check for .then() without .catch() nearby
			if tsThenCall.MatchString(content) {
				// Look at this line and nearby lines for .catch()
				hasCatch := false
				// Check current line
				if tsCatchCall.MatchString(content) {
					hasCatch = true
				}
				// Check the next few lines (up to 5)
				if !hasCatch {
					for j := i + 1; j < len(lines) && j <= i+5; j++ {
						if tsCatchCall.MatchString(lines[j].Content) {
							hasCatch = true
							break
						}
						// Stop if we hit a line that isn't part of the chain
						nextTrimmed := strings.TrimSpace(lines[j].Content)
						if nextTrimmed != "" && !strings.HasPrefix(nextTrimmed, ".") && !strings.HasPrefix(nextTrimmed, ")") {
							break
						}
					}
				}

				if !hasCatch {
					issues = append(issues, Issue{
						ID:         "errorhandling/ts-unhandled-promise",
						Severity:   SeverityWarning,
						Category:   "errorhandling-ts",
						File:       fileDiff.Path,
						Line:       line.NewNum,
						Message:    "Promise chain with .then() but no .catch() handler",
						Suggestion: "Add a .catch() handler or use async/await with try-catch to handle promise rejections",
					})
				}
			}

			// Check for await without try-catch
			if tsAwaitExpr.MatchString(content) {
				// Look backwards for a `try {` in the preceding lines
				hasTry := false
				for j := i - 1; j >= 0 && j >= i-20; j-- {
					prevTrimmed := strings.TrimSpace(lines[j].Content)
					if strings.Contains(prevTrimmed, "try") && strings.Contains(prevTrimmed, "{") {
						hasTry = true
						break
					}
					// Also check for try on its own line followed by {
					if prevTrimmed == "try" {
						hasTry = true
						break
					}
				}

				if !hasTry {
					issues = append(issues, Issue{
						ID:         "errorhandling/ts-unhandled-await",
						Severity:   SeverityWarning,
						Category:   "errorhandling-ts",
						File:       fileDiff.Path,
						Line:       line.NewNum,
						Message:    "await expression without try-catch block",
						Suggestion: "Wrap await calls in try-catch to handle potential rejections, or use .catch() on the promise",
					})
				}
			}
		}
	}

	return issues
}

// --- Python error handling detection ---

var (
	// Bare except: `except:`
	pyBareExcept = regexp.MustCompile(`^\s*except\s*:`)
	// except Exception or except Exception as e
	pyBroadExcept = regexp.MustCompile(`^\s*except\s+(Exception|BaseException)\s*(as\s+\w+)?\s*:`)
	// pass statement
	pyPass = regexp.MustCompile(`^\s*pass\s*$`)
)

func checkPythonErrorHandling(fileDiff git.FileDiff) []Issue {
	issues := make([]Issue, 0)

	for _, hunk := range fileDiff.Hunks {
		lines := hunk.Lines

		for i, line := range lines {
			if line.Type != "added" {
				continue
			}

			content := line.Content
			trimmed := strings.TrimSpace(content)

			// Skip comments.
			if strings.HasPrefix(trimmed, "#") {
				continue
			}

			// Check for bare except
			if pyBareExcept.MatchString(content) {
				issues = append(issues, Issue{
					ID:         "errorhandling/python-bare-except",
					Severity:   SeverityWarning,
					Category:   "errorhandling-python",
					File:       fileDiff.Path,
					Line:       line.NewNum,
					Message:    "Bare except clause catches all exceptions including KeyboardInterrupt and SystemExit",
					Suggestion: "Specify the exception type, e.g., 'except ValueError:' or 'except (TypeError, ValueError):'",
				})
			}

			// Check for except Exception/BaseException followed by pass
			if pyBroadExcept.MatchString(content) {
				// Check if the next added line is just `pass`
				if i+1 < len(lines) && lines[i+1].Type == "added" {
					nextContent := lines[i+1].Content
					if pyPass.MatchString(nextContent) {
						issues = append(issues, Issue{
							ID:         "errorhandling/python-except-pass",
							Severity:   SeverityWarning,
							Category:   "errorhandling-python",
							File:       fileDiff.Path,
							Line:       line.NewNum,
							Message:    "Broad exception caught and silently ignored with pass",
							Suggestion: "At minimum, log the exception. If you intend to ignore it, add a comment explaining why",
						})
					}
				}
			}
		}
	}

	return issues
}

// --- Java error handling detection ---

var (
	// Empty catch block: catch (...) { }
	javaEmptyCatch = regexp.MustCompile(`catch\s*\([^)]+\)\s*\{\s*\}`)
	// Overly broad catch: catch (Exception e)
	javaBroadCatch = regexp.MustCompile(`catch\s*\(\s*Exception\s+\w+\s*\)`)
	// Overly broad throws: throws Exception
	javaBroadThrows = regexp.MustCompile(`throws\s+Exception\b`)
)

func checkJavaErrorHandling(fileDiff git.FileDiff) []Issue {
	issues := make([]Issue, 0)

	for _, hunk := range fileDiff.Hunks {
		for _, line := range hunk.Lines {
			if line.Type != "added" {
				continue
			}

			content := line.Content
			trimmed := strings.TrimSpace(content)

			// Skip comments.
			if strings.HasPrefix(trimmed, "//") || strings.HasPrefix(trimmed, "/*") || strings.HasPrefix(trimmed, "*") {
				continue
			}

			// Check for empty catch blocks
			if javaEmptyCatch.MatchString(content) {
				issues = append(issues, Issue{
					ID:         "errorhandling/java-empty-catch",
					Severity:   SeverityWarning,
					Category:   "errorhandling-java",
					File:       fileDiff.Path,
					Line:       line.NewNum,
					Message:    "Empty catch block silently swallows exception",
					Suggestion: "At minimum, log the exception. If intentionally ignoring, add a comment explaining why",
				})
				continue // don't also flag as broad catch
			}

			// Check for overly broad catch
			if javaBroadCatch.MatchString(content) {
				issues = append(issues, Issue{
					ID:         "errorhandling/java-broad-catch",
					Severity:   SeverityInfo,
					Category:   "errorhandling-java",
					File:       fileDiff.Path,
					Line:       line.NewNum,
					Message:    "Catching overly broad Exception type",
					Suggestion: "Catch specific exception types (IOException, IllegalArgumentException, etc.) to avoid masking unexpected errors",
				})
			}

			// Check for overly broad throws
			if javaBroadThrows.MatchString(content) {
				issues = append(issues, Issue{
					ID:         "errorhandling/java-broad-throws",
					Severity:   SeverityInfo,
					Category:   "errorhandling-java",
					File:       fileDiff.Path,
					Line:       line.NewNum,
					Message:    "Method declares overly broad 'throws Exception'",
					Suggestion: "Declare specific exception types in the throws clause to communicate expected failure modes",
				})
			}
		}
	}

	return issues
}

// --- Rust error handling detection ---

var (
	// .unwrap() call
	rustUnwrap = regexp.MustCompile(`\.unwrap\(\)`)
	// .expect(" with generic messages
	rustGenericExpect = regexp.MustCompile(`\.expect\(\s*"(?i)(failed|error|unexpected|something went wrong|unwrap failed|should not fail)`)
)

func checkRustErrorHandling(fileDiff git.FileDiff) []Issue {
	issues := make([]Issue, 0)

	for _, hunk := range fileDiff.Hunks {
		for _, line := range hunk.Lines {
			if line.Type != "added" {
				continue
			}

			content := line.Content
			trimmed := strings.TrimSpace(content)

			// Skip comments.
			if strings.HasPrefix(trimmed, "//") || strings.HasPrefix(trimmed, "/*") || strings.HasPrefix(trimmed, "*") {
				continue
			}

			// Check for .unwrap()
			if rustUnwrap.MatchString(content) {
				issues = append(issues, Issue{
					ID:         "errorhandling/rust-unwrap",
					Severity:   SeverityWarning,
					Category:   "errorhandling-rust",
					File:       fileDiff.Path,
					Line:       line.NewNum,
					Message:    "Use of .unwrap() will panic on None/Err values",
					Suggestion: "Use pattern matching, .unwrap_or(), .unwrap_or_else(), or the ? operator instead of .unwrap()",
				})
			}

			// Check for .expect() with generic messages
			if rustGenericExpect.MatchString(content) {
				issues = append(issues, Issue{
					ID:         "errorhandling/rust-generic-expect",
					Severity:   SeverityInfo,
					Category:   "errorhandling-rust",
					File:       fileDiff.Path,
					Line:       line.NewNum,
					Message:    "Generic .expect() message provides little debugging context",
					Suggestion: "Use a descriptive message that explains what operation failed and why it's unexpected, e.g., .expect(\"config file must exist at startup\")",
				})
			}
		}
	}

	return issues
}
