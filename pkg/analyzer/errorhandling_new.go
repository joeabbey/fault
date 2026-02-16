package analyzer

import (
	"regexp"
	"strings"

	"github.com/joeabbey/fault/pkg/git"
)

// C/C++ error handling patterns
var (
	cUncheckedMalloc  = regexp.MustCompile(`\bmalloc\s*\(`)
	cUncheckedCalloc  = regexp.MustCompile(`\bcalloc\s*\(`)
	cUncheckedRealloc = regexp.MustCompile(`\brealloc\s*\(`)
	cUncheckedFopen   = regexp.MustCompile(`\bfopen\s*\(`)
	cNullCheck        = regexp.MustCompile(`\bif\s*\(\s*\w+\s*[!=]=\s*NULL`)
	cNullCheckAlt     = regexp.MustCompile(`\bif\s*\(\s*!\s*\w+\s*\)`)
)

func checkCErrorHandling(fileDiff git.FileDiff) []Issue {
	issues := make([]Issue, 0)

	for _, hunk := range fileDiff.Hunks {
		for i, line := range hunk.Lines {
			if line.Type != "added" {
				continue
			}

			content := line.Content
			trimmed := strings.TrimSpace(content)

			// Skip comments
			if strings.HasPrefix(trimmed, "//") || strings.HasPrefix(trimmed, "/*") || strings.HasPrefix(trimmed, "*") {
				continue
			}

			// Check for malloc/calloc/realloc without NULL check on next line
			if cUncheckedMalloc.MatchString(content) || cUncheckedCalloc.MatchString(content) || cUncheckedRealloc.MatchString(content) {
				// Look ahead for NULL check
				hasCheck := false
				for j := i + 1; j < len(hunk.Lines) && j <= i+3; j++ {
					nextContent := hunk.Lines[j].Content
					if cNullCheck.MatchString(nextContent) || cNullCheckAlt.MatchString(nextContent) {
						hasCheck = true
						break
					}
				}
				if !hasCheck {
					issues = append(issues, Issue{
						ID:         "errorhandling/unchecked-alloc",
						Severity:   SeverityWarning,
						Category:   "errorhandling",
						File:       fileDiff.Path,
						Line:       line.NewNum,
						Message:    "Memory allocation without NULL check",
						Suggestion: "Check the return value for NULL before using the allocated memory",
					})
				}
			}

			// Check for fopen without NULL check
			if cUncheckedFopen.MatchString(content) {
				hasCheck := false
				for j := i + 1; j < len(hunk.Lines) && j <= i+3; j++ {
					nextContent := hunk.Lines[j].Content
					if cNullCheck.MatchString(nextContent) || cNullCheckAlt.MatchString(nextContent) {
						hasCheck = true
						break
					}
				}
				if !hasCheck {
					issues = append(issues, Issue{
						ID:         "errorhandling/unchecked-fopen",
						Severity:   SeverityWarning,
						Category:   "errorhandling",
						File:       fileDiff.Path,
						Line:       line.NewNum,
						Message:    "fopen() without NULL check",
						Suggestion: "Check the return value of fopen() for NULL before using the file handle",
					})
				}
			}
		}
	}

	return issues
}

// Dart error handling patterns
var (
	dartEmptyCatch = regexp.MustCompile(`\bcatch\s*\([^)]*\)\s*\{\s*\}`)
	dartCatchAll   = regexp.MustCompile(`\bon\s+Exception\s+catch`)
)

func checkDartErrorHandling(fileDiff git.FileDiff) []Issue {
	issues := make([]Issue, 0)

	for _, hunk := range fileDiff.Hunks {
		for _, line := range hunk.Lines {
			if line.Type != "added" {
				continue
			}

			content := line.Content
			trimmed := strings.TrimSpace(content)

			if strings.HasPrefix(trimmed, "//") {
				continue
			}

			if dartEmptyCatch.MatchString(content) {
				issues = append(issues, Issue{
					ID:         "errorhandling/empty-catch",
					Severity:   SeverityWarning,
					Category:   "errorhandling",
					File:       fileDiff.Path,
					Line:       line.NewNum,
					Message:    "Empty catch block swallows errors silently",
					Suggestion: "Handle the error or rethrow it",
				})
			}

			if dartCatchAll.MatchString(content) {
				issues = append(issues, Issue{
					ID:         "errorhandling/broad-catch",
					Severity:   SeverityWarning,
					Category:   "errorhandling",
					File:       fileDiff.Path,
					Line:       line.NewNum,
					Message:    "Catching broad 'Exception' type hides specific errors",
					Suggestion: "Catch specific exception types instead",
				})
			}
		}
	}

	return issues
}

// Elixir error handling patterns
var (
	elixirBareRescue = regexp.MustCompile(`\brescue\s*$`)
	elixirRescueAll  = regexp.MustCompile(`\brescue\s+_\s*->`)
)

func checkElixirErrorHandling(fileDiff git.FileDiff) []Issue {
	issues := make([]Issue, 0)

	for _, hunk := range fileDiff.Hunks {
		for _, line := range hunk.Lines {
			if line.Type != "added" {
				continue
			}

			content := line.Content
			trimmed := strings.TrimSpace(content)

			if strings.HasPrefix(trimmed, "#") {
				continue
			}

			if elixirBareRescue.MatchString(trimmed) || elixirRescueAll.MatchString(content) {
				issues = append(issues, Issue{
					ID:         "errorhandling/bare-rescue",
					Severity:   SeverityWarning,
					Category:   "errorhandling",
					File:       fileDiff.Path,
					Line:       line.NewNum,
					Message:    "Bare rescue clause catches all exceptions",
					Suggestion: "Rescue specific exception types (e.g., rescue ArgumentError ->)",
				})
			}
		}
	}

	return issues
}

// Scala error handling patterns
var (
	scalaEmptyCatch   = regexp.MustCompile(`\bcatch\s*\{\s*\}`)
	scalaCatchAllCase = regexp.MustCompile(`case\s+_\s*:\s*Throwable\s*=>`)
	scalaCatchAll     = regexp.MustCompile(`case\s+_\s*=>`)
)

func checkScalaErrorHandling(fileDiff git.FileDiff) []Issue {
	issues := make([]Issue, 0)

	inCatchBlock := false
	for _, hunk := range fileDiff.Hunks {
		for _, line := range hunk.Lines {
			if line.Type != "added" {
				continue
			}

			content := line.Content
			trimmed := strings.TrimSpace(content)

			if strings.HasPrefix(trimmed, "//") {
				continue
			}

			if strings.Contains(content, "catch") && strings.Contains(content, "{") {
				inCatchBlock = true
			}
			if inCatchBlock && strings.Contains(trimmed, "}") {
				inCatchBlock = false
			}

			if scalaEmptyCatch.MatchString(content) {
				issues = append(issues, Issue{
					ID:         "errorhandling/empty-catch",
					Severity:   SeverityWarning,
					Category:   "errorhandling",
					File:       fileDiff.Path,
					Line:       line.NewNum,
					Message:    "Empty catch block swallows exceptions",
					Suggestion: "Handle the exception or log it",
				})
			}

			if inCatchBlock && (scalaCatchAllCase.MatchString(content) || scalaCatchAll.MatchString(trimmed)) {
				issues = append(issues, Issue{
					ID:         "errorhandling/catch-all",
					Severity:   SeverityWarning,
					Category:   "errorhandling",
					File:       fileDiff.Path,
					Line:       line.NewNum,
					Message:    "Catching all exceptions with wildcard pattern",
					Suggestion: "Catch specific exception types instead of using catch-all",
				})
			}
		}
	}

	return issues
}
