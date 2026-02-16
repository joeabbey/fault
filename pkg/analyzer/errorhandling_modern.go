package analyzer

import (
	"regexp"
	"strings"

	"github.com/joeabbey/fault/pkg/git"
)

// --- Zig error handling patterns ---

var (
	zigCatchUnreachableEH = regexp.MustCompile(`\bcatch\s+unreachable\b`)
	zigTryNoHandle        = regexp.MustCompile(`\btry\s+\w`)
	zigAtPanic            = regexp.MustCompile(`@panic\s*\(`)
)

func checkZigErrorHandling(fileDiff git.FileDiff) []Issue {
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

			if zigCatchUnreachableEH.MatchString(content) {
				issues = append(issues, Issue{
					ID: "errorhandling/catch-unreachable", Severity: SeverityWarning, Category: "errorhandling",
					File: fileDiff.Path, Line: line.NewNum,
					Message:    "catch unreachable will crash if the error actually occurs",
					Suggestion: "Handle the error explicitly with catch |err| or propagate with try",
				})
			}
			if zigAtPanic.MatchString(content) {
				issues = append(issues, Issue{
					ID: "errorhandling/panic", Severity: SeverityWarning, Category: "errorhandling",
					File: fileDiff.Path, Line: line.NewNum,
					Message:    "@panic() will crash the program — use error returns instead",
					Suggestion: "Return an error instead of panicking; reserve @panic for truly unrecoverable situations",
				})
			}
		}
	}
	return issues
}

// --- Nim error handling patterns ---

var (
	nimBareExcept    = regexp.MustCompile(`\bexcept\s*:`)
	nimDiscardError  = regexp.MustCompile(`discard\s+\w+\(`)
)

func checkNimErrorHandling(fileDiff git.FileDiff) []Issue {
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

			if nimBareExcept.MatchString(trimmed) {
				issues = append(issues, Issue{
					ID: "errorhandling/bare-except", Severity: SeverityWarning, Category: "errorhandling",
					File: fileDiff.Path, Line: line.NewNum,
					Message:    "Bare except: catches all exceptions including system exceptions",
					Suggestion: "Catch specific exception types (e.g., except ValueError:)",
				})
			}
		}
	}
	return issues
}

// --- Crystal error handling patterns ---

var (
	crystalEmptyRescue   = regexp.MustCompile(`rescue\s*$`)
	crystalBroadRescue   = regexp.MustCompile(`rescue\s+Exception\b`)
)

func checkCrystalErrorHandling(fileDiff git.FileDiff) []Issue {
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

			if crystalEmptyRescue.MatchString(trimmed) {
				issues = append(issues, Issue{
					ID: "errorhandling/empty-rescue", Severity: SeverityWarning, Category: "errorhandling",
					File: fileDiff.Path, Line: line.NewNum,
					Message:    "Empty rescue block catches all exceptions silently",
					Suggestion: "Rescue specific exception types or at minimum log the error",
				})
			}
			if crystalBroadRescue.MatchString(content) {
				issues = append(issues, Issue{
					ID: "errorhandling/broad-rescue", Severity: SeverityWarning, Category: "errorhandling",
					File: fileDiff.Path, Line: line.NewNum,
					Message:    "Rescuing Exception is too broad — catches system exceptions too",
					Suggestion: "Rescue specific exception types instead of the base Exception class",
				})
			}
		}
	}
	return issues
}

// --- V language error handling patterns ---

var (
	vlangOrPanicEH   = regexp.MustCompile(`or\s*\{\s*panic\s*\(`)
	vlangEmptyOr     = regexp.MustCompile(`or\s*\{\s*\}`)
)

func checkVlangErrorHandling(fileDiff git.FileDiff) []Issue {
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

			if vlangOrPanicEH.MatchString(content) {
				issues = append(issues, Issue{
					ID: "errorhandling/or-panic", Severity: SeverityWarning, Category: "errorhandling",
					File: fileDiff.Path, Line: line.NewNum,
					Message:    "or { panic(err) } crashes on error instead of handling it",
					Suggestion: "Handle the error with or { return err } or provide a default value",
				})
			}
			if vlangEmptyOr.MatchString(content) {
				issues = append(issues, Issue{
					ID: "errorhandling/empty-or-block", Severity: SeverityWarning, Category: "errorhandling",
					File: fileDiff.Path, Line: line.NewNum,
					Message:    "Empty or {} block silently swallows errors",
					Suggestion: "Handle the error or at minimum log it in the or block",
				})
			}
		}
	}
	return issues
}

// --- D language error handling patterns ---

var (
	dlangEmptyCatch      = regexp.MustCompile(`\bcatch\s*\([^)]*\)\s*\{\s*\}`)
	dlangCatchException  = regexp.MustCompile(`\bcatch\s*\(\s*Exception\s+\w+\s*\)\s*\{\s*\}`)
	dlangNothrowTryCatch = regexp.MustCompile(`nothrow.*try`)
)

func checkDlangErrorHandling(fileDiff git.FileDiff) []Issue {
	issues := make([]Issue, 0)

	for _, hunk := range fileDiff.Hunks {
		for _, line := range hunk.Lines {
			if line.Type != "added" {
				continue
			}
			content := line.Content
			trimmed := strings.TrimSpace(content)
			if strings.HasPrefix(trimmed, "//") || strings.HasPrefix(trimmed, "/*") || strings.HasPrefix(trimmed, "/+") {
				continue
			}

			if dlangEmptyCatch.MatchString(content) || dlangCatchException.MatchString(content) {
				issues = append(issues, Issue{
					ID: "errorhandling/empty-catch", Severity: SeverityWarning, Category: "errorhandling",
					File: fileDiff.Path, Line: line.NewNum,
					Message:    "Empty catch block swallows exceptions silently",
					Suggestion: "Handle the exception, log it, or rethrow it",
				})
			}
		}
	}
	return issues
}
