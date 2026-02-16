package analyzer

import (
	"regexp"
	"strings"

	"github.com/joeabbey/fault/pkg/git"
)

// Haskell error handling
var (
	haskellFromJust   = regexp.MustCompile(`\bfromJust\b`)
	haskellErrorCall  = regexp.MustCompile(`\berror\s+"`)
	haskellUndefined  = regexp.MustCompile(`\bundefined\b`)
)

func checkHaskellErrorHandling(fileDiff git.FileDiff) []Issue {
	issues := make([]Issue, 0)
	for _, hunk := range fileDiff.Hunks {
		for _, line := range hunk.Lines {
			if line.Type != "added" { continue }
			trimmed := strings.TrimSpace(line.Content)
			if strings.HasPrefix(trimmed, "--") { continue }
			if haskellFromJust.MatchString(line.Content) {
				issues = append(issues, Issue{ID: "errorhandling/haskell-fromjust", Severity: SeverityWarning, Category: "errorhandling", File: fileDiff.Path, Line: line.NewNum, Message: "fromJust crashes on Nothing", Suggestion: "Use pattern matching or fromMaybe with a default value"})
			}
			if haskellErrorCall.MatchString(line.Content) {
				issues = append(issues, Issue{ID: "errorhandling/haskell-error", Severity: SeverityWarning, Category: "errorhandling", File: fileDiff.Path, Line: line.NewNum, Message: "error throws an exception that terminates the program", Suggestion: "Return Either or Maybe instead of using error for recoverable failures"})
			}
			if haskellUndefined.MatchString(line.Content) {
				issues = append(issues, Issue{ID: "errorhandling/haskell-undefined", Severity: SeverityError, Category: "errorhandling", File: fileDiff.Path, Line: line.NewNum, Message: "undefined will crash at runtime", Suggestion: "Implement the function or use a typed placeholder"})
			}
		}
	}
	return issues
}

// Clojure error handling
var (
	clojureCatchAll = regexp.MustCompile(`\(catch\s+Exception\s`)
)

func checkClojureErrorHandling(fileDiff git.FileDiff) []Issue {
	issues := make([]Issue, 0)
	for _, hunk := range fileDiff.Hunks {
		for _, line := range hunk.Lines {
			if line.Type != "added" { continue }
			trimmed := strings.TrimSpace(line.Content)
			if strings.HasPrefix(trimmed, ";") { continue }
			if clojureCatchAll.MatchString(line.Content) {
				issues = append(issues, Issue{ID: "errorhandling/clojure-catch-all", Severity: SeverityWarning, Category: "errorhandling", File: fileDiff.Path, Line: line.NewNum, Message: "Catching broad Exception type hides specific errors", Suggestion: "Catch specific exception types"})
			}
		}
	}
	return issues
}

// Erlang error handling
var (
	erlangCatchAllError = regexp.MustCompile(`catch\s+_\s*->`)
)

func checkErlangErrorHandling(fileDiff git.FileDiff) []Issue {
	issues := make([]Issue, 0)
	for _, hunk := range fileDiff.Hunks {
		for _, line := range hunk.Lines {
			if line.Type != "added" { continue }
			trimmed := strings.TrimSpace(line.Content)
			if strings.HasPrefix(trimmed, "%") { continue }
			if erlangCatchAllError.MatchString(line.Content) {
				issues = append(issues, Issue{ID: "errorhandling/erlang-catch-all", Severity: SeverityWarning, Category: "errorhandling", File: fileDiff.Path, Line: line.NewNum, Message: "Catch-all clause swallows all errors", Suggestion: "Handle specific error patterns and let others propagate"})
			}
		}
	}
	return issues
}

// F# error handling
var (
	fsharpFailwith = regexp.MustCompile(`\bfailwith\b`)
)

func checkFsharpErrorHandling(fileDiff git.FileDiff) []Issue {
	issues := make([]Issue, 0)
	for _, hunk := range fileDiff.Hunks {
		for _, line := range hunk.Lines {
			if line.Type != "added" { continue }
			trimmed := strings.TrimSpace(line.Content)
			if strings.HasPrefix(trimmed, "//") { continue }
			if fsharpFailwith.MatchString(line.Content) {
				issues = append(issues, Issue{ID: "errorhandling/fsharp-failwith", Severity: SeverityWarning, Category: "errorhandling", File: fileDiff.Path, Line: line.NewNum, Message: "failwith throws an exception", Suggestion: "Return Result or Option type instead of using failwith in library code"})
			}
		}
	}
	return issues
}

// OCaml error handling
var (
	ocamlCatchAll = regexp.MustCompile(`with\s+_\s*->`)
)

func checkOcamlErrorHandling(fileDiff git.FileDiff) []Issue {
	issues := make([]Issue, 0)
	for _, hunk := range fileDiff.Hunks {
		for _, line := range hunk.Lines {
			if line.Type != "added" { continue }
			trimmed := strings.TrimSpace(line.Content)
			if strings.HasPrefix(trimmed, "(*") { continue }
			if ocamlCatchAll.MatchString(line.Content) {
				issues = append(issues, Issue{ID: "errorhandling/ocaml-catch-all", Severity: SeverityWarning, Category: "errorhandling", File: fileDiff.Path, Line: line.NewNum, Message: "Wildcard exception handler catches all exceptions", Suggestion: "Handle specific exception types and let others propagate"})
			}
		}
	}
	return issues
}
