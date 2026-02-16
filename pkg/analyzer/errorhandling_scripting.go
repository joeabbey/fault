package analyzer

import (
	"regexp"
	"strings"

	"github.com/joeabbey/fault/pkg/git"
)

// Perl error handling
var (
	perlEvalCatchAll = regexp.MustCompile(`eval\s*\{[^}]*\}\s*;\s*if\s*\(\s*\$@\s*\)`)
	perlDieNoMsg     = regexp.MustCompile(`\bdie\s*;`)
)

func checkPerlErrorHandling(fileDiff git.FileDiff) []Issue {
	issues := make([]Issue, 0)
	for _, hunk := range fileDiff.Hunks {
		for _, line := range hunk.Lines {
			if line.Type != "added" {
				continue
			}
			trimmed := strings.TrimSpace(line.Content)
			if strings.HasPrefix(trimmed, "#") {
				continue
			}
			if perlDieNoMsg.MatchString(line.Content) {
				issues = append(issues, Issue{ID: "errorhandling/perl-die-no-message", Severity: SeverityWarning, Category: "errorhandling", File: fileDiff.Path, Line: line.NewNum, Message: "die without message provides no diagnostic info", Suggestion: "Always include an error message with die"})
			}
		}
	}
	return issues
}

// PowerShell error handling
var (
	psCatchAll          = regexp.MustCompile(`(?i)catch\s*\{`)
	psSilentlyContinue  = regexp.MustCompile(`(?i)-ErrorAction\s+SilentlyContinue`)
)

func checkPowershellErrorHandling(fileDiff git.FileDiff) []Issue {
	issues := make([]Issue, 0)
	for _, hunk := range fileDiff.Hunks {
		for _, line := range hunk.Lines {
			if line.Type != "added" {
				continue
			}
			trimmed := strings.TrimSpace(line.Content)
			if strings.HasPrefix(trimmed, "#") {
				continue
			}
			if psSilentlyContinue.MatchString(line.Content) {
				issues = append(issues, Issue{ID: "errorhandling/powershell-silently-continue", Severity: SeverityWarning, Category: "errorhandling", File: fileDiff.Path, Line: line.NewNum, Message: "SilentlyContinue suppresses errors", Suggestion: "Handle errors explicitly with try/catch or -ErrorAction Stop"})
			}
		}
	}
	return issues
}

// Groovy error handling
var (
	groovyCatchAll = regexp.MustCompile(`catch\s*\(\s*Exception\s`)
)

func checkGroovyErrorHandling(fileDiff git.FileDiff) []Issue {
	issues := make([]Issue, 0)
	for _, hunk := range fileDiff.Hunks {
		for _, line := range hunk.Lines {
			if line.Type != "added" {
				continue
			}
			trimmed := strings.TrimSpace(line.Content)
			if strings.HasPrefix(trimmed, "//") {
				continue
			}
			if groovyCatchAll.MatchString(line.Content) {
				issues = append(issues, Issue{ID: "errorhandling/groovy-catch-all", Severity: SeverityWarning, Category: "errorhandling", File: fileDiff.Path, Line: line.NewNum, Message: "Catching broad Exception type hides specific errors", Suggestion: "Catch specific exception types"})
			}
		}
	}
	return issues
}
