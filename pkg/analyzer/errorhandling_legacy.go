package analyzer

import (
	"regexp"
	"strings"

	"github.com/joeabbey/fault/pkg/git"
)

// Visual Basic error handling
var (
	vbEmptyCatch = regexp.MustCompile(`(?i)^\s*Catch\b`)
)

func checkVisualBasicErrorHandling(fileDiff git.FileDiff) []Issue {
	issues := make([]Issue, 0)
	for _, hunk := range fileDiff.Hunks {
		for _, line := range hunk.Lines {
			if line.Type != "added" {
				continue
			}
			trimmed := strings.TrimSpace(line.Content)
			if strings.HasPrefix(trimmed, "'") {
				continue
			}
			if vbEmptyCatch.MatchString(trimmed) && !strings.Contains(strings.ToLower(trimmed), " as ") && !strings.Contains(strings.ToLower(trimmed), " when ") {
				issues = append(issues, Issue{
					ID:         "errorhandling/vb-empty-catch",
					Severity:   SeverityWarning,
					Category:   "errorhandling",
					File:       fileDiff.Path,
					Line:       line.NewNum,
					Message:    "Catch without exception type catches all exceptions",
					Suggestion: "Catch specific exception types (e.g., Catch ex As IOException)",
				})
			}
		}
	}
	return issues
}

// COBOL: minimal error handling patterns
func checkCobolErrorHandling(fileDiff git.FileDiff) []Issue { return make([]Issue, 0) }

// Ada error handling
var (
	adaWhenOthers = regexp.MustCompile(`(?i)\bwhen\s+others\s*=>`)
)

func checkAdaErrorHandling(fileDiff git.FileDiff) []Issue {
	issues := make([]Issue, 0)
	for _, hunk := range fileDiff.Hunks {
		for _, line := range hunk.Lines {
			if line.Type != "added" {
				continue
			}
			trimmed := strings.TrimSpace(line.Content)
			if strings.HasPrefix(trimmed, "--") {
				continue
			}
			if adaWhenOthers.MatchString(line.Content) {
				issues = append(issues, Issue{
					ID:         "errorhandling/ada-when-others",
					Severity:   SeverityInfo,
					Category:   "errorhandling",
					File:       fileDiff.Path,
					Line:       line.NewNum,
					Message:    "'when others' catches all exceptions",
					Suggestion: "Handle specific exceptions before 'when others'",
				})
			}
		}
	}
	return issues
}

// Pascal error handling
var (
	pascalExceptAll = regexp.MustCompile(`(?i)^\s*except\s*$`)
)

func checkPascalErrorHandling(fileDiff git.FileDiff) []Issue {
	issues := make([]Issue, 0)
	for _, hunk := range fileDiff.Hunks {
		for _, line := range hunk.Lines {
			if line.Type != "added" {
				continue
			}
			trimmed := strings.TrimSpace(line.Content)
			if strings.HasPrefix(trimmed, "//") || strings.HasPrefix(trimmed, "{") {
				continue
			}
			if pascalExceptAll.MatchString(trimmed) {
				issues = append(issues, Issue{
					ID:         "errorhandling/pascal-bare-except",
					Severity:   SeverityWarning,
					Category:   "errorhandling",
					File:       fileDiff.Path,
					Line:       line.NewNum,
					Message:    "Bare 'except' catches all exceptions",
					Suggestion: "Use 'on E: ExceptionType do' for specific handling",
				})
			}
		}
	}
	return issues
}
