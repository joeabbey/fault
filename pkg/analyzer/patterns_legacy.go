package analyzer

import (
	"regexp"
	"strings"

	"github.com/joeabbey/fault/pkg/git"
)

// Visual Basic anti-patterns
var (
	vbOnErrorResumeNext = regexp.MustCompile(`(?i)\bOn\s+Error\s+Resume\s+Next\b`)
)

func checkVisualBasicPatterns(fileDiff git.FileDiff) []Issue {
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
			if vbOnErrorResumeNext.MatchString(line.Content) {
				issues = append(issues, Issue{
					ID:         "patterns/vb-on-error-resume-next",
					Severity:   SeverityWarning,
					Category:   "patterns",
					File:       fileDiff.Path,
					Line:       line.NewNum,
					Message:    "On Error Resume Next silently ignores all errors",
					Suggestion: "Use structured Try/Catch error handling",
				})
			}
		}
	}
	return issues
}

// COBOL anti-patterns
var (
	cobolGoto = regexp.MustCompile(`(?i)\bGO\s*TO\b`)
)

func checkCobolPatterns(fileDiff git.FileDiff) []Issue {
	issues := make([]Issue, 0)
	for _, hunk := range fileDiff.Hunks {
		for _, line := range hunk.Lines {
			if line.Type != "added" {
				continue
			}
			trimmed := strings.TrimSpace(line.Content)
			if strings.HasPrefix(trimmed, "*>") {
				continue
			}
			raw := line.Content
			if len(raw) > 6 && (raw[6] == '*' || raw[6] == '/') {
				continue
			}
			if cobolGoto.MatchString(line.Content) {
				issues = append(issues, Issue{
					ID:         "patterns/cobol-goto",
					Severity:   SeverityWarning,
					Category:   "patterns",
					File:       fileDiff.Path,
					Line:       line.NewNum,
					Message:    "GO TO makes control flow hard to follow",
					Suggestion: "Use PERFORM for structured control flow",
				})
			}
		}
	}
	return issues
}

// Ada anti-patterns
var (
	adaGoto = regexp.MustCompile(`(?i)\bgoto\b`)
)

func checkAdaPatterns(fileDiff git.FileDiff) []Issue {
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
			if adaGoto.MatchString(line.Content) {
				issues = append(issues, Issue{
					ID:         "patterns/ada-goto",
					Severity:   SeverityWarning,
					Category:   "patterns",
					File:       fileDiff.Path,
					Line:       line.NewNum,
					Message:    "goto makes control flow hard to follow",
					Suggestion: "Use structured control flow (loop, if, case)",
				})
			}
		}
	}
	return issues
}

// Pascal anti-patterns
var (
	pascalGoto = regexp.MustCompile(`(?i)\bgoto\b`)
)

func checkPascalPatterns(fileDiff git.FileDiff) []Issue {
	issues := make([]Issue, 0)
	for _, hunk := range fileDiff.Hunks {
		for _, line := range hunk.Lines {
			if line.Type != "added" {
				continue
			}
			trimmed := strings.TrimSpace(line.Content)
			if strings.HasPrefix(trimmed, "//") || strings.HasPrefix(trimmed, "{") || strings.HasPrefix(trimmed, "(*") {
				continue
			}
			if pascalGoto.MatchString(line.Content) {
				issues = append(issues, Issue{
					ID:         "patterns/pascal-goto",
					Severity:   SeverityWarning,
					Category:   "patterns",
					File:       fileDiff.Path,
					Line:       line.NewNum,
					Message:    "goto makes control flow hard to follow",
					Suggestion: "Use structured control flow (for, while, repeat, case)",
				})
			}
		}
	}
	return issues
}
