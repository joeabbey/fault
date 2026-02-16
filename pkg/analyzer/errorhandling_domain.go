package analyzer

import (
	"regexp"
	"strings"

	"github.com/joeabbey/fault/pkg/git"
)

// Julia error handling
var (
	juliaCatchAll = regexp.MustCompile(`\bcatch\s+\w+\s*$`)
)

func checkJuliaErrorHandling(fileDiff git.FileDiff) []Issue {
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
			if juliaCatchAll.MatchString(trimmed) {
				issues = append(issues, Issue{
					ID:         "errorhandling/julia-catch-all",
					Severity:   SeverityWarning,
					Category:   "errorhandling",
					File:       fileDiff.Path,
					Line:       line.NewNum,
					Message:    "Broad catch clause may hide errors",
					Suggestion: "Catch specific exception types",
				})
			}
		}
	}
	return issues
}

// Solidity error handling
var (
	solidityRequireNoMsg = regexp.MustCompile(`\brequire\s*\([^,)]+\)\s*;`)
)

func checkSolidityErrorHandling(fileDiff git.FileDiff) []Issue {
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
			if solidityRequireNoMsg.MatchString(line.Content) {
				issues = append(issues, Issue{
					ID:         "errorhandling/solidity-require-no-message",
					Severity:   SeverityInfo,
					Category:   "errorhandling",
					File:       fileDiff.Path,
					Line:       line.NewNum,
					Message:    "require() without error message provides poor debugging",
					Suggestion: "Add a descriptive error message to require()",
				})
			}
		}
	}
	return issues
}

// Fortran, Terraform, Protobuf have minimal error handling patterns
func checkFortranErrorHandling(fileDiff git.FileDiff) []Issue  { return make([]Issue, 0) }
func checkTerraformErrorHandling(fileDiff git.FileDiff) []Issue { return make([]Issue, 0) }
func checkProtobufErrorHandling(fileDiff git.FileDiff) []Issue  { return make([]Issue, 0) }
