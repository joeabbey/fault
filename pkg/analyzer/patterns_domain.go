package analyzer

import (
	"regexp"
	"strings"

	"github.com/joeabbey/fault/pkg/git"
)

// Julia anti-patterns
var (
	juliaGlobalVar = regexp.MustCompile(`\bglobal\s+\w+`)
)

func checkJuliaPatterns(fileDiff git.FileDiff) []Issue {
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
			if juliaGlobalVar.MatchString(line.Content) {
				issues = append(issues, Issue{
					ID:         "patterns/julia-global-var",
					Severity:   SeverityWarning,
					Category:   "patterns",
					File:       fileDiff.Path,
					Line:       line.NewNum,
					Message:    "Global variable reduces performance",
					Suggestion: "Use const or pass values as function arguments",
				})
			}
		}
	}
	return issues
}

// Fortran anti-patterns
var (
	fortranGoto = regexp.MustCompile(`(?i)\bgoto\b`)
)

func checkFortranPatterns(fileDiff git.FileDiff) []Issue {
	issues := make([]Issue, 0)
	for _, hunk := range fileDiff.Hunks {
		for _, line := range hunk.Lines {
			if line.Type != "added" {
				continue
			}
			trimmed := strings.TrimSpace(line.Content)
			if strings.HasPrefix(trimmed, "!") {
				continue
			}
			if fortranGoto.MatchString(line.Content) {
				issues = append(issues, Issue{
					ID:         "patterns/fortran-goto",
					Severity:   SeverityWarning,
					Category:   "patterns",
					File:       fileDiff.Path,
					Line:       line.NewNum,
					Message:    "GOTO makes control flow hard to follow",
					Suggestion: "Use structured constructs (DO, IF, SELECT CASE)",
				})
			}
		}
	}
	return issues
}

// Solidity anti-patterns
func checkSolidityPatterns(fileDiff git.FileDiff) []Issue {
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
			if strings.Contains(trimmed, "pragma") {
				continue
			}
		}
	}
	return issues
}

// Terraform anti-patterns
var (
	tfHardcodedAmi = regexp.MustCompile(`ami\s*=\s*"ami-[a-f0-9]+"`)
)

func checkTerraformPatterns(fileDiff git.FileDiff) []Issue {
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
			if tfHardcodedAmi.MatchString(line.Content) {
				issues = append(issues, Issue{
					ID:         "patterns/terraform-hardcoded-ami",
					Severity:   SeverityInfo,
					Category:   "patterns",
					File:       fileDiff.Path,
					Line:       line.NewNum,
					Message:    "Hardcoded AMI ID may be region-specific",
					Suggestion: "Use a data source or variable for AMI IDs",
				})
			}
		}
	}
	return issues
}

// Protobuf anti-patterns (minimal - protobuf is declarative)
func checkProtobufPatterns(fileDiff git.FileDiff) []Issue {
	return make([]Issue, 0)
}
