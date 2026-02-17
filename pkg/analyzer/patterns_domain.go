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
var (
	solidityUncheckedCall = regexp.MustCompile(`\.call\s*[\({]`)
	solidityCallReturn    = regexp.MustCompile(`\(\s*bool\s+\w+\s*,`)
	solidityMagicNumber   = regexp.MustCompile(`(?:=|return|==|!=|>=|<=|>|<)\s*\d{2,}`)
	solidityFunction      = regexp.MustCompile(`^\s*function\s+\w+`)
	solidityEmitEvent     = regexp.MustCompile(`\bemit\s+\w+`)
)

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
			// Unchecked low-level .call() return value
			if solidityUncheckedCall.MatchString(line.Content) && !solidityCallReturn.MatchString(line.Content) {
				issues = append(issues, Issue{
					ID:         "patterns/solidity-unchecked-call",
					Severity:   SeverityWarning,
					Category:   "patterns",
					File:       fileDiff.Path,
					Line:       line.NewNum,
					Message:    "Low-level .call() without checking return value",
					Suggestion: "Capture and check the bool return: (bool success, ) = addr.call(...)",
				})
			}
			// Magic numbers
			if solidityMagicNumber.MatchString(line.Content) && !strings.Contains(trimmed, "constant") && !strings.Contains(trimmed, "immutable") {
				issues = append(issues, Issue{
					ID:         "patterns/solidity-magic-number",
					Severity:   SeverityInfo,
					Category:   "patterns",
					File:       fileDiff.Path,
					Line:       line.NewNum,
					Message:    "Magic number without named constant",
					Suggestion: "Define a named constant for clarity and maintainability",
				})
			}
			// State-changing functions without events
			if solidityFunction.MatchString(line.Content) && !strings.Contains(trimmed, "view") && !strings.Contains(trimmed, "pure") && !solidityEmitEvent.MatchString(line.Content) {
				issues = append(issues, Issue{
					ID:         "patterns/solidity-missing-event",
					Severity:   SeverityInfo,
					Category:   "patterns",
					File:       fileDiff.Path,
					Line:       line.NewNum,
					Message:    "State-changing function without event emission",
					Suggestion: "Emit an event for off-chain tracking of state changes",
				})
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

// Protobuf anti-patterns
var (
	protobufDeprecated   = regexp.MustCompile(`\[deprecated\s*=\s*true\]`)
	protobufFieldNumber  = regexp.MustCompile(`=\s*(\d+)\s*;`)
	protobufRepeated     = regexp.MustCompile(`^\s*repeated\s+`)
)

func checkProtobufPatterns(fileDiff git.FileDiff) []Issue {
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
			// Deprecated option usage
			if protobufDeprecated.MatchString(line.Content) {
				issues = append(issues, Issue{
					ID:         "patterns/protobuf-deprecated-field",
					Severity:   SeverityInfo,
					Category:   "patterns",
					File:       fileDiff.Path,
					Line:       line.NewNum,
					Message:    "Field marked as deprecated",
					Suggestion: "Plan to remove deprecated fields in a future version",
				})
			}
			// High field numbers for frequently-used fields (repeated fields)
			if protobufRepeated.MatchString(line.Content) {
				matches := protobufFieldNumber.FindStringSubmatch(line.Content)
				if len(matches) > 1 {
					num := 0
					for _, c := range matches[1] {
						num = num*10 + int(c-'0')
					}
					if num > 15 {
						issues = append(issues, Issue{
							ID:         "patterns/protobuf-high-field-number",
							Severity:   SeverityInfo,
							Category:   "patterns",
							File:       fileDiff.Path,
							Line:       line.NewNum,
							Message:    "Repeated field with number > 15 uses less efficient encoding",
							Suggestion: "Use field numbers 1-15 for frequently-used fields (1-byte tag encoding)",
						})
					}
				}
			}
		}
	}
	return issues
}
