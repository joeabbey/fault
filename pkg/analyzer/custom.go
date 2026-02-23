package analyzer

import (
	"log"
	"path/filepath"
	"regexp"
	"strings"
)

// CustomRuleConfig defines a single custom rule from config.
type CustomRuleConfig struct {
	ID       string
	Pattern  string // regex
	Files    string // glob
	Severity string // error, warning, info
	Message  string
}

// CustomRuleAnalyzer checks files against user-defined regex patterns.
type CustomRuleAnalyzer struct {
	rules []CustomRuleConfig
}

// NewCustomRuleAnalyzer creates a new custom rule analyzer.
func NewCustomRuleAnalyzer(rules []CustomRuleConfig) *CustomRuleAnalyzer {
	return &CustomRuleAnalyzer{rules: rules}
}

// Name returns the analyzer name.
func (a *CustomRuleAnalyzer) Name() string { return "custom" }

// Analyze checks parsed files against custom rules.
func (a *CustomRuleAnalyzer) Analyze(ctx *AnalysisContext) ([]Issue, error) {
	issues := make([]Issue, 0)

	for _, rule := range a.rules {
		re, err := regexp.Compile(rule.Pattern)
		if err != nil {
			log.Printf("warning: custom rule %q has invalid regex %q: %v", rule.ID, rule.Pattern, err)
			continue
		}

		severity := parseSeverity(rule.Severity)

		for _, fileDiff := range ctx.Diff.Files {
			if fileDiff.Status == "deleted" {
				continue
			}

			// Check glob match
			if rule.Files != "" {
				matched, err := filepath.Match(rule.Files, filepath.Base(fileDiff.Path))
				if err != nil || !matched {
					// Also try matching against the full path
					matched2, _ := filepath.Match(rule.Files, fileDiff.Path)
					if !matched2 {
						continue
					}
				}
			}

			// Search through added/changed lines (hunks)
			for _, hunk := range fileDiff.Hunks {
				for _, line := range hunk.Lines {
					if line.Type != "added" {
						continue
					}
					if re.MatchString(line.Content) {
						issues = append(issues, Issue{
							ID:       rule.ID,
							Severity: severity,
							Category: "custom",
							File:     fileDiff.Path,
							Line:     line.NewNum,
							Message:  rule.Message,
						})
					}
				}
			}
		}
	}

	return issues, nil
}

// parseSeverity converts a severity string to a Severity value.
func parseSeverity(s string) Severity {
	switch strings.ToLower(s) {
	case "error":
		return SeverityError
	case "warning":
		return SeverityWarning
	case "info":
		return SeverityInfo
	default:
		return SeverityWarning
	}
}
