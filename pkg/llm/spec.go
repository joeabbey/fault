package llm

import (
	"context"
	"fmt"
	"strings"

	"github.com/joeabbey/fault/pkg/analyzer"
	"github.com/joeabbey/fault/pkg/git"
)

// SpecResult holds the comparison between a spec and a diff.
type SpecResult struct {
	Implemented []string `json:"implemented"` // features from spec that were implemented
	Missing     []string `json:"missing"`     // features from spec NOT in the diff
	Unexpected  []string `json:"unexpected"`  // changes in diff NOT in the spec
	Score       float64  `json:"score"`       // 0.0-1.0, how well diff matches spec
}

// CompareSpec uses the LLM to compare a specification against actual code changes.
func CompareSpec(ctx context.Context, client *Client, specContent string, diff *git.Diff) (*SpecResult, error) {
	if client == nil {
		return nil, fmt.Errorf("LLM client is nil")
	}

	if specContent == "" {
		return nil, fmt.Errorf("spec content is empty")
	}

	if diff == nil || len(diff.Files) == 0 {
		return &SpecResult{
			Implemented: make([]string, 0),
			Missing:     make([]string, 0),
			Unexpected:  make([]string, 0),
			Score:       0.0,
		}, nil
	}

	// Build the user prompt with spec and diff
	userPrompt := buildSpecPrompt(specContent, diff)

	// Load the system prompt
	systemPrompt := LoadPrompt("spec_compare")
	if systemPrompt == "" {
		return nil, fmt.Errorf("failed to load spec_compare prompt template")
	}

	// Send to LLM and extract JSON
	var result SpecResult
	if err := client.ExtractJSON(ctx, systemPrompt, userPrompt, &result); err != nil {
		return nil, fmt.Errorf("LLM spec comparison failed: %w", err)
	}

	// Validate the result
	result = validateSpecResult(result)

	return &result, nil
}

// SpecResultToIssues converts a SpecResult into analyzer Issues.
// Missing features become warnings; unexpected changes become info-level issues.
func SpecResultToIssues(result *SpecResult) []analyzer.Issue {
	issues := make([]analyzer.Issue, 0, len(result.Missing)+len(result.Unexpected))

	for i, missing := range result.Missing {
		issues = append(issues, analyzer.Issue{
			ID:       fmt.Sprintf("spec-missing-%d", i+1),
			Severity: analyzer.SeverityWarning,
			Category: "spec",
			Message:  fmt.Sprintf("Spec requirement not implemented: %s", missing),
		})
	}

	for i, unexpected := range result.Unexpected {
		issues = append(issues, analyzer.Issue{
			ID:       fmt.Sprintf("spec-unexpected-%d", i+1),
			Severity: analyzer.SeverityInfo,
			Category: "spec",
			Message:  fmt.Sprintf("Change not described in spec: %s", unexpected),
		})
	}

	return issues
}

// buildSpecPrompt constructs the user prompt for spec comparison.
func buildSpecPrompt(specContent string, diff *git.Diff) string {
	var b strings.Builder

	b.WriteString("## Specification\n\n")
	b.WriteString(specContent)
	b.WriteString("\n\n")

	b.WriteString("## Code Changes (Diff)\n\n")
	b.WriteString(BuildDiffSummary(diff))

	return b.String()
}

// validateSpecResult ensures the result has valid data and clamped scores.
func validateSpecResult(result SpecResult) SpecResult {
	if result.Implemented == nil {
		result.Implemented = make([]string, 0)
	}
	if result.Missing == nil {
		result.Missing = make([]string, 0)
	}
	if result.Unexpected == nil {
		result.Unexpected = make([]string, 0)
	}

	result.Score = clampScore(result.Score)

	return result
}

// StructuredSpecResult holds per-requirement analysis from the LLM.
type StructuredSpecResult struct {
	Requirements []RequirementResult `json:"requirements"`
	OverallScore float64             `json:"overall_score"`
	Summary      string              `json:"summary"`
}

// RequirementResult is the LLM's assessment of a single requirement.
type RequirementResult struct {
	ID         string  `json:"id"`
	Status     string  `json:"status"`     // "implemented", "partial", "missing"
	Evidence   string  `json:"evidence"`
	Confidence float64 `json:"confidence"` // 0.0-1.0
}

// CompareSpecStructured uses the LLM to do per-requirement spec validation.
func CompareSpecStructured(ctx context.Context, client *Client, specYAML string, diffSummary string) (*StructuredSpecResult, error) {
	if client == nil {
		return nil, fmt.Errorf("LLM client is nil")
	}

	if specYAML == "" {
		return nil, fmt.Errorf("spec content is empty")
	}

	if diffSummary == "" {
		return &StructuredSpecResult{
			Requirements: make([]RequirementResult, 0),
			OverallScore: 0.0,
			Summary:      "No code changes to evaluate.",
		}, nil
	}

	userPrompt := buildStructuredSpecPrompt(specYAML, diffSummary)

	systemPrompt := LoadPrompt("spec_structured")
	if systemPrompt == "" {
		return nil, fmt.Errorf("failed to load spec_structured prompt template")
	}

	var result StructuredSpecResult
	if err := client.ExtractJSON(ctx, systemPrompt, userPrompt, &result); err != nil {
		return nil, fmt.Errorf("LLM structured spec comparison failed: %w", err)
	}

	result = validateStructuredSpecResult(result)
	return &result, nil
}

// StructuredSpecResultToIssues converts per-requirement results into analyzer Issues.
func StructuredSpecResultToIssues(result *StructuredSpecResult) []analyzer.Issue {
	issues := make([]analyzer.Issue, 0)

	for _, req := range result.Requirements {
		switch req.Status {
		case "missing":
			issues = append(issues, analyzer.Issue{
				ID:         fmt.Sprintf("spec-llm-missing-%s", req.ID),
				Severity:   analyzer.SeverityWarning,
				Category:   "spec",
				Message:    fmt.Sprintf("Requirement %s not implemented: %s", req.ID, req.Evidence),
				Suggestion: fmt.Sprintf("Implement requirement %s (confidence: %.0f%%)", req.ID, req.Confidence*100),
			})
		case "partial":
			issues = append(issues, analyzer.Issue{
				ID:         fmt.Sprintf("spec-llm-partial-%s", req.ID),
				Severity:   analyzer.SeverityInfo,
				Category:   "spec",
				Message:    fmt.Sprintf("Requirement %s partially implemented: %s", req.ID, req.Evidence),
				Suggestion: fmt.Sprintf("Complete requirement %s (confidence: %.0f%%)", req.ID, req.Confidence*100),
			})
		}
	}

	return issues
}

// buildStructuredSpecPrompt constructs the user prompt for structured spec comparison.
func buildStructuredSpecPrompt(specYAML string, diffSummary string) string {
	var b strings.Builder

	b.WriteString("## Specification (YAML)\n\n```yaml\n")
	b.WriteString(specYAML)
	b.WriteString("\n```\n\n")

	b.WriteString("## Code Changes (Diff)\n\n")
	b.WriteString(diffSummary)

	return b.String()
}

// validateStructuredSpecResult ensures the result has valid data.
func validateStructuredSpecResult(result StructuredSpecResult) StructuredSpecResult {
	if result.Requirements == nil {
		result.Requirements = make([]RequirementResult, 0)
	}

	for i := range result.Requirements {
		result.Requirements[i].Confidence = clampScore(result.Requirements[i].Confidence)
		switch result.Requirements[i].Status {
		case "implemented", "partial", "missing":
			// valid
		default:
			result.Requirements[i].Status = "missing"
		}
	}

	result.OverallScore = clampScore(result.OverallScore)

	return result
}
