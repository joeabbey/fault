package analyzer

import (
	"encoding/json"
	"fmt"
	"os"
	"strings"
)

// BaselineEntry represents a single suppressed issue.
type BaselineEntry struct {
	ID       string `json:"id"`
	Category string `json:"category"`
	File     string `json:"file"`
	Message  string `json:"message"`
}

// Baseline holds a set of known issues to suppress.
type Baseline struct {
	Version int             `json:"version"`
	Issues  []BaselineEntry `json:"issues"`
}

// SaveBaseline saves the current issues as a baseline file.
func SaveBaseline(path string, result *AnalysisResult) error {
	entries := make([]BaselineEntry, 0, len(result.Issues))
	for _, issue := range result.Issues {
		entries = append(entries, BaselineEntry{
			ID:       issue.ID,
			Category: issue.Category,
			File:     issue.File,
			Message:  issue.Message,
		})
	}

	baseline := Baseline{
		Version: 1,
		Issues:  entries,
	}

	data, err := json.MarshalIndent(baseline, "", "  ")
	if err != nil {
		return fmt.Errorf("marshaling baseline: %w", err)
	}

	if err := os.WriteFile(path, data, 0644); err != nil {
		return fmt.Errorf("writing baseline: %w", err)
	}

	return nil
}

// LoadBaseline loads a baseline from a JSON file.
func LoadBaseline(path string) (*Baseline, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, fmt.Errorf("reading baseline: %w", err)
	}

	var baseline Baseline
	if err := json.Unmarshal(data, &baseline); err != nil {
		return nil, fmt.Errorf("parsing baseline: %w", err)
	}

	return &baseline, nil
}

// FilterBaseline removes issues that match baseline entries, returning only new issues.
// Matching is by ID, category, and file. Line numbers are ignored because they shift
// as code changes. Message matching uses a contains check for fuzzy matching.
func FilterBaseline(issues []Issue, baseline *Baseline) []Issue {
	if baseline == nil || len(baseline.Issues) == 0 {
		return issues
	}

	filtered := make([]Issue, 0, len(issues))
	for _, issue := range issues {
		if !matchesBaseline(issue, baseline) {
			filtered = append(filtered, issue)
		}
	}
	return filtered
}

// matchesBaseline checks if an issue matches any entry in the baseline.
func matchesBaseline(issue Issue, baseline *Baseline) bool {
	for _, entry := range baseline.Issues {
		if entry.ID == issue.ID &&
			entry.Category == issue.Category &&
			entry.File == issue.File &&
			(strings.Contains(issue.Message, entry.Message) || strings.Contains(entry.Message, issue.Message)) {
			return true
		}
	}
	return false
}

// ParseSuppressionComments parses a line for fault:ignore directives.
// Returns the list of suppressed categories, or ["*"] for suppress-all.
// Returns nil if no suppression directive is found.
func ParseSuppressionComments(line string) []string {
	// Look for "fault:ignore" in the line
	idx := strings.Index(line, "fault:ignore")
	if idx < 0 {
		return nil
	}

	rest := strings.TrimSpace(line[idx+len("fault:ignore"):])

	// If nothing follows or rest starts with a non-word character that isn't a category
	if rest == "" {
		return []string{"*"}
	}

	// Split the rest by whitespace and commas to get categories
	categories := make([]string, 0)
	for _, part := range strings.FieldsFunc(rest, func(r rune) bool {
		return r == ',' || r == ' ' || r == '\t'
	}) {
		// Stop at comment terminators like */ or //
		if part == "*/" || part == "//" {
			break
		}
		categories = append(categories, part)
	}

	if len(categories) == 0 {
		return []string{"*"}
	}

	return categories
}

// FilterSuppressed removes issues that are suppressed by inline comments.
// For each issue at line N, it checks if line N-1 or line N contains a
// fault:ignore directive matching the issue's category.
func FilterSuppressed(issues []Issue, fileLines map[string][]string) []Issue {
	if len(fileLines) == 0 {
		return issues
	}

	filtered := make([]Issue, 0, len(issues))
	for _, issue := range issues {
		lines, ok := fileLines[issue.File]
		if !ok || issue.Line <= 0 {
			filtered = append(filtered, issue)
			continue
		}

		if isSuppressed(issue, lines) {
			continue
		}
		filtered = append(filtered, issue)
	}
	return filtered
}

// isSuppressed checks if an issue is suppressed by an inline comment.
func isSuppressed(issue Issue, lines []string) bool {
	// Check the line itself (1-indexed to 0-indexed)
	lineIdx := issue.Line - 1
	if lineIdx >= 0 && lineIdx < len(lines) {
		if cats := ParseSuppressionComments(lines[lineIdx]); cats != nil {
			if matchesSuppression(issue.Category, cats) {
				return true
			}
		}
	}

	// Check the line above
	prevIdx := lineIdx - 1
	if prevIdx >= 0 && prevIdx < len(lines) {
		if cats := ParseSuppressionComments(lines[prevIdx]); cats != nil {
			if matchesSuppression(issue.Category, cats) {
				return true
			}
		}
	}

	return false
}

// matchesSuppression checks if a category matches a list of suppressed categories.
func matchesSuppression(category string, suppressed []string) bool {
	for _, s := range suppressed {
		if s == "*" || s == category {
			return true
		}
	}
	return false
}
