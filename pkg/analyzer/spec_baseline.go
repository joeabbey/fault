package analyzer

import (
	"encoding/json"
	"fmt"
	"os"
	"strings"
	"time"
)

// SpecBaselineFileName is the default filename for the spec baseline.
const SpecBaselineFileName = ".fault-spec-baseline.json"

// SpecBaseline tracks which spec requirements have already been implemented.
type SpecBaseline struct {
	Version            int       `json:"version"`
	SpecFile           string    `json:"spec_file"`
	Implemented        []string  `json:"implemented"`
	AcceptedUnexpected []string  `json:"accepted_unexpected"`
	UpdatedAt          time.Time `json:"updated_at"`
}

// SaveSpecBaseline writes the spec baseline to a JSON file.
func SaveSpecBaseline(path string, baseline *SpecBaseline) error {
	baseline.UpdatedAt = time.Now().UTC()

	data, err := json.MarshalIndent(baseline, "", "  ")
	if err != nil {
		return fmt.Errorf("marshaling spec baseline: %w", err)
	}

	if err := os.WriteFile(path, data, 0644); err != nil {
		return fmt.Errorf("writing spec baseline: %w", err)
	}

	return nil
}

// LoadSpecBaseline loads a spec baseline from a JSON file.
// Returns nil, nil if the file does not exist.
func LoadSpecBaseline(path string) (*SpecBaseline, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		if os.IsNotExist(err) {
			return nil, nil
		}
		return nil, fmt.Errorf("reading spec baseline: %w", err)
	}

	var baseline SpecBaseline
	if err := json.Unmarshal(data, &baseline); err != nil {
		return nil, fmt.Errorf("parsing spec baseline: %w", err)
	}

	return &baseline, nil
}

// MergeImplemented adds new items to the baseline's Implemented list,
// deduplicating by case-insensitive exact match. Returns the number of new items added.
func MergeImplemented(baseline *SpecBaseline, newItems []string) int {
	return mergeUnique(&baseline.Implemented, newItems)
}

// MergeAcceptedUnexpected adds new items to the baseline's AcceptedUnexpected list,
// deduplicating by case-insensitive exact match. Returns the number of new items added.
func MergeAcceptedUnexpected(baseline *SpecBaseline, newItems []string) int {
	return mergeUnique(&baseline.AcceptedUnexpected, newItems)
}

// mergeUnique appends items not already present (case-insensitive) and returns the count added.
func mergeUnique(existing *[]string, newItems []string) int {
	added := 0
	for _, item := range newItems {
		if !containsCaseInsensitive(*existing, item) {
			*existing = append(*existing, item)
			added++
		}
	}
	return added
}

func containsCaseInsensitive(items []string, target string) bool {
	norm := strings.ToLower(strings.TrimSpace(target))
	for _, item := range items {
		if strings.ToLower(strings.TrimSpace(item)) == norm {
			return true
		}
	}
	return false
}

// FilterSpecUnexpected removes unexpected items that match accepted entries in the baseline.
// Uses bidirectional strings.Contains fuzzy matching (same as matchesBaseline).
func FilterSpecUnexpected(unexpected []string, baseline *SpecBaseline) []string {
	if baseline == nil || len(baseline.AcceptedUnexpected) == 0 {
		return unexpected
	}

	filtered := make([]string, 0, len(unexpected))
	for _, item := range unexpected {
		if !matchesAccepted(item, baseline.AcceptedUnexpected) {
			filtered = append(filtered, item)
		}
	}
	return filtered
}

func matchesAccepted(item string, accepted []string) bool {
	lower := strings.ToLower(item)
	for _, a := range accepted {
		aLower := strings.ToLower(a)
		if strings.Contains(lower, aLower) || strings.Contains(aLower, lower) {
			return true
		}
	}
	return false
}

// AugmentSpecContent appends an "Already Implemented" section to the spec content
// listing items from the baseline. Returns spec unchanged if baseline is nil or empty.
func AugmentSpecContent(specContent string, baseline *SpecBaseline) string {
	if baseline == nil || len(baseline.Implemented) == 0 {
		return specContent
	}

	var b strings.Builder
	b.WriteString(specContent)
	b.WriteString("\n\n## Already Implemented\n\n")
	b.WriteString("The following requirements have been completed in prior changes. ")
	b.WriteString("Do NOT include these in \"missing\".\n\n")
	for _, item := range baseline.Implemented {
		b.WriteString("- ")
		b.WriteString(item)
		b.WriteString("\n")
	}

	return b.String()
}
