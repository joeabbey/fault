package spec

import (
	"fmt"
	"os"
	"regexp"

	"gopkg.in/yaml.v3"
)

// SpecFileName is the default spec file name.
const SpecFileName = ".fault-spec.yaml"

// Spec represents a parsed specification file.
type Spec struct {
	Version      int           `yaml:"version" json:"version"`
	Title        string        `yaml:"title" json:"title"`
	Requirements []Requirement `yaml:"requirements" json:"requirements"`
}

// Requirement represents a single requirement in the spec.
type Requirement struct {
	ID          string   `yaml:"id" json:"id"`
	Description string   `yaml:"description" json:"description"`
	Priority    string   `yaml:"priority" json:"priority"`
	Status      string   `yaml:"status" json:"status"`
	Targets     []string `yaml:"targets" json:"targets"`
	Tags        []string `yaml:"tags" json:"tags"`
}

// validIDPattern matches requirement IDs: letters, digits, hyphens, underscores, ending with -digits.
var validIDPattern = regexp.MustCompile(`^[A-Za-z][A-Za-z0-9_-]*-\d+$`)

// validPriorities are the allowed priority values.
var validPriorities = map[string]bool{
	"high":   true,
	"medium": true,
	"low":    true,
}

// validStatuses are the allowed status values.
var validStatuses = map[string]bool{
	"planned":       true,
	"in-progress":   true,
	"implemented":   true,
	"verified":      true,
}

// LoadSpec reads and parses a .fault-spec.yaml file.
func LoadSpec(path string) (*Spec, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, fmt.Errorf("reading spec file: %w", err)
	}

	return ParseSpec(data)
}

// ParseSpec parses spec YAML bytes into a Spec.
func ParseSpec(data []byte) (*Spec, error) {
	var s Spec
	if err := yaml.Unmarshal(data, &s); err != nil {
		return nil, fmt.Errorf("parsing spec YAML: %w", err)
	}

	if err := s.Validate(); err != nil {
		return nil, err
	}

	return &s, nil
}

// Validate checks the spec for structural errors.
func (s *Spec) Validate() error {
	if s.Version != 1 {
		return fmt.Errorf("unsupported spec version: %d (expected 1)", s.Version)
	}

	if s.Title == "" {
		return fmt.Errorf("spec title is required")
	}

	seen := make(map[string]bool, len(s.Requirements))
	for i, req := range s.Requirements {
		if req.ID == "" {
			return fmt.Errorf("requirement %d: id is required", i)
		}
		if !validIDPattern.MatchString(req.ID) {
			return fmt.Errorf("requirement %q: id must match pattern [A-Za-z][A-Za-z0-9_-]*-\\d+ (e.g. REQ-001)", req.ID)
		}
		if seen[req.ID] {
			return fmt.Errorf("requirement %q: duplicate id", req.ID)
		}
		seen[req.ID] = true

		if req.Description == "" {
			return fmt.Errorf("requirement %q: description is required", req.ID)
		}

		if req.Priority != "" && !validPriorities[req.Priority] {
			return fmt.Errorf("requirement %q: invalid priority %q (expected high, medium, or low)", req.ID, req.Priority)
		}

		if req.Status != "" && !validStatuses[req.Status] {
			return fmt.Errorf("requirement %q: invalid status %q (expected planned, in-progress, implemented, or verified)", req.ID, req.Status)
		}
	}

	return nil
}

// RequirementByID returns the requirement with the given ID, or nil.
func (s *Spec) RequirementByID(id string) *Requirement {
	for i := range s.Requirements {
		if s.Requirements[i].ID == id {
			return &s.Requirements[i]
		}
	}
	return nil
}

// RequirementIDs returns all requirement IDs in order.
func (s *Spec) RequirementIDs() []string {
	ids := make([]string, len(s.Requirements))
	for i, req := range s.Requirements {
		ids[i] = req.ID
	}
	return ids
}
