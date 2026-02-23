package spec

import (
	"testing"
)

func TestParseSpec_Valid(t *testing.T) {
	yaml := []byte(`
version: 1
title: "Feature X Specification"
requirements:
  - id: REQ-001
    description: "JWT authentication middleware"
    priority: high
    status: planned
    targets: ["pkg/auth/*.go"]
    tags: ["auth"]
  - id: REQ-002
    description: "Rate limiting on API endpoints"
    priority: medium
    status: in-progress
    targets: ["pkg/api/*.go"]
    tags: ["api", "security"]
`)

	s, err := ParseSpec(yaml)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	if s.Version != 1 {
		t.Errorf("version = %d, want 1", s.Version)
	}
	if s.Title != "Feature X Specification" {
		t.Errorf("title = %q, want %q", s.Title, "Feature X Specification")
	}
	if len(s.Requirements) != 2 {
		t.Fatalf("len(requirements) = %d, want 2", len(s.Requirements))
	}

	req := s.Requirements[0]
	if req.ID != "REQ-001" {
		t.Errorf("req[0].ID = %q, want %q", req.ID, "REQ-001")
	}
	if req.Priority != "high" {
		t.Errorf("req[0].Priority = %q, want %q", req.Priority, "high")
	}
	if len(req.Targets) != 1 || req.Targets[0] != "pkg/auth/*.go" {
		t.Errorf("req[0].Targets = %v, want [pkg/auth/*.go]", req.Targets)
	}
}

func TestParseSpec_InvalidVersion(t *testing.T) {
	yaml := []byte(`
version: 2
title: "Test"
requirements:
  - id: REQ-001
    description: "test"
`)
	_, err := ParseSpec(yaml)
	if err == nil {
		t.Fatal("expected error for version 2")
	}
}

func TestParseSpec_MissingTitle(t *testing.T) {
	yaml := []byte(`
version: 1
requirements:
  - id: REQ-001
    description: "test"
`)
	_, err := ParseSpec(yaml)
	if err == nil {
		t.Fatal("expected error for missing title")
	}
}

func TestParseSpec_DuplicateID(t *testing.T) {
	yaml := []byte(`
version: 1
title: "Test"
requirements:
  - id: REQ-001
    description: "first"
  - id: REQ-001
    description: "duplicate"
`)
	_, err := ParseSpec(yaml)
	if err == nil {
		t.Fatal("expected error for duplicate ID")
	}
}

func TestParseSpec_InvalidID(t *testing.T) {
	tests := []struct {
		name string
		id   string
	}{
		{"no trailing number", "REQ"},
		{"starts with number", "1REQ-001"},
		{"empty", ""},
		{"spaces", "REQ 001"},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			yaml := []byte(`
version: 1
title: "Test"
requirements:
  - id: "` + tt.id + `"
    description: "test"
`)
			_, err := ParseSpec(yaml)
			if err == nil {
				t.Fatalf("expected error for id %q", tt.id)
			}
		})
	}
}

func TestParseSpec_InvalidPriority(t *testing.T) {
	yaml := []byte(`
version: 1
title: "Test"
requirements:
  - id: REQ-001
    description: "test"
    priority: critical
`)
	_, err := ParseSpec(yaml)
	if err == nil {
		t.Fatal("expected error for invalid priority")
	}
}

func TestParseSpec_InvalidStatus(t *testing.T) {
	yaml := []byte(`
version: 1
title: "Test"
requirements:
  - id: REQ-001
    description: "test"
    status: done
`)
	_, err := ParseSpec(yaml)
	if err == nil {
		t.Fatal("expected error for invalid status")
	}
}

func TestParseSpec_EmptyRequirements(t *testing.T) {
	yaml := []byte(`
version: 1
title: "Test"
requirements: []
`)
	s, err := ParseSpec(yaml)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(s.Requirements) != 0 {
		t.Errorf("len(requirements) = %d, want 0", len(s.Requirements))
	}
}

func TestRequirementByID(t *testing.T) {
	s := &Spec{
		Version: 1,
		Title:   "Test",
		Requirements: []Requirement{
			{ID: "REQ-001", Description: "first"},
			{ID: "REQ-002", Description: "second"},
		},
	}

	req := s.RequirementByID("REQ-002")
	if req == nil {
		t.Fatal("expected non-nil requirement")
	}
	if req.Description != "second" {
		t.Errorf("Description = %q, want %q", req.Description, "second")
	}

	if got := s.RequirementByID("REQ-999"); got != nil {
		t.Errorf("expected nil for unknown ID, got %v", got)
	}
}

func TestRequirementIDs(t *testing.T) {
	s := &Spec{
		Version: 1,
		Title:   "Test",
		Requirements: []Requirement{
			{ID: "REQ-001", Description: "first"},
			{ID: "AUTH-002", Description: "second"},
		},
	}

	ids := s.RequirementIDs()
	if len(ids) != 2 {
		t.Fatalf("len(ids) = %d, want 2", len(ids))
	}
	if ids[0] != "REQ-001" || ids[1] != "AUTH-002" {
		t.Errorf("ids = %v, want [REQ-001 AUTH-002]", ids)
	}
}

func TestValidIDPattern(t *testing.T) {
	valid := []string{
		"REQ-001",
		"AUTH-1",
		"My_Feature-42",
		"a-1",
		"PERF-100",
	}
	for _, id := range valid {
		if !validIDPattern.MatchString(id) {
			t.Errorf("expected %q to be valid", id)
		}
	}

	invalid := []string{
		"001",
		"-REQ-001",
		"REQ",
		"REQ-",
		"1REQ-001",
		"",
	}
	for _, id := range invalid {
		if validIDPattern.MatchString(id) {
			t.Errorf("expected %q to be invalid", id)
		}
	}
}
