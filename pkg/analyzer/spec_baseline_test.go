package analyzer

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

func TestSpecBaseline_SaveLoadRoundTrip(t *testing.T) {
	dir := t.TempDir()
	path := filepath.Join(dir, SpecBaselineFileName)

	baseline := &SpecBaseline{
		Version:            1,
		SpecFile:           "SPEC.md",
		Implemented:        []string{"User authentication", "Password hashing"},
		AcceptedUnexpected: []string{"Debug logging"},
	}

	if err := SaveSpecBaseline(path, baseline); err != nil {
		t.Fatalf("SaveSpecBaseline: %v", err)
	}

	loaded, err := LoadSpecBaseline(path)
	if err != nil {
		t.Fatalf("LoadSpecBaseline: %v", err)
	}

	if loaded.Version != 1 {
		t.Errorf("Version = %d, want 1", loaded.Version)
	}
	if loaded.SpecFile != "SPEC.md" {
		t.Errorf("SpecFile = %q, want %q", loaded.SpecFile, "SPEC.md")
	}
	if len(loaded.Implemented) != 2 {
		t.Errorf("Implemented count = %d, want 2", len(loaded.Implemented))
	}
	if len(loaded.AcceptedUnexpected) != 1 {
		t.Errorf("AcceptedUnexpected count = %d, want 1", len(loaded.AcceptedUnexpected))
	}
	if loaded.UpdatedAt.IsZero() {
		t.Error("UpdatedAt should not be zero")
	}
}

func TestSpecBaseline_LoadMissingFile(t *testing.T) {
	baseline, err := LoadSpecBaseline("/nonexistent/path/.fault-spec-baseline.json")
	if err != nil {
		t.Fatalf("LoadSpecBaseline should not error for missing file: %v", err)
	}
	if baseline != nil {
		t.Fatal("LoadSpecBaseline should return nil for missing file")
	}
}

func TestSpecBaseline_LoadInvalidJSON(t *testing.T) {
	dir := t.TempDir()
	path := filepath.Join(dir, SpecBaselineFileName)

	if err := os.WriteFile(path, []byte("not json"), 0644); err != nil {
		t.Fatal(err)
	}

	_, err := LoadSpecBaseline(path)
	if err == nil {
		t.Fatal("LoadSpecBaseline should error for invalid JSON")
	}
}

func TestMergeImplemented_Dedup(t *testing.T) {
	baseline := &SpecBaseline{
		Version:     1,
		Implemented: []string{"User authentication"},
	}

	added := MergeImplemented(baseline, []string{
		"user authentication", // duplicate (case-insensitive)
		"Password hashing",   // new
		"User Authentication", // duplicate (case-insensitive)
	})

	if added != 1 {
		t.Errorf("added = %d, want 1", added)
	}
	if len(baseline.Implemented) != 2 {
		t.Errorf("Implemented count = %d, want 2", len(baseline.Implemented))
	}
}

func TestMergeImplemented_Empty(t *testing.T) {
	baseline := &SpecBaseline{Version: 1}

	added := MergeImplemented(baseline, []string{"Feature A", "Feature B"})

	if added != 2 {
		t.Errorf("added = %d, want 2", added)
	}
	if len(baseline.Implemented) != 2 {
		t.Errorf("Implemented count = %d, want 2", len(baseline.Implemented))
	}
}

func TestMergeAcceptedUnexpected(t *testing.T) {
	baseline := &SpecBaseline{
		Version:            1,
		AcceptedUnexpected: []string{"Debug logging"},
	}

	added := MergeAcceptedUnexpected(baseline, []string{
		"debug logging", // duplicate
		"Extra config",  // new
	})

	if added != 1 {
		t.Errorf("added = %d, want 1", added)
	}
	if len(baseline.AcceptedUnexpected) != 2 {
		t.Errorf("AcceptedUnexpected count = %d, want 2", len(baseline.AcceptedUnexpected))
	}
}

func TestFilterSpecUnexpected_FuzzyMatch(t *testing.T) {
	baseline := &SpecBaseline{
		AcceptedUnexpected: []string{"Debug logging added"},
	}

	unexpected := []string{
		"Debug logging added to auth module", // contains accepted
		"New API endpoint",                   // no match
	}

	filtered := FilterSpecUnexpected(unexpected, baseline)

	if len(filtered) != 1 {
		t.Fatalf("filtered count = %d, want 1", len(filtered))
	}
	if filtered[0] != "New API endpoint" {
		t.Errorf("filtered[0] = %q, want %q", filtered[0], "New API endpoint")
	}
}

func TestFilterSpecUnexpected_NilBaseline(t *testing.T) {
	unexpected := []string{"Some change"}
	filtered := FilterSpecUnexpected(unexpected, nil)

	if len(filtered) != 1 {
		t.Errorf("filtered count = %d, want 1", len(filtered))
	}
}

func TestFilterSpecUnexpected_EmptyAccepted(t *testing.T) {
	baseline := &SpecBaseline{AcceptedUnexpected: []string{}}
	unexpected := []string{"Some change"}
	filtered := FilterSpecUnexpected(unexpected, baseline)

	if len(filtered) != 1 {
		t.Errorf("filtered count = %d, want 1", len(filtered))
	}
}

func TestAugmentSpecContent_NilBaseline(t *testing.T) {
	spec := "# My Spec\n\n- Feature A"
	result := AugmentSpecContent(spec, nil)

	if result != spec {
		t.Error("AugmentSpecContent should return spec unchanged for nil baseline")
	}
}

func TestAugmentSpecContent_EmptyImplemented(t *testing.T) {
	spec := "# My Spec\n\n- Feature A"
	baseline := &SpecBaseline{Implemented: []string{}}
	result := AugmentSpecContent(spec, baseline)

	if result != spec {
		t.Error("AugmentSpecContent should return spec unchanged for empty Implemented")
	}
}

func TestAugmentSpecContent_Populated(t *testing.T) {
	spec := "# My Spec\n\n- Feature A\n- Feature B"
	baseline := &SpecBaseline{
		Implemented: []string{"User auth", "Password hashing"},
	}

	result := AugmentSpecContent(spec, baseline)

	if !strings.Contains(result, "## Already Implemented") {
		t.Error("result should contain '## Already Implemented' header")
	}
	if !strings.Contains(result, "- User auth") {
		t.Error("result should contain '- User auth'")
	}
	if !strings.Contains(result, "- Password hashing") {
		t.Error("result should contain '- Password hashing'")
	}
	if !strings.HasPrefix(result, spec) {
		t.Error("result should start with original spec content")
	}
}
