package analyzer

import (
	"os"
	"path/filepath"
	"testing"

	"github.com/joeabbey/fault/pkg/config"
	"github.com/joeabbey/fault/pkg/git"
	"github.com/joeabbey/fault/pkg/parser"
)

func TestSpecAnalyzer_Name(t *testing.T) {
	a := NewSpecAnalyzer()
	if a.Name() != "spec" {
		t.Errorf("Name() = %q, want %q", a.Name(), "spec")
	}
}

func TestSpecAnalyzer_NoSpecFile(t *testing.T) {
	a := NewSpecAnalyzer()
	ctx := &AnalysisContext{
		Config: &config.Config{},
	}

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(issues) != 0 {
		t.Errorf("expected no issues when spec file is empty, got %d", len(issues))
	}
}

func TestSpecAnalyzer_OrphanedAnchor(t *testing.T) {
	dir := t.TempDir()

	// Write spec with one requirement
	specContent := `version: 1
title: "Test Spec"
requirements:
  - id: REQ-001
    description: "The only requirement"
`
	specPath := filepath.Join(dir, ".fault-spec.yaml")
	if err := os.WriteFile(specPath, []byte(specContent), 0644); err != nil {
		t.Fatal(err)
	}

	// Write a source file with an orphaned anchor
	srcDir := filepath.Join(dir, "pkg")
	if err := os.MkdirAll(srcDir, 0755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(srcDir, "main.go"), []byte("// spec:REQ-001\n// spec:ORPHAN-999\nfunc Foo() {}\n"), 0644); err != nil {
		t.Fatal(err)
	}

	a := NewSpecAnalyzer()
	ctx := &AnalysisContext{
		RepoPath: dir,
		Config: &config.Config{
			LLM: config.LLMConfig{SpecFile: specPath},
		},
	}

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	orphanCount := 0
	for _, issue := range issues {
		if issue.Severity == SeverityError && issue.Category == "spec" {
			orphanCount++
		}
	}
	if orphanCount != 1 {
		t.Errorf("expected 1 orphaned anchor error, got %d; issues: %v", orphanCount, issues)
	}
}

func TestSpecAnalyzer_UnanchoredRequirement(t *testing.T) {
	dir := t.TempDir()

	// Spec with two requirements
	specContent := `version: 1
title: "Test Spec"
requirements:
  - id: REQ-001
    description: "Anchored requirement"
    priority: high
  - id: REQ-002
    description: "Unanchored requirement"
    priority: medium
`
	specPath := filepath.Join(dir, ".fault-spec.yaml")
	if err := os.WriteFile(specPath, []byte(specContent), 0644); err != nil {
		t.Fatal(err)
	}

	// Only anchor REQ-001
	if err := os.WriteFile(filepath.Join(dir, "main.go"), []byte("// spec:REQ-001\nfunc Foo() {}\n"), 0644); err != nil {
		t.Fatal(err)
	}

	a := NewSpecAnalyzer()
	ctx := &AnalysisContext{
		RepoPath: dir,
		Config: &config.Config{
			LLM: config.LLMConfig{SpecFile: specPath},
		},
	}

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	unanchoredCount := 0
	for _, issue := range issues {
		if issue.Category == "spec" && (issue.Severity == SeverityWarning || issue.Severity == SeverityInfo) {
			if issue.ID == "spec-unanchored-REQ-002" {
				unanchoredCount++
			}
		}
	}
	if unanchoredCount != 1 {
		t.Errorf("expected 1 unanchored requirement, got %d; issues: %v", unanchoredCount, issues)
	}
}

func TestSpecAnalyzer_TargetMismatch(t *testing.T) {
	dir := t.TempDir()

	// Spec targeting pkg/auth/*.go
	specContent := `version: 1
title: "Test Spec"
requirements:
  - id: REQ-001
    description: "Auth requirement"
    targets: ["pkg/auth/*.go"]
`
	specPath := filepath.Join(dir, ".fault-spec.yaml")
	if err := os.WriteFile(specPath, []byte(specContent), 0644); err != nil {
		t.Fatal(err)
	}

	// Put anchor in wrong directory
	wrongDir := filepath.Join(dir, "pkg", "api")
	if err := os.MkdirAll(wrongDir, 0755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(wrongDir, "handler.go"), []byte("// spec:REQ-001\nfunc Handle() {}\n"), 0644); err != nil {
		t.Fatal(err)
	}

	a := NewSpecAnalyzer()
	ctx := &AnalysisContext{
		RepoPath: dir,
		Config: &config.Config{
			LLM: config.LLMConfig{SpecFile: specPath},
		},
	}

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	targetMismatch := 0
	for _, issue := range issues {
		if issue.Category == "spec" && issue.Severity == SeverityWarning {
			targetMismatch++
		}
	}
	if targetMismatch != 1 {
		t.Errorf("expected 1 target mismatch warning, got %d; issues: %v", targetMismatch, issues)
	}
}

func TestSpecAnalyzer_ModifiedAnchoredCode(t *testing.T) {
	dir := t.TempDir()

	specContent := `version: 1
title: "Test Spec"
requirements:
  - id: REQ-001
    description: "Test requirement"
`
	specPath := filepath.Join(dir, ".fault-spec.yaml")
	if err := os.WriteFile(specPath, []byte(specContent), 0644); err != nil {
		t.Fatal(err)
	}

	if err := os.WriteFile(filepath.Join(dir, "main.go"), []byte("// spec:REQ-001\nfunc Foo() {}\n"), 0644); err != nil {
		t.Fatal(err)
	}

	a := NewSpecAnalyzer()
	ctx := &AnalysisContext{
		RepoPath: dir,
		Config: &config.Config{
			LLM: config.LLMConfig{SpecFile: specPath},
		},
		Diff: &git.Diff{
			Files: []git.FileDiff{
				{
					Path:   "main.go",
					Status: "modified",
					Hunks: []git.Hunk{
						{NewStart: 1, NewCount: 3, Lines: []git.Line{
							{Type: "added", NewNum: 2, Content: "func Foo() { return }"},
						}},
					},
				},
			},
		},
		ParsedFiles: map[string]*parser.ParsedFile{},
	}

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	modifiedCount := 0
	for _, issue := range issues {
		if issue.Category == "spec" && issue.Severity == SeverityInfo && issue.File == "main.go" {
			modifiedCount++
		}
	}
	if modifiedCount != 1 {
		t.Errorf("expected 1 modified anchored code info, got %d; issues: %v", modifiedCount, issues)
	}
}

func TestSeverityForPriority(t *testing.T) {
	if got := severityForPriority("high"); got != SeverityWarning {
		t.Errorf("high priority = %q, want %q", got, SeverityWarning)
	}
	if got := severityForPriority("medium"); got != SeverityInfo {
		t.Errorf("medium priority = %q, want %q", got, SeverityInfo)
	}
	if got := severityForPriority("low"); got != SeverityInfo {
		t.Errorf("low priority = %q, want %q", got, SeverityInfo)
	}
	if got := severityForPriority(""); got != SeverityInfo {
		t.Errorf("empty priority = %q, want %q", got, SeverityInfo)
	}
}

func TestMatchesAnyTarget(t *testing.T) {
	if !matchesAnyTarget("pkg/auth/login.go", []string{"pkg/auth/*.go"}) {
		t.Error("expected pkg/auth/login.go to match pkg/auth/*.go")
	}
	if matchesAnyTarget("pkg/api/handler.go", []string{"pkg/auth/*.go"}) {
		t.Error("expected pkg/api/handler.go to NOT match pkg/auth/*.go")
	}
	if matchesAnyTarget("pkg/auth/login.go", []string{}) {
		t.Error("expected no match with empty targets")
	}
}
