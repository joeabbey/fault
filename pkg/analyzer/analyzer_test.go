package analyzer

import (
	"fmt"
	"testing"

	"github.com/joeabbey/fault/pkg/config"
	"github.com/joeabbey/fault/pkg/git"
	"github.com/joeabbey/fault/pkg/parser"
)

// mockAnalyzer is a test analyzer that returns preset issues.
type mockAnalyzer struct {
	name   string
	issues []Issue
	err    error
}

func (m *mockAnalyzer) Name() string { return m.name }
func (m *mockAnalyzer) Analyze(ctx *AnalysisContext) ([]Issue, error) {
	return m.issues, m.err
}

func TestRunnerCallsEnabledAnalyzers(t *testing.T) {
	cfg := config.DefaultConfig()

	a1 := &mockAnalyzer{
		name: "imports",
		issues: []Issue{
			{ID: "imp-1", Severity: SeverityWarning, Category: "imports", File: "main.go", Line: 5, Message: "unused import"},
		},
	}
	a2 := &mockAnalyzer{
		name: "consistency",
		issues: []Issue{
			{ID: "con-1", Severity: SeverityError, Category: "consistency", File: "main.go", Line: 10, Message: "inconsistent naming"},
		},
	}

	runner := NewRunner(cfg, []Analyzer{a1, a2})
	diff := &git.Diff{
		Files: []git.FileDiff{
			{Path: "main.go", Status: "modified"},
		},
		Mode: "staged",
	}

	result := runner.Run("/repo", diff, make(map[string]*parser.ParsedFile))

	if len(result.Issues) != 2 {
		t.Fatalf("expected 2 issues, got %d", len(result.Issues))
	}

	// Errors should be sorted first
	if result.Issues[0].Severity != SeverityError {
		t.Error("expected first issue to be error severity")
	}
	if result.Issues[1].Severity != SeverityWarning {
		t.Error("expected second issue to be warning severity")
	}
}

func TestRunnerSkipsDisabledAnalyzers(t *testing.T) {
	cfg := config.DefaultConfig()
	cfg.Analyzers.Imports = false

	a1 := &mockAnalyzer{
		name: "imports",
		issues: []Issue{
			{ID: "imp-1", Severity: SeverityWarning, Category: "imports", Message: "should not appear"},
		},
	}
	a2 := &mockAnalyzer{
		name: "consistency",
		issues: []Issue{
			{ID: "con-1", Severity: SeverityError, Category: "consistency", Message: "should appear"},
		},
	}

	runner := NewRunner(cfg, []Analyzer{a1, a2})
	diff := &git.Diff{Files: make([]git.FileDiff, 0), Mode: "staged"}

	result := runner.Run("/repo", diff, make(map[string]*parser.ParsedFile))

	if len(result.Issues) != 1 {
		t.Fatalf("expected 1 issue (imports disabled), got %d", len(result.Issues))
	}
	if result.Issues[0].Category != "consistency" {
		t.Errorf("expected consistency issue, got %q", result.Issues[0].Category)
	}
}

func TestRunnerHandlesAnalyzerErrors(t *testing.T) {
	cfg := config.DefaultConfig()

	a1 := &mockAnalyzer{
		name: "imports",
		err:  fmt.Errorf("analyzer crashed"),
	}
	a2 := &mockAnalyzer{
		name: "consistency",
		issues: []Issue{
			{ID: "con-1", Severity: SeverityWarning, Category: "consistency", Message: "found issue"},
		},
	}

	runner := NewRunner(cfg, []Analyzer{a1, a2})
	diff := &git.Diff{Files: make([]git.FileDiff, 0), Mode: "staged"}

	result := runner.Run("/repo", diff, make(map[string]*parser.ParsedFile))

	// Should have the consistency issue plus an info issue about the failure
	if len(result.Issues) < 2 {
		t.Fatalf("expected at least 2 issues (1 real + 1 error info), got %d", len(result.Issues))
	}

	// The real issue should still be present
	found := false
	for _, issue := range result.Issues {
		if issue.Category == "consistency" {
			found = true
		}
	}
	if !found {
		t.Error("expected consistency issue to still be present after analyzer error")
	}
}

func TestRunnerIssuesSorted(t *testing.T) {
	cfg := config.DefaultConfig()

	a1 := &mockAnalyzer{
		name: "imports",
		issues: []Issue{
			{ID: "1", Severity: SeverityInfo, File: "z.go", Line: 1, Message: "info"},
			{ID: "2", Severity: SeverityWarning, File: "a.go", Line: 5, Message: "warning"},
			{ID: "3", Severity: SeverityError, File: "b.go", Line: 3, Message: "error"},
			{ID: "4", Severity: SeverityError, File: "a.go", Line: 1, Message: "error a"},
		},
	}

	runner := NewRunner(cfg, []Analyzer{a1})
	diff := &git.Diff{Files: make([]git.FileDiff, 0), Mode: "staged"}

	result := runner.Run("/repo", diff, make(map[string]*parser.ParsedFile))

	if len(result.Issues) != 4 {
		t.Fatalf("expected 4 issues, got %d", len(result.Issues))
	}

	// Order: errors (a.go:1, b.go:3), warning (a.go:5), info (z.go:1)
	expected := []struct {
		severity Severity
		file     string
	}{
		{SeverityError, "a.go"},
		{SeverityError, "b.go"},
		{SeverityWarning, "a.go"},
		{SeverityInfo, "z.go"},
	}

	for i, exp := range expected {
		if result.Issues[i].Severity != exp.severity {
			t.Errorf("issue %d: expected severity %q, got %q", i, exp.severity, result.Issues[i].Severity)
		}
		if result.Issues[i].File != exp.file {
			t.Errorf("issue %d: expected file %q, got %q", i, exp.file, result.Issues[i].File)
		}
	}
}

func TestAnalysisResultCounts(t *testing.T) {
	result := &AnalysisResult{
		Issues: []Issue{
			{Severity: SeverityError},
			{Severity: SeverityError},
			{Severity: SeverityWarning},
			{Severity: SeverityInfo},
			{Severity: SeverityInfo},
			{Severity: SeverityInfo},
		},
	}

	if result.ErrorCount() != 2 {
		t.Errorf("expected 2 errors, got %d", result.ErrorCount())
	}
	if result.WarningCount() != 1 {
		t.Errorf("expected 1 warning, got %d", result.WarningCount())
	}
	if result.InfoCount() != 3 {
		t.Errorf("expected 3 info, got %d", result.InfoCount())
	}
}

func TestShouldBlock(t *testing.T) {
	tests := []struct {
		name    string
		issues  []Issue
		blockOn string
		want    bool
	}{
		{
			name:    "error blocks on error",
			issues:  []Issue{{Severity: SeverityError}},
			blockOn: "error",
			want:    true,
		},
		{
			name:    "warning does not block on error",
			issues:  []Issue{{Severity: SeverityWarning}},
			blockOn: "error",
			want:    false,
		},
		{
			name:    "warning blocks on warning",
			issues:  []Issue{{Severity: SeverityWarning}},
			blockOn: "warning",
			want:    true,
		},
		{
			name:    "info does not block on warning",
			issues:  []Issue{{Severity: SeverityInfo}},
			blockOn: "warning",
			want:    false,
		},
		{
			name:    "no issues does not block",
			issues:  make([]Issue, 0),
			blockOn: "error",
			want:    false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := &AnalysisResult{Issues: tt.issues}
			got := result.ShouldBlock(tt.blockOn)
			if got != tt.want {
				t.Errorf("ShouldBlock(%q) = %v, want %v", tt.blockOn, got, tt.want)
			}
		})
	}
}

func TestBuildSummary(t *testing.T) {
	tests := []struct {
		name     string
		result   *AnalysisResult
		contains string
	}{
		{
			name: "no issues",
			result: &AnalysisResult{
				FilesChanged: 3,
				Issues:       make([]Issue, 0),
			},
			contains: "No issues found in 3 files",
		},
		{
			name: "mixed issues",
			result: &AnalysisResult{
				FilesChanged: 5,
				Issues: []Issue{
					{Severity: SeverityError},
					{Severity: SeverityWarning},
					{Severity: SeverityWarning},
				},
			},
			contains: "Found 3 issues",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			summary := buildSummary(tt.result)
			if !containsStr(summary, tt.contains) {
				t.Errorf("summary %q does not contain %q", summary, tt.contains)
			}
		})
	}
}

func containsStr(s, substr string) bool {
	return len(s) >= len(substr) && searchStr(s, substr)
}

func searchStr(s, substr string) bool {
	for i := 0; i <= len(s)-len(substr); i++ {
		if s[i:i+len(substr)] == substr {
			return true
		}
	}
	return false
}
