package llm

import (
	"context"
	"encoding/json"
	"net/http"
	"net/http/httptest"
	"testing"

	"github.com/joeabbey/fault/pkg/analyzer"
	"github.com/joeabbey/fault/pkg/git"
)

func TestCompareSpec_Success(t *testing.T) {
	expectedResult := SpecResult{
		Implemented: []string{"User authentication", "Password hashing"},
		Missing:     []string{"Email verification", "Rate limiting"},
		Unexpected:  []string{"Debug logging added"},
		Score:       0.6,
	}

	server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		resultJSON, _ := json.Marshal(expectedResult)
		resp := Response{
			Content: []ContentBlock{
				{Type: "text", Text: string(resultJSON)},
			},
			Usage: Usage{InputTokens: 200, OutputTokens: 100},
		}
		w.Header().Set("Content-Type", "application/json")
		json.NewEncoder(w).Encode(resp)
	}))
	defer server.Close()

	client := NewClient("test-key", WithBaseURL(server.URL))
	ctx := context.Background()

	diff := &git.Diff{
		Files: []git.FileDiff{
			{Path: "auth.go", Status: "added", Hunks: []git.Hunk{
				{Lines: []git.Line{{Type: "added", Content: "func Login() {}"}}},
			}},
		},
	}

	specContent := `# Auth Feature Spec
- User authentication
- Password hashing
- Email verification
- Rate limiting
`

	result, err := CompareSpec(ctx, client, specContent, diff)
	if err != nil {
		t.Fatalf("CompareSpec: %v", err)
	}

	if len(result.Implemented) != 2 {
		t.Errorf("Implemented count = %d, want 2", len(result.Implemented))
	}
	if len(result.Missing) != 2 {
		t.Errorf("Missing count = %d, want 2", len(result.Missing))
	}
	if len(result.Unexpected) != 1 {
		t.Errorf("Unexpected count = %d, want 1", len(result.Unexpected))
	}
	if result.Score != 0.6 {
		t.Errorf("Score = %f, want 0.6", result.Score)
	}
}

func TestCompareSpec_EmptyDiff(t *testing.T) {
	client := NewClient("test-key")
	ctx := context.Background()

	diff := &git.Diff{Files: make([]git.FileDiff, 0)}

	result, err := CompareSpec(ctx, client, "some spec", diff)
	if err != nil {
		t.Fatalf("CompareSpec: %v", err)
	}

	if result.Score != 0.0 {
		t.Errorf("Score = %f, want 0.0 for empty diff", result.Score)
	}
	if len(result.Implemented) != 0 {
		t.Errorf("Implemented should be empty for empty diff")
	}
}

func TestCompareSpec_NilClient(t *testing.T) {
	ctx := context.Background()
	diff := &git.Diff{Files: []git.FileDiff{{Path: "main.go"}}}

	_, err := CompareSpec(ctx, nil, "spec", diff)
	if err == nil {
		t.Fatal("expected error for nil client, got nil")
	}
}

func TestCompareSpec_EmptySpec(t *testing.T) {
	client := NewClient("test-key")
	ctx := context.Background()

	diff := &git.Diff{Files: []git.FileDiff{{Path: "main.go"}}}

	_, err := CompareSpec(ctx, client, "", diff)
	if err == nil {
		t.Fatal("expected error for empty spec, got nil")
	}
}

func TestCompareSpec_NilArrays(t *testing.T) {
	// LLM returns null arrays instead of empty arrays
	resultWithNulls := `{"implemented": null, "missing": null, "unexpected": null, "score": 0.5}`

	server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		resp := Response{
			Content: []ContentBlock{
				{Type: "text", Text: resultWithNulls},
			},
			Usage: Usage{InputTokens: 200, OutputTokens: 100},
		}
		w.Header().Set("Content-Type", "application/json")
		json.NewEncoder(w).Encode(resp)
	}))
	defer server.Close()

	client := NewClient("test-key", WithBaseURL(server.URL))
	ctx := context.Background()

	diff := &git.Diff{
		Files: []git.FileDiff{
			{Path: "main.go", Status: "modified"},
		},
	}

	result, err := CompareSpec(ctx, client, "some spec", diff)
	if err != nil {
		t.Fatalf("CompareSpec: %v", err)
	}

	// Nil arrays should be converted to empty slices
	if result.Implemented == nil {
		t.Error("Implemented should not be nil")
	}
	if result.Missing == nil {
		t.Error("Missing should not be nil")
	}
	if result.Unexpected == nil {
		t.Error("Unexpected should not be nil")
	}
}

func TestSpecResultToIssues(t *testing.T) {
	result := &SpecResult{
		Implemented: []string{"Feature A", "Feature B"},
		Missing:     []string{"Feature C", "Feature D"},
		Unexpected:  []string{"Unexpected change X"},
		Score:       0.5,
	}

	issues := SpecResultToIssues(result)

	// Should have 2 missing (warning) + 1 unexpected (info) = 3 issues
	if len(issues) != 3 {
		t.Fatalf("len(issues) = %d, want 3", len(issues))
	}

	// First two should be warnings for missing features
	for i := 0; i < 2; i++ {
		if issues[i].Severity != analyzer.SeverityWarning {
			t.Errorf("issues[%d].Severity = %q, want %q", i, issues[i].Severity, analyzer.SeverityWarning)
		}
		if issues[i].Category != "spec" {
			t.Errorf("issues[%d].Category = %q, want %q", i, issues[i].Category, "spec")
		}
	}

	// Last should be info for unexpected
	if issues[2].Severity != analyzer.SeverityInfo {
		t.Errorf("issues[2].Severity = %q, want %q", issues[2].Severity, analyzer.SeverityInfo)
	}
}

func TestSpecResultToIssues_Empty(t *testing.T) {
	result := &SpecResult{
		Implemented: []string{"All features done"},
		Missing:     make([]string, 0),
		Unexpected:  make([]string, 0),
		Score:       1.0,
	}

	issues := SpecResultToIssues(result)
	if len(issues) != 0 {
		t.Errorf("len(issues) = %d, want 0 for perfect spec match", len(issues))
	}
}

func TestCompareSpec_ScoreClamping(t *testing.T) {
	outOfRange := SpecResult{
		Implemented: make([]string, 0),
		Missing:     make([]string, 0),
		Unexpected:  make([]string, 0),
		Score:       1.5,
	}

	server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		resultJSON, _ := json.Marshal(outOfRange)
		resp := Response{
			Content: []ContentBlock{
				{Type: "text", Text: string(resultJSON)},
			},
			Usage: Usage{InputTokens: 100, OutputTokens: 50},
		}
		w.Header().Set("Content-Type", "application/json")
		json.NewEncoder(w).Encode(resp)
	}))
	defer server.Close()

	client := NewClient("test-key", WithBaseURL(server.URL))
	ctx := context.Background()

	diff := &git.Diff{
		Files: []git.FileDiff{
			{Path: "main.go", Status: "modified"},
		},
	}

	result, err := CompareSpec(ctx, client, "some spec", diff)
	if err != nil {
		t.Fatalf("CompareSpec: %v", err)
	}

	if result.Score != 1.0 {
		t.Errorf("Score = %f, want 1.0 (clamped from 1.5)", result.Score)
	}
}
