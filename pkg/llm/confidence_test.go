package llm

import (
	"context"
	"encoding/json"
	"net/http"
	"net/http/httptest"
	"testing"

	"github.com/joeabbey/fault/pkg/git"
	"github.com/joeabbey/fault/pkg/parser"
)

func TestScoreConfidence_Success(t *testing.T) {
	expectedResult := ConfidenceResult{
		Overall: 0.85,
		PerFile: map[string]float64{
			"main.go":      0.9,
			"handler.go":   0.8,
		},
		Reasoning: "Code is well-structured with proper error handling.",
	}

	server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		resultJSON, _ := json.Marshal(expectedResult)
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
			{Path: "main.go", Status: "modified", Hunks: []git.Hunk{
				{Lines: []git.Line{{Type: "added", Content: "fmt.Println(\"hello\")"}}},
			}},
			{Path: "handler.go", Status: "added", Hunks: []git.Hunk{
				{Lines: []git.Line{{Type: "added", Content: "func Handle() {}"}}},
			}},
		},
	}

	parsedFiles := map[string]*parser.ParsedFile{
		"main.go": {
			Path:     "main.go",
			Language: "go",
			Imports:  []parser.Import{{Path: "fmt", Line: 1}},
			Exports:  []parser.Export{{Name: "Main", Kind: "function", Line: 5}},
		},
	}

	result, err := client.ScoreConfidence(ctx, diff, parsedFiles)
	if err != nil {
		t.Fatalf("ScoreConfidence: %v", err)
	}

	if result.Overall != 0.85 {
		t.Errorf("Overall = %f, want 0.85", result.Overall)
	}
	if result.PerFile["main.go"] != 0.9 {
		t.Errorf("PerFile[\"main.go\"] = %f, want 0.9", result.PerFile["main.go"])
	}
	if result.PerFile["handler.go"] != 0.8 {
		t.Errorf("PerFile[\"handler.go\"] = %f, want 0.8", result.PerFile["handler.go"])
	}
}

// ScoreConfidence is a package-level function, test it properly
func (c *Client) ScoreConfidence(ctx context.Context, diff *git.Diff, parsedFiles map[string]*parser.ParsedFile) (*ConfidenceResult, error) {
	return ScoreConfidence(ctx, c, diff, parsedFiles)
}

func TestScoreConfidence_MalformedJSON(t *testing.T) {
	server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		resp := Response{
			Content: []ContentBlock{
				{Type: "text", Text: "This is not valid JSON at all!"},
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

	// Should gracefully degrade, not error
	result, err := ScoreConfidence(ctx, client, diff, nil)
	if err != nil {
		t.Fatalf("ScoreConfidence should not error on malformed JSON: %v", err)
	}

	if result.Overall != 0.5 {
		t.Errorf("Overall = %f, want 0.5 (default)", result.Overall)
	}
	if result.PerFile["main.go"] != 0.5 {
		t.Errorf("PerFile[\"main.go\"] = %f, want 0.5 (default)", result.PerFile["main.go"])
	}
}

func TestScoreConfidence_EmptyDiff(t *testing.T) {
	client := NewClient("test-key")
	ctx := context.Background()

	diff := &git.Diff{Files: make([]git.FileDiff, 0)}

	result, err := ScoreConfidence(ctx, client, diff, nil)
	if err != nil {
		t.Fatalf("ScoreConfidence: %v", err)
	}

	if result.Overall != 1.0 {
		t.Errorf("Overall = %f, want 1.0 for empty diff", result.Overall)
	}
}

func TestScoreConfidence_NilClient(t *testing.T) {
	ctx := context.Background()
	diff := &git.Diff{Files: []git.FileDiff{{Path: "main.go"}}}

	_, err := ScoreConfidence(ctx, nil, diff, nil)
	if err == nil {
		t.Fatal("expected error for nil client, got nil")
	}
}

func TestScoreConfidence_NilDiff(t *testing.T) {
	client := NewClient("test-key")
	ctx := context.Background()

	result, err := ScoreConfidence(ctx, client, nil, nil)
	if err != nil {
		t.Fatalf("ScoreConfidence: %v", err)
	}

	if result.Overall != 1.0 {
		t.Errorf("Overall = %f, want 1.0 for nil diff", result.Overall)
	}
}

func TestScoreConfidence_ScoreClamping(t *testing.T) {
	outOfRangeResult := ConfidenceResult{
		Overall: 1.5,
		PerFile: map[string]float64{
			"main.go": -0.3,
		},
		Reasoning: "Out of range scores.",
	}

	server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		resultJSON, _ := json.Marshal(outOfRangeResult)
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

	result, err := ScoreConfidence(ctx, client, diff, nil)
	if err != nil {
		t.Fatalf("ScoreConfidence: %v", err)
	}

	if result.Overall != 1.0 {
		t.Errorf("Overall = %f, want 1.0 (clamped from 1.5)", result.Overall)
	}
	if result.PerFile["main.go"] != 0.0 {
		t.Errorf("PerFile[\"main.go\"] = %f, want 0.0 (clamped from -0.3)", result.PerFile["main.go"])
	}
}

func TestBuildDiffSummary(t *testing.T) {
	diff := &git.Diff{
		Files: []git.FileDiff{
			{
				Path:   "main.go",
				Status: "modified",
				Hunks: []git.Hunk{
					{
						Lines: []git.Line{
							{Type: "context", Content: "package main"},
							{Type: "added", Content: "import \"fmt\""},
							{Type: "removed", Content: "import \"os\""},
						},
					},
				},
			},
			{
				Path:     "binary.png",
				Status:   "added",
				IsBinary: true,
			},
		},
	}

	summary := BuildDiffSummary(diff)
	if summary == "" {
		t.Fatal("BuildDiffSummary returned empty string")
	}

	// Check that file paths appear in the summary
	if !containsString(summary, "main.go") {
		t.Error("summary should contain 'main.go'")
	}
	if !containsString(summary, "binary.png") {
		t.Error("summary should contain 'binary.png'")
	}
	if !containsString(summary, "(binary file)") {
		t.Error("summary should contain '(binary file)'")
	}
}

func TestClampScore(t *testing.T) {
	tests := []struct {
		input float64
		want  float64
	}{
		{0.5, 0.5},
		{0.0, 0.0},
		{1.0, 1.0},
		{-0.1, 0.0},
		{1.5, 1.0},
		{-100.0, 0.0},
		{100.0, 1.0},
	}

	for _, tt := range tests {
		got := clampScore(tt.input)
		if got != tt.want {
			t.Errorf("clampScore(%f) = %f, want %f", tt.input, got, tt.want)
		}
	}
}

func containsString(s, substr string) bool {
	return len(s) >= len(substr) && (s == substr || len(s) > 0 && containsSubstring(s, substr))
}

func containsSubstring(s, substr string) bool {
	for i := 0; i <= len(s)-len(substr); i++ {
		if s[i:i+len(substr)] == substr {
			return true
		}
	}
	return false
}
