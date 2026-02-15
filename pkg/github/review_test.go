package github

import (
	"encoding/json"
	"io"
	"net/http"
	"net/http/httptest"
	"strings"
	"testing"
	"time"

	"github.com/joeabbey/fault/pkg/analyzer"
)

func TestBuildReviewComments(t *testing.T) {
	result := &analyzer.AnalysisResult{
		FilesChanged: 2,
		Issues: []analyzer.Issue{
			{
				ID:       "security/hardcoded-secret",
				Severity: analyzer.SeverityWarning,
				Category: "security",
				File:     "config.go",
				Line:     42,
				Message:  "Hardcoded secret detected in variable assignment",
			},
			{
				ID:       "import/broken-import",
				Severity: analyzer.SeverityError,
				Category: "imports",
				File:     "main.go",
				Line:     5,
				Message:  "Broken import 'missing/pkg'",
			},
		},
		Duration:  100 * time.Millisecond,
		Timestamp: time.Now(),
	}

	review := BuildReview(result, "abc123")
	if review == nil {
		t.Fatal("expected non-nil review")
	}

	if review.CommitID != "abc123" {
		t.Errorf("expected commit_id 'abc123', got %q", review.CommitID)
	}
	if review.Event != "COMMENT" {
		t.Errorf("expected event 'COMMENT', got %q", review.Event)
	}
	if len(review.Comments) != 2 {
		t.Fatalf("expected 2 comments, got %d", len(review.Comments))
	}

	c0 := review.Comments[0]
	if c0.Path != "config.go" {
		t.Errorf("expected path 'config.go', got %q", c0.Path)
	}
	if c0.Line != 42 {
		t.Errorf("expected line 42, got %d", c0.Line)
	}
	if c0.Side != "RIGHT" {
		t.Errorf("expected side 'RIGHT', got %q", c0.Side)
	}
	if !strings.Contains(c0.Body, "Hardcoded secret") {
		t.Errorf("expected body to contain issue message, got %q", c0.Body)
	}
	if !strings.Contains(c0.Body, "[security]") {
		t.Errorf("expected body to contain category, got %q", c0.Body)
	}
	if !strings.Contains(c0.Body, "Found by [Fault]") {
		t.Errorf("expected body to contain Fault attribution, got %q", c0.Body)
	}

	c1 := review.Comments[1]
	if c1.Path != "main.go" {
		t.Errorf("expected path 'main.go', got %q", c1.Path)
	}
	if c1.Line != 5 {
		t.Errorf("expected line 5, got %d", c1.Line)
	}
}

func TestBuildReviewNoLineIssues(t *testing.T) {
	result := &analyzer.AnalysisResult{
		FilesChanged: 1,
		Issues: []analyzer.Issue{
			{
				ID:       "tests/no-test",
				Severity: analyzer.SeverityWarning,
				Category: "tests",
				File:     "pkg/service.go",
				Line:     0,
				Message:  "No test file for service.go",
			},
			{
				ID:       "internal/error",
				Severity: analyzer.SeverityInfo,
				Category: "internal",
				File:     "",
				Line:     0,
				Message:  "Analyzer timeout",
			},
		},
		Duration:  50 * time.Millisecond,
		Timestamp: time.Now(),
	}

	review := BuildReview(result, "def456")
	if review == nil {
		t.Fatal("expected non-nil review")
	}

	if len(review.Comments) != 0 {
		t.Errorf("expected 0 inline comments for issues without lines, got %d", len(review.Comments))
	}

	if !strings.Contains(review.Body, "Issues without line information") {
		t.Errorf("expected summary body to contain summary-only issues, got %q", review.Body)
	}
	if !strings.Contains(review.Body, "No test file for service.go") {
		t.Errorf("expected summary body to list issue message, got %q", review.Body)
	}
	if !strings.Contains(review.Body, "(no file)") {
		t.Errorf("expected summary body to show '(no file)' for issue without file, got %q", review.Body)
	}
}

func TestBuildReviewSeverityEmoji(t *testing.T) {
	tests := []struct {
		severity analyzer.Severity
		want     string
	}{
		{analyzer.SeverityError, "\U0001f534"},
		{analyzer.SeverityWarning, "\u26a0\ufe0f"},
		{analyzer.SeverityInfo, "\u2139\ufe0f"},
	}

	for _, tt := range tests {
		got := severityEmoji(tt.severity)
		if got != tt.want {
			t.Errorf("severityEmoji(%q) = %q, want %q", tt.severity, got, tt.want)
		}
	}
}

func TestBuildReviewEmpty(t *testing.T) {
	result := &analyzer.AnalysisResult{
		FilesChanged: 3,
		Issues:       make([]analyzer.Issue, 0),
		Duration:     100 * time.Millisecond,
		Timestamp:    time.Now(),
	}

	review := BuildReview(result, "abc123")
	if review != nil {
		t.Errorf("expected nil review for empty issues, got %+v", review)
	}
}

func TestPostReview(t *testing.T) {
	var receivedBody ReviewRequest
	var receivedMethod string
	var receivedPath string

	server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		receivedMethod = r.Method
		receivedPath = r.URL.Path

		body, _ := io.ReadAll(r.Body)
		json.Unmarshal(body, &receivedBody)

		w.WriteHeader(http.StatusCreated)
		w.Write([]byte(`{"id": 1}`))
	}))
	defer server.Close()

	client := NewClient("test-token")
	client.apiBase = server.URL

	review := &ReviewRequest{
		CommitID: "abc123",
		Body:     "Test summary",
		Event:    "COMMENT",
		Comments: []ReviewComment{
			{Path: "file.go", Line: 10, Side: "RIGHT", Body: "Test comment"},
		},
	}

	err := client.PostReview("owner", "repo", 42, review)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	if receivedMethod != "POST" {
		t.Errorf("expected POST method, got %q", receivedMethod)
	}
	if receivedPath != "/repos/owner/repo/pulls/42/reviews" {
		t.Errorf("expected path '/repos/owner/repo/pulls/42/reviews', got %q", receivedPath)
	}
	if receivedBody.CommitID != "abc123" {
		t.Errorf("expected commit_id 'abc123', got %q", receivedBody.CommitID)
	}
	if receivedBody.Event != "COMMENT" {
		t.Errorf("expected event 'COMMENT', got %q", receivedBody.Event)
	}
	if len(receivedBody.Comments) != 1 {
		t.Fatalf("expected 1 comment, got %d", len(receivedBody.Comments))
	}
}

func TestPostReviewAuth(t *testing.T) {
	var receivedAuth string

	server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		receivedAuth = r.Header.Get("Authorization")
		w.WriteHeader(http.StatusCreated)
		w.Write([]byte(`{"id": 1}`))
	}))
	defer server.Close()

	client := NewClient("my-secret-token")
	client.apiBase = server.URL

	review := &ReviewRequest{
		CommitID: "abc123",
		Body:     "Test",
		Event:    "COMMENT",
		Comments: []ReviewComment{},
	}

	err := client.PostReview("owner", "repo", 1, review)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	if receivedAuth != "Bearer my-secret-token" {
		t.Errorf("expected 'Bearer my-secret-token', got %q", receivedAuth)
	}
}

func TestPostReviewError(t *testing.T) {
	server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.WriteHeader(http.StatusUnprocessableEntity)
		w.Write([]byte(`{"message": "Validation Failed"}`))
	}))
	defer server.Close()

	client := NewClient("test-token")
	client.apiBase = server.URL

	review := &ReviewRequest{
		CommitID: "abc123",
		Body:     "Test",
		Event:    "COMMENT",
		Comments: []ReviewComment{},
	}

	err := client.PostReview("owner", "repo", 1, review)
	if err == nil {
		t.Fatal("expected error for non-2xx response")
	}

	if !strings.Contains(err.Error(), "422") {
		t.Errorf("expected error to contain status code, got %q", err.Error())
	}
	if !strings.Contains(err.Error(), "Validation Failed") {
		t.Errorf("expected error to contain response body, got %q", err.Error())
	}
}

func TestBuildReviewSuggestion(t *testing.T) {
	result := &analyzer.AnalysisResult{
		FilesChanged: 1,
		Issues: []analyzer.Issue{
			{
				ID:         "security/hardcoded-secret",
				Severity:   analyzer.SeverityWarning,
				Category:   "security",
				File:       "config.go",
				Line:       10,
				Message:    "Hardcoded secret detected",
				Suggestion: "Use environment variables instead of hardcoded values",
			},
		},
		Duration:  50 * time.Millisecond,
		Timestamp: time.Now(),
	}

	review := BuildReview(result, "abc123")
	if review == nil {
		t.Fatal("expected non-nil review")
	}

	if len(review.Comments) != 1 {
		t.Fatalf("expected 1 comment, got %d", len(review.Comments))
	}

	body := review.Comments[0].Body
	if !strings.Contains(body, "**Suggestion:**") {
		t.Errorf("expected body to contain suggestion header, got %q", body)
	}
	if !strings.Contains(body, "Use environment variables") {
		t.Errorf("expected body to contain suggestion text, got %q", body)
	}
}
