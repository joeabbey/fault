package reporter

import (
	"bytes"
	"encoding/json"
	"io"
	"net/http"
	"net/http/httptest"
	"os"
	"path/filepath"
	"testing"
	"time"

	"github.com/joeabbey/fault/pkg/analyzer"
	"github.com/joeabbey/fault/pkg/github"
)

func TestGitHubReporterInActions(t *testing.T) {
	// Set up a mock GitHub API server
	var postedReview github.ReviewRequest
	var gotRequest bool
	server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		gotRequest = true
		body, _ := io.ReadAll(r.Body)
		json.Unmarshal(body, &postedReview)
		w.WriteHeader(http.StatusCreated)
		w.Write([]byte(`{"id": 1}`))
	}))
	defer server.Close()

	// Write a PR event payload
	tmpDir := t.TempDir()
	eventPath := filepath.Join(tmpDir, "event.json")
	eventPayload := `{"pull_request": {"number": 42, "head": {"sha": "abc123"}}}`
	os.WriteFile(eventPath, []byte(eventPayload), 0644)

	// We need to write SARIF to a file, so use tmpDir as working dir
	origDir, _ := os.Getwd()
	os.Chdir(tmpDir)
	defer os.Chdir(origDir)

	// Set environment
	t.Setenv("GITHUB_ACTIONS", "true")
	t.Setenv("GITHUB_TOKEN", "test-token")
	t.Setenv("GITHUB_REPOSITORY", "owner/repo")
	t.Setenv("GITHUB_EVENT_PATH", eventPath)

	// We need to inject the mock server URL. Since the reporter creates its own client,
	// we test via the parseRepository/parseEventPayload helpers and verify SARIF file is written.
	result := &analyzer.AnalysisResult{
		FilesChanged: 1,
		Issues: []analyzer.Issue{
			{
				ID:       "security/hardcoded-secret",
				Severity: analyzer.SeverityWarning,
				Category: "security",
				File:     "config.go",
				Line:     42,
				Message:  "Hardcoded secret detected",
			},
		},
		Duration:  50 * time.Millisecond,
		Timestamp: time.Now(),
	}

	// The actual PostReview will fail (no real API), but SARIF should still be written.
	// We suppress the error log from PostReview.
	r := NewGitHubReporter()
	exitCode := r.Report(result, "error")

	// Should not block on warnings when blockOn=error
	if exitCode != 0 {
		t.Errorf("expected exit code 0 (warnings only, blockOn=error), got %d", exitCode)
	}

	// Verify SARIF file was written
	sarifData, err := os.ReadFile(filepath.Join(tmpDir, "fault-results.sarif"))
	if err != nil {
		t.Fatalf("expected fault-results.sarif to be written: %v", err)
	}

	var sarif map[string]interface{}
	if err := json.Unmarshal(sarifData, &sarif); err != nil {
		t.Fatalf("fault-results.sarif is not valid JSON: %v", err)
	}

	if sarif["version"] != "2.1.0" {
		t.Errorf("expected SARIF version 2.1.0, got %v", sarif["version"])
	}

	// The HTTP call to the real GitHub API will fail, which is expected.
	// In integration, this would use the mock server.
	_ = gotRequest
	_ = postedReview
	_ = server
}

func TestGitHubReporterFallback(t *testing.T) {
	// Unset all GitHub Actions env vars
	t.Setenv("GITHUB_ACTIONS", "")
	t.Setenv("GITHUB_TOKEN", "")
	t.Setenv("GITHUB_REPOSITORY", "")
	t.Setenv("GITHUB_EVENT_PATH", "")

	result := &analyzer.AnalysisResult{
		FilesChanged: 1,
		Issues: []analyzer.Issue{
			{
				ID:       "import/broken",
				Severity: analyzer.SeverityError,
				Category: "imports",
				File:     "main.go",
				Line:     5,
				Message:  "Broken import",
			},
		},
		Duration:  50 * time.Millisecond,
		Timestamp: time.Now(),
	}

	// Capture stdout
	oldStdout := os.Stdout
	rr, w, _ := os.Pipe()
	os.Stdout = w

	r := NewGitHubReporter()
	exitCode := r.Report(result, "error")

	w.Close()
	os.Stdout = oldStdout

	var buf bytes.Buffer
	io.Copy(&buf, rr)

	// Should block on errors
	if exitCode != 1 {
		t.Errorf("expected exit code 1 for errors, got %d", exitCode)
	}

	// Should produce SARIF on stdout
	var sarif map[string]interface{}
	if err := json.Unmarshal(buf.Bytes(), &sarif); err != nil {
		t.Fatalf("expected SARIF on stdout, got: %s", buf.String())
	}

	if sarif["version"] != "2.1.0" {
		t.Errorf("expected SARIF version 2.1.0, got %v", sarif["version"])
	}
}

func TestGitHubReporterNonPREvent(t *testing.T) {
	// Write a push event payload (no pull_request key)
	tmpDir := t.TempDir()
	eventPath := filepath.Join(tmpDir, "event.json")
	eventPayload := `{"ref": "refs/heads/main", "after": "abc123"}`
	os.WriteFile(eventPath, []byte(eventPayload), 0644)

	origDir, _ := os.Getwd()
	os.Chdir(tmpDir)
	defer os.Chdir(origDir)

	t.Setenv("GITHUB_ACTIONS", "true")
	t.Setenv("GITHUB_TOKEN", "test-token")
	t.Setenv("GITHUB_REPOSITORY", "owner/repo")
	t.Setenv("GITHUB_EVENT_PATH", eventPath)

	result := &analyzer.AnalysisResult{
		FilesChanged: 1,
		Issues: []analyzer.Issue{
			{
				ID:       "import/broken",
				Severity: analyzer.SeverityWarning,
				Category: "imports",
				File:     "main.go",
				Line:     5,
				Message:  "Broken import",
			},
		},
		Duration:  50 * time.Millisecond,
		Timestamp: time.Now(),
	}

	r := NewGitHubReporter()
	exitCode := r.Report(result, "error")

	// Warnings only, blockOn=error -> 0
	if exitCode != 0 {
		t.Errorf("expected exit code 0, got %d", exitCode)
	}

	// SARIF file should still be written
	sarifData, err := os.ReadFile(filepath.Join(tmpDir, "fault-results.sarif"))
	if err != nil {
		t.Fatalf("expected fault-results.sarif to be written: %v", err)
	}

	var sarif map[string]interface{}
	if err := json.Unmarshal(sarifData, &sarif); err != nil {
		t.Fatalf("fault-results.sarif is not valid JSON: %v", err)
	}
}

func TestGitHubReporterExitCode(t *testing.T) {
	// Use fallback mode (no GH env) for exit code testing
	t.Setenv("GITHUB_ACTIONS", "")

	tests := []struct {
		name    string
		issues  []analyzer.Issue
		blockOn string
		want    int
	}{
		{
			name:    "errors with block_on=error returns 1",
			issues:  []analyzer.Issue{{ID: "a", Severity: analyzer.SeverityError, Category: "a", File: "a.go", Message: "x"}},
			blockOn: "error",
			want:    1,
		},
		{
			name:    "warnings with block_on=error returns 0",
			issues:  []analyzer.Issue{{ID: "a", Severity: analyzer.SeverityWarning, Category: "a", File: "a.go", Message: "x"}},
			blockOn: "error",
			want:    0,
		},
		{
			name:    "warnings with block_on=warning returns 1",
			issues:  []analyzer.Issue{{ID: "a", Severity: analyzer.SeverityWarning, Category: "a", File: "a.go", Message: "x"}},
			blockOn: "warning",
			want:    1,
		},
		{
			name:    "no issues returns 0",
			issues:  make([]analyzer.Issue, 0),
			blockOn: "error",
			want:    0,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Capture stdout to prevent noise
			oldStdout := os.Stdout
			_, w, _ := os.Pipe()
			os.Stdout = w

			r := NewGitHubReporter()
			result := &analyzer.AnalysisResult{
				Issues:       tt.issues,
				FilesChanged: 1,
				Duration:     time.Millisecond,
				Timestamp:    time.Now(),
			}
			got := r.Report(result, tt.blockOn)

			w.Close()
			os.Stdout = oldStdout

			if got != tt.want {
				t.Errorf("exit code = %d, want %d", got, tt.want)
			}
		})
	}
}

func TestParseRepository(t *testing.T) {
	tests := []struct {
		input     string
		wantOwner string
		wantRepo  string
		wantOK    bool
	}{
		{"owner/repo", "owner", "repo", true},
		{"joeabbey/fault", "joeabbey", "fault", true},
		{"invalid", "", "", false},
		{"/repo", "", "", false},
		{"owner/", "", "", false},
		{"", "", "", false},
	}

	for _, tt := range tests {
		owner, repo, ok := parseRepository(tt.input)
		if ok != tt.wantOK || owner != tt.wantOwner || repo != tt.wantRepo {
			t.Errorf("parseRepository(%q) = (%q, %q, %v), want (%q, %q, %v)",
				tt.input, owner, repo, ok, tt.wantOwner, tt.wantRepo, tt.wantOK)
		}
	}
}

func TestParseEventPayload(t *testing.T) {
	tests := []struct {
		name      string
		payload   string
		wantPR    int
		wantSHA   string
		wantError bool
	}{
		{
			name:    "valid PR event",
			payload: `{"pull_request": {"number": 42, "head": {"sha": "abc123"}}}`,
			wantPR:  42,
			wantSHA: "abc123",
		},
		{
			name:      "push event (no pull_request)",
			payload:   `{"ref": "refs/heads/main"}`,
			wantError: true,
		},
		{
			name:      "invalid JSON",
			payload:   `{invalid`,
			wantError: true,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			tmpDir := t.TempDir()
			path := filepath.Join(tmpDir, "event.json")
			os.WriteFile(path, []byte(tt.payload), 0644)

			prNumber, sha, err := parseEventPayload(path)
			if tt.wantError {
				if err == nil {
					t.Errorf("expected error, got nil")
				}
				return
			}
			if err != nil {
				t.Fatalf("unexpected error: %v", err)
			}
			if prNumber != tt.wantPR {
				t.Errorf("expected PR number %d, got %d", tt.wantPR, prNumber)
			}
			if sha != tt.wantSHA {
				t.Errorf("expected SHA %q, got %q", tt.wantSHA, sha)
			}
		})
	}
}
