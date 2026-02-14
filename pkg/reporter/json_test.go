package reporter

import (
	"bytes"
	"encoding/json"
	"testing"
	"time"

	"github.com/joeabbey/fault/pkg/analyzer"
)

func TestJSONReporterNoIssues(t *testing.T) {
	var buf bytes.Buffer
	r := NewJSONReporterWithWriter(&buf)

	result := &analyzer.AnalysisResult{
		RepoPath:     "/test/repo",
		FilesChanged: 3,
		Issues:       make([]analyzer.Issue, 0),
		Duration:     100 * time.Millisecond,
		Timestamp:    time.Date(2025, 1, 1, 0, 0, 0, 0, time.UTC),
	}

	exitCode := r.Report(result, "error")
	if exitCode != 0 {
		t.Errorf("expected exit code 0, got %d", exitCode)
	}

	// Verify valid JSON.
	var parsed map[string]interface{}
	if err := json.Unmarshal(buf.Bytes(), &parsed); err != nil {
		t.Fatalf("output is not valid JSON: %v\nOutput: %s", err, buf.String())
	}

	// Verify issues is an empty array, not null.
	issuesRaw, ok := parsed["issues"]
	if !ok {
		t.Fatal("expected 'issues' key in JSON output")
	}
	issuesSlice, ok := issuesRaw.([]interface{})
	if !ok {
		t.Fatalf("expected 'issues' to be an array, got %T", issuesRaw)
	}
	if len(issuesSlice) != 0 {
		t.Errorf("expected empty issues array, got %d items", len(issuesSlice))
	}
}

func TestJSONReporterWithErrors(t *testing.T) {
	var buf bytes.Buffer
	r := NewJSONReporterWithWriter(&buf)

	result := &analyzer.AnalysisResult{
		RepoPath:     "/test/repo",
		FilesChanged: 2,
		Issues: []analyzer.Issue{
			{
				ID:       "imp-1",
				Severity: analyzer.SeverityError,
				Category: "imports",
				File:     "main.go",
				Line:     10,
				Message:  "unused import 'fmt'",
			},
		},
		Duration:  50 * time.Millisecond,
		Timestamp: time.Date(2025, 1, 1, 0, 0, 0, 0, time.UTC),
	}

	exitCode := r.Report(result, "error")
	if exitCode != 1 {
		t.Errorf("expected exit code 1 with errors, got %d", exitCode)
	}

	// Verify valid JSON.
	var parsed map[string]interface{}
	if err := json.Unmarshal(buf.Bytes(), &parsed); err != nil {
		t.Fatalf("output is not valid JSON: %v", err)
	}

	// Verify issue content.
	issuesRaw := parsed["issues"].([]interface{})
	if len(issuesRaw) != 1 {
		t.Fatalf("expected 1 issue, got %d", len(issuesRaw))
	}
	issue := issuesRaw[0].(map[string]interface{})
	if issue["id"] != "imp-1" {
		t.Errorf("expected issue id 'imp-1', got %v", issue["id"])
	}
	if issue["severity"] != "error" {
		t.Errorf("expected severity 'error', got %v", issue["severity"])
	}
	if issue["file"] != "main.go" {
		t.Errorf("expected file 'main.go', got %v", issue["file"])
	}
}

func TestJSONReporterWarningsBlockOnWarning(t *testing.T) {
	var buf bytes.Buffer
	r := NewJSONReporterWithWriter(&buf)

	result := &analyzer.AnalysisResult{
		FilesChanged: 1,
		Issues: []analyzer.Issue{
			{Severity: analyzer.SeverityWarning, Category: "test", File: "a.go", Message: "warn"},
		},
		Duration:  10 * time.Millisecond,
		Timestamp: time.Date(2025, 1, 1, 0, 0, 0, 0, time.UTC),
	}

	exitCode := r.Report(result, "error")
	if exitCode != 0 {
		t.Errorf("expected exit code 0 for warnings with block_on=error, got %d", exitCode)
	}

	buf.Reset()
	exitCode = r.Report(result, "warning")
	if exitCode != 1 {
		t.Errorf("expected exit code 1 for warnings with block_on=warning, got %d", exitCode)
	}
}

func TestJSONReporterOutputIsPrettyPrinted(t *testing.T) {
	var buf bytes.Buffer
	r := NewJSONReporterWithWriter(&buf)

	result := &analyzer.AnalysisResult{
		RepoPath:     "/test/repo",
		FilesChanged: 1,
		Issues:       make([]analyzer.Issue, 0),
		Duration:     10 * time.Millisecond,
		Timestamp:    time.Date(2025, 1, 1, 0, 0, 0, 0, time.UTC),
	}

	r.Report(result, "error")
	output := buf.String()

	// Pretty-printed JSON has newlines.
	if !bytes.Contains(buf.Bytes(), []byte("\n")) {
		t.Errorf("expected pretty-printed JSON with newlines, got: %s", output)
	}
	// Pretty-printed JSON has indentation.
	if !bytes.Contains(buf.Bytes(), []byte("  ")) {
		t.Errorf("expected indented JSON, got: %s", output)
	}
}

func TestJSONReporterInfoOnlyNeverBlocks(t *testing.T) {
	var buf bytes.Buffer
	r := NewJSONReporterWithWriter(&buf)

	result := &analyzer.AnalysisResult{
		FilesChanged: 1,
		Issues: []analyzer.Issue{
			{Severity: analyzer.SeverityInfo, Category: "test", File: "a.go", Message: "info"},
		},
		Duration:  time.Millisecond,
		Timestamp: time.Date(2025, 1, 1, 0, 0, 0, 0, time.UTC),
	}

	exitCode := r.Report(result, "warning")
	if exitCode != 0 {
		t.Errorf("expected exit code 0 for info-only with block_on=warning, got %d", exitCode)
	}
}
