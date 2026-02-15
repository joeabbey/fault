package reporter

import (
	"bytes"
	"strings"
	"testing"
	"time"

	"github.com/joeabbey/fault/pkg/analyzer"
	"github.com/joeabbey/fault/pkg/git"
)

func TestTerminalReporterNoIssues(t *testing.T) {
	var buf bytes.Buffer
	r := NewTerminalReporterWithWriter(&buf, true, true)

	result := &analyzer.AnalysisResult{
		FilesChanged: 3,
		Issues:       make([]analyzer.Issue, 0),
		Duration:     100 * time.Millisecond,
	}

	exitCode := r.Report(result, "error")
	if exitCode != 0 {
		t.Errorf("expected exit code 0, got %d", exitCode)
	}

	output := buf.String()
	if !strings.Contains(output, "PASS") {
		t.Error("expected PASS in output")
	}
	if !strings.Contains(output, "No issues found") {
		t.Error("expected 'No issues found' in output")
	}
}

func TestTerminalReporterWithErrors(t *testing.T) {
	var buf bytes.Buffer
	r := NewTerminalReporterWithWriter(&buf, true, true)

	result := &analyzer.AnalysisResult{
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
		Duration: 50 * time.Millisecond,
	}

	exitCode := r.Report(result, "error")
	if exitCode != 1 {
		t.Errorf("expected exit code 1, got %d", exitCode)
	}

	output := buf.String()
	if !strings.Contains(output, "main.go:10") {
		t.Errorf("expected 'main.go:10' in output, got:\n%s", output)
	}
	if !strings.Contains(output, "error") {
		t.Error("expected 'error' in output")
	}
	if !strings.Contains(output, "[imports]") {
		t.Error("expected '[imports]' in output")
	}
	if !strings.Contains(output, "unused import 'fmt'") {
		t.Error("expected message in output")
	}
}

func TestTerminalReporterWarningsOnlyBlockOnWarning(t *testing.T) {
	var buf bytes.Buffer
	r := NewTerminalReporterWithWriter(&buf, true, true)

	result := &analyzer.AnalysisResult{
		FilesChanged: 1,
		Issues: []analyzer.Issue{
			{Severity: analyzer.SeverityWarning, Category: "test", File: "a.go", Message: "warn"},
		},
		Duration: 10 * time.Millisecond,
	}

	// With block_on=error, warnings don't block
	exitCode := r.Report(result, "error")
	if exitCode != 0 {
		t.Errorf("expected exit code 0 for warnings with block_on=error, got %d", exitCode)
	}

	// With block_on=warning, warnings do block
	buf.Reset()
	exitCode = r.Report(result, "warning")
	if exitCode != 1 {
		t.Errorf("expected exit code 1 for warnings with block_on=warning, got %d", exitCode)
	}
}

func TestTerminalReporterWithSuggestion(t *testing.T) {
	var buf bytes.Buffer
	r := NewTerminalReporterWithWriter(&buf, true, true)

	result := &analyzer.AnalysisResult{
		FilesChanged: 1,
		Issues: []analyzer.Issue{
			{
				Severity:   analyzer.SeverityWarning,
				Category:   "imports",
				File:       "main.go",
				Line:       5,
				Message:    "unused import",
				Suggestion: "Remove the import or use it",
			},
		},
		Duration: 10 * time.Millisecond,
	}

	r.Report(result, "error")
	output := buf.String()

	if !strings.Contains(output, "suggestion: Remove the import or use it") {
		t.Errorf("expected suggestion in output, got:\n%s", output)
	}
}

func TestTerminalReporterWithRelatedFiles(t *testing.T) {
	var buf bytes.Buffer
	r := NewTerminalReporterWithWriter(&buf, true, true)

	result := &analyzer.AnalysisResult{
		FilesChanged: 2,
		Issues: []analyzer.Issue{
			{
				Severity:     analyzer.SeverityError,
				Category:     "references",
				File:         "main.go",
				Line:         10,
				Message:      "broken reference to helper()",
				RelatedFiles: []string{"utils.go", "helpers.go"},
			},
		},
		Duration: 10 * time.Millisecond,
	}

	r.Report(result, "error")
	output := buf.String()

	if !strings.Contains(output, "related: utils.go, helpers.go") {
		t.Errorf("expected related files in output, got:\n%s", output)
	}
}

func TestTerminalReporterSummaryLine(t *testing.T) {
	var buf bytes.Buffer
	r := NewTerminalReporterWithWriter(&buf, true, true)

	result := &analyzer.AnalysisResult{
		FilesChanged: 5,
		Issues: []analyzer.Issue{
			{Severity: analyzer.SeverityError, Category: "a", File: "a.go", Message: "err1"},
			{Severity: analyzer.SeverityError, Category: "a", File: "b.go", Message: "err2"},
			{Severity: analyzer.SeverityWarning, Category: "b", File: "c.go", Message: "warn1"},
			{Severity: analyzer.SeverityInfo, Category: "c", File: "d.go", Message: "info1"},
		},
		Duration: 1500 * time.Millisecond,
	}

	r.Report(result, "error")
	output := buf.String()

	if !strings.Contains(output, "4 issues") {
		t.Errorf("expected '4 issues' in output, got:\n%s", output)
	}
	if !strings.Contains(output, "2 errors") {
		t.Errorf("expected '2 errors' in output, got:\n%s", output)
	}
	if !strings.Contains(output, "1 warnings") {
		t.Errorf("expected '1 warnings' in output, got:\n%s", output)
	}
	if !strings.Contains(output, "5 files") {
		t.Errorf("expected '5 files' in output, got:\n%s", output)
	}
	if !strings.Contains(output, "1.5s") {
		t.Errorf("expected '1.5s' duration in output, got:\n%s", output)
	}
}

func TestFormatDuration(t *testing.T) {
	tests := []struct {
		ms   int64
		want string
	}{
		{50, "50ms"},
		{999, "999ms"},
		{1000, "1.0s"},
		{1500, "1.5s"},
		{10000, "10.0s"},
	}

	for _, tt := range tests {
		d := time.Duration(tt.ms) * time.Millisecond
		got := formatDuration(d)
		if got != tt.want {
			t.Errorf("formatDuration(%dms) = %q, want %q", tt.ms, got, tt.want)
		}
	}
}

func TestExitCodeLogic(t *testing.T) {
	tests := []struct {
		name    string
		issues  []analyzer.Issue
		blockOn string
		want    int
	}{
		{
			name:    "no issues = 0",
			issues:  make([]analyzer.Issue, 0),
			blockOn: "error",
			want:    0,
		},
		{
			name:    "errors with block_on=error = 1",
			issues:  []analyzer.Issue{{Severity: analyzer.SeverityError, Category: "a", File: "a.go", Message: "x"}},
			blockOn: "error",
			want:    1,
		},
		{
			name:    "warnings with block_on=error = 0",
			issues:  []analyzer.Issue{{Severity: analyzer.SeverityWarning, Category: "a", File: "a.go", Message: "x"}},
			blockOn: "error",
			want:    0,
		},
		{
			name:    "warnings with block_on=warning = 1",
			issues:  []analyzer.Issue{{Severity: analyzer.SeverityWarning, Category: "a", File: "a.go", Message: "x"}},
			blockOn: "warning",
			want:    1,
		},
		{
			name:    "info only = 0",
			issues:  []analyzer.Issue{{Severity: analyzer.SeverityInfo, Category: "a", File: "a.go", Message: "x"}},
			blockOn: "warning",
			want:    0,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			var buf bytes.Buffer
			r := NewTerminalReporterWithWriter(&buf, true, true)
			result := &analyzer.AnalysisResult{
				Issues:       tt.issues,
				FilesChanged: 1,
				Duration:     time.Millisecond,
			}
			got := r.Report(result, tt.blockOn)
			if got != tt.want {
				t.Errorf("exit code = %d, want %d", got, tt.want)
			}
		})
	}
}

func TestVerboseOutput(t *testing.T) {
	var buf bytes.Buffer
	r := NewTerminalReporterWithWriter(&buf, true, false) // verbose (compact=false)

	diff := &git.Diff{
		Files: []git.FileDiff{
			{
				Path:   "pkg/api/server.go",
				Status: "modified",
				Hunks: []git.Hunk{
					{
						NewStart: 40,
						NewCount: 5,
						Lines: []git.Line{
							{Type: "context", Content: "import (", NewNum: 40, OldNum: 40},
							{Type: "context", Content: `    "fmt"`, NewNum: 41, OldNum: 41},
							{Type: "added", Content: `    "github.com/foo/bar"`, NewNum: 42},
							{Type: "context", Content: `    "net/http"`, NewNum: 43, OldNum: 42},
							{Type: "context", Content: ")", NewNum: 44, OldNum: 43},
						},
					},
				},
			},
		},
	}
	r.SetDiff(diff)

	result := &analyzer.AnalysisResult{
		FilesChanged: 1,
		Issues: []analyzer.Issue{
			{
				Severity:     analyzer.SeverityError,
				Category:     "import",
				File:         "pkg/api/server.go",
				Line:         42,
				Message:      `Import "github.com/foo/bar" could not be resolved`,
				Suggestion:   "Check the import path and ensure the target file exists",
				RelatedFiles: []string{"pkg/api/client.go"},
			},
		},
		Duration: 142 * time.Millisecond,
	}

	r.Report(result, "error")
	output := buf.String()

	// Check severity + category + location header
	if !strings.Contains(output, "error") {
		t.Errorf("expected 'error' severity in output, got:\n%s", output)
	}
	if !strings.Contains(output, "[import]") {
		t.Errorf("expected '[import]' category in output, got:\n%s", output)
	}
	if !strings.Contains(output, "pkg/api/server.go:42") {
		t.Errorf("expected file:line in output, got:\n%s", output)
	}

	// Check message on its own line
	if !strings.Contains(output, `Import "github.com/foo/bar" could not be resolved`) {
		t.Errorf("expected message in output, got:\n%s", output)
	}

	// Check code context
	if !strings.Contains(output, "import (") {
		t.Errorf("expected code context in output, got:\n%s", output)
	}
	if !strings.Contains(output, `"github.com/foo/bar"`) {
		t.Errorf("expected issue line content in output, got:\n%s", output)
	}
	if !strings.Contains(output, "<-- here") {
		t.Errorf("expected '<-- here' marker in output, got:\n%s", output)
	}

	// Check fix suggestion
	if !strings.Contains(output, "fix:") {
		t.Errorf("expected 'fix:' prefix in output, got:\n%s", output)
	}
	if !strings.Contains(output, "Check the import path") {
		t.Errorf("expected suggestion text in output, got:\n%s", output)
	}

	// Check related files
	if !strings.Contains(output, "related:") {
		t.Errorf("expected 'related:' prefix in output, got:\n%s", output)
	}
	if !strings.Contains(output, "pkg/api/client.go") {
		t.Errorf("expected related file in output, got:\n%s", output)
	}

	// Check em-dash summary format
	if !strings.Contains(output, "\u2014") {
		t.Errorf("expected em-dash in summary, got:\n%s", output)
	}
}

func TestCompactOutput(t *testing.T) {
	var buf bytes.Buffer
	r := NewTerminalReporterWithWriter(&buf, true, true) // compact

	result := &analyzer.AnalysisResult{
		FilesChanged: 1,
		Issues: []analyzer.Issue{
			{
				Severity: analyzer.SeverityError,
				Category: "imports",
				File:     "main.go",
				Line:     10,
				Message:  "unused import 'fmt'",
			},
		},
		Duration: 50 * time.Millisecond,
	}

	r.Report(result, "error")
	output := buf.String()

	// Compact format: "main.go:10 error [imports] unused import 'fmt'"
	if !strings.Contains(output, "main.go:10 error [imports] unused import 'fmt'") {
		t.Errorf("expected compact single-line format, got:\n%s", output)
	}

	// Should NOT contain verbose markers
	if strings.Contains(output, "<-- here") {
		t.Error("compact mode should not contain '<-- here' marker")
	}
	if strings.Contains(output, "fix:") {
		t.Error("compact mode should not contain 'fix:' prefix")
	}
}

func TestVerboseNoContext(t *testing.T) {
	var buf bytes.Buffer
	r := NewTerminalReporterWithWriter(&buf, true, false) // verbose, no diff set

	result := &analyzer.AnalysisResult{
		FilesChanged: 1,
		Issues: []analyzer.Issue{
			{
				Severity:   analyzer.SeverityWarning,
				Category:   "test",
				File:       "main.go",
				Line:       5,
				Message:    "test message",
				Suggestion: "try this fix",
			},
		},
		Duration: 10 * time.Millisecond,
	}

	r.Report(result, "error")
	output := buf.String()

	// Should still show severity, message, and suggestion
	if !strings.Contains(output, "warning") {
		t.Errorf("expected 'warning' in output, got:\n%s", output)
	}
	if !strings.Contains(output, "test message") {
		t.Errorf("expected message in output, got:\n%s", output)
	}
	if !strings.Contains(output, "fix:") {
		t.Errorf("expected 'fix:' in output, got:\n%s", output)
	}
	if !strings.Contains(output, "try this fix") {
		t.Errorf("expected suggestion in output, got:\n%s", output)
	}

	// Should NOT have code context (no diff)
	if strings.Contains(output, "<-- here") {
		t.Error("should not have code context when diff is nil")
	}
}

func TestVerboseNoSuggestion(t *testing.T) {
	var buf bytes.Buffer
	r := NewTerminalReporterWithWriter(&buf, true, false) // verbose

	result := &analyzer.AnalysisResult{
		FilesChanged: 1,
		Issues: []analyzer.Issue{
			{
				Severity: analyzer.SeverityError,
				Category: "imports",
				File:     "main.go",
				Line:     10,
				Message:  "unused import",
			},
		},
		Duration: 10 * time.Millisecond,
	}

	r.Report(result, "error")
	output := buf.String()

	// Should have severity and message
	if !strings.Contains(output, "error") {
		t.Errorf("expected 'error' in output, got:\n%s", output)
	}
	if !strings.Contains(output, "unused import") {
		t.Errorf("expected message in output, got:\n%s", output)
	}

	// Should NOT have fix: line
	if strings.Contains(output, "fix:") {
		t.Error("should not have 'fix:' when suggestion is empty")
	}
}
