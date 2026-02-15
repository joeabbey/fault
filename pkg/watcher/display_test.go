package watcher

import (
	"bytes"
	"strings"
	"testing"
	"time"

	"github.com/joeabbey/fault/pkg/analyzer"
)

func TestDisplayClear(t *testing.T) {
	var buf bytes.Buffer
	d := NewDisplay(&buf, false)
	d.Clear()

	got := buf.String()
	if got != ansiClear {
		t.Errorf("Clear() wrote %q, want %q", got, ansiClear)
	}
}

func TestDisplayRenderHeader(t *testing.T) {
	var buf bytes.Buffer
	d := NewDisplay(&buf, true) // noColor for predictable output
	d.RenderHeader("/home/user/project", "v0.2.0")

	got := buf.String()
	if !strings.Contains(got, "fault watch") {
		t.Errorf("header should contain 'fault watch', got %q", got)
	}
	if !strings.Contains(got, "v0.2.0") {
		t.Errorf("header should contain version, got %q", got)
	}
	if !strings.Contains(got, "/home/user/project") {
		t.Errorf("header should contain repo root, got %q", got)
	}
}

func TestDisplayRenderResultWithIssues(t *testing.T) {
	issues := []analyzer.Issue{
		{
			Severity: analyzer.SeverityError,
			Category: "import",
			File:     "pkg/api/server.go",
			Line:     42,
			Message:  "Import could not be resolved",
		},
		{
			Severity: analyzer.SeverityWarning,
			Category: "security",
			File:     "pkg/auth/login.go",
			Line:     15,
			Message:  "Hardcoded secret detected",
		},
		{
			Severity: analyzer.SeverityInfo,
			Category: "patterns",
			File:     "pkg/util/debug.go",
			Line:     8,
			Message:  "console.log statement",
		},
	}

	t.Run("no color", func(t *testing.T) {
		var buf bytes.Buffer
		d := NewDisplay(&buf, true) // noColor

		result := RunResult{
			Result: &analyzer.AnalysisResult{
				FilesChanged: 5,
				Issues:       issues,
				Duration:     142 * time.Millisecond,
			},
			Timestamp: time.Now(),
			Duration:  142 * time.Millisecond,
			Trigger:   "pkg/api/server.go",
		}

		d.RenderResult(result)
		got := buf.String()

		if !strings.Contains(got, "3 issues") {
			t.Errorf("should show issue count, got %q", got)
		}
		if !strings.Contains(got, "1 error") {
			t.Errorf("should show error count, got %q", got)
		}
		if !strings.Contains(got, "1 warning") {
			t.Errorf("should show warning count, got %q", got)
		}
		if !strings.Contains(got, "1 info") {
			t.Errorf("should show info count, got %q", got)
		}
		if !strings.Contains(got, "5 files") {
			t.Errorf("should show file count, got %q", got)
		}
		if !strings.Contains(got, "pkg/api/server.go:42") {
			t.Errorf("should show file:line, got %q", got)
		}
		if !strings.Contains(got, "Import could not be resolved") {
			t.Errorf("should show issue message, got %q", got)
		}
		if !strings.Contains(got, "Triggered by: pkg/api/server.go") {
			t.Errorf("should show trigger, got %q", got)
		}
		if !strings.Contains(got, "ERROR") {
			t.Errorf("should show ERROR severity label, got %q", got)
		}
		if !strings.Contains(got, "WARNING") {
			t.Errorf("should show WARNING severity label, got %q", got)
		}
		if !strings.Contains(got, "INFO") {
			t.Errorf("should show INFO severity label, got %q", got)
		}
		// No ANSI codes in noColor mode
		if strings.Contains(got, ansiRed) {
			t.Errorf("noColor output should not contain red ANSI, got %q", got)
		}
	})

	t.Run("with color", func(t *testing.T) {
		var buf bytes.Buffer
		d := NewDisplay(&buf, false) // color enabled

		result := RunResult{
			Result: &analyzer.AnalysisResult{
				FilesChanged: 5,
				Issues:       issues,
				Duration:     142 * time.Millisecond,
			},
			Timestamp: time.Now(),
			Duration:  142 * time.Millisecond,
			Trigger:   "pkg/api/server.go",
		}

		d.RenderResult(result)
		got := buf.String()

		if !strings.Contains(got, "3 issues") {
			t.Errorf("should show issue count, got %q", got)
		}
		// Should contain ANSI color codes for severity
		if !strings.Contains(got, ansiRed) {
			t.Errorf("colored output should contain red for errors, got %q", got)
		}
		if !strings.Contains(got, ansiYellow) {
			t.Errorf("colored output should contain yellow for warnings, got %q", got)
		}
		if !strings.Contains(got, ansiCyan) {
			t.Errorf("colored output should contain cyan for info, got %q", got)
		}
	})
}

func TestDisplayRenderResultNoIssues(t *testing.T) {
	var buf bytes.Buffer
	d := NewDisplay(&buf, true)

	result := RunResult{
		Result: &analyzer.AnalysisResult{
			FilesChanged: 3,
			Issues:       []analyzer.Issue{},
			Duration:     50 * time.Millisecond,
		},
		Timestamp: time.Now(),
		Duration:  50 * time.Millisecond,
	}

	d.RenderResult(result)
	got := buf.String()

	if !strings.Contains(got, "No issues") {
		t.Errorf("should show 'No issues' for clean result, got %q", got)
	}
}

func TestDisplayRenderWaiting(t *testing.T) {
	var buf bytes.Buffer
	d := NewDisplay(&buf, true)
	d.RenderWaiting()

	got := buf.String()
	if !strings.Contains(got, "Watching for changes") {
		t.Errorf("should show watching message, got %q", got)
	}
	if !strings.Contains(got, "Ctrl+C") {
		t.Errorf("should mention Ctrl+C, got %q", got)
	}
}

func TestDisplayRenderExit(t *testing.T) {
	var buf bytes.Buffer
	d := NewDisplay(&buf, true)
	d.RenderExit(10, 25, 5*time.Minute+30*time.Second)

	got := buf.String()
	if !strings.Contains(got, "5m30s") {
		t.Errorf("should format duration, got %q", got)
	}
	if !strings.Contains(got, "10 checks") {
		t.Errorf("should show run count, got %q", got)
	}
	if !strings.Contains(got, "25 total issues") {
		t.Errorf("should show total issues, got %q", got)
	}
}

func TestDisplayNoColor(t *testing.T) {
	var buf bytes.Buffer
	d := NewDisplay(&buf, true) // noColor=true

	d.RenderHeader("/project", "v1.0")
	d.RenderWaiting()

	got := buf.String()

	// Should not contain any ANSI escape sequences
	if strings.Contains(got, "\033[") {
		t.Errorf("noColor output should not contain ANSI escapes, got %q", got)
	}
}

func TestDisplayColorMode(t *testing.T) {
	var buf bytes.Buffer
	d := NewDisplay(&buf, false) // color enabled

	d.RenderHeader("/project", "v1.0")
	d.RenderWaiting()

	got := buf.String()

	// Should contain ANSI escape sequences
	if !strings.Contains(got, "\033[") {
		t.Errorf("color output should contain ANSI escapes, got %q", got)
	}
}

func TestFormatAgo(t *testing.T) {
	tests := []struct {
		d    time.Duration
		want string
	}{
		{0, "just now"},
		{500 * time.Millisecond, "just now"},
		{2 * time.Second, "2s ago"},
		{45 * time.Second, "45s ago"},
		{90 * time.Second, "1m ago"},
		{5 * time.Minute, "5m ago"},
	}

	for _, tt := range tests {
		got := formatAgo(tt.d)
		if got != tt.want {
			t.Errorf("formatAgo(%v) = %q, want %q", tt.d, got, tt.want)
		}
	}
}

func TestFormatMs(t *testing.T) {
	tests := []struct {
		d    time.Duration
		want string
	}{
		{50 * time.Millisecond, "50ms"},
		{142 * time.Millisecond, "142ms"},
		{999 * time.Millisecond, "999ms"},
		{1500 * time.Millisecond, "1.5s"},
		{2000 * time.Millisecond, "2.0s"},
	}

	for _, tt := range tests {
		got := formatMs(tt.d)
		if got != tt.want {
			t.Errorf("formatMs(%v) = %q, want %q", tt.d, got, tt.want)
		}
	}
}

func TestFormatWatchDuration(t *testing.T) {
	tests := []struct {
		d    time.Duration
		want string
	}{
		{30 * time.Second, "30s"},
		{60 * time.Second, "1m"},
		{90 * time.Second, "1m30s"},
		{5 * time.Minute, "5m"},
		{5*time.Minute + 30*time.Second, "5m30s"},
	}

	for _, tt := range tests {
		got := formatWatchDuration(tt.d)
		if got != tt.want {
			t.Errorf("formatWatchDuration(%v) = %q, want %q", tt.d, got, tt.want)
		}
	}
}
