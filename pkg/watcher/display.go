package watcher

import (
	"fmt"
	"io"
	"strings"
	"time"

	"github.com/joeabbey/fault/pkg/analyzer"
)

// ANSI escape sequences
const (
	ansiClear  = "\033[2J\033[H"
	ansiBold   = "\033[1m"
	ansiRed    = "\033[31m"
	ansiYellow = "\033[33m"
	ansiCyan   = "\033[36m"
	ansiGreen  = "\033[32m"
	ansiDim    = "\033[2m"
	ansiReset  = "\033[0m"
)

// Display renders watch mode output to the terminal.
type Display struct {
	out     io.Writer
	noColor bool
}

// NewDisplay creates a new Display.
func NewDisplay(out io.Writer, noColor bool) *Display {
	return &Display{
		out:     out,
		noColor: noColor,
	}
}

// Clear clears the terminal screen.
func (d *Display) Clear() {
	fmt.Fprint(d.out, ansiClear)
}

// RenderHeader shows the watch mode banner.
func (d *Display) RenderHeader(repoRoot string, version string) {
	if d.noColor {
		fmt.Fprintf(d.out, "  fault watch  %s  %s\n\n", version, repoRoot)
	} else {
		fmt.Fprintf(d.out, "  %s%sfault watch%s  %s%s%s  %s\n\n",
			ansiBold, ansiCyan, ansiReset,
			ansiDim, version, ansiReset,
			repoRoot)
	}
}

// RenderResult shows the latest analysis result.
func (d *Display) RenderResult(result RunResult) {
	r := result.Result

	// Timing info
	ago := time.Since(result.Timestamp)
	agoStr := formatAgo(ago)
	durationStr := formatMs(result.Duration)

	triggerLine := ""
	if result.Trigger != "" {
		triggerLine = fmt.Sprintf("  Triggered by: %s", result.Trigger)
	}

	if d.noColor {
		fmt.Fprintf(d.out, "  Last check: %s (%s)%s\n\n", agoStr, durationStr, triggerLine)
	} else {
		fmt.Fprintf(d.out, "  Last check: %s (%s)%s\n\n", agoStr, durationStr, triggerLine)
	}

	// Issue count summary
	total := len(r.Issues)
	if total == 0 {
		if d.noColor {
			fmt.Fprintf(d.out, "  No issues in %d files\n\n", r.FilesChanged)
		} else {
			fmt.Fprintf(d.out, "  %s%sNo issues%s in %d files\n\n",
				ansiBold, ansiGreen, ansiReset, r.FilesChanged)
		}
		return
	}

	errors := r.ErrorCount()
	warnings := r.WarningCount()
	infos := r.InfoCount()

	parts := make([]string, 0)
	if errors > 0 {
		if d.noColor {
			parts = append(parts, fmt.Sprintf("%d error", errors))
		} else {
			parts = append(parts, fmt.Sprintf("%s%d error%s", ansiRed, errors, ansiReset))
		}
	}
	if warnings > 0 {
		if d.noColor {
			parts = append(parts, fmt.Sprintf("%d warning", warnings))
		} else {
			parts = append(parts, fmt.Sprintf("%s%d warning%s", ansiYellow, warnings, ansiReset))
		}
	}
	if infos > 0 {
		if d.noColor {
			parts = append(parts, fmt.Sprintf("%d info", infos))
		} else {
			parts = append(parts, fmt.Sprintf("%s%d info%s", ansiCyan, infos, ansiReset))
		}
	}

	fmt.Fprintf(d.out, "  %d issues (%s) in %d files\n\n", total, strings.Join(parts, ", "), r.FilesChanged)

	// Compact issue list
	for _, issue := range r.Issues {
		d.renderIssue(issue)
	}
	fmt.Fprintln(d.out)
}

// renderIssue renders a single issue in compact format.
func (d *Display) renderIssue(issue analyzer.Issue) {
	severity := strings.ToUpper(string(issue.Severity))
	location := issue.File
	if issue.Line > 0 {
		location = fmt.Sprintf("%s:%d", issue.File, issue.Line)
	}
	category := fmt.Sprintf("[%s]", issue.Category)

	if d.noColor {
		fmt.Fprintf(d.out, "  %-8s %-14s %-30s %s\n", severity, category, location, issue.Message)
		return
	}

	var severityStr string
	switch issue.Severity {
	case analyzer.SeverityError:
		severityStr = fmt.Sprintf("%s%s%-8s%s", ansiBold, ansiRed, severity, ansiReset)
	case analyzer.SeverityWarning:
		severityStr = fmt.Sprintf("%s%s%-8s%s", ansiBold, ansiYellow, severity, ansiReset)
	default:
		severityStr = fmt.Sprintf("%s%-8s%s", ansiCyan, severity, ansiReset)
	}

	fmt.Fprintf(d.out, "  %s %s%-14s%s %-30s %s\n",
		severityStr, ansiDim, category, ansiReset, location, issue.Message)
}

// RenderWaiting shows the idle state.
func (d *Display) RenderWaiting() {
	if d.noColor {
		fmt.Fprintln(d.out, "  Watching for changes... (Ctrl+C to stop)")
	} else {
		fmt.Fprintf(d.out, "  %sWatching for changes... (Ctrl+C to stop)%s\n", ansiDim, ansiReset)
	}
}

// RenderExit shows the summary when watch mode ends.
func (d *Display) RenderExit(totalRuns int, totalIssues int, duration time.Duration) {
	fmt.Fprintf(d.out, "\n  Watched for %s, ran %d checks, found %d total issues\n",
		formatWatchDuration(duration), totalRuns, totalIssues)
}

// formatAgo formats a duration as a human-readable "ago" string.
func formatAgo(d time.Duration) string {
	secs := int(d.Seconds())
	if secs < 1 {
		return "just now"
	}
	if secs < 60 {
		return fmt.Sprintf("%ds ago", secs)
	}
	mins := secs / 60
	return fmt.Sprintf("%dm ago", mins)
}

// formatMs formats a duration in milliseconds.
func formatMs(d time.Duration) string {
	ms := d.Milliseconds()
	if ms < 1000 {
		return fmt.Sprintf("%dms", ms)
	}
	return fmt.Sprintf("%.1fs", float64(ms)/1000.0)
}

// formatWatchDuration formats the total watch duration.
func formatWatchDuration(d time.Duration) string {
	secs := int(d.Seconds())
	if secs < 60 {
		return fmt.Sprintf("%ds", secs)
	}
	mins := secs / 60
	secs = secs % 60
	if secs == 0 {
		return fmt.Sprintf("%dm", mins)
	}
	return fmt.Sprintf("%dm%ds", mins, secs)
}
