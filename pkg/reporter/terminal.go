package reporter

import (
	"fmt"
	"io"
	"os"
	"strings"

	"github.com/fatih/color"
	"github.com/joeabbey/fault/pkg/analyzer"
)

// TerminalReporter outputs analysis results to the terminal with colors.
type TerminalReporter struct {
	out     io.Writer
	noColor bool
}

// NewTerminalReporter creates a terminal reporter.
// Set noColor to true to disable color output.
func NewTerminalReporter(noColor bool) *TerminalReporter {
	// Also check NO_COLOR env var
	if os.Getenv("NO_COLOR") != "" {
		noColor = true
	}

	if noColor {
		color.NoColor = true
	}

	return &TerminalReporter{
		out:     os.Stdout,
		noColor: noColor,
	}
}

// NewTerminalReporterWithWriter creates a terminal reporter with a custom writer.
func NewTerminalReporterWithWriter(w io.Writer, noColor bool) *TerminalReporter {
	if noColor {
		color.NoColor = true
	}
	return &TerminalReporter{
		out:     w,
		noColor: noColor,
	}
}

// Report outputs the analysis result to the terminal.
func (r *TerminalReporter) Report(result *analyzer.AnalysisResult, blockOn string) int {
	if len(result.Issues) == 0 {
		r.printSuccess(result)
		return 0
	}

	r.printIssues(result)
	r.printSummary(result)

	if result.ShouldBlock(blockOn) {
		return 1
	}
	return 0
}

// printSuccess prints a clean success message.
func (r *TerminalReporter) printSuccess(result *analyzer.AnalysisResult) {
	green := color.New(color.FgGreen, color.Bold)
	msg := green.Sprintf("PASS")
	fmt.Fprintf(r.out, "%s  No issues found in %d changed files (%s)\n",
		msg,
		result.FilesChanged,
		formatDuration(result.Duration),
	)
}

// printIssues prints each issue with color-coded severity.
func (r *TerminalReporter) printIssues(result *analyzer.AnalysisResult) {
	for _, issue := range result.Issues {
		r.printIssue(issue)
	}
	fmt.Fprintln(r.out)
}

// printIssue prints a single issue.
func (r *TerminalReporter) printIssue(issue analyzer.Issue) {
	// File location
	location := issue.File
	if issue.Line > 0 {
		location = fmt.Sprintf("%s:%d", issue.File, issue.Line)
	}

	// Severity badge
	badge := r.severityBadge(issue.Severity)

	// Category
	category := fmt.Sprintf("[%s]", issue.Category)

	fmt.Fprintf(r.out, "%s %s %s %s\n", location, badge, category, issue.Message)

	// Suggestion (indented)
	if issue.Suggestion != "" {
		dim := color.New(color.FgHiBlack)
		suggestion := dim.Sprintf("  suggestion: %s", issue.Suggestion)
		fmt.Fprintln(r.out, suggestion)
	}

	// Related files (indented)
	if len(issue.RelatedFiles) > 0 {
		dim := color.New(color.FgHiBlack)
		related := dim.Sprintf("  related: %s", strings.Join(issue.RelatedFiles, ", "))
		fmt.Fprintln(r.out, related)
	}
}

// severityBadge returns a color-coded severity string.
func (r *TerminalReporter) severityBadge(severity analyzer.Severity) string {
	switch severity {
	case analyzer.SeverityError:
		c := color.New(color.FgRed, color.Bold)
		return c.Sprint("error")
	case analyzer.SeverityWarning:
		c := color.New(color.FgYellow, color.Bold)
		return c.Sprint("warning")
	case analyzer.SeverityInfo:
		c := color.New(color.FgBlue)
		return c.Sprint("info")
	default:
		return string(severity)
	}
}

// printSummary prints the summary line.
func (r *TerminalReporter) printSummary(result *analyzer.AnalysisResult) {
	errors := result.ErrorCount()
	warnings := result.WarningCount()
	infos := result.InfoCount()
	total := len(result.Issues)

	parts := make([]string, 0)

	if errors > 0 {
		c := color.New(color.FgRed, color.Bold)
		parts = append(parts, c.Sprintf("%d errors", errors))
	}
	if warnings > 0 {
		c := color.New(color.FgYellow, color.Bold)
		parts = append(parts, c.Sprintf("%d warnings", warnings))
	}
	if infos > 0 {
		c := color.New(color.FgBlue)
		parts = append(parts, c.Sprintf("%d info", infos))
	}

	summary := fmt.Sprintf("Found %d issues", total)
	if len(parts) > 0 {
		summary += " (" + strings.Join(parts, ", ") + ")"
	}
	summary += fmt.Sprintf(" in %d files (%s)", result.FilesChanged, formatDuration(result.Duration))

	fmt.Fprintln(r.out, summary)
}

// formatDuration formats a duration for display.
func formatDuration(d interface{ Milliseconds() int64 }) string {
	ms := d.Milliseconds()
	if ms < 1000 {
		return fmt.Sprintf("%dms", ms)
	}
	return fmt.Sprintf("%.1fs", float64(ms)/1000.0)
}
