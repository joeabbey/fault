package reporter

import (
	"fmt"
	"io"
	"os"
	"strings"

	"github.com/fatih/color"
	"github.com/joeabbey/fault/pkg/analyzer"
	"github.com/joeabbey/fault/pkg/git"
)

// TerminalReporter outputs analysis results to the terminal with colors.
type TerminalReporter struct {
	out     io.Writer
	noColor bool
	compact bool
	diff    *git.Diff
}

// NewTerminalReporter creates a terminal reporter.
// Set noColor to true to disable color output.
// Set compact to true for single-line output (CI/hooks).
func NewTerminalReporter(noColor bool, compact bool) *TerminalReporter {
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
		compact: compact,
	}
}

// NewTerminalReporterWithWriter creates a terminal reporter with a custom writer.
func NewTerminalReporterWithWriter(w io.Writer, noColor bool, compact bool) *TerminalReporter {
	if noColor {
		color.NoColor = true
	}
	return &TerminalReporter{
		out:     w,
		noColor: noColor,
		compact: compact,
	}
}

// SetDiff sets the git diff for code context extraction in verbose mode.
func (r *TerminalReporter) SetDiff(diff *git.Diff) {
	r.diff = diff
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

	if result.Confidence != nil && !r.compact {
		dim := color.New(color.FgHiBlack)
		fmt.Fprintf(r.out, "  %s %.2f\n", dim.Sprint("confidence:"), result.Confidence.Score)
		if len(result.Confidence.Factors) > 0 {
			reason := strings.TrimSpace(result.Confidence.Factors[0])
			if reason != "" {
				fmt.Fprintf(r.out, "  %s %s\n", dim.Sprint("reason:"), reason)
			}
		}
	}
}

// printIssues prints each issue with color-coded severity.
func (r *TerminalReporter) printIssues(result *analyzer.AnalysisResult) {
	for _, issue := range result.Issues {
		if r.compact {
			r.printIssueCompact(issue)
		} else {
			r.printIssueVerbose(issue)
		}
	}
	fmt.Fprintln(r.out)
}

// printIssueCompact prints a single issue in compact single-line format.
func (r *TerminalReporter) printIssueCompact(issue analyzer.Issue) {
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

// printIssueVerbose prints a single issue in verbose format with code context.
func (r *TerminalReporter) printIssueVerbose(issue analyzer.Issue) {
	// File location
	location := issue.File
	if issue.Line > 0 {
		location = fmt.Sprintf("%s:%d", issue.File, issue.Line)
	}

	// Severity badge and category on first line
	badge := r.severityBadge(issue.Severity)
	category := fmt.Sprintf("[%s]", issue.Category)
	fmt.Fprintf(r.out, "  %s  %s %s\n", badge, category, location)

	// Message on its own line
	fmt.Fprintf(r.out, "  %s\n", issue.Message)

	// Code context block
	if issue.Line > 0 {
		ctx := ExtractContext(issue.File, issue.Line, r.diff, 3)
		if ctx != nil {
			fmt.Fprintln(r.out)
			r.printCodeContext(ctx)
		}
	}

	// Suggestion
	if issue.Suggestion != "" {
		fmt.Fprintln(r.out)
		green := color.New(color.FgGreen)
		fmt.Fprintf(r.out, "  %s %s\n", green.Sprint("fix:"), issue.Suggestion)
	}

	// Related files
	if len(issue.RelatedFiles) > 0 {
		dim := color.New(color.FgHiBlack)
		fmt.Fprintf(r.out, "  %s %s\n", dim.Sprint("related:"), strings.Join(issue.RelatedFiles, ", "))
	}

	// Blank line between issues
	fmt.Fprintln(r.out)
}

// printCodeContext renders the code context block with line numbers.
func (r *TerminalReporter) printCodeContext(ctx *CodeContext) {
	// Find max line number for width alignment
	maxNum := 0
	for _, l := range ctx.Lines {
		if l.Number > maxNum {
			maxNum = l.Number
		}
	}
	width := len(fmt.Sprintf("%d", maxNum))

	dim := color.New(color.FgHiBlack)
	marker := color.New(color.FgCyan, color.Bold)

	for _, l := range ctx.Lines {
		numStr := fmt.Sprintf("%*d", width, l.Number)

		addMark := " "
		if l.IsAdded {
			addMark = "+"
		}

		suffix := ""
		if l.IsIssue {
			suffix = marker.Sprint("   <-- here")
		}

		fmt.Fprintf(r.out, "    %s %s%s %s%s\n",
			dim.Sprint(numStr),
			dim.Sprint("|"),
			addMark,
			l.Content,
			suffix,
		)
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
		c := color.New(color.FgCyan)
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
		c := color.New(color.FgCyan)
		parts = append(parts, c.Sprintf("%d info", infos))
	}

	// Build summary: "  7 issues (3 errors, 2 warnings, 2 info) in 5 files — 142ms"
	summary := fmt.Sprintf("  %d issues", total)
	if len(parts) > 0 {
		summary += " (" + strings.Join(parts, ", ") + ")"
	}
	summary += fmt.Sprintf(" in %d files", result.FilesChanged)
	summary += fmt.Sprintf(" \u2014 %s", formatDuration(result.Duration))

	fmt.Fprintln(r.out, summary)

	if result.Confidence != nil {
		dim := color.New(color.FgHiBlack)
		fmt.Fprintf(r.out, "  %s %.2f\n", dim.Sprint("confidence:"), result.Confidence.Score)
		if !r.compact && len(result.Confidence.Factors) > 0 {
			reason := strings.TrimSpace(result.Confidence.Factors[0])
			if reason != "" {
				fmt.Fprintf(r.out, "  %s %s\n", dim.Sprint("reason:"), reason)
			}
		}
	}
}

// formatDuration formats a duration for display.
func formatDuration(d interface{ Milliseconds() int64 }) string {
	ms := d.Milliseconds()
	if ms < 1000 {
		return fmt.Sprintf("%dms", ms)
	}
	return fmt.Sprintf("%.1fs", float64(ms)/1000.0)
}
