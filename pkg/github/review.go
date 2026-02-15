package github

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io"
	"net/http"
	"strings"

	"github.com/joeabbey/fault/pkg/analyzer"
)

// ReviewComment is an inline comment on a PR diff.
type ReviewComment struct {
	Path string `json:"path"`
	Line int    `json:"line"`
	Side string `json:"side"`
	Body string `json:"body"`
}

// ReviewRequest is the payload for the GitHub Create Review API.
type ReviewRequest struct {
	CommitID string          `json:"commit_id"`
	Body     string          `json:"body"`
	Event    string          `json:"event"`
	Comments []ReviewComment `json:"comments"`
}

// Client posts PR reviews via the GitHub REST API.
type Client struct {
	token      string
	apiBase    string
	httpClient *http.Client
}

// NewClient creates a GitHub API client with the given token.
func NewClient(token string) *Client {
	return &Client{
		token:      token,
		apiBase:    "https://api.github.com",
		httpClient: http.DefaultClient,
	}
}

// PostReview creates a pull request review with inline comments.
func (c *Client) PostReview(owner, repo string, prNumber int, review *ReviewRequest) error {
	body, err := json.Marshal(review)
	if err != nil {
		return fmt.Errorf("marshaling review: %w", err)
	}

	url := fmt.Sprintf("%s/repos/%s/%s/pulls/%d/reviews", c.apiBase, owner, repo, prNumber)
	req, err := http.NewRequest("POST", url, bytes.NewReader(body))
	if err != nil {
		return fmt.Errorf("creating request: %w", err)
	}

	req.Header.Set("Authorization", "Bearer "+c.token)
	req.Header.Set("Accept", "application/vnd.github+json")
	req.Header.Set("Content-Type", "application/json")

	resp, err := c.httpClient.Do(req)
	if err != nil {
		return fmt.Errorf("posting review: %w", err)
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusOK && resp.StatusCode != http.StatusCreated {
		respBody, _ := io.ReadAll(resp.Body)
		return fmt.Errorf("GitHub API returned %d: %s", resp.StatusCode, string(respBody))
	}

	return nil
}

// BuildReview converts AnalysisResult issues into a ReviewRequest.
// Returns nil if there are no issues (caller should skip posting).
func BuildReview(result *analyzer.AnalysisResult, commitSHA string) *ReviewRequest {
	if len(result.Issues) == 0 {
		return nil
	}

	comments := make([]ReviewComment, 0)
	var summaryOnlyIssues []analyzer.Issue

	for _, issue := range result.Issues {
		if issue.File != "" && issue.Line > 0 {
			comments = append(comments, ReviewComment{
				Path: issue.File,
				Line: issue.Line,
				Side: "RIGHT",
				Body: formatCommentBody(issue),
			})
		} else {
			summaryOnlyIssues = append(summaryOnlyIssues, issue)
		}
	}

	body := buildSummaryBody(result, summaryOnlyIssues)

	return &ReviewRequest{
		CommitID: commitSHA,
		Body:     body,
		Event:    "COMMENT",
		Comments: comments,
	}
}

// formatCommentBody creates the markdown body for an inline review comment.
func formatCommentBody(issue analyzer.Issue) string {
	emoji := severityEmoji(issue.Severity)
	var b strings.Builder

	fmt.Fprintf(&b, "**%s %s** `[%s]`\n\n", emoji, issue.Severity, issue.Category)
	b.WriteString(issue.Message)

	if issue.Suggestion != "" {
		fmt.Fprintf(&b, "\n\n> **Suggestion:** %s", issue.Suggestion)
	}

	b.WriteString("\n\n<sub>Found by [Fault](https://fault.jabbey.io)</sub>")

	return b.String()
}

// severityEmoji returns an emoji for the given severity level.
func severityEmoji(s analyzer.Severity) string {
	switch s {
	case analyzer.SeverityError:
		return "\U0001f534" // red circle
	case analyzer.SeverityWarning:
		return "\u26a0\ufe0f" // warning
	case analyzer.SeverityInfo:
		return "\u2139\ufe0f" // info
	default:
		return "\u2139\ufe0f"
	}
}

// buildSummaryBody creates the review summary text.
func buildSummaryBody(result *analyzer.AnalysisResult, summaryOnlyIssues []analyzer.Issue) string {
	errors := result.ErrorCount()
	warnings := result.WarningCount()
	infos := result.InfoCount()
	total := len(result.Issues)

	var b strings.Builder
	fmt.Fprintf(&b, "Fault found %d issues", total)

	parts := make([]string, 0)
	if errors > 0 {
		parts = append(parts, fmt.Sprintf("%d errors", errors))
	}
	if warnings > 0 {
		parts = append(parts, fmt.Sprintf("%d warnings", warnings))
	}
	if infos > 0 {
		parts = append(parts, fmt.Sprintf("%d info", infos))
	}
	if len(parts) > 0 {
		fmt.Fprintf(&b, " (%s)", strings.Join(parts, ", "))
	}

	fmt.Fprintf(&b, " in %d files", result.FilesChanged)

	if len(summaryOnlyIssues) > 0 {
		b.WriteString("\n\n**Issues without line information:**\n")
		for _, issue := range summaryOnlyIssues {
			emoji := severityEmoji(issue.Severity)
			file := issue.File
			if file == "" {
				file = "(no file)"
			}
			fmt.Fprintf(&b, "- %s **%s** `[%s]` %s — %s\n", emoji, issue.Severity, issue.Category, file, issue.Message)
		}
	}

	return b.String()
}
