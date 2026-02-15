package reporter

import (
	"encoding/json"
	"fmt"
	"log"
	"os"
	"strconv"
	"strings"

	"github.com/joeabbey/fault/pkg/analyzer"
	"github.com/joeabbey/fault/pkg/github"
)

// GitHubReporter posts findings as PR review comments and writes SARIF.
type GitHubReporter struct{}

// NewGitHubReporter creates a GitHub reporter.
func NewGitHubReporter() *GitHubReporter {
	return &GitHubReporter{}
}

// Report posts PR review comments via GitHub API, then writes SARIF to fault-results.sarif.
// Falls back to SARIF-only output when not in GitHub Actions environment.
func (r *GitHubReporter) Report(result *analyzer.AnalysisResult, blockOn string) int {
	inActions := os.Getenv("GITHUB_ACTIONS") == "true"
	token := os.Getenv("GITHUB_TOKEN")
	repository := os.Getenv("GITHUB_REPOSITORY")
	eventPath := os.Getenv("GITHUB_EVENT_PATH")

	if !inActions || token == "" || repository == "" || eventPath == "" {
		// Fall back to SARIF on stdout
		return NewSARIFReporter().Report(result, blockOn)
	}

	// Parse owner/repo
	owner, repo, ok := parseRepository(repository)
	if !ok {
		log.Printf("warning: could not parse GITHUB_REPOSITORY %q", repository)
		return NewSARIFReporter().Report(result, blockOn)
	}

	// Parse event payload to get PR number and head SHA
	prNumber, commitSHA, err := parseEventPayload(eventPath)
	if err != nil {
		// Not a pull_request event — just do SARIF
		log.Printf("info: not a pull_request event, writing SARIF only: %v", err)
		return writeSARIFFile(result, blockOn)
	}

	// Build and post review
	review := github.BuildReview(result, commitSHA)
	if review != nil {
		client := github.NewClient(token)
		if err := client.PostReview(owner, repo, prNumber, review); err != nil {
			log.Printf("warning: failed to post PR review: %v", err)
		}
	}

	// Also write SARIF for Code Scanning
	return writeSARIFFile(result, blockOn)
}

// writeSARIFFile writes SARIF to fault-results.sarif and returns the exit code.
func writeSARIFFile(result *analyzer.AnalysisResult, blockOn string) int {
	f, err := os.Create("fault-results.sarif")
	if err != nil {
		log.Printf("warning: could not create fault-results.sarif: %v", err)
		return NewSARIFReporter().Report(result, blockOn)
	}
	defer f.Close()

	rep := NewSARIFReporterWithWriter(f)
	return rep.Report(result, blockOn)
}

// parseRepository splits "owner/repo" into its parts.
func parseRepository(repo string) (owner, name string, ok bool) {
	parts := strings.SplitN(repo, "/", 2)
	if len(parts) != 2 || parts[0] == "" || parts[1] == "" {
		return "", "", false
	}
	return parts[0], parts[1], true
}

// parseEventPayload reads the GitHub event JSON to extract PR number and head SHA.
func parseEventPayload(path string) (prNumber int, commitSHA string, err error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return 0, "", fmt.Errorf("reading event payload: %w", err)
	}

	var event struct {
		PullRequest *struct {
			Number int `json:"number"`
			Head   struct {
				SHA string `json:"sha"`
			} `json:"head"`
		} `json:"pull_request"`
	}

	if err := json.Unmarshal(data, &event); err != nil {
		return 0, "", fmt.Errorf("parsing event payload: %w", err)
	}

	if event.PullRequest == nil {
		return 0, "", fmt.Errorf("event is not a pull_request")
	}

	if event.PullRequest.Number == 0 {
		return 0, "", fmt.Errorf("pull_request number is 0")
	}

	return event.PullRequest.Number, event.PullRequest.Head.SHA, nil
}

// parsePRNumber extracts a PR number from a string (helper for testing).
func parsePRNumber(s string) (int, error) {
	return strconv.Atoi(s)
}
