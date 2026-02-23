package git

import (
	"fmt"
	"strings"
)

// RefDiff returns the diff between two git refs (e.g., commits, tags, branches).
func (r *Repo) RefDiff(baseRef, headRef string) (*Diff, error) {
	output, err := r.git("diff", baseRef+"..."+headRef, "-U3")
	if err != nil {
		return nil, fmt.Errorf("getting ref diff %s...%s: %w", baseRef, headRef, err)
	}
	return ParseDiff(output, "ref")
}

// HeadSHA returns the full SHA of the current HEAD commit.
func (r *Repo) HeadSHA() (string, error) {
	output, err := r.git("rev-parse", "HEAD")
	if err != nil {
		return "", fmt.Errorf("getting HEAD SHA: %w", err)
	}
	return strings.TrimSpace(output), nil
}

// CommitLog returns the commit log between a base ref and HEAD.
// Each entry contains the short SHA, author, and subject line.
func (r *Repo) CommitLog(baseRef string) ([]CommitEntry, error) {
	output, err := r.git("log", "--format=%H\x1f%an\x1f%s", baseRef+"..HEAD")
	if err != nil {
		return nil, fmt.Errorf("getting commit log since %s: %w", baseRef, err)
	}

	lines := strings.Split(strings.TrimSpace(output), "\n")
	entries := make([]CommitEntry, 0, len(lines))
	for _, line := range lines {
		line = strings.TrimSpace(line)
		if line == "" {
			continue
		}
		parts := strings.SplitN(line, "\x1f", 3)
		if len(parts) < 3 {
			continue
		}
		entries = append(entries, CommitEntry{
			SHA:     parts[0],
			Author:  parts[1],
			Subject: parts[2],
		})
	}
	return entries, nil
}

// CommitEntry represents a single commit in the log.
type CommitEntry struct {
	SHA     string `json:"sha"`
	Author  string `json:"author"`
	Subject string `json:"subject"`
}

// RemoteURL returns the URL of the "origin" remote, or empty string if none.
func (r *Repo) RemoteURL() string {
	output, err := r.git("remote", "get-url", "origin")
	if err != nil {
		return ""
	}
	return strings.TrimSpace(output)
}

// MergeBase returns the common ancestor of two refs.
func (r *Repo) MergeBase(ref1, ref2 string) (string, error) {
	output, err := r.git("merge-base", ref1, ref2)
	if err != nil {
		return "", fmt.Errorf("finding merge base of %s and %s: %w", ref1, ref2, err)
	}
	return strings.TrimSpace(output), nil
}
