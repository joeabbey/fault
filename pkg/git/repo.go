package git

import (
	"fmt"
	"os/exec"
	"path/filepath"
	"strings"
)

// Repo provides git operations on a repository.
type Repo struct {
	Path string
}

// NewRepo creates a Repo for the given directory.
// It verifies the path is inside a git repository.
func NewRepo(path string) (*Repo, error) {
	absPath, err := filepath.Abs(path)
	if err != nil {
		return nil, fmt.Errorf("resolving path: %w", err)
	}

	// Verify it's a git repo
	cmd := exec.Command("git", "-C", absPath, "rev-parse", "--git-dir")
	if err := cmd.Run(); err != nil {
		return nil, fmt.Errorf("%s is not a git repository", absPath)
	}

	return &Repo{Path: absPath}, nil
}

// StagedDiff returns the diff of staged changes.
func (r *Repo) StagedDiff() (*Diff, error) {
	output, err := r.git("diff", "--cached", "-U3")
	if err != nil {
		return nil, fmt.Errorf("getting staged diff: %w", err)
	}
	return ParseDiff(output, "staged")
}

// UnstagedDiff returns the diff of unstaged working tree changes.
func (r *Repo) UnstagedDiff() (*Diff, error) {
	output, err := r.git("diff", "-U3")
	if err != nil {
		return nil, fmt.Errorf("getting unstaged diff: %w", err)
	}
	return ParseDiff(output, "unstaged")
}

// BranchDiff returns the diff between the current branch and a base branch.
func (r *Repo) BranchDiff(baseBranch string) (*Diff, error) {
	output, err := r.git("diff", baseBranch+"...HEAD", "-U3")
	if err != nil {
		return nil, fmt.Errorf("getting branch diff against %s: %w", baseBranch, err)
	}
	return ParseDiff(output, "branch")
}

// AutoDiff returns staged changes if anything is staged, otherwise unstaged.
func (r *Repo) AutoDiff() (*Diff, error) {
	staged, err := r.StagedDiff()
	if err != nil {
		return nil, err
	}
	if len(staged.Files) > 0 {
		return staged, nil
	}

	return r.UnstagedDiff()
}

// HasStagedChanges reports whether there are staged changes.
func (r *Repo) HasStagedChanges() (bool, error) {
	output, err := r.git("diff", "--cached", "--name-only")
	if err != nil {
		return false, err
	}
	return strings.TrimSpace(output) != "", nil
}

// CurrentBranch returns the name of the current branch.
func (r *Repo) CurrentBranch() (string, error) {
	output, err := r.git("rev-parse", "--abbrev-ref", "HEAD")
	if err != nil {
		return "", fmt.Errorf("getting current branch: %w", err)
	}
	return strings.TrimSpace(output), nil
}

// RepoRoot returns the root directory of the git repository.
func (r *Repo) RepoRoot() (string, error) {
	output, err := r.git("rev-parse", "--show-toplevel")
	if err != nil {
		return "", fmt.Errorf("getting repo root: %w", err)
	}
	return strings.TrimSpace(output), nil
}

// FileContent returns the content of a file at a given ref (e.g., "HEAD", "HEAD~1").
// If ref is empty, returns the working tree version.
func (r *Repo) FileContent(path string, ref string) ([]byte, error) {
	if ref == "" {
		absPath := filepath.Join(r.Path, path)
		cmd := exec.Command("cat", absPath)
		out, err := cmd.Output()
		if err != nil {
			return nil, fmt.Errorf("reading file %s: %w", path, err)
		}
		return out, nil
	}

	output, err := r.git("show", ref+":"+path)
	if err != nil {
		return nil, fmt.Errorf("getting file %s at %s: %w", path, ref, err)
	}
	return []byte(output), nil
}

// git executes a git command in the repo and returns stdout.
func (r *Repo) git(args ...string) (string, error) {
	cmdArgs := append([]string{"-C", r.Path}, args...)
	cmd := exec.Command("git", cmdArgs...)
	out, err := cmd.CombinedOutput()
	if err != nil {
		return "", fmt.Errorf("git %s: %s (%w)", strings.Join(args, " "), string(out), err)
	}
	return string(out), nil
}
