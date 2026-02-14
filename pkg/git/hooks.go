package git

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"
)

const (
	// hookMarker identifies hooks installed by fault.
	hookMarker = "Installed by fault"

	// hookScript is the pre-commit hook content installed by fault.
	hookScript = `#!/bin/sh
# Installed by fault - pre-commit validation for AI agent output
# To bypass: git commit --no-verify

fault check --staged
exit $?
`
)

// InstallHook writes a pre-commit hook to the repository's .git/hooks directory.
// If a pre-commit hook already exists and was not installed by fault, it returns
// an error unless force is true.
func InstallHook(repoPath string, force bool) error {
	hookPath, err := hookFilePath(repoPath)
	if err != nil {
		return err
	}

	// Check for existing hook
	if content, err := os.ReadFile(hookPath); err == nil {
		if !isOurHook(string(content)) && !force {
			return fmt.Errorf(
				"pre-commit hook already exists and was not installed by fault; use --force to overwrite",
			)
		}
	}

	// Ensure hooks directory exists
	hookDir := filepath.Dir(hookPath)
	if err := os.MkdirAll(hookDir, 0755); err != nil {
		return fmt.Errorf("creating hooks directory: %w", err)
	}

	if err := os.WriteFile(hookPath, []byte(hookScript), 0755); err != nil {
		return fmt.Errorf("writing pre-commit hook: %w", err)
	}

	return nil
}

// UninstallHook removes the pre-commit hook if it was installed by fault.
// Returns an error if the hook exists but was not installed by fault.
func UninstallHook(repoPath string) error {
	hookPath, err := hookFilePath(repoPath)
	if err != nil {
		return err
	}

	content, err := os.ReadFile(hookPath)
	if err != nil {
		if os.IsNotExist(err) {
			return fmt.Errorf("no pre-commit hook is installed")
		}
		return fmt.Errorf("reading pre-commit hook: %w", err)
	}

	if !isOurHook(string(content)) {
		return fmt.Errorf("pre-commit hook was not installed by fault; refusing to remove")
	}

	if err := os.Remove(hookPath); err != nil {
		return fmt.Errorf("removing pre-commit hook: %w", err)
	}

	return nil
}

// IsHookInstalled reports whether a fault-managed pre-commit hook is installed.
func IsHookInstalled(repoPath string) bool {
	hookPath, err := hookFilePath(repoPath)
	if err != nil {
		return false
	}

	content, err := os.ReadFile(hookPath)
	if err != nil {
		return false
	}

	return isOurHook(string(content))
}

// hookFilePath returns the path to .git/hooks/pre-commit for the given repo.
func hookFilePath(repoPath string) (string, error) {
	gitDir := filepath.Join(repoPath, ".git")
	info, err := os.Stat(gitDir)
	if err != nil {
		return "", fmt.Errorf("not a git repository: %s", repoPath)
	}

	// Handle worktrees: .git may be a file pointing to the actual git dir
	if !info.IsDir() {
		data, err := os.ReadFile(gitDir)
		if err != nil {
			return "", fmt.Errorf("reading .git file: %w", err)
		}
		line := strings.TrimSpace(string(data))
		if !strings.HasPrefix(line, "gitdir: ") {
			return "", fmt.Errorf("unexpected .git file content: %s", line)
		}
		gitDir = strings.TrimPrefix(line, "gitdir: ")
		if !filepath.IsAbs(gitDir) {
			gitDir = filepath.Join(repoPath, gitDir)
		}
	}

	return filepath.Join(gitDir, "hooks", "pre-commit"), nil
}

// isOurHook checks if hook content was installed by fault.
func isOurHook(content string) bool {
	return strings.Contains(content, hookMarker)
}
