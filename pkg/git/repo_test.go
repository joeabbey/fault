package git

import (
	"os"
	"os/exec"
	"path/filepath"
	"testing"
)

// setupTestRepo creates a temporary git repo with an initial commit.
func setupTestRepo(t *testing.T) string {
	t.Helper()
	dir := t.TempDir()

	cmds := [][]string{
		{"git", "init"},
		{"git", "config", "user.email", "test@test.com"},
		{"git", "config", "user.name", "Test"},
	}
	for _, args := range cmds {
		cmd := exec.Command(args[0], args[1:]...)
		cmd.Dir = dir
		if out, err := cmd.CombinedOutput(); err != nil {
			t.Fatalf("setup %v: %s (%v)", args, out, err)
		}
	}

	// Create initial commit
	err := os.WriteFile(filepath.Join(dir, "README.md"), []byte("# Test\n"), 0644)
	if err != nil {
		t.Fatal(err)
	}
	for _, args := range [][]string{
		{"git", "add", "README.md"},
		{"git", "commit", "-m", "initial"},
	} {
		cmd := exec.Command(args[0], args[1:]...)
		cmd.Dir = dir
		if out, err := cmd.CombinedOutput(); err != nil {
			t.Fatalf("setup %v: %s (%v)", args, out, err)
		}
	}

	return dir
}

func TestNewRepo(t *testing.T) {
	dir := setupTestRepo(t)
	repo, err := NewRepo(dir)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if repo.Path != dir {
		t.Errorf("expected path %s, got %s", dir, repo.Path)
	}
}

func TestNewRepoNotGit(t *testing.T) {
	dir := t.TempDir()
	_, err := NewRepo(dir)
	if err == nil {
		t.Fatal("expected error for non-git directory")
	}
}

func TestStagedDiff(t *testing.T) {
	dir := setupTestRepo(t)
	repo, err := NewRepo(dir)
	if err != nil {
		t.Fatal(err)
	}

	// Create a new file and stage it
	err = os.WriteFile(filepath.Join(dir, "new.go"), []byte("package main\n\nfunc New() {}\n"), 0644)
	if err != nil {
		t.Fatal(err)
	}
	cmd := exec.Command("git", "add", "new.go")
	cmd.Dir = dir
	if out, err := cmd.CombinedOutput(); err != nil {
		t.Fatalf("git add: %s (%v)", out, err)
	}

	diff, err := repo.StagedDiff()
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	if diff.Mode != "staged" {
		t.Errorf("expected mode staged, got %q", diff.Mode)
	}
	if len(diff.Files) != 1 {
		t.Fatalf("expected 1 file, got %d", len(diff.Files))
	}
	if diff.Files[0].Path != "new.go" {
		t.Errorf("expected path new.go, got %q", diff.Files[0].Path)
	}
	if diff.Files[0].Status != "added" {
		t.Errorf("expected status added, got %q", diff.Files[0].Status)
	}
}

func TestUnstagedDiff(t *testing.T) {
	dir := setupTestRepo(t)
	repo, err := NewRepo(dir)
	if err != nil {
		t.Fatal(err)
	}

	// Modify tracked file without staging
	err = os.WriteFile(filepath.Join(dir, "README.md"), []byte("# Modified\nNew content\n"), 0644)
	if err != nil {
		t.Fatal(err)
	}

	diff, err := repo.UnstagedDiff()
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	if diff.Mode != "unstaged" {
		t.Errorf("expected mode unstaged, got %q", diff.Mode)
	}
	if len(diff.Files) != 1 {
		t.Fatalf("expected 1 file, got %d", len(diff.Files))
	}
}

func TestAutoDiff(t *testing.T) {
	dir := setupTestRepo(t)
	repo, err := NewRepo(dir)
	if err != nil {
		t.Fatal(err)
	}

	// With staged changes, should return staged
	err = os.WriteFile(filepath.Join(dir, "staged.go"), []byte("package main\n"), 0644)
	if err != nil {
		t.Fatal(err)
	}
	cmd := exec.Command("git", "add", "staged.go")
	cmd.Dir = dir
	if out, err := cmd.CombinedOutput(); err != nil {
		t.Fatalf("git add: %s (%v)", out, err)
	}

	diff, err := repo.AutoDiff()
	if err != nil {
		t.Fatal(err)
	}
	if diff.Mode != "staged" {
		t.Errorf("expected mode staged, got %q", diff.Mode)
	}
}

func TestCurrentBranch(t *testing.T) {
	dir := setupTestRepo(t)
	repo, err := NewRepo(dir)
	if err != nil {
		t.Fatal(err)
	}

	branch, err := repo.CurrentBranch()
	if err != nil {
		t.Fatal(err)
	}
	// Default branch could be master or main
	if branch != "master" && branch != "main" {
		t.Errorf("expected master or main, got %q", branch)
	}
}
