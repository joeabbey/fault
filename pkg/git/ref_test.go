package git

import (
	"os"
	"os/exec"
	"path/filepath"
	"testing"
)

func TestHeadSHA(t *testing.T) {
	dir := setupTestRepo(t)
	repo, err := NewRepo(dir)
	if err != nil {
		t.Fatal(err)
	}

	sha, err := repo.HeadSHA()
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(sha) != 40 {
		t.Errorf("expected 40-char SHA, got %d chars: %q", len(sha), sha)
	}
}

func TestCommitLog(t *testing.T) {
	dir := setupTestRepo(t)
	repo, err := NewRepo(dir)
	if err != nil {
		t.Fatal(err)
	}

	// Get the initial commit SHA to use as base
	initialSHA, err := repo.HeadSHA()
	if err != nil {
		t.Fatal(err)
	}

	// Create a second commit
	err = os.WriteFile(filepath.Join(dir, "second.txt"), []byte("second\n"), 0644)
	if err != nil {
		t.Fatal(err)
	}
	for _, args := range [][]string{
		{"git", "add", "second.txt"},
		{"git", "commit", "-m", "add second file"},
	} {
		cmd := exec.Command(args[0], args[1:]...)
		cmd.Dir = dir
		if out, err := cmd.CombinedOutput(); err != nil {
			t.Fatalf("%v: %s (%v)", args, out, err)
		}
	}

	entries, err := repo.CommitLog(initialSHA)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(entries) != 1 {
		t.Fatalf("expected 1 commit entry, got %d", len(entries))
	}
	if entries[0].Subject != "add second file" {
		t.Errorf("expected subject 'add second file', got %q", entries[0].Subject)
	}
	if entries[0].Author != "Test" {
		t.Errorf("expected author 'Test', got %q", entries[0].Author)
	}
}

func TestRefDiff(t *testing.T) {
	dir := setupTestRepo(t)
	repo, err := NewRepo(dir)
	if err != nil {
		t.Fatal(err)
	}

	initialSHA, err := repo.HeadSHA()
	if err != nil {
		t.Fatal(err)
	}

	// Create a second commit
	err = os.WriteFile(filepath.Join(dir, "new.go"), []byte("package main\n"), 0644)
	if err != nil {
		t.Fatal(err)
	}
	for _, args := range [][]string{
		{"git", "add", "new.go"},
		{"git", "commit", "-m", "add new.go"},
	} {
		cmd := exec.Command(args[0], args[1:]...)
		cmd.Dir = dir
		if out, err := cmd.CombinedOutput(); err != nil {
			t.Fatalf("%v: %s (%v)", args, out, err)
		}
	}

	headSHA, err := repo.HeadSHA()
	if err != nil {
		t.Fatal(err)
	}

	diff, err := repo.RefDiff(initialSHA, headSHA)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if diff.Mode != "ref" {
		t.Errorf("expected mode 'ref', got %q", diff.Mode)
	}
	if len(diff.Files) != 1 {
		t.Fatalf("expected 1 file, got %d", len(diff.Files))
	}
	if diff.Files[0].Path != "new.go" {
		t.Errorf("expected path 'new.go', got %q", diff.Files[0].Path)
	}
}

func TestRemoteURL(t *testing.T) {
	dir := setupTestRepo(t)
	repo, err := NewRepo(dir)
	if err != nil {
		t.Fatal(err)
	}

	// No remote configured — should return empty string
	url := repo.RemoteURL()
	if url != "" {
		t.Errorf("expected empty remote URL, got %q", url)
	}
}

func TestMergeBase(t *testing.T) {
	dir := setupTestRepo(t)
	repo, err := NewRepo(dir)
	if err != nil {
		t.Fatal(err)
	}

	initialSHA, err := repo.HeadSHA()
	if err != nil {
		t.Fatal(err)
	}

	// Create a second commit
	err = os.WriteFile(filepath.Join(dir, "other.txt"), []byte("other\n"), 0644)
	if err != nil {
		t.Fatal(err)
	}
	for _, args := range [][]string{
		{"git", "add", "other.txt"},
		{"git", "commit", "-m", "add other"},
	} {
		cmd := exec.Command(args[0], args[1:]...)
		cmd.Dir = dir
		if out, err := cmd.CombinedOutput(); err != nil {
			t.Fatalf("%v: %s (%v)", args, out, err)
		}
	}

	headSHA, err := repo.HeadSHA()
	if err != nil {
		t.Fatal(err)
	}

	base, err := repo.MergeBase(initialSHA, headSHA)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if base != initialSHA {
		t.Errorf("expected merge base %s, got %s", initialSHA, base)
	}
}
