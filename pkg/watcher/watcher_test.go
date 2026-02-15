package watcher

import (
	"context"
	"os"
	"os/exec"
	"path/filepath"
	"sync"
	"testing"
	"time"

	"github.com/joeabbey/fault/pkg/analyzer"
	"github.com/joeabbey/fault/pkg/config"
	"github.com/joeabbey/fault/pkg/git"
	"github.com/joeabbey/fault/pkg/index"
	"github.com/joeabbey/fault/pkg/parser"
)

// setupTestRepo creates a temp git repo with a Go file and returns the path
// and cleanup function.
func setupTestRepo(t *testing.T) string {
	t.Helper()
	dir := t.TempDir()

	// git init
	cmd := exec.Command("git", "init", dir)
	cmd.Env = append(os.Environ(),
		"GIT_CONFIG_GLOBAL=/dev/null",
		"GIT_CONFIG_SYSTEM=/dev/null",
	)
	if out, err := cmd.CombinedOutput(); err != nil {
		t.Fatalf("git init failed: %v\n%s", err, out)
	}

	// Configure git user
	for _, args := range [][]string{
		{"config", "user.email", "test@example.com"},
		{"config", "user.name", "Test"},
	} {
		cmd = exec.Command("git", append([]string{"-C", dir}, args...)...)
		if out, err := cmd.CombinedOutput(); err != nil {
			t.Fatalf("git config failed: %v\n%s", err, out)
		}
	}

	// Create a Go source file and commit it
	goFile := filepath.Join(dir, "main.go")
	if err := os.WriteFile(goFile, []byte("package main\n\nfunc main() {}\n"), 0644); err != nil {
		t.Fatal(err)
	}

	cmd = exec.Command("git", "-C", dir, "add", ".")
	if out, err := cmd.CombinedOutput(); err != nil {
		t.Fatalf("git add failed: %v\n%s", err, out)
	}

	cmd = exec.Command("git", "-C", dir, "commit", "-m", "initial")
	if out, err := cmd.CombinedOutput(); err != nil {
		t.Fatalf("git commit failed: %v\n%s", err, out)
	}

	return dir
}

func newTestConfig() *config.Config {
	return &config.Config{
		Version:   1,
		Languages: []string{"go", "typescript", "python"},
		BlockOn:   "error",
		Analyzers: config.AnalyzersConfig{
			Imports:       true,
			Consistency:   true,
			References:    true,
			Tests:         true,
			Patterns:      true,
			Security:      true,
			Hallucination: true,
		},
		Ignore: []string{"vendor/", "node_modules/"},
		Watch: config.WatchConfig{
			PollInterval: "50ms",
			Debounce:     "50ms",
		},
	}
}

func TestWatcherDetectsChange(t *testing.T) {
	dir := setupTestRepo(t)

	repo, err := git.NewRepo(dir)
	if err != nil {
		t.Fatal(err)
	}

	cfg := newTestConfig()
	reg := parser.NewRegistry()
	reg.Register(parser.NewGoParser())

	analyzers := []analyzer.Analyzer{
		analyzer.NewImportAnalyzer(),
	}
	idx := index.NewIndex(dir, cfg)

	var mu sync.Mutex
	results := make([]RunResult, 0)

	onChange := func(r RunResult) {
		mu.Lock()
		results = append(results, r)
		mu.Unlock()
	}

	opts := Options{
		PollInterval: 50 * time.Millisecond,
		Debounce:     50 * time.Millisecond,
	}

	w := New(dir, repo, cfg, reg, analyzers, idx, opts, onChange, nil)

	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	go w.Watch(ctx)

	// Wait for initial run
	time.Sleep(200 * time.Millisecond)

	// Modify a file (create unstaged changes)
	goFile := filepath.Join(dir, "main.go")
	if err := os.WriteFile(goFile, []byte("package main\n\nimport \"fmt\"\n\nfunc main() {\n\tfmt.Println(\"hello\")\n}\n"), 0644); err != nil {
		t.Fatal(err)
	}

	// Wait for the watcher to detect and analyze
	time.Sleep(500 * time.Millisecond)
	cancel()

	mu.Lock()
	defer mu.Unlock()

	// Should have at least 2 runs: initial + after change
	if len(results) < 2 {
		t.Errorf("expected at least 2 analysis runs, got %d", len(results))
	}
}

func TestWatcherDebounce(t *testing.T) {
	dir := setupTestRepo(t)

	repo, err := git.NewRepo(dir)
	if err != nil {
		t.Fatal(err)
	}

	cfg := newTestConfig()
	reg := parser.NewRegistry()
	reg.Register(parser.NewGoParser())

	analyzers := []analyzer.Analyzer{
		analyzer.NewImportAnalyzer(),
	}
	idx := index.NewIndex(dir, cfg)

	var mu sync.Mutex
	runCount := 0

	onChange := func(r RunResult) {
		mu.Lock()
		runCount++
		mu.Unlock()
	}

	opts := Options{
		PollInterval: 50 * time.Millisecond,
		Debounce:     300 * time.Millisecond, // longer debounce
	}

	w := New(dir, repo, cfg, reg, analyzers, idx, opts, onChange, nil)

	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	go w.Watch(ctx)

	// Wait for initial run
	time.Sleep(200 * time.Millisecond)

	mu.Lock()
	initialRuns := runCount
	mu.Unlock()

	// Make rapid changes within the debounce window
	goFile := filepath.Join(dir, "main.go")
	for i := 0; i < 5; i++ {
		content := []byte("package main\n\n// change " + string(rune('A'+i)) + "\nfunc main() {}\n")
		if err := os.WriteFile(goFile, content, 0644); err != nil {
			t.Fatal(err)
		}
		time.Sleep(50 * time.Millisecond)
	}

	// Wait for debounce to complete plus some buffer
	time.Sleep(600 * time.Millisecond)
	cancel()

	mu.Lock()
	finalRuns := runCount
	mu.Unlock()

	// Should have initial run + only 1 debounced run (not 5 separate runs)
	runsAfterChanges := finalRuns - initialRuns
	if runsAfterChanges > 2 {
		t.Errorf("expected at most 2 runs after rapid changes (debounce should coalesce), got %d", runsAfterChanges)
	}
}

func TestWatcherIgnoresIgnoredPaths(t *testing.T) {
	dir := setupTestRepo(t)

	// Create a vendor directory with a Go file
	vendorDir := filepath.Join(dir, "vendor")
	if err := os.MkdirAll(vendorDir, 0755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(vendorDir, "lib.go"), []byte("package lib\n"), 0644); err != nil {
		t.Fatal(err)
	}

	repo, err := git.NewRepo(dir)
	if err != nil {
		t.Fatal(err)
	}

	cfg := newTestConfig()
	reg := parser.NewRegistry()
	reg.Register(parser.NewGoParser())

	analyzers := []analyzer.Analyzer{
		analyzer.NewImportAnalyzer(),
	}
	idx := index.NewIndex(dir, cfg)

	var mu sync.Mutex
	runCount := 0

	onChange := func(r RunResult) {
		mu.Lock()
		runCount++
		mu.Unlock()
	}

	opts := Options{
		PollInterval: 50 * time.Millisecond,
		Debounce:     50 * time.Millisecond,
	}

	w := New(dir, repo, cfg, reg, analyzers, idx, opts, onChange, nil)

	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	go w.Watch(ctx)

	// Wait for initial run
	time.Sleep(200 * time.Millisecond)

	mu.Lock()
	initialRuns := runCount
	mu.Unlock()

	// Modify vendor file only
	if err := os.WriteFile(filepath.Join(vendorDir, "lib.go"), []byte("package lib\n// changed\n"), 0644); err != nil {
		t.Fatal(err)
	}

	// Wait for potential detection
	time.Sleep(300 * time.Millisecond)
	cancel()

	mu.Lock()
	finalRuns := runCount
	mu.Unlock()

	if finalRuns > initialRuns {
		t.Errorf("expected no additional runs from vendor changes, got %d extra", finalRuns-initialRuns)
	}
}

func TestWatcherSkipsNonSource(t *testing.T) {
	dir := setupTestRepo(t)

	repo, err := git.NewRepo(dir)
	if err != nil {
		t.Fatal(err)
	}

	cfg := newTestConfig()
	reg := parser.NewRegistry()
	reg.Register(parser.NewGoParser())

	analyzers := []analyzer.Analyzer{
		analyzer.NewImportAnalyzer(),
	}
	idx := index.NewIndex(dir, cfg)

	var mu sync.Mutex
	runCount := 0

	onChange := func(r RunResult) {
		mu.Lock()
		runCount++
		mu.Unlock()
	}

	opts := Options{
		PollInterval: 50 * time.Millisecond,
		Debounce:     50 * time.Millisecond,
	}

	w := New(dir, repo, cfg, reg, analyzers, idx, opts, onChange, nil)

	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	go w.Watch(ctx)

	// Wait for initial run
	time.Sleep(200 * time.Millisecond)

	mu.Lock()
	initialRuns := runCount
	mu.Unlock()

	// Create/modify non-source files
	if err := os.WriteFile(filepath.Join(dir, "README.md"), []byte("# Hello\n"), 0644); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(dir, "data.txt"), []byte("some data\n"), 0644); err != nil {
		t.Fatal(err)
	}

	// Wait for potential detection
	time.Sleep(300 * time.Millisecond)
	cancel()

	mu.Lock()
	finalRuns := runCount
	mu.Unlock()

	if finalRuns > initialRuns {
		t.Errorf("expected no additional runs from non-source file changes, got %d extra", finalRuns-initialRuns)
	}
}

func TestWatcherContextCancel(t *testing.T) {
	dir := setupTestRepo(t)

	repo, err := git.NewRepo(dir)
	if err != nil {
		t.Fatal(err)
	}

	cfg := newTestConfig()
	reg := parser.NewRegistry()
	reg.Register(parser.NewGoParser())

	analyzers := []analyzer.Analyzer{
		analyzer.NewImportAnalyzer(),
	}
	idx := index.NewIndex(dir, cfg)

	opts := Options{
		PollInterval: 50 * time.Millisecond,
		Debounce:     50 * time.Millisecond,
	}

	w := New(dir, repo, cfg, reg, analyzers, idx, opts, func(RunResult) {}, nil)

	ctx, cancel := context.WithCancel(context.Background())

	done := make(chan error, 1)
	go func() {
		done <- w.Watch(ctx)
	}()

	// Let it run briefly
	time.Sleep(100 * time.Millisecond)
	cancel()

	select {
	case err := <-done:
		if err != nil {
			t.Errorf("expected nil error on cancel, got %v", err)
		}
	case <-time.After(2 * time.Second):
		t.Fatal("watcher did not stop within 2 seconds after cancel")
	}
}

func TestWatcherFirstRun(t *testing.T) {
	dir := setupTestRepo(t)

	// Create an unstaged change
	goFile := filepath.Join(dir, "main.go")
	if err := os.WriteFile(goFile, []byte("package main\n\nimport \"os\"\n\nfunc main() { os.Exit(0) }\n"), 0644); err != nil {
		t.Fatal(err)
	}

	repo, err := git.NewRepo(dir)
	if err != nil {
		t.Fatal(err)
	}

	cfg := newTestConfig()
	reg := parser.NewRegistry()
	reg.Register(parser.NewGoParser())

	analyzers := []analyzer.Analyzer{
		analyzer.NewImportAnalyzer(),
	}
	idx := index.NewIndex(dir, cfg)

	var mu sync.Mutex
	results := make([]RunResult, 0)

	onChange := func(r RunResult) {
		mu.Lock()
		results = append(results, r)
		mu.Unlock()
	}

	opts := Options{
		PollInterval: 50 * time.Millisecond,
		Debounce:     50 * time.Millisecond,
	}

	w := New(dir, repo, cfg, reg, analyzers, idx, opts, onChange, nil)

	ctx, cancel := context.WithCancel(context.Background())

	go w.Watch(ctx)

	// Wait for initial analysis to complete
	time.Sleep(200 * time.Millisecond)
	cancel()

	mu.Lock()
	defer mu.Unlock()

	// Should have at least 1 run (the initial run)
	if len(results) < 1 {
		t.Error("expected at least 1 initial analysis run")
	}
}
