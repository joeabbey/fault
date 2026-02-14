package analyzer

import (
	"os"
	"path/filepath"
	"testing"

	"github.com/joeabbey/fault/pkg/config"
	"github.com/joeabbey/fault/pkg/git"
	"github.com/joeabbey/fault/pkg/parser"
)

func TestTestImpactAnalyzerName(t *testing.T) {
	a := NewTestImpactAnalyzer()
	if a.Name() != "tests" {
		t.Errorf("expected name 'tests', got %q", a.Name())
	}
}

func TestTestImpactAnalyzerNewFileNoTest(t *testing.T) {
	a := NewTestImpactAnalyzer()
	ctx := &AnalysisContext{
		RepoPath:    "/nonexistent",
		Diff:        &git.Diff{Files: []git.FileDiff{{Path: "pkg/service.go", Status: "added"}}},
		ParsedFiles: make(map[string]*parser.ParsedFile),
		Config:      config.DefaultConfig(),
	}

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(issues) != 1 {
		t.Fatalf("expected 1 issue, got %d", len(issues))
	}
	if issues[0].ID != "tests/new-file-no-test" {
		t.Errorf("expected issue ID 'tests/new-file-no-test', got %q", issues[0].ID)
	}
	if issues[0].Severity != SeverityWarning {
		t.Errorf("expected warning severity, got %q", issues[0].Severity)
	}
}

func TestTestImpactAnalyzerModifiedFileNoTest(t *testing.T) {
	a := NewTestImpactAnalyzer()
	ctx := &AnalysisContext{
		RepoPath:    "/nonexistent",
		Diff:        &git.Diff{Files: []git.FileDiff{{Path: "pkg/service.go", Status: "modified"}}},
		ParsedFiles: make(map[string]*parser.ParsedFile),
		Config:      config.DefaultConfig(),
	}

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(issues) != 1 {
		t.Fatalf("expected 1 issue, got %d", len(issues))
	}
	if issues[0].ID != "tests/changed-no-test" {
		t.Errorf("expected issue ID 'tests/changed-no-test', got %q", issues[0].ID)
	}
}

func TestTestImpactAnalyzerSourceAndTestBothChanged(t *testing.T) {
	a := NewTestImpactAnalyzer()
	ctx := &AnalysisContext{
		RepoPath: "/nonexistent",
		Diff: &git.Diff{Files: []git.FileDiff{
			{Path: "pkg/service.go", Status: "modified"},
			{Path: "pkg/service_test.go", Status: "modified"},
		}},
		ParsedFiles: make(map[string]*parser.ParsedFile),
		Config:      config.DefaultConfig(),
	}

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	// Source has test in the diff, test file has source in the diff.
	// Neither should produce a warning.
	for _, issue := range issues {
		if issue.ID == "tests/new-file-no-test" || issue.ID == "tests/changed-no-test" {
			t.Errorf("unexpected missing-test issue for %s", issue.File)
		}
	}
}

func TestTestImpactAnalyzerTestOnlyChange(t *testing.T) {
	// Create a temp dir with a source file so fileExistsOnDisk works.
	tmpDir := t.TempDir()
	if err := os.MkdirAll(filepath.Join(tmpDir, "pkg"), 0755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(tmpDir, "pkg", "service.go"), []byte("package pkg"), 0644); err != nil {
		t.Fatal(err)
	}

	a := NewTestImpactAnalyzer()
	ctx := &AnalysisContext{
		RepoPath: tmpDir,
		Diff: &git.Diff{Files: []git.FileDiff{
			{Path: "pkg/service_test.go", Status: "modified"},
		}},
		ParsedFiles: make(map[string]*parser.ParsedFile),
		Config:      config.DefaultConfig(),
	}

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(issues) != 1 {
		t.Fatalf("expected 1 issue, got %d", len(issues))
	}
	if issues[0].ID != "tests/test-only-change" {
		t.Errorf("expected 'tests/test-only-change', got %q", issues[0].ID)
	}
	if issues[0].Severity != SeverityInfo {
		t.Errorf("expected info severity, got %q", issues[0].Severity)
	}
}

func TestTestImpactAnalyzerDeletedFileSkipped(t *testing.T) {
	a := NewTestImpactAnalyzer()
	ctx := &AnalysisContext{
		RepoPath:    "/nonexistent",
		Diff:        &git.Diff{Files: []git.FileDiff{{Path: "pkg/old.go", Status: "deleted"}}},
		ParsedFiles: make(map[string]*parser.ParsedFile),
		Config:      config.DefaultConfig(),
	}

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(issues) != 0 {
		t.Errorf("expected 0 issues for deleted file, got %d", len(issues))
	}
}

func TestTestImpactAnalyzerNonSourceFileSkipped(t *testing.T) {
	a := NewTestImpactAnalyzer()
	ctx := &AnalysisContext{
		RepoPath: "/nonexistent",
		Diff: &git.Diff{Files: []git.FileDiff{
			{Path: "README.md", Status: "modified"},
			{Path: ".gitignore", Status: "modified"},
			{Path: "Makefile", Status: "modified"},
		}},
		ParsedFiles: make(map[string]*parser.ParsedFile),
		Config:      config.DefaultConfig(),
	}

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(issues) != 0 {
		t.Errorf("expected 0 issues for non-source files, got %d", len(issues))
	}
}

func TestTestImpactAnalyzerSourceWithExistingTest(t *testing.T) {
	// Create a temp dir with a test file on disk.
	tmpDir := t.TempDir()
	if err := os.MkdirAll(filepath.Join(tmpDir, "pkg"), 0755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(tmpDir, "pkg", "service_test.go"), []byte("package pkg"), 0644); err != nil {
		t.Fatal(err)
	}

	a := NewTestImpactAnalyzer()
	ctx := &AnalysisContext{
		RepoPath: tmpDir,
		Diff: &git.Diff{Files: []git.FileDiff{
			{Path: "pkg/service.go", Status: "modified"},
		}},
		ParsedFiles: make(map[string]*parser.ParsedFile),
		Config:      config.DefaultConfig(),
	}

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	// Test file exists on disk, so no issue.
	if len(issues) != 0 {
		t.Errorf("expected 0 issues (test exists on disk), got %d: %+v", len(issues), issues)
	}
}

func TestTestImpactAnalyzerEmptyDiff(t *testing.T) {
	a := NewTestImpactAnalyzer()
	ctx := &AnalysisContext{
		RepoPath:    "/nonexistent",
		Diff:        &git.Diff{Files: make([]git.FileDiff, 0)},
		ParsedFiles: make(map[string]*parser.ParsedFile),
		Config:      config.DefaultConfig(),
	}

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(issues) != 0 {
		t.Errorf("expected 0 issues for empty diff, got %d", len(issues))
	}
}

func TestTestImpactAnalyzerNilDiff(t *testing.T) {
	a := NewTestImpactAnalyzer()
	ctx := &AnalysisContext{
		RepoPath:    "/nonexistent",
		Diff:        nil,
		ParsedFiles: make(map[string]*parser.ParsedFile),
		Config:      config.DefaultConfig(),
	}

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(issues) != 0 {
		t.Errorf("expected 0 issues for nil diff, got %d", len(issues))
	}
}

// --- Test file detection helpers ---

func TestIsTestFile(t *testing.T) {
	tests := []struct {
		path string
		want bool
	}{
		// Go
		{"pkg/service_test.go", true},
		{"pkg/service.go", false},
		// TypeScript/JavaScript
		{"src/app.test.ts", true},
		{"src/app.spec.ts", true},
		{"src/__tests__/app.test.ts", true},
		{"src/app.ts", false},
		{"src/app.test.js", true},
		{"src/app.spec.jsx", true},
		// Python
		{"test_app.py", true},
		{"app.py", false},
		{"tests/test_utils.py", true},
	}

	for _, tt := range tests {
		got := isTestFile(tt.path)
		if got != tt.want {
			t.Errorf("isTestFile(%q) = %v, want %v", tt.path, got, tt.want)
		}
	}
}

func TestPossibleTestPaths(t *testing.T) {
	tests := []struct {
		source   string
		contains string
	}{
		{"pkg/service.go", "pkg/service_test.go"},
		{"src/app.ts", "src/app.test.ts"},
		{"src/app.ts", "src/app.spec.ts"},
		{"src/app.ts", "src/__tests__/app.test.ts"},
		{"lib/utils.py", "lib/test_utils.py"},
		{"lib/utils.py", "lib/tests/test_utils.py"},
	}

	for _, tt := range tests {
		paths := possibleTestPaths(tt.source)
		found := false
		for _, p := range paths {
			if p == tt.contains {
				found = true
				break
			}
		}
		if !found {
			t.Errorf("possibleTestPaths(%q) does not contain %q; got %v", tt.source, tt.contains, paths)
		}
	}
}

func TestSourcePathFromTest(t *testing.T) {
	tests := []struct {
		test string
		want string
	}{
		{"pkg/service_test.go", "pkg/service.go"},
		{"src/app.test.ts", "src/app.ts"},
		{"src/app.spec.ts", "src/app.ts"},
		{"src/__tests__/app.test.ts", "src/app.ts"},
		{"lib/test_utils.py", "lib/utils.py"},
		{"lib/tests/test_utils.py", "lib/utils.py"},
	}

	for _, tt := range tests {
		got := sourcePathFromTest(tt.test)
		if got != tt.want {
			t.Errorf("sourcePathFromTest(%q) = %q, want %q", tt.test, got, tt.want)
		}
	}
}

func TestTestImpactAnalyzerTSNewFileNoTest(t *testing.T) {
	a := NewTestImpactAnalyzer()
	ctx := &AnalysisContext{
		RepoPath:    "/nonexistent",
		Diff:        &git.Diff{Files: []git.FileDiff{{Path: "src/utils.ts", Status: "added"}}},
		ParsedFiles: make(map[string]*parser.ParsedFile),
		Config:      config.DefaultConfig(),
	}

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(issues) != 1 {
		t.Fatalf("expected 1 issue, got %d", len(issues))
	}
	if issues[0].ID != "tests/new-file-no-test" {
		t.Errorf("expected 'tests/new-file-no-test', got %q", issues[0].ID)
	}
}

func TestTestImpactAnalyzerPythonNewFileNoTest(t *testing.T) {
	a := NewTestImpactAnalyzer()
	ctx := &AnalysisContext{
		RepoPath:    "/nonexistent",
		Diff:        &git.Diff{Files: []git.FileDiff{{Path: "lib/helpers.py", Status: "added"}}},
		ParsedFiles: make(map[string]*parser.ParsedFile),
		Config:      config.DefaultConfig(),
	}

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(issues) != 1 {
		t.Fatalf("expected 1 issue, got %d", len(issues))
	}
	if issues[0].ID != "tests/new-file-no-test" {
		t.Errorf("expected 'tests/new-file-no-test', got %q", issues[0].ID)
	}
}

func TestTestImpactAnalyzerTSWithTestInDiff(t *testing.T) {
	a := NewTestImpactAnalyzer()
	ctx := &AnalysisContext{
		RepoPath: "/nonexistent",
		Diff: &git.Diff{Files: []git.FileDiff{
			{Path: "src/utils.ts", Status: "modified"},
			{Path: "src/utils.test.ts", Status: "modified"},
		}},
		ParsedFiles: make(map[string]*parser.ParsedFile),
		Config:      config.DefaultConfig(),
	}

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	// Source has matching test in the diff. No missing-test warnings.
	for _, issue := range issues {
		if issue.ID == "tests/new-file-no-test" || issue.ID == "tests/changed-no-test" {
			t.Errorf("unexpected missing-test issue: %+v", issue)
		}
	}
}
