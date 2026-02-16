package analyzer

import (
	"os"
	"path/filepath"
	"testing"

	"github.com/joeabbey/fault/pkg/config"
	"github.com/joeabbey/fault/pkg/git"
	"github.com/joeabbey/fault/pkg/index"
	"github.com/joeabbey/fault/pkg/parser"
)

func TestDepGraphAnalyzerName(t *testing.T) {
	a := NewDepGraphAnalyzer()
	if a.Name() != "depgraph" {
		t.Errorf("expected name 'depgraph', got %q", a.Name())
	}
}

func TestDepGraphNilIndex(t *testing.T) {
	a := NewDepGraphAnalyzer()
	ctx := &AnalysisContext{
		RepoPath:    "/nonexistent",
		Diff:        &git.Diff{Files: []git.FileDiff{{Path: "main.go", Status: "modified"}}},
		ParsedFiles: make(map[string]*parser.ParsedFile),
		Config:      config.DefaultConfig(),
		Index:       nil,
	}

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(issues) != 0 {
		t.Errorf("expected 0 issues when index is nil, got %d", len(issues))
	}
}

func TestDepGraphEmptyDiff(t *testing.T) {
	a := NewDepGraphAnalyzer()
	cfg := config.DefaultConfig()
	idx := index.NewIndex("/nonexistent", cfg)

	ctx := &AnalysisContext{
		RepoPath:    "/nonexistent",
		Diff:        &git.Diff{Files: make([]git.FileDiff, 0)},
		ParsedFiles: make(map[string]*parser.ParsedFile),
		Config:      cfg,
		Index:       idx,
	}

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(issues) != 0 {
		t.Errorf("expected 0 issues for empty diff, got %d", len(issues))
	}
}

// --- Circular dependency tests ---

func TestDepGraphCircularDependency(t *testing.T) {
	a := NewDepGraphAnalyzer()
	cfg := config.DefaultConfig()
	idx := index.NewIndex("/repo", cfg)

	// Create a cycle: a.ts -> b.ts -> c.ts -> a.ts
	idx.Files = map[string]*index.FileEntry{
		"src/a.ts": {
			Path:     "src/a.ts",
			Language: "typescript",
			Exports:  make([]parser.Export, 0),
			Imports:  []parser.Import{{Path: "./b", Line: 1}},
		},
		"src/b.ts": {
			Path:     "src/b.ts",
			Language: "typescript",
			Exports:  make([]parser.Export, 0),
			Imports:  []parser.Import{{Path: "./c", Line: 1}},
		},
		"src/c.ts": {
			Path:     "src/c.ts",
			Language: "typescript",
			Exports:  make([]parser.Export, 0),
			Imports:  []parser.Import{{Path: "./a", Line: 1}},
		},
	}

	ctx := &AnalysisContext{
		RepoPath: "/repo",
		Diff: &git.Diff{
			Files: []git.FileDiff{
				{Path: "src/a.ts", Status: "modified"},
			},
		},
		ParsedFiles: make(map[string]*parser.ParsedFile),
		Config:      cfg,
		Index:       idx,
	}

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "depgraph/circular-dependency")
	if found == nil {
		t.Fatal("expected circular-dependency issue for a -> b -> c -> a cycle")
	}
	if found.Severity != SeverityWarning {
		t.Errorf("expected warning severity, got %q", found.Severity)
	}
	if found.Category != "depgraph" {
		t.Errorf("expected category 'depgraph', got %q", found.Category)
	}
}

func TestDepGraphNoCircularDependency(t *testing.T) {
	a := NewDepGraphAnalyzer()
	cfg := config.DefaultConfig()
	idx := index.NewIndex("/repo", cfg)

	// Linear dependency: a.ts -> b.ts -> c.ts (no cycle)
	idx.Files = map[string]*index.FileEntry{
		"src/a.ts": {
			Path:     "src/a.ts",
			Language: "typescript",
			Exports:  make([]parser.Export, 0),
			Imports:  []parser.Import{{Path: "./b", Line: 1}},
		},
		"src/b.ts": {
			Path:     "src/b.ts",
			Language: "typescript",
			Exports:  make([]parser.Export, 0),
			Imports:  []parser.Import{{Path: "./c", Line: 1}},
		},
		"src/c.ts": {
			Path:     "src/c.ts",
			Language: "typescript",
			Exports:  make([]parser.Export, 0),
			Imports:  make([]parser.Import, 0),
		},
	}

	ctx := &AnalysisContext{
		RepoPath: "/repo",
		Diff: &git.Diff{
			Files: []git.FileDiff{
				{Path: "src/a.ts", Status: "modified"},
			},
		},
		ParsedFiles: make(map[string]*parser.ParsedFile),
		Config:      cfg,
		Index:       idx,
	}

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "depgraph/circular-dependency")
	if found != nil {
		t.Errorf("expected no circular-dependency issue for linear chain, got: %s", found.Message)
	}
}

func TestDepGraphCircularOnlyReportedForDiffFiles(t *testing.T) {
	a := NewDepGraphAnalyzer()
	cfg := config.DefaultConfig()
	idx := index.NewIndex("/repo", cfg)

	// Cycle: b.ts -> c.ts -> b.ts, but only d.ts is in the diff
	idx.Files = map[string]*index.FileEntry{
		"src/b.ts": {
			Path:     "src/b.ts",
			Language: "typescript",
			Exports:  make([]parser.Export, 0),
			Imports:  []parser.Import{{Path: "./c", Line: 1}},
		},
		"src/c.ts": {
			Path:     "src/c.ts",
			Language: "typescript",
			Exports:  make([]parser.Export, 0),
			Imports:  []parser.Import{{Path: "./b", Line: 1}},
		},
		"src/d.ts": {
			Path:     "src/d.ts",
			Language: "typescript",
			Exports:  make([]parser.Export, 0),
			Imports:  make([]parser.Import, 0),
		},
	}

	ctx := &AnalysisContext{
		RepoPath: "/repo",
		Diff: &git.Diff{
			Files: []git.FileDiff{
				{Path: "src/d.ts", Status: "modified"},
			},
		},
		ParsedFiles: make(map[string]*parser.ParsedFile),
		Config:      cfg,
		Index:       idx,
	}

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "depgraph/circular-dependency")
	if found != nil {
		t.Error("expected no circular-dependency issue when cycle doesn't involve diff files")
	}
}

func TestDepGraphVendorSkipped(t *testing.T) {
	a := NewDepGraphAnalyzer()
	cfg := config.DefaultConfig()
	idx := index.NewIndex("/repo", cfg)

	// Cycle in vendor — should be skipped.
	idx.Files = map[string]*index.FileEntry{
		"vendor/a.ts": {
			Path:     "vendor/a.ts",
			Language: "typescript",
			Exports:  make([]parser.Export, 0),
			Imports:  []parser.Import{{Path: "./b", Line: 1}},
		},
		"vendor/b.ts": {
			Path:     "vendor/b.ts",
			Language: "typescript",
			Exports:  make([]parser.Export, 0),
			Imports:  []parser.Import{{Path: "./a", Line: 1}},
		},
	}

	ctx := &AnalysisContext{
		RepoPath: "/repo",
		Diff: &git.Diff{
			Files: []git.FileDiff{
				{Path: "vendor/a.ts", Status: "modified"},
			},
		},
		ParsedFiles: make(map[string]*parser.ParsedFile),
		Config:      cfg,
		Index:       idx,
	}

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	if len(issues) != 0 {
		t.Errorf("expected no issues for vendor/ files, got %d", len(issues))
	}
}

// --- Unused dependency tests ---

func TestDepGraphUnusedNodeDep(t *testing.T) {
	a := NewDepGraphAnalyzer()
	cfg := config.DefaultConfig()

	// Create a temp dir with a package.json.
	tmpDir := t.TempDir()
	pkgJSON := `{
  "dependencies": {
    "express": "^4.0.0",
    "unused-pkg": "^1.0.0"
  }
}`
	if err := os.WriteFile(filepath.Join(tmpDir, "package.json"), []byte(pkgJSON), 0644); err != nil {
		t.Fatal(err)
	}

	idx := index.NewIndex(tmpDir, cfg)
	// Only express is imported.
	idx.Files = map[string]*index.FileEntry{
		"src/app.ts": {
			Path:     "src/app.ts",
			Language: "typescript",
			Exports:  make([]parser.Export, 0),
			Imports:  []parser.Import{{Path: "express", Line: 1}},
		},
	}

	ctx := &AnalysisContext{
		RepoPath: tmpDir,
		Diff: &git.Diff{
			Files: []git.FileDiff{
				{Path: "package.json", Status: "modified"},
			},
		},
		ParsedFiles: make(map[string]*parser.ParsedFile),
		Config:      cfg,
		Index:       idx,
	}

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	// Should find unused-pkg as unused.
	var unusedFound bool
	for _, issue := range issues {
		if issue.ID == "depgraph/unused-dependency" && issue.Message == "Dependency appears unused: unused-pkg" {
			unusedFound = true
			if issue.Severity != SeverityInfo {
				t.Errorf("expected info severity, got %q", issue.Severity)
			}
		}
	}
	if !unusedFound {
		t.Error("expected unused-dependency issue for unused-pkg")
	}

	// Express should NOT be flagged.
	for _, issue := range issues {
		if issue.ID == "depgraph/unused-dependency" && issue.Message == "Dependency appears unused: express" {
			t.Error("express should not be flagged as unused")
		}
	}
}

func TestDepGraphUnusedGoDep(t *testing.T) {
	a := NewDepGraphAnalyzer()
	cfg := config.DefaultConfig()

	tmpDir := t.TempDir()
	goMod := `module example.com/myapp

go 1.21

require (
	github.com/used/pkg v1.0.0
	github.com/unused/pkg v2.0.0
	github.com/indirect/pkg v1.0.0 // indirect
)
`
	if err := os.WriteFile(filepath.Join(tmpDir, "go.mod"), []byte(goMod), 0644); err != nil {
		t.Fatal(err)
	}

	idx := index.NewIndex(tmpDir, cfg)
	idx.Files = map[string]*index.FileEntry{
		"main.go": {
			Path:     "main.go",
			Language: "go",
			Exports:  make([]parser.Export, 0),
			Imports:  []parser.Import{{Path: "github.com/used/pkg", Line: 3}},
		},
	}

	ctx := &AnalysisContext{
		RepoPath: tmpDir,
		Diff: &git.Diff{
			Files: []git.FileDiff{
				{Path: "go.mod", Status: "modified"},
			},
		},
		ParsedFiles: make(map[string]*parser.ParsedFile),
		Config:      cfg,
		Index:       idx,
	}

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	// github.com/unused/pkg should be flagged.
	var unusedFound bool
	for _, issue := range issues {
		if issue.ID == "depgraph/unused-dependency" && issue.Message == "Dependency appears unused: github.com/unused/pkg" {
			unusedFound = true
		}
	}
	if !unusedFound {
		t.Error("expected unused-dependency issue for github.com/unused/pkg")
	}

	// github.com/used/pkg should NOT be flagged.
	for _, issue := range issues {
		if issue.ID == "depgraph/unused-dependency" && issue.Message == "Dependency appears unused: github.com/used/pkg" {
			t.Error("github.com/used/pkg should not be flagged as unused")
		}
	}

	// Indirect dependencies should NOT be flagged.
	for _, issue := range issues {
		if issue.ID == "depgraph/unused-dependency" && issue.Message == "Dependency appears unused: github.com/indirect/pkg" {
			t.Error("indirect dependency should not be flagged")
		}
	}
}

func TestDepGraphUnusedPythonDep(t *testing.T) {
	a := NewDepGraphAnalyzer()
	cfg := config.DefaultConfig()

	tmpDir := t.TempDir()
	reqTxt := `flask==2.0.0
unused-lib>=1.0
requests
`
	if err := os.WriteFile(filepath.Join(tmpDir, "requirements.txt"), []byte(reqTxt), 0644); err != nil {
		t.Fatal(err)
	}

	idx := index.NewIndex(tmpDir, cfg)
	idx.Files = map[string]*index.FileEntry{
		"app.py": {
			Path:     "app.py",
			Language: "python",
			Exports:  make([]parser.Export, 0),
			Imports: []parser.Import{
				{Path: "flask", Line: 1},
				{Path: "requests", Line: 2},
			},
		},
	}

	ctx := &AnalysisContext{
		RepoPath: tmpDir,
		Diff: &git.Diff{
			Files: []git.FileDiff{
				{Path: "requirements.txt", Status: "modified"},
			},
		},
		ParsedFiles: make(map[string]*parser.ParsedFile),
		Config:      cfg,
		Index:       idx,
	}

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	// unused-lib should be flagged (Python normalizes hyphens to underscores).
	var unusedFound bool
	for _, issue := range issues {
		if issue.ID == "depgraph/unused-dependency" && issue.Message == "Dependency appears unused: unused-lib" {
			unusedFound = true
		}
	}
	if !unusedFound {
		t.Error("expected unused-dependency issue for unused-lib")
	}

	// flask and requests should NOT be flagged.
	for _, issue := range issues {
		if issue.ID == "depgraph/unused-dependency" {
			if issue.Message == "Dependency appears unused: flask" {
				t.Error("flask should not be flagged as unused")
			}
			if issue.Message == "Dependency appears unused: requests" {
				t.Error("requests should not be flagged as unused")
			}
		}
	}
}

func TestDepGraphUnusedRustDep(t *testing.T) {
	a := NewDepGraphAnalyzer()
	cfg := config.DefaultConfig()

	tmpDir := t.TempDir()
	cargoToml := `[package]
name = "myapp"
version = "0.1.0"

[dependencies]
serde = "1.0"
unused-crate = "0.5"
`
	if err := os.WriteFile(filepath.Join(tmpDir, "Cargo.toml"), []byte(cargoToml), 0644); err != nil {
		t.Fatal(err)
	}

	idx := index.NewIndex(tmpDir, cfg)
	idx.Files = map[string]*index.FileEntry{
		"src/main.rs": {
			Path:     "src/main.rs",
			Language: "rust",
			Exports:  make([]parser.Export, 0),
			Imports:  []parser.Import{{Path: "serde", Line: 1}},
		},
	}

	ctx := &AnalysisContext{
		RepoPath: tmpDir,
		Diff: &git.Diff{
			Files: []git.FileDiff{
				{Path: "Cargo.toml", Status: "modified"},
			},
		},
		ParsedFiles: make(map[string]*parser.ParsedFile),
		Config:      cfg,
		Index:       idx,
	}

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	var unusedFound bool
	for _, issue := range issues {
		if issue.ID == "depgraph/unused-dependency" && issue.Message == "Dependency appears unused: unused-crate" {
			unusedFound = true
		}
	}
	if !unusedFound {
		t.Error("expected unused-dependency issue for unused-crate")
	}

	for _, issue := range issues {
		if issue.ID == "depgraph/unused-dependency" && issue.Message == "Dependency appears unused: serde" {
			t.Error("serde should not be flagged as unused")
		}
	}
}

func TestDepGraphManifestNotInDiff(t *testing.T) {
	a := NewDepGraphAnalyzer()
	cfg := config.DefaultConfig()

	tmpDir := t.TempDir()
	pkgJSON := `{"dependencies": {"unused-pkg": "^1.0.0"}}`
	if err := os.WriteFile(filepath.Join(tmpDir, "package.json"), []byte(pkgJSON), 0644); err != nil {
		t.Fatal(err)
	}

	idx := index.NewIndex(tmpDir, cfg)
	idx.Files = map[string]*index.FileEntry{
		"src/app.ts": {
			Path:     "src/app.ts",
			Language: "typescript",
			Exports:  make([]parser.Export, 0),
			Imports:  make([]parser.Import, 0),
		},
	}

	ctx := &AnalysisContext{
		RepoPath: tmpDir,
		Diff: &git.Diff{
			Files: []git.FileDiff{
				// package.json is NOT in the diff.
				{Path: "src/app.ts", Status: "modified"},
			},
		},
		ParsedFiles: make(map[string]*parser.ParsedFile),
		Config:      cfg,
		Index:       idx,
	}

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "depgraph/unused-dependency")
	if found != nil {
		t.Error("should not check unused deps when manifest is not in the diff")
	}
}

// --- Helper tests ---

func TestDepGraphCanonicalCycleKey(t *testing.T) {
	// Same cycle rotated should produce the same key.
	cycle1 := []string{"b.ts", "c.ts", "a.ts", "b.ts"}
	cycle2 := []string{"a.ts", "b.ts", "c.ts", "a.ts"}
	cycle3 := []string{"c.ts", "a.ts", "b.ts", "c.ts"}

	key1 := canonicalCycleKey(cycle1)
	key2 := canonicalCycleKey(cycle2)
	key3 := canonicalCycleKey(cycle3)

	if key1 != key2 || key2 != key3 {
		t.Errorf("canonical keys should match for rotated cycles: %q, %q, %q", key1, key2, key3)
	}
}

func TestDepGraphPythonHyphenUnderscore(t *testing.T) {
	// Python: "my-package" in requirements.txt is imported as "my_package".
	allImports := map[string]bool{
		"my_package": true,
	}

	if !isPythonDepUsed("my-package", allImports) {
		t.Error("my-package should be recognized as used via my_package import")
	}
}

func TestDepGraphRustHyphenUnderscore(t *testing.T) {
	// Rust: "my-crate" in Cargo.toml is imported as "my_crate".
	allImports := map[string]bool{
		"my_crate": true,
	}

	if !isRustDepUsed("my-crate", allImports) {
		t.Error("my-crate should be recognized as used via my_crate import")
	}
}

func TestDepGraphDeletedManifestSkipped(t *testing.T) {
	a := NewDepGraphAnalyzer()
	cfg := config.DefaultConfig()
	idx := index.NewIndex("/repo", cfg)
	idx.Files = map[string]*index.FileEntry{}

	ctx := &AnalysisContext{
		RepoPath: "/repo",
		Diff: &git.Diff{
			Files: []git.FileDiff{
				{Path: "package.json", Status: "deleted"},
			},
		},
		ParsedFiles: make(map[string]*parser.ParsedFile),
		Config:      cfg,
		Index:       idx,
	}

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "depgraph/unused-dependency")
	if found != nil {
		t.Error("should not check unused deps for deleted manifest files")
	}
}
