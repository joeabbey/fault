package analyzer

import (
	"testing"

	"github.com/joeabbey/fault/pkg/config"
	"github.com/joeabbey/fault/pkg/git"
	"github.com/joeabbey/fault/pkg/index"
	"github.com/joeabbey/fault/pkg/parser"
)

func TestDeadCodeAnalyzerName(t *testing.T) {
	a := NewDeadCodeAnalyzer()
	if a.Name() != "deadcode" {
		t.Errorf("expected name %q, got %q", "deadcode", a.Name())
	}
}

func TestDeadCodeAnalyzerNilIndex(t *testing.T) {
	a := NewDeadCodeAnalyzer()
	ctx := &AnalysisContext{
		RepoPath: "/repo",
		Diff: &git.Diff{
			Files: []git.FileDiff{{Path: "src/utils.ts", Status: "modified"}},
		},
		ParsedFiles: map[string]*parser.ParsedFile{
			"src/utils.ts": {
				Path:     "src/utils.ts",
				Language: "typescript",
				Exports:  []parser.Export{{Name: "helper", Kind: "function", Line: 5}},
			},
		},
		Config: config.DefaultConfig(),
		Index:  nil,
	}

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(issues) != 0 {
		t.Errorf("expected 0 issues when index is nil, got %d", len(issues))
	}
}

func TestDeadCodeAnalyzerNilDiff(t *testing.T) {
	a := NewDeadCodeAnalyzer()
	ctx := &AnalysisContext{
		RepoPath:    "/repo",
		Diff:        nil,
		ParsedFiles: map[string]*parser.ParsedFile{},
		Config:      config.DefaultConfig(),
		Index:       buildTestIndex(nil),
	}

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(issues) != 0 {
		t.Errorf("expected 0 issues when diff is nil, got %d", len(issues))
	}
}

func TestDeadCodeAnalyzerTSUnusedExport(t *testing.T) {
	a := NewDeadCodeAnalyzer()
	idx := buildTestIndex(map[string]*index.FileEntry{
		"src/utils.ts": {
			Path:     "src/utils.ts",
			Language: "typescript",
			Exports: []parser.Export{
				{Name: "helper", Kind: "function", Line: 5},
				{Name: "unused", Kind: "function", Line: 10},
			},
			Imports: make([]parser.Import, 0),
		},
		"src/app.ts": {
			Path:     "src/app.ts",
			Language: "typescript",
			Exports:  make([]parser.Export, 0),
			Imports: []parser.Import{
				{Path: "./utils", Names: []string{"helper"}, Line: 1},
			},
		},
	})

	ctx := &AnalysisContext{
		RepoPath: "/repo",
		Diff: &git.Diff{
			Files: []git.FileDiff{{Path: "src/utils.ts", Status: "modified"}},
		},
		ParsedFiles: map[string]*parser.ParsedFile{
			"src/utils.ts": {
				Path:     "src/utils.ts",
				Language: "typescript",
				Exports: []parser.Export{
					{Name: "helper", Kind: "function", Line: 5},
					{Name: "unused", Kind: "function", Line: 10},
				},
				Imports: make([]parser.Import, 0),
			},
		},
		Config: config.DefaultConfig(),
		Index:  idx,
	}

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	// "unused" should be flagged, "helper" should not
	foundUnused := false
	foundHelper := false
	for _, issue := range issues {
		if issue.Category != "deadcode" {
			continue
		}
		if issue.File == "src/utils.ts" {
			if containsString(issue.Message, "unused") {
				foundUnused = true
			}
			if containsString(issue.Message, "helper") {
				foundHelper = true
			}
		}
	}

	if !foundUnused {
		t.Error("expected dead code warning for 'unused' export")
	}
	if foundHelper {
		t.Error("did not expect dead code warning for 'helper' — it is imported by app.ts")
	}
}

func TestDeadCodeAnalyzerTSAllUsed(t *testing.T) {
	a := NewDeadCodeAnalyzer()
	idx := buildTestIndex(map[string]*index.FileEntry{
		"src/utils.ts": {
			Path:     "src/utils.ts",
			Language: "typescript",
			Exports: []parser.Export{
				{Name: "helper", Kind: "function", Line: 5},
			},
			Imports: make([]parser.Import, 0),
		},
		"src/app.ts": {
			Path:     "src/app.ts",
			Language: "typescript",
			Exports:  make([]parser.Export, 0),
			Imports: []parser.Import{
				{Path: "./utils", Names: []string{"helper"}, Line: 1},
			},
		},
	})

	ctx := &AnalysisContext{
		RepoPath: "/repo",
		Diff: &git.Diff{
			Files: []git.FileDiff{{Path: "src/utils.ts", Status: "modified"}},
		},
		ParsedFiles: map[string]*parser.ParsedFile{
			"src/utils.ts": {
				Path:     "src/utils.ts",
				Language: "typescript",
				Exports: []parser.Export{
					{Name: "helper", Kind: "function", Line: 5},
				},
				Imports: make([]parser.Import, 0),
			},
		},
		Config: config.DefaultConfig(),
		Index:  idx,
	}

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	deadIssues := filterDeadCodeIssues(issues)
	if len(deadIssues) != 0 {
		t.Errorf("expected 0 dead code issues, got %d: %v", len(deadIssues), deadIssues)
	}
}

func TestDeadCodeAnalyzerGoUnusedExport(t *testing.T) {
	a := NewDeadCodeAnalyzer()
	idx := buildTestIndex(map[string]*index.FileEntry{
		"pkg/store/store.go": {
			Path:     "pkg/store/store.go",
			Language: "go",
			Exports: []parser.Export{
				{Name: "GetData", Kind: "function", Line: 10},
				{Name: "Unused", Kind: "function", Line: 20},
			},
			Imports: make([]parser.Import, 0),
		},
		"pkg/api/handler.go": {
			Path:     "pkg/api/handler.go",
			Language: "go",
			Exports:  make([]parser.Export, 0),
			Imports: []parser.Import{
				{Path: "github.com/myproject/pkg/store", Line: 5},
			},
		},
	})

	ctx := &AnalysisContext{
		RepoPath: "/repo",
		Diff: &git.Diff{
			Files: []git.FileDiff{{Path: "pkg/store/store.go", Status: "modified"}},
		},
		ParsedFiles: map[string]*parser.ParsedFile{
			"pkg/store/store.go": {
				Path:     "pkg/store/store.go",
				Language: "go",
				Exports: []parser.Export{
					{Name: "GetData", Kind: "function", Line: 10},
					{Name: "Unused", Kind: "function", Line: 20},
				},
				Imports: make([]parser.Import, 0),
			},
		},
		Config: config.DefaultConfig(),
		Index:  idx,
	}

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	// In Go, if any file imports the package, ALL exports are considered used.
	// handler.go imports pkg/store, so both GetData and Unused should NOT be flagged.
	deadIssues := filterDeadCodeIssues(issues)
	if len(deadIssues) != 0 {
		t.Errorf("expected 0 dead code issues (package is imported), got %d: %v", len(deadIssues), deadIssues)
	}
}

func TestDeadCodeAnalyzerGoUnimportedPackage(t *testing.T) {
	a := NewDeadCodeAnalyzer()
	idx := buildTestIndex(map[string]*index.FileEntry{
		"pkg/orphan/orphan.go": {
			Path:     "pkg/orphan/orphan.go",
			Language: "go",
			Exports: []parser.Export{
				{Name: "DoStuff", Kind: "function", Line: 5},
			},
			Imports: make([]parser.Import, 0),
		},
		"pkg/api/handler.go": {
			Path:     "pkg/api/handler.go",
			Language: "go",
			Exports:  make([]parser.Export, 0),
			Imports: []parser.Import{
				{Path: "github.com/myproject/pkg/store", Line: 5},
			},
		},
	})

	ctx := &AnalysisContext{
		RepoPath: "/repo",
		Diff: &git.Diff{
			Files: []git.FileDiff{{Path: "pkg/orphan/orphan.go", Status: "modified"}},
		},
		ParsedFiles: map[string]*parser.ParsedFile{
			"pkg/orphan/orphan.go": {
				Path:     "pkg/orphan/orphan.go",
				Language: "go",
				Exports: []parser.Export{
					{Name: "DoStuff", Kind: "function", Line: 5},
				},
				Imports: make([]parser.Import, 0),
			},
		},
		Config: config.DefaultConfig(),
		Index:  idx,
	}

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	deadIssues := filterDeadCodeIssues(issues)
	if len(deadIssues) != 1 {
		t.Fatalf("expected 1 dead code issue, got %d: %v", len(deadIssues), deadIssues)
	}
	if !containsString(deadIssues[0].Message, "DoStuff") {
		t.Errorf("expected issue about 'DoStuff', got: %s", deadIssues[0].Message)
	}
}

func TestDeadCodeAnalyzerGoSkipsMainPackage(t *testing.T) {
	a := NewDeadCodeAnalyzer()
	idx := buildTestIndex(map[string]*index.FileEntry{
		"cmd/server/main.go": {
			Path:     "cmd/server/main.go",
			Language: "go",
			Exports: []parser.Export{
				{Name: "Main", Kind: "function", Line: 5},
			},
			Imports: make([]parser.Import, 0),
		},
	})

	ctx := &AnalysisContext{
		RepoPath: "/repo",
		Diff: &git.Diff{
			Files: []git.FileDiff{{Path: "cmd/server/main.go", Status: "modified"}},
		},
		ParsedFiles: map[string]*parser.ParsedFile{
			"cmd/server/main.go": {
				Path:     "cmd/server/main.go",
				Language: "go",
				Exports: []parser.Export{
					{Name: "Main", Kind: "function", Line: 5},
				},
				Imports: make([]parser.Import, 0),
			},
		},
		Config: config.DefaultConfig(),
		Index:  idx,
	}

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	deadIssues := filterDeadCodeIssues(issues)
	if len(deadIssues) != 0 {
		t.Errorf("expected 0 dead code issues for main package, got %d", len(deadIssues))
	}
}

func TestDeadCodeAnalyzerGoSkipsTestFiles(t *testing.T) {
	a := NewDeadCodeAnalyzer()
	idx := buildTestIndex(map[string]*index.FileEntry{
		"pkg/store/store_test.go": {
			Path:     "pkg/store/store_test.go",
			Language: "go",
			Exports: []parser.Export{
				{Name: "TestHelper", Kind: "function", Line: 5},
			},
			Imports: make([]parser.Import, 0),
		},
	})

	ctx := &AnalysisContext{
		RepoPath: "/repo",
		Diff: &git.Diff{
			Files: []git.FileDiff{{Path: "pkg/store/store_test.go", Status: "modified"}},
		},
		ParsedFiles: map[string]*parser.ParsedFile{
			"pkg/store/store_test.go": {
				Path:     "pkg/store/store_test.go",
				Language: "go",
				Exports: []parser.Export{
					{Name: "TestHelper", Kind: "function", Line: 5},
				},
				Imports: make([]parser.Import, 0),
			},
		},
		Config: config.DefaultConfig(),
		Index:  idx,
	}

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	deadIssues := filterDeadCodeIssues(issues)
	if len(deadIssues) != 0 {
		t.Errorf("expected 0 dead code issues for test files, got %d", len(deadIssues))
	}
}

func TestDeadCodeAnalyzerPythonUnusedExport(t *testing.T) {
	a := NewDeadCodeAnalyzer()
	idx := buildTestIndex(map[string]*index.FileEntry{
		"mypackage/utils.py": {
			Path:     "mypackage/utils.py",
			Language: "python",
			Exports: []parser.Export{
				{Name: "helper", Kind: "function", Line: 5},
				{Name: "orphan", Kind: "function", Line: 15},
			},
			Imports: make([]parser.Import, 0),
		},
		"mypackage/main.py": {
			Path:     "mypackage/main.py",
			Language: "python",
			Exports:  make([]parser.Export, 0),
			Imports: []parser.Import{
				{Path: ".utils", Names: []string{"helper"}, Line: 1},
			},
		},
	})

	ctx := &AnalysisContext{
		RepoPath: "/repo",
		Diff: &git.Diff{
			Files: []git.FileDiff{{Path: "mypackage/utils.py", Status: "modified"}},
		},
		ParsedFiles: map[string]*parser.ParsedFile{
			"mypackage/utils.py": {
				Path:     "mypackage/utils.py",
				Language: "python",
				Exports: []parser.Export{
					{Name: "helper", Kind: "function", Line: 5},
					{Name: "orphan", Kind: "function", Line: 15},
				},
				Imports: make([]parser.Import, 0),
			},
		},
		Config: config.DefaultConfig(),
		Index:  idx,
	}

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	foundOrphan := false
	foundHelper := false
	for _, issue := range issues {
		if issue.Category != "deadcode" {
			continue
		}
		if containsString(issue.Message, "orphan") {
			foundOrphan = true
		}
		if containsString(issue.Message, "helper") {
			foundHelper = true
		}
	}

	if !foundOrphan {
		t.Error("expected dead code warning for 'orphan'")
	}
	if foundHelper {
		t.Error("did not expect dead code warning for 'helper' — it is imported")
	}
}

func TestDeadCodeAnalyzerPythonSkipsPrivate(t *testing.T) {
	a := NewDeadCodeAnalyzer()
	idx := buildTestIndex(map[string]*index.FileEntry{
		"mypackage/utils.py": {
			Path:     "mypackage/utils.py",
			Language: "python",
			Exports: []parser.Export{
				{Name: "_private_helper", Kind: "function", Line: 5},
			},
			Imports: make([]parser.Import, 0),
		},
	})

	ctx := &AnalysisContext{
		RepoPath: "/repo",
		Diff: &git.Diff{
			Files: []git.FileDiff{{Path: "mypackage/utils.py", Status: "modified"}},
		},
		ParsedFiles: map[string]*parser.ParsedFile{
			"mypackage/utils.py": {
				Path:     "mypackage/utils.py",
				Language: "python",
				Exports: []parser.Export{
					{Name: "_private_helper", Kind: "function", Line: 5},
				},
				Imports: make([]parser.Import, 0),
			},
		},
		Config: config.DefaultConfig(),
		Index:  idx,
	}

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	deadIssues := filterDeadCodeIssues(issues)
	if len(deadIssues) != 0 {
		t.Errorf("expected 0 dead code issues for private Python symbol, got %d", len(deadIssues))
	}
}

func TestDeadCodeAnalyzerSkipsDeletedFiles(t *testing.T) {
	a := NewDeadCodeAnalyzer()
	idx := buildTestIndex(map[string]*index.FileEntry{})

	ctx := &AnalysisContext{
		RepoPath: "/repo",
		Diff: &git.Diff{
			Files: []git.FileDiff{{Path: "src/removed.ts", Status: "deleted"}},
		},
		ParsedFiles: map[string]*parser.ParsedFile{},
		Config:      config.DefaultConfig(),
		Index:       idx,
	}

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(issues) != 0 {
		t.Errorf("expected 0 issues for deleted files, got %d", len(issues))
	}
}

func TestDeadCodeAnalyzerSkipsBarrelFiles(t *testing.T) {
	a := NewDeadCodeAnalyzer()
	idx := buildTestIndex(map[string]*index.FileEntry{
		"src/components/index.ts": {
			Path:     "src/components/index.ts",
			Language: "typescript",
			Exports: []parser.Export{
				{Name: "Button", Kind: "variable", Line: 1},
			},
			Imports: make([]parser.Import, 0),
		},
	})

	ctx := &AnalysisContext{
		RepoPath: "/repo",
		Diff: &git.Diff{
			Files: []git.FileDiff{{Path: "src/components/index.ts", Status: "modified"}},
		},
		ParsedFiles: map[string]*parser.ParsedFile{
			"src/components/index.ts": {
				Path:     "src/components/index.ts",
				Language: "typescript",
				Exports: []parser.Export{
					{Name: "Button", Kind: "variable", Line: 1},
				},
				Imports: make([]parser.Import, 0),
			},
		},
		Config: config.DefaultConfig(),
		Index:  idx,
	}

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	deadIssues := filterDeadCodeIssues(issues)
	if len(deadIssues) != 0 {
		t.Errorf("expected 0 dead code issues for barrel file, got %d", len(deadIssues))
	}
}

func TestDeadCodeAnalyzerSkipsInitPy(t *testing.T) {
	a := NewDeadCodeAnalyzer()
	idx := buildTestIndex(map[string]*index.FileEntry{
		"mypackage/__init__.py": {
			Path:     "mypackage/__init__.py",
			Language: "python",
			Exports: []parser.Export{
				{Name: "helper", Kind: "function", Line: 1},
			},
			Imports: make([]parser.Import, 0),
		},
	})

	ctx := &AnalysisContext{
		RepoPath: "/repo",
		Diff: &git.Diff{
			Files: []git.FileDiff{{Path: "mypackage/__init__.py", Status: "modified"}},
		},
		ParsedFiles: map[string]*parser.ParsedFile{
			"mypackage/__init__.py": {
				Path:     "mypackage/__init__.py",
				Language: "python",
				Exports: []parser.Export{
					{Name: "helper", Kind: "function", Line: 1},
				},
				Imports: make([]parser.Import, 0),
			},
		},
		Config: config.DefaultConfig(),
		Index:  idx,
	}

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	deadIssues := filterDeadCodeIssues(issues)
	if len(deadIssues) != 0 {
		t.Errorf("expected 0 dead code issues for __init__.py, got %d", len(deadIssues))
	}
}

func TestDeadCodeAnalyzerIssueFormat(t *testing.T) {
	a := NewDeadCodeAnalyzer()
	idx := buildTestIndex(map[string]*index.FileEntry{
		"src/utils.ts": {
			Path:     "src/utils.ts",
			Language: "typescript",
			Exports: []parser.Export{
				{Name: "orphanFunc", Kind: "function", Line: 42},
			},
			Imports: make([]parser.Import, 0),
		},
	})

	ctx := &AnalysisContext{
		RepoPath: "/repo",
		Diff: &git.Diff{
			Files: []git.FileDiff{{Path: "src/utils.ts", Status: "added"}},
		},
		ParsedFiles: map[string]*parser.ParsedFile{
			"src/utils.ts": {
				Path:     "src/utils.ts",
				Language: "typescript",
				Exports: []parser.Export{
					{Name: "orphanFunc", Kind: "function", Line: 42},
				},
				Imports: make([]parser.Import, 0),
			},
		},
		Config: config.DefaultConfig(),
		Index:  idx,
	}

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	if len(issues) != 1 {
		t.Fatalf("expected 1 issue, got %d", len(issues))
	}

	issue := issues[0]
	if issue.ID != "deadcode-src/utils.ts-orphanFunc-42" {
		t.Errorf("unexpected ID: %s", issue.ID)
	}
	if issue.FixID != "deadcode-unused-export" {
		t.Errorf("unexpected FixID: %s", issue.FixID)
	}
	if issue.Severity != SeverityWarning {
		t.Errorf("unexpected severity: %s", issue.Severity)
	}
	if issue.Category != "deadcode" {
		t.Errorf("unexpected category: %s", issue.Category)
	}
	if issue.File != "src/utils.ts" {
		t.Errorf("unexpected file: %s", issue.File)
	}
	if issue.Line != 42 {
		t.Errorf("unexpected line: %d", issue.Line)
	}
	if !containsString(issue.Message, "orphanFunc") {
		t.Errorf("expected message to mention 'orphanFunc': %s", issue.Message)
	}
}

func TestDeadCodeAnalyzerTSWildcardImport(t *testing.T) {
	// When an import has no named imports (wildcard/default), any export from that
	// file should be considered used.
	a := NewDeadCodeAnalyzer()
	idx := buildTestIndex(map[string]*index.FileEntry{
		"src/utils.ts": {
			Path:     "src/utils.ts",
			Language: "typescript",
			Exports: []parser.Export{
				{Name: "helper", Kind: "function", Line: 5},
			},
			Imports: make([]parser.Import, 0),
		},
		"src/app.ts": {
			Path:     "src/app.ts",
			Language: "typescript",
			Exports:  make([]parser.Export, 0),
			Imports: []parser.Import{
				{Path: "./utils", Names: []string{}, Line: 1}, // wildcard/default import
			},
		},
	})

	ctx := &AnalysisContext{
		RepoPath: "/repo",
		Diff: &git.Diff{
			Files: []git.FileDiff{{Path: "src/utils.ts", Status: "modified"}},
		},
		ParsedFiles: map[string]*parser.ParsedFile{
			"src/utils.ts": {
				Path:     "src/utils.ts",
				Language: "typescript",
				Exports: []parser.Export{
					{Name: "helper", Kind: "function", Line: 5},
				},
				Imports: make([]parser.Import, 0),
			},
		},
		Config: config.DefaultConfig(),
		Index:  idx,
	}

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	deadIssues := filterDeadCodeIssues(issues)
	if len(deadIssues) != 0 {
		t.Errorf("expected 0 dead code issues for wildcard import, got %d", len(deadIssues))
	}
}

// --- Test helpers ---

// buildTestIndex creates an Index populated with the given file entries.
func buildTestIndex(files map[string]*index.FileEntry) *index.Index {
	idx := &index.Index{
		Files:    make(map[string]*index.FileEntry),
		RepoRoot: "/repo",
	}
	if files != nil {
		idx.Files = files
	}
	return idx
}

// filterDeadCodeIssues returns only issues with category "deadcode".
func filterDeadCodeIssues(issues []Issue) []Issue {
	result := make([]Issue, 0)
	for _, issue := range issues {
		if issue.Category == "deadcode" {
			result = append(result, issue)
		}
	}
	return result
}

// containsString checks if s contains substr.
func containsString(s, substr string) bool {
	return len(s) >= len(substr) && (s == substr || len(s) > 0 && stringContains(s, substr))
}

func stringContains(s, substr string) bool {
	for i := 0; i <= len(s)-len(substr); i++ {
		if s[i:i+len(substr)] == substr {
			return true
		}
	}
	return false
}
