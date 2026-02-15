package analyzer

import (
	"os"
	"path/filepath"
	"testing"

	"github.com/joeabbey/fault/pkg/config"
	"github.com/joeabbey/fault/pkg/git"
	"github.com/joeabbey/fault/pkg/parser"
)

func TestHallucinationAnalyzerName(t *testing.T) {
	h := NewHallucinationAnalyzer()
	if h.Name() != "hallucination" {
		t.Errorf("expected name 'hallucination', got %q", h.Name())
	}
}

func TestHallucinationEmptyDiff(t *testing.T) {
	h := NewHallucinationAnalyzer()
	ctx := &AnalysisContext{
		RepoPath:    "/nonexistent",
		Diff:        &git.Diff{Files: make([]git.FileDiff, 0)},
		ParsedFiles: make(map[string]*parser.ParsedFile),
		Config:      config.DefaultConfig(),
	}

	issues, err := h.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(issues) != 0 {
		t.Errorf("expected 0 issues for empty diff, got %d", len(issues))
	}
}

func TestHallucinationNilDiff(t *testing.T) {
	h := NewHallucinationAnalyzer()
	ctx := &AnalysisContext{
		RepoPath:    "/nonexistent",
		Diff:        nil,
		ParsedFiles: make(map[string]*parser.ParsedFile),
		Config:      config.DefaultConfig(),
	}

	issues, err := h.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(issues) != 0 {
		t.Errorf("expected 0 issues for nil diff, got %d", len(issues))
	}
}

// --- Phantom Go import tests ---

func TestPhantomGoImport(t *testing.T) {
	// Create a temp repo with a go.mod.
	tmp := t.TempDir()
	writeFile(t, filepath.Join(tmp, "go.mod"), `module example.com/myapp

go 1.21

require (
	github.com/real/package v1.0.0
)
`)

	h := NewHallucinationAnalyzer()
	ctx := &AnalysisContext{
		RepoPath: tmp,
		Diff: &git.Diff{Files: []git.FileDiff{
			{Path: "main.go", Status: "modified", Hunks: []git.Hunk{{
				Lines: []git.Line{{Type: "added", Content: `import "github.com/fake/nonexistent"`, NewNum: 3}},
			}}},
		}},
		ParsedFiles: map[string]*parser.ParsedFile{
			"main.go": {
				Path:     "main.go",
				Language: "go",
				Imports: []parser.Import{
					{Path: "github.com/fake/nonexistent", Line: 3},
				},
			},
		},
		Config: config.DefaultConfig(),
	}

	issues, err := h.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "hallucination/phantom-import")
	if found == nil {
		t.Fatal("expected phantom-import issue for unknown Go package")
	}
	if found.Severity != SeverityError {
		t.Errorf("expected error severity, got %q", found.Severity)
	}
}

func TestValidGoStdlibImport(t *testing.T) {
	tmp := t.TempDir()
	writeFile(t, filepath.Join(tmp, "go.mod"), `module example.com/myapp

go 1.21
`)

	h := NewHallucinationAnalyzer()
	ctx := &AnalysisContext{
		RepoPath: tmp,
		Diff: &git.Diff{Files: []git.FileDiff{
			{Path: "main.go", Status: "modified", Hunks: []git.Hunk{{
				Lines: []git.Line{{Type: "added", Content: `import "fmt"`, NewNum: 3}},
			}}},
		}},
		ParsedFiles: map[string]*parser.ParsedFile{
			"main.go": {
				Path:     "main.go",
				Language: "go",
				Imports: []parser.Import{
					{Path: "fmt", Line: 3},
					{Path: "net/http", Line: 4},
					{Path: "encoding/json", Line: 5},
				},
			},
		},
		Config: config.DefaultConfig(),
	}

	issues, err := h.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "hallucination/phantom-import")
	if found != nil {
		t.Errorf("did not expect phantom-import for stdlib imports, got: %s", found.Message)
	}
}

func TestValidGoModuleImport(t *testing.T) {
	tmp := t.TempDir()
	writeFile(t, filepath.Join(tmp, "go.mod"), `module example.com/myapp

go 1.21

require (
	github.com/real/package v1.0.0
)
`)

	h := NewHallucinationAnalyzer()
	ctx := &AnalysisContext{
		RepoPath: tmp,
		Diff: &git.Diff{Files: []git.FileDiff{
			{Path: "main.go", Status: "modified", Hunks: []git.Hunk{{
				Lines: []git.Line{{Type: "added", Content: `import "github.com/real/package/sub"`, NewNum: 3}},
			}}},
		}},
		ParsedFiles: map[string]*parser.ParsedFile{
			"main.go": {
				Path:     "main.go",
				Language: "go",
				Imports: []parser.Import{
					{Path: "github.com/real/package/sub", Line: 3},
					{Path: "example.com/myapp/internal/util", Line: 4},
				},
			},
		},
		Config: config.DefaultConfig(),
	}

	issues, err := h.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "hallucination/phantom-import")
	if found != nil {
		t.Errorf("did not expect phantom-import for valid go.mod require, got: %s", found.Message)
	}
}

func TestGoImportNoGoModFailsOpen(t *testing.T) {
	tmp := t.TempDir()
	// No go.mod — should fail open.

	h := NewHallucinationAnalyzer()
	ctx := &AnalysisContext{
		RepoPath: tmp,
		Diff: &git.Diff{Files: []git.FileDiff{
			{Path: "main.go", Status: "modified", Hunks: []git.Hunk{{
				Lines: []git.Line{{Type: "added", Content: `import "github.com/anything"`, NewNum: 3}},
			}}},
		}},
		ParsedFiles: map[string]*parser.ParsedFile{
			"main.go": {
				Path:     "main.go",
				Language: "go",
				Imports:  []parser.Import{{Path: "github.com/anything", Line: 3}},
			},
		},
		Config: config.DefaultConfig(),
	}

	issues, err := h.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "hallucination/phantom-import")
	if found != nil {
		t.Errorf("expected fail-open with no go.mod, but got: %s", found.Message)
	}
}

// --- Phantom JS/TS import tests ---

func TestPhantomNpmPackage(t *testing.T) {
	tmp := t.TempDir()
	writeFile(t, filepath.Join(tmp, "package.json"), `{
  "dependencies": {
    "react": "^18.0.0"
  },
  "devDependencies": {
    "typescript": "^5.0.0"
  }
}`)

	h := NewHallucinationAnalyzer()
	ctx := &AnalysisContext{
		RepoPath: tmp,
		Diff: &git.Diff{Files: []git.FileDiff{
			{Path: "app.ts", Status: "modified", Hunks: []git.Hunk{{
				Lines: []git.Line{{Type: "added", Content: `import { stuff } from "nonexistent-pkg"`, NewNum: 1}},
			}}},
		}},
		ParsedFiles: map[string]*parser.ParsedFile{
			"app.ts": {
				Path:     "app.ts",
				Language: "typescript",
				Imports: []parser.Import{
					{Path: "nonexistent-pkg", Line: 1},
				},
			},
		},
		Config: config.DefaultConfig(),
	}

	issues, err := h.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "hallucination/phantom-import")
	if found == nil {
		t.Fatal("expected phantom-import issue for unknown npm package")
	}
}

func TestValidNpmPackage(t *testing.T) {
	tmp := t.TempDir()
	writeFile(t, filepath.Join(tmp, "package.json"), `{
  "dependencies": {
    "react": "^18.0.0",
    "@scope/pkg": "^1.0.0"
  },
  "devDependencies": {
    "typescript": "^5.0.0"
  }
}`)

	h := NewHallucinationAnalyzer()
	ctx := &AnalysisContext{
		RepoPath: tmp,
		Diff: &git.Diff{Files: []git.FileDiff{
			{Path: "app.ts", Status: "modified", Hunks: []git.Hunk{{
				Lines: []git.Line{{Type: "added", Content: `import React from "react"`, NewNum: 1}},
			}}},
		}},
		ParsedFiles: map[string]*parser.ParsedFile{
			"app.ts": {
				Path:     "app.ts",
				Language: "typescript",
				Imports: []parser.Import{
					{Path: "react", Line: 1},
					{Path: "react/jsx-runtime", Line: 2},
					{Path: "@scope/pkg", Line: 3},
					{Path: "@scope/pkg/sub", Line: 4},
					{Path: "fs", Line: 5},
					{Path: "node:path", Line: 6},
				},
			},
		},
		Config: config.DefaultConfig(),
	}

	issues, err := h.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "hallucination/phantom-import")
	if found != nil {
		t.Errorf("did not expect phantom-import for valid packages, got: %s", found.Message)
	}
}

func TestRelativeImportMissingFile(t *testing.T) {
	tmp := t.TempDir()
	writeFile(t, filepath.Join(tmp, "package.json"), `{"dependencies": {}}`)
	// Don't create the target file.

	h := NewHallucinationAnalyzer()
	ctx := &AnalysisContext{
		RepoPath: tmp,
		Diff: &git.Diff{Files: []git.FileDiff{
			{Path: "src/app.ts", Status: "modified", Hunks: []git.Hunk{{
				Lines: []git.Line{{Type: "added", Content: `import { foo } from "./missing"`, NewNum: 1}},
			}}},
		}},
		ParsedFiles: map[string]*parser.ParsedFile{
			"src/app.ts": {
				Path:     "src/app.ts",
				Language: "typescript",
				Imports: []parser.Import{
					{Path: "./missing", Line: 1},
				},
			},
		},
		Config: config.DefaultConfig(),
	}

	issues, err := h.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "hallucination/missing-import-target")
	if found == nil {
		t.Fatal("expected missing-import-target issue for relative import to missing file")
	}
}

func TestRelativeImportExistingFile(t *testing.T) {
	tmp := t.TempDir()
	writeFile(t, filepath.Join(tmp, "package.json"), `{"dependencies": {}}`)
	os.MkdirAll(filepath.Join(tmp, "src"), 0755)
	writeFile(t, filepath.Join(tmp, "src", "utils.ts"), `export function foo() {}`)

	h := NewHallucinationAnalyzer()
	ctx := &AnalysisContext{
		RepoPath: tmp,
		Diff: &git.Diff{Files: []git.FileDiff{
			{Path: "src/app.ts", Status: "modified", Hunks: []git.Hunk{{
				Lines: []git.Line{{Type: "added", Content: `import { foo } from "./utils"`, NewNum: 1}},
			}}},
		}},
		ParsedFiles: map[string]*parser.ParsedFile{
			"src/app.ts": {
				Path:     "src/app.ts",
				Language: "typescript",
				Imports: []parser.Import{
					{Path: "./utils", Line: 1},
				},
			},
		},
		Config: config.DefaultConfig(),
	}

	issues, err := h.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "hallucination/missing-import-target")
	if found != nil {
		t.Errorf("did not expect missing-import-target for existing file, got: %s", found.Message)
	}
}

func TestRelativeImportIndexFile(t *testing.T) {
	tmp := t.TempDir()
	writeFile(t, filepath.Join(tmp, "package.json"), `{"dependencies": {}}`)
	os.MkdirAll(filepath.Join(tmp, "src", "components"), 0755)
	writeFile(t, filepath.Join(tmp, "src", "components", "index.ts"), `export {}`)

	h := NewHallucinationAnalyzer()
	ctx := &AnalysisContext{
		RepoPath: tmp,
		Diff: &git.Diff{Files: []git.FileDiff{
			{Path: "src/app.ts", Status: "modified", Hunks: []git.Hunk{{
				Lines: []git.Line{{Type: "added", Content: `import {} from "./components"`, NewNum: 1}},
			}}},
		}},
		ParsedFiles: map[string]*parser.ParsedFile{
			"src/app.ts": {
				Path:     "src/app.ts",
				Language: "typescript",
				Imports: []parser.Import{
					{Path: "./components", Line: 1},
				},
			},
		},
		Config: config.DefaultConfig(),
	}

	issues, err := h.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "hallucination/missing-import-target")
	if found != nil {
		t.Errorf("did not expect missing-import-target for directory with index.ts, got: %s", found.Message)
	}
}

// --- Phantom Python import tests ---

func TestPhantomPythonImport(t *testing.T) {
	tmp := t.TempDir()
	writeFile(t, filepath.Join(tmp, "requirements.txt"), "requests==2.28.0\nflask>=2.0\n")

	h := NewHallucinationAnalyzer()
	ctx := &AnalysisContext{
		RepoPath: tmp,
		Diff: &git.Diff{Files: []git.FileDiff{
			{Path: "app.py", Status: "modified", Hunks: []git.Hunk{{
				Lines: []git.Line{{Type: "added", Content: `import nonexistent_pkg`, NewNum: 1}},
			}}},
		}},
		ParsedFiles: map[string]*parser.ParsedFile{
			"app.py": {
				Path:     "app.py",
				Language: "python",
				Imports: []parser.Import{
					{Path: "nonexistent_pkg", Line: 1},
				},
			},
		},
		Config: config.DefaultConfig(),
	}

	issues, err := h.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "hallucination/phantom-import")
	if found == nil {
		t.Fatal("expected phantom-import issue for unknown Python package")
	}
}

func TestValidPythonStdlibImport(t *testing.T) {
	tmp := t.TempDir()
	writeFile(t, filepath.Join(tmp, "requirements.txt"), "requests==2.28.0\n")

	h := NewHallucinationAnalyzer()
	ctx := &AnalysisContext{
		RepoPath: tmp,
		Diff: &git.Diff{Files: []git.FileDiff{
			{Path: "app.py", Status: "modified", Hunks: []git.Hunk{{
				Lines: []git.Line{{Type: "added", Content: `import os`, NewNum: 1}},
			}}},
		}},
		ParsedFiles: map[string]*parser.ParsedFile{
			"app.py": {
				Path:     "app.py",
				Language: "python",
				Imports: []parser.Import{
					{Path: "os", Line: 1},
					{Path: "sys", Line: 2},
					{Path: "json", Line: 3},
					{Path: "typing", Line: 4},
					{Path: "pathlib", Line: 5},
					{Path: "collections.abc", Line: 6},
				},
			},
		},
		Config: config.DefaultConfig(),
	}

	issues, err := h.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "hallucination/phantom-import")
	if found != nil {
		t.Errorf("did not expect phantom-import for Python stdlib, got: %s", found.Message)
	}
}

func TestValidPythonRequirementImport(t *testing.T) {
	tmp := t.TempDir()
	writeFile(t, filepath.Join(tmp, "requirements.txt"), "requests==2.28.0\nFlask>=2.0\nmy_utils~=1.0\n")

	h := NewHallucinationAnalyzer()
	ctx := &AnalysisContext{
		RepoPath: tmp,
		Diff: &git.Diff{Files: []git.FileDiff{
			{Path: "app.py", Status: "modified", Hunks: []git.Hunk{{
				Lines: []git.Line{{Type: "added", Content: `import requests`, NewNum: 1}},
			}}},
		}},
		ParsedFiles: map[string]*parser.ParsedFile{
			"app.py": {
				Path:     "app.py",
				Language: "python",
				Imports: []parser.Import{
					{Path: "requests", Line: 1},
					{Path: "flask", Line: 2},
					{Path: "my_utils", Line: 3},
				},
			},
		},
		Config: config.DefaultConfig(),
	}

	issues, err := h.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "hallucination/phantom-import")
	if found != nil {
		t.Errorf("did not expect phantom-import for valid requirements, got: %s", found.Message)
	}
}

// --- Stub function tests ---

func TestStubGoFunction(t *testing.T) {
	h := NewHallucinationAnalyzer()
	ctx := &AnalysisContext{
		RepoPath: "/nonexistent",
		Diff: &git.Diff{Files: []git.FileDiff{
			{Path: "handler.go", Status: "modified", Hunks: []git.Hunk{{
				Lines: []git.Line{
					{Type: "added", Content: "func HandleRequest(w http.ResponseWriter, r *http.Request) {", NewNum: 10},
					{Type: "added", Content: "	return nil", NewNum: 11},
					{Type: "added", Content: "}", NewNum: 12},
				},
			}}},
		}},
		ParsedFiles: make(map[string]*parser.ParsedFile),
		Config:      config.DefaultConfig(),
	}

	issues, err := h.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "hallucination/stub-implementation")
	if found == nil {
		t.Fatal("expected stub-implementation issue for Go function returning nil")
	}
	if found.Severity != SeverityWarning {
		t.Errorf("expected warning severity, got %q", found.Severity)
	}
}

func TestStubTSFunction(t *testing.T) {
	h := NewHallucinationAnalyzer()
	ctx := &AnalysisContext{
		RepoPath: "/nonexistent",
		Diff: &git.Diff{Files: []git.FileDiff{
			{Path: "service.ts", Status: "modified", Hunks: []git.Hunk{{
				Lines: []git.Line{
					{Type: "added", Content: `export async function fetchData(url: string) {`, NewNum: 5},
					{Type: "added", Content: `  throw new Error("not implemented");`, NewNum: 6},
					{Type: "added", Content: `}`, NewNum: 7},
				},
			}}},
		}},
		ParsedFiles: make(map[string]*parser.ParsedFile),
		Config:      config.DefaultConfig(),
	}

	issues, err := h.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "hallucination/stub-implementation")
	if found == nil {
		t.Fatal("expected stub-implementation issue for TS function throwing not implemented")
	}
}

func TestStubPythonFunction(t *testing.T) {
	h := NewHallucinationAnalyzer()
	ctx := &AnalysisContext{
		RepoPath: "/nonexistent",
		Diff: &git.Diff{Files: []git.FileDiff{
			{Path: "service.py", Status: "modified", Hunks: []git.Hunk{{
				Lines: []git.Line{
					{Type: "added", Content: "def process_data(data):", NewNum: 10},
					{Type: "added", Content: "    pass", NewNum: 11},
				},
			}}},
		}},
		ParsedFiles: make(map[string]*parser.ParsedFile),
		Config:      config.DefaultConfig(),
	}

	issues, err := h.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "hallucination/stub-implementation")
	if found == nil {
		t.Fatal("expected stub-implementation issue for Python function with pass")
	}
}

func TestShortRealFunctionNotFlagged(t *testing.T) {
	h := NewHallucinationAnalyzer()
	ctx := &AnalysisContext{
		RepoPath: "/nonexistent",
		Diff: &git.Diff{Files: []git.FileDiff{
			{Path: "util.go", Status: "modified", Hunks: []git.Hunk{{
				Lines: []git.Line{
					{Type: "added", Content: "func (s *Server) Name() string {", NewNum: 10},
					{Type: "added", Content: `	return s.name`, NewNum: 11},
					{Type: "added", Content: "}", NewNum: 12},
				},
			}}},
		}},
		ParsedFiles: make(map[string]*parser.ParsedFile),
		Config:      config.DefaultConfig(),
	}

	issues, err := h.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "hallucination/stub-implementation")
	if found != nil {
		t.Errorf("did not expect stub-implementation for real getter function, got: %s", found.Message)
	}
}

func TestTestFileStubsNotFlagged(t *testing.T) {
	h := NewHallucinationAnalyzer()
	ctx := &AnalysisContext{
		RepoPath: "/nonexistent",
		Diff: &git.Diff{Files: []git.FileDiff{
			{Path: "handler_test.go", Status: "modified", Hunks: []git.Hunk{{
				Lines: []git.Line{
					{Type: "added", Content: "func mockHandler() error {", NewNum: 10},
					{Type: "added", Content: "	return nil", NewNum: 11},
					{Type: "added", Content: "}", NewNum: 12},
				},
			}}},
		}},
		ParsedFiles: make(map[string]*parser.ParsedFile),
		Config:      config.DefaultConfig(),
	}

	issues, err := h.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "hallucination/stub-implementation")
	if found != nil {
		t.Errorf("did not expect stub-implementation in test file, got: %s", found.Message)
	}
}

func TestStubPanicTODO(t *testing.T) {
	h := NewHallucinationAnalyzer()
	ctx := &AnalysisContext{
		RepoPath: "/nonexistent",
		Diff: &git.Diff{Files: []git.FileDiff{
			{Path: "store.go", Status: "modified", Hunks: []git.Hunk{{
				Lines: []git.Line{
					{Type: "added", Content: `func (s *Store) Save(item Item) error {`, NewNum: 20},
					{Type: "added", Content: `	panic("TODO")`, NewNum: 21},
					{Type: "added", Content: `}`, NewNum: 22},
				},
			}}},
		}},
		ParsedFiles: make(map[string]*parser.ParsedFile),
		Config:      config.DefaultConfig(),
	}

	issues, err := h.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "hallucination/stub-implementation")
	if found == nil {
		t.Fatal("expected stub-implementation issue for panic(TODO)")
	}
}

// --- Vendor/node_modules skipped ---

func TestVendorFilesSkipped(t *testing.T) {
	h := NewHallucinationAnalyzer()
	ctx := &AnalysisContext{
		RepoPath: "/nonexistent",
		Diff: &git.Diff{Files: []git.FileDiff{
			{Path: "vendor/github.com/pkg/main.go", Status: "modified", Hunks: []git.Hunk{{
				Lines: []git.Line{
					{Type: "added", Content: "func Stub() { return nil }", NewNum: 1},
				},
			}}},
		}},
		ParsedFiles: make(map[string]*parser.ParsedFile),
		Config:      config.DefaultConfig(),
	}

	issues, err := h.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	if len(issues) != 0 {
		t.Errorf("expected 0 issues for vendor files, got %d", len(issues))
	}
}

func TestNodeModulesSkipped(t *testing.T) {
	h := NewHallucinationAnalyzer()
	ctx := &AnalysisContext{
		RepoPath: "/nonexistent",
		Diff: &git.Diff{Files: []git.FileDiff{
			{Path: "node_modules/some-pkg/index.js", Status: "modified", Hunks: []git.Hunk{{
				Lines: []git.Line{
					{Type: "added", Content: "function stub() { return {} }", NewNum: 1},
				},
			}}},
		}},
		ParsedFiles: make(map[string]*parser.ParsedFile),
		Config:      config.DefaultConfig(),
	}

	issues, err := h.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	if len(issues) != 0 {
		t.Errorf("expected 0 issues for node_modules files, got %d", len(issues))
	}
}

// --- Deleted files skipped ---

func TestDeletedFileSkipped(t *testing.T) {
	h := NewHallucinationAnalyzer()
	ctx := &AnalysisContext{
		RepoPath: "/nonexistent",
		Diff: &git.Diff{Files: []git.FileDiff{
			{Path: "old.go", Status: "deleted", Hunks: []git.Hunk{{
				Lines: []git.Line{
					{Type: "added", Content: "func Stub() { return nil }", NewNum: 1},
				},
			}}},
		}},
		ParsedFiles: make(map[string]*parser.ParsedFile),
		Config:      config.DefaultConfig(),
	}

	issues, err := h.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	if len(issues) != 0 {
		t.Errorf("expected 0 issues for deleted file, got %d", len(issues))
	}
}

// --- No package.json/requirements fails open ---

func TestJSImportNoPackageJSONFailsOpen(t *testing.T) {
	tmp := t.TempDir()
	// No package.json.

	h := NewHallucinationAnalyzer()
	ctx := &AnalysisContext{
		RepoPath: tmp,
		Diff: &git.Diff{Files: []git.FileDiff{
			{Path: "app.ts", Status: "modified", Hunks: []git.Hunk{{
				Lines: []git.Line{{Type: "added", Content: `import anything from "whatever"`, NewNum: 1}},
			}}},
		}},
		ParsedFiles: map[string]*parser.ParsedFile{
			"app.ts": {
				Path:     "app.ts",
				Language: "typescript",
				Imports:  []parser.Import{{Path: "whatever", Line: 1}},
			},
		},
		Config: config.DefaultConfig(),
	}

	issues, err := h.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "hallucination/phantom-import")
	if found != nil {
		t.Errorf("expected fail-open with no package.json, but got: %s", found.Message)
	}
}

func TestPythonImportNoRequirementsFailsOpen(t *testing.T) {
	tmp := t.TempDir()
	// No requirements.txt or pyproject.toml.

	h := NewHallucinationAnalyzer()
	ctx := &AnalysisContext{
		RepoPath: tmp,
		Diff: &git.Diff{Files: []git.FileDiff{
			{Path: "app.py", Status: "modified", Hunks: []git.Hunk{{
				Lines: []git.Line{{Type: "added", Content: `import whatever`, NewNum: 1}},
			}}},
		}},
		ParsedFiles: map[string]*parser.ParsedFile{
			"app.py": {
				Path:     "app.py",
				Language: "python",
				Imports:  []parser.Import{{Path: "whatever", Line: 1}},
			},
		},
		Config: config.DefaultConfig(),
	}

	issues, err := h.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "hallucination/phantom-import")
	if found != nil {
		t.Errorf("expected fail-open with no requirements, but got: %s", found.Message)
	}
}

// --- Multi-body function not flagged as stub ---

func TestMultiLineFunctionNotStub(t *testing.T) {
	h := NewHallucinationAnalyzer()
	ctx := &AnalysisContext{
		RepoPath: "/nonexistent",
		Diff: &git.Diff{Files: []git.FileDiff{
			{Path: "handler.go", Status: "modified", Hunks: []git.Hunk{{
				Lines: []git.Line{
					{Type: "added", Content: "func Process(data []byte) error {", NewNum: 10},
					{Type: "added", Content: "	result := parse(data)", NewNum: 11},
					{Type: "added", Content: "	return validate(result)", NewNum: 12},
					{Type: "added", Content: "}", NewNum: 13},
				},
			}}},
		}},
		ParsedFiles: make(map[string]*parser.ParsedFile),
		Config:      config.DefaultConfig(),
	}

	issues, err := h.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "hallucination/stub-implementation")
	if found != nil {
		t.Errorf("did not expect stub-implementation for multi-line function, got: %s", found.Message)
	}
}

// --- Pyproject.toml support ---

func TestPyprojectTomlDeps(t *testing.T) {
	tmp := t.TempDir()
	writeFile(t, filepath.Join(tmp, "pyproject.toml"), `[project]
name = "myapp"
dependencies = [
    "requests>=2.28",
    "click",
]
`)

	h := NewHallucinationAnalyzer()
	ctx := &AnalysisContext{
		RepoPath: tmp,
		Diff: &git.Diff{Files: []git.FileDiff{
			{Path: "app.py", Status: "modified", Hunks: []git.Hunk{{
				Lines: []git.Line{{Type: "added", Content: `import requests`, NewNum: 1}},
			}}},
		}},
		ParsedFiles: map[string]*parser.ParsedFile{
			"app.py": {
				Path:     "app.py",
				Language: "python",
				Imports: []parser.Import{
					{Path: "requests", Line: 1},
					{Path: "click", Line: 2},
				},
			},
		},
		Config: config.DefaultConfig(),
	}

	issues, err := h.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "hallucination/phantom-import")
	if found != nil {
		t.Errorf("did not expect phantom-import for pyproject.toml deps, got: %s", found.Message)
	}
}

// --- Helper ---

func writeFile(t *testing.T, path, content string) {
	t.Helper()
	dir := filepath.Dir(path)
	if err := os.MkdirAll(dir, 0755); err != nil {
		t.Fatalf("mkdir %s: %v", dir, err)
	}
	if err := os.WriteFile(path, []byte(content), 0644); err != nil {
		t.Fatalf("write %s: %v", path, err)
	}
}
