package analyzer

import (
	"testing"

	"github.com/joeabbey/fault/pkg/config"
	"github.com/joeabbey/fault/pkg/git"
	"github.com/joeabbey/fault/pkg/parser"
)

func TestImportAnalyzerName(t *testing.T) {
	a := NewImportAnalyzer()
	if a.Name() != "imports" {
		t.Errorf("expected name %q, got %q", "imports", a.Name())
	}
}

func TestImportAnalyzerEmptyContext(t *testing.T) {
	a := NewImportAnalyzer()
	ctx := &AnalysisContext{
		RepoPath:    "/repo",
		Diff:        &git.Diff{Files: make([]git.FileDiff, 0)},
		ParsedFiles: make(map[string]*parser.ParsedFile),
		Config:      config.DefaultConfig(),
	}

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(issues) != 0 {
		t.Errorf("expected 0 issues, got %d", len(issues))
	}
}

func TestImportAnalyzerNilDiff(t *testing.T) {
	a := NewImportAnalyzer()
	ctx := &AnalysisContext{
		RepoPath:    "/repo",
		Diff:        nil,
		ParsedFiles: map[string]*parser.ParsedFile{"a.ts": {}},
		Config:      config.DefaultConfig(),
	}

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(issues) != 0 {
		t.Errorf("expected 0 issues, got %d", len(issues))
	}
}

func TestImportAnalyzerBrokenTSImport(t *testing.T) {
	a := NewImportAnalyzer()
	ctx := &AnalysisContext{
		RepoPath: "/repo",
		Diff: &git.Diff{
			Files: []git.FileDiff{
				{Path: "src/app.ts", Status: "modified"},
			},
			Mode: "staged",
		},
		ParsedFiles: map[string]*parser.ParsedFile{
			"src/app.ts": {
				Path:     "src/app.ts",
				Language: "typescript",
				Imports: []parser.Import{
					{Path: "./utils", Names: []string{"helper"}, Line: 1},
				},
				Exports: make([]parser.Export, 0),
				Symbols: make([]parser.Symbol, 0),
			},
			// Note: no "src/utils.ts" exists
		},
		Config: config.DefaultConfig(),
	}

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(issues) == 0 {
		t.Fatal("expected at least 1 issue for broken import, got 0")
	}

	found := false
	for _, issue := range issues {
		if issue.Severity == SeverityError && issue.Category == "imports" {
			found = true
			break
		}
	}
	if !found {
		t.Error("expected an error-severity import issue")
	}
}

func TestImportAnalyzerValidTSImport(t *testing.T) {
	a := NewImportAnalyzer()
	ctx := &AnalysisContext{
		RepoPath: "/repo",
		Diff: &git.Diff{
			Files: []git.FileDiff{
				{Path: "src/app.ts", Status: "modified"},
			},
			Mode: "staged",
		},
		ParsedFiles: map[string]*parser.ParsedFile{
			"src/app.ts": {
				Path:     "src/app.ts",
				Language: "typescript",
				Imports: []parser.Import{
					{Path: "./utils", Names: []string{"helper"}, Line: 1},
				},
				Exports: make([]parser.Export, 0),
				Symbols: make([]parser.Symbol, 0),
			},
			"src/utils.ts": {
				Path:     "src/utils.ts",
				Language: "typescript",
				Imports:  make([]parser.Import, 0),
				Exports: []parser.Export{
					{Name: "helper", Kind: "function", Line: 5},
				},
				Symbols: []parser.Symbol{
					{Name: "helper", Kind: "function", Exported: true, Line: 5},
				},
			},
		},
		Config: config.DefaultConfig(),
	}

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	// Filter out only import-category issues for the app.ts file
	importIssues := filterIssues(issues, "imports", "src/app.ts")
	if len(importIssues) != 0 {
		t.Errorf("expected 0 import issues for valid import, got %d: %v", len(importIssues), importIssues)
	}
}

func TestImportAnalyzerNameNotExported(t *testing.T) {
	a := NewImportAnalyzer()
	ctx := &AnalysisContext{
		RepoPath: "/repo",
		Diff: &git.Diff{
			Files: []git.FileDiff{
				{Path: "src/app.ts", Status: "modified"},
			},
			Mode: "staged",
		},
		ParsedFiles: map[string]*parser.ParsedFile{
			"src/app.ts": {
				Path:     "src/app.ts",
				Language: "typescript",
				Imports: []parser.Import{
					{Path: "./utils", Names: []string{"nonexistent"}, Line: 1},
				},
				Exports: make([]parser.Export, 0),
				Symbols: make([]parser.Symbol, 0),
			},
			"src/utils.ts": {
				Path:     "src/utils.ts",
				Language: "typescript",
				Imports:  make([]parser.Import, 0),
				Exports: []parser.Export{
					{Name: "helper", Kind: "function", Line: 5},
				},
				Symbols: make([]parser.Symbol, 0),
			},
		},
		Config: config.DefaultConfig(),
	}

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := false
	for _, issue := range issues {
		if issue.Category == "imports" && issue.Severity == SeverityError &&
			issue.File == "src/app.ts" {
			found = true
			break
		}
	}
	if !found {
		t.Error("expected an error for importing nonexistent name")
	}
}

func TestImportAnalyzerDeletedFileImport(t *testing.T) {
	a := NewImportAnalyzer()
	ctx := &AnalysisContext{
		RepoPath: "/repo",
		Diff: &git.Diff{
			Files: []git.FileDiff{
				{Path: "src/app.ts", Status: "modified"},
				{Path: "src/utils.ts", Status: "deleted"},
			},
			Mode: "staged",
		},
		ParsedFiles: map[string]*parser.ParsedFile{
			"src/app.ts": {
				Path:     "src/app.ts",
				Language: "typescript",
				Imports: []parser.Import{
					{Path: "./utils", Names: []string{"helper"}, Line: 1},
				},
				Exports: make([]parser.Export, 0),
				Symbols: make([]parser.Symbol, 0),
			},
		},
		Config: config.DefaultConfig(),
	}

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := false
	for _, issue := range issues {
		if issue.Category == "imports" && issue.Severity == SeverityError &&
			issue.File == "src/app.ts" {
			found = true
			break
		}
	}
	if !found {
		t.Error("expected error for import pointing to deleted file")
	}
}

func TestImportAnalyzerExternalImportSkipped(t *testing.T) {
	a := NewImportAnalyzer()
	ctx := &AnalysisContext{
		RepoPath: "/repo",
		Diff: &git.Diff{
			Files: []git.FileDiff{
				{Path: "src/app.ts", Status: "modified"},
			},
			Mode: "staged",
		},
		ParsedFiles: map[string]*parser.ParsedFile{
			"src/app.ts": {
				Path:     "src/app.ts",
				Language: "typescript",
				Imports: []parser.Import{
					{Path: "react", Names: []string{"useState"}, Line: 1},
					{Path: "@types/node", Names: make([]string, 0), Line: 2},
				},
				Exports: make([]parser.Export, 0),
				Symbols: make([]parser.Symbol, 0),
			},
		},
		Config: config.DefaultConfig(),
	}

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	// External imports should not generate issues
	importIssues := filterIssues(issues, "imports", "src/app.ts")
	if len(importIssues) != 0 {
		t.Errorf("expected 0 issues for external imports, got %d", len(importIssues))
	}
}

func TestImportAnalyzerRemovedExportStillImported(t *testing.T) {
	a := NewImportAnalyzer()
	ctx := &AnalysisContext{
		RepoPath: "/repo",
		Diff: &git.Diff{
			Files: []git.FileDiff{
				{
					Path:   "src/utils.ts",
					Status: "modified",
					Hunks: []git.Hunk{
						{
							OldStart: 5,
							OldCount: 3,
							NewStart: 5,
							NewCount: 1,
							Lines: []git.Line{
								{Type: "removed", Content: "export function oldHelper() {", OldNum: 5},
								{Type: "removed", Content: "  return true;", OldNum: 6},
								{Type: "removed", Content: "}", OldNum: 7},
								{Type: "added", Content: "// oldHelper was removed", NewNum: 5},
							},
						},
					},
				},
			},
			Mode: "staged",
		},
		ParsedFiles: map[string]*parser.ParsedFile{
			"src/utils.ts": {
				Path:     "src/utils.ts",
				Language: "typescript",
				Imports:  make([]parser.Import, 0),
				Exports: []parser.Export{
					{Name: "newHelper", Kind: "function", Line: 10},
				},
				Symbols: make([]parser.Symbol, 0),
			},
			"src/app.ts": {
				Path:     "src/app.ts",
				Language: "typescript",
				Imports: []parser.Import{
					{Path: "./utils", Names: []string{"oldHelper"}, Line: 1},
				},
				Exports: make([]parser.Export, 0),
				Symbols: make([]parser.Symbol, 0),
			},
		},
		Config: config.DefaultConfig(),
	}

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := false
	for _, issue := range issues {
		if issue.Category == "imports" && issue.File == "src/app.ts" {
			found = true
			break
		}
	}
	if !found {
		t.Error("expected warning for removed export still imported")
	}
}

func TestImportAnalyzerPythonRelativeImport(t *testing.T) {
	a := NewImportAnalyzer()
	ctx := &AnalysisContext{
		RepoPath: "/repo",
		Diff: &git.Diff{
			Files: []git.FileDiff{
				{Path: "pkg/main.py", Status: "modified"},
			},
			Mode: "staged",
		},
		ParsedFiles: map[string]*parser.ParsedFile{
			"pkg/main.py": {
				Path:     "pkg/main.py",
				Language: "python",
				Imports: []parser.Import{
					{Path: ".utils", Names: []string{"helper"}, Line: 1},
				},
				Exports: make([]parser.Export, 0),
				Symbols: make([]parser.Symbol, 0),
			},
			"pkg/utils.py": {
				Path:     "pkg/utils.py",
				Language: "python",
				Imports:  make([]parser.Import, 0),
				Exports: []parser.Export{
					{Name: "helper", Kind: "function", Line: 3},
				},
				Symbols: []parser.Symbol{
					{Name: "helper", Kind: "function", Exported: true, Line: 3},
				},
			},
		},
		Config: config.DefaultConfig(),
	}

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	importIssues := filterIssues(issues, "imports", "pkg/main.py")
	if len(importIssues) != 0 {
		t.Errorf("expected 0 import issues for valid Python import, got %d: %v", len(importIssues), importIssues)
	}
}

func TestImportAnalyzerPythonBrokenRelativeImport(t *testing.T) {
	a := NewImportAnalyzer()
	ctx := &AnalysisContext{
		RepoPath: "/repo",
		Diff: &git.Diff{
			Files: []git.FileDiff{
				{Path: "pkg/main.py", Status: "modified"},
				{Path: "pkg/utils.py", Status: "deleted"},
			},
			Mode: "staged",
		},
		ParsedFiles: map[string]*parser.ParsedFile{
			"pkg/main.py": {
				Path:     "pkg/main.py",
				Language: "python",
				Imports: []parser.Import{
					{Path: ".utils", Names: []string{"helper"}, Line: 1},
				},
				Exports: make([]parser.Export, 0),
				Symbols: make([]parser.Symbol, 0),
			},
		},
		Config: config.DefaultConfig(),
	}

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := false
	for _, issue := range issues {
		if issue.Category == "imports" && issue.File == "pkg/main.py" && issue.Severity == SeverityError {
			found = true
			break
		}
	}
	if !found {
		t.Error("expected error for Python import pointing to deleted module")
	}
}

func TestImportAnalyzerGoStdlibSkipped(t *testing.T) {
	a := NewImportAnalyzer()
	ctx := &AnalysisContext{
		RepoPath: "/repo",
		Diff: &git.Diff{
			Files: []git.FileDiff{
				{Path: "main.go", Status: "modified"},
			},
			Mode: "staged",
		},
		ParsedFiles: map[string]*parser.ParsedFile{
			"main.go": {
				Path:     "main.go",
				Language: "go",
				Imports: []parser.Import{
					{Path: "fmt", Names: make([]string, 0), Line: 3},
					{Path: "os", Names: make([]string, 0), Line: 4},
					{Path: "net/http", Names: make([]string, 0), Line: 5},
				},
				Exports: make([]parser.Export, 0),
				Symbols: make([]parser.Symbol, 0),
			},
		},
		Config: config.DefaultConfig(),
	}

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	importIssues := filterIssues(issues, "imports", "main.go")
	if len(importIssues) != 0 {
		t.Errorf("expected 0 issues for Go stdlib imports, got %d", len(importIssues))
	}
}

func TestImportAnalyzerTSIndexFile(t *testing.T) {
	a := NewImportAnalyzer()
	ctx := &AnalysisContext{
		RepoPath: "/repo",
		Diff: &git.Diff{
			Files: []git.FileDiff{
				{Path: "src/app.ts", Status: "modified"},
			},
			Mode: "staged",
		},
		ParsedFiles: map[string]*parser.ParsedFile{
			"src/app.ts": {
				Path:     "src/app.ts",
				Language: "typescript",
				Imports: []parser.Import{
					{Path: "./components", Names: []string{"Button"}, Line: 1},
				},
				Exports: make([]parser.Export, 0),
				Symbols: make([]parser.Symbol, 0),
			},
			"src/components/index.ts": {
				Path:     "src/components/index.ts",
				Language: "typescript",
				Imports:  make([]parser.Import, 0),
				Exports: []parser.Export{
					{Name: "Button", Kind: "class", Line: 1},
				},
				Symbols: make([]parser.Symbol, 0),
			},
		},
		Config: config.DefaultConfig(),
	}

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	importIssues := filterIssues(issues, "imports", "src/app.ts")
	if len(importIssues) != 0 {
		t.Errorf("expected 0 issues for index file import, got %d: %v", len(importIssues), importIssues)
	}
}

func TestImportAnalyzerWildcardReExport(t *testing.T) {
	a := NewImportAnalyzer()
	ctx := &AnalysisContext{
		RepoPath: "/repo",
		Diff: &git.Diff{
			Files: []git.FileDiff{
				{Path: "src/app.ts", Status: "modified"},
			},
			Mode: "staged",
		},
		ParsedFiles: map[string]*parser.ParsedFile{
			"src/app.ts": {
				Path:     "src/app.ts",
				Language: "typescript",
				Imports: []parser.Import{
					{Path: "./barrel", Names: []string{"SomeExport"}, Line: 1},
				},
				Exports: make([]parser.Export, 0),
				Symbols: make([]parser.Symbol, 0),
			},
			"src/barrel.ts": {
				Path:     "src/barrel.ts",
				Language: "typescript",
				Imports:  make([]parser.Import, 0),
				Exports: []parser.Export{
					{Name: "*", Kind: "re-export", Line: 1},
				},
				Symbols: make([]parser.Symbol, 0),
			},
		},
		Config: config.DefaultConfig(),
	}

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	importIssues := filterIssues(issues, "imports", "src/app.ts")
	if len(importIssues) != 0 {
		t.Errorf("expected 0 issues when barrel file has wildcard re-export, got %d", len(importIssues))
	}
}

// --- Helper functions for tests ---

func filterIssues(issues []Issue, category, file string) []Issue {
	filtered := make([]Issue, 0)
	for _, issue := range issues {
		if issue.Category == category && issue.File == file {
			filtered = append(filtered, issue)
		}
	}
	return filtered
}

// --- Helper function tests ---

func TestIsRelativeImport(t *testing.T) {
	tests := []struct {
		path string
		want bool
	}{
		{"./foo", true},
		{"../foo", true},
		{".", true},
		{"..", true},
		{"react", false},
		{"@types/node", false},
		{"fs", false},
	}

	for _, tt := range tests {
		got := isRelativeImport(tt.path)
		if got != tt.want {
			t.Errorf("isRelativeImport(%q) = %v, want %v", tt.path, got, tt.want)
		}
	}
}

func TestIsGoStdlib(t *testing.T) {
	tests := []struct {
		path string
		want bool
	}{
		{"fmt", true},
		{"os", true},
		{"net/http", true},
		{"encoding/json", true},
		{"github.com/foo/bar", false},
		{"golang.org/x/tools", false},
	}

	for _, tt := range tests {
		got := isGoStdlib(tt.path)
		if got != tt.want {
			t.Errorf("isGoStdlib(%q) = %v, want %v", tt.path, got, tt.want)
		}
	}
}

func TestResolveRelativeImport(t *testing.T) {
	tests := []struct {
		importer string
		impPath  string
		want     string
	}{
		{"src/app.ts", "./utils", "src/utils"},
		{"src/app.ts", "../lib/helper", "lib/helper"},
		{"src/deep/file.ts", "./sibling", "src/deep/sibling"},
	}

	for _, tt := range tests {
		got := resolveRelativeImport(tt.importer, tt.impPath)
		if got != tt.want {
			t.Errorf("resolveRelativeImport(%q, %q) = %q, want %q", tt.importer, tt.impPath, got, tt.want)
		}
	}
}

func TestResolvePythonImport(t *testing.T) {
	tests := []struct {
		importer string
		impPath  string
		want     string
	}{
		{"pkg/main.py", ".utils", "pkg/utils"},
		{"pkg/sub/main.py", "..utils", "pkg/utils"},
		{"pkg/main.py", ".models.user", "pkg/models/user"},
	}

	for _, tt := range tests {
		got := resolvePythonImport(tt.importer, tt.impPath)
		if got != tt.want {
			t.Errorf("resolvePythonImport(%q, %q) = %q, want %q", tt.importer, tt.impPath, got, tt.want)
		}
	}
}

func TestExtractExportNames(t *testing.T) {
	tests := []struct {
		line string
		want []string
	}{
		{"export function foo() {", []string{"foo"}},
		{"export class MyClass {", []string{"MyClass"}},
		{"export const bar = 5", []string{"bar"}},
		{"export type Config = {}", []string{"Config"}},
		{"export interface IFoo {", []string{"IFoo"}},
		{"not an export", nil},
	}

	for _, tt := range tests {
		got := extractExportNames(tt.line)
		if len(got) != len(tt.want) {
			t.Errorf("extractExportNames(%q) = %v, want %v", tt.line, got, tt.want)
			continue
		}
		for i := range got {
			if got[i] != tt.want[i] {
				t.Errorf("extractExportNames(%q)[%d] = %q, want %q", tt.line, i, got[i], tt.want[i])
			}
		}
	}
}

func TestExtractPythonDefName(t *testing.T) {
	tests := []struct {
		line string
		want string
	}{
		{"def foo(x, y):", "foo"},
		{"class MyClass:", "MyClass"},
		{"class Base(object):", "Base"},
		{"not a def", ""},
	}

	for _, tt := range tests {
		got := extractPythonDefName(tt.line)
		if got != tt.want {
			t.Errorf("extractPythonDefName(%q) = %q, want %q", tt.line, got, tt.want)
		}
	}
}

func TestExtractGoExportedName(t *testing.T) {
	tests := []struct {
		line string
		want string
	}{
		{"func Foo() {", "Foo"},
		{"func (r *Repo) Bar() {", "Bar"},
		{"type Config struct {", "Config"},
		{"var MyVar = 5", "MyVar"},
		{"func privateFunc() {", ""},
		{"not a declaration", ""},
	}

	for _, tt := range tests {
		got := extractGoExportedName(tt.line)
		if got != tt.want {
			t.Errorf("extractGoExportedName(%q) = %q, want %q", tt.line, got, tt.want)
		}
	}
}

func TestBuildExportMap(t *testing.T) {
	parsedFiles := map[string]*parser.ParsedFile{
		"utils.ts": {
			Exports: []parser.Export{
				{Name: "foo", Kind: "function"},
				{Name: "bar", Kind: "variable"},
			},
			Symbols: []parser.Symbol{
				{Name: "baz", Kind: "function", Exported: true},
			},
		},
	}

	em := buildExportMap(parsedFiles)
	if !em["utils.ts"]["foo"] {
		t.Error("expected foo in export map")
	}
	if !em["utils.ts"]["bar"] {
		t.Error("expected bar in export map")
	}
	if !em["utils.ts"]["baz"] {
		t.Error("expected baz in export map (from symbols)")
	}
}
