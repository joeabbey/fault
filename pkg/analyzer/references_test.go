package analyzer

import (
	"testing"

	"github.com/joeabbey/fault/pkg/config"
	"github.com/joeabbey/fault/pkg/git"
	"github.com/joeabbey/fault/pkg/parser"
)

func TestReferenceAnalyzerName(t *testing.T) {
	a := NewReferenceAnalyzer()
	if a.Name() != "references" {
		t.Errorf("expected name %q, got %q", "references", a.Name())
	}
}

func TestReferenceAnalyzerEmptyContext(t *testing.T) {
	a := NewReferenceAnalyzer()
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

func TestReferenceAnalyzerNilDiff(t *testing.T) {
	a := NewReferenceAnalyzer()
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

func TestReferenceAnalyzerDeletedFileStillImported(t *testing.T) {
	a := NewReferenceAnalyzer()
	ctx := &AnalysisContext{
		RepoPath: "/repo",
		Diff: &git.Diff{
			Files: []git.FileDiff{
				{Path: "src/helpers.ts", Status: "deleted"},
				{Path: "src/app.ts", Status: "modified"},
			},
			Mode: "staged",
		},
		ParsedFiles: map[string]*parser.ParsedFile{
			"src/app.ts": {
				Path:     "src/app.ts",
				Language: "typescript",
				Imports: []parser.Import{
					{Path: "./helpers", Names: []string{"format"}, Line: 2},
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
		if issue.Category == "references" && issue.Severity == SeverityError &&
			issue.File == "src/app.ts" {
			found = true
			break
		}
	}
	if !found {
		t.Error("expected error for import referencing deleted file")
	}
}

func TestReferenceAnalyzerDeletedPythonModule(t *testing.T) {
	a := NewReferenceAnalyzer()
	ctx := &AnalysisContext{
		RepoPath: "/repo",
		Diff: &git.Diff{
			Files: []git.FileDiff{
				{Path: "pkg/utils.py", Status: "deleted"},
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
		},
		Config: config.DefaultConfig(),
	}

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := false
	for _, issue := range issues {
		if issue.Category == "references" && issue.Severity == SeverityError &&
			issue.File == "pkg/main.py" {
			found = true
			break
		}
	}
	if !found {
		t.Error("expected error for Python import referencing deleted module")
	}
}

func TestReferenceAnalyzerRenamedFileStaleReference(t *testing.T) {
	a := NewReferenceAnalyzer()
	ctx := &AnalysisContext{
		RepoPath: "/repo",
		Diff: &git.Diff{
			Files: []git.FileDiff{
				{Path: "src/formatting.ts", OldPath: "src/helpers.ts", Status: "renamed"},
				{Path: "src/app.ts", Status: "modified"},
			},
			Mode: "staged",
		},
		ParsedFiles: map[string]*parser.ParsedFile{
			"src/app.ts": {
				Path:     "src/app.ts",
				Language: "typescript",
				Imports: []parser.Import{
					{Path: "./helpers", Names: []string{"format"}, Line: 2},
				},
				Exports: make([]parser.Export, 0),
				Symbols: make([]parser.Symbol, 0),
			},
			"src/formatting.ts": {
				Path:     "src/formatting.ts",
				Language: "typescript",
				Imports:  make([]parser.Import, 0),
				Exports: []parser.Export{
					{Name: "format", Kind: "function", Line: 5},
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
		if issue.Category == "references" && issue.Severity == SeverityWarning &&
			issue.File == "src/app.ts" {
			found = true
			break
		}
	}
	if !found {
		t.Error("expected warning for stale import after file rename")
	}
}

func TestReferenceAnalyzerDeletedSymbolStillReferenced(t *testing.T) {
	a := NewReferenceAnalyzer()
	ctx := &AnalysisContext{
		RepoPath: "/repo",
		Diff: &git.Diff{
			Files: []git.FileDiff{
				{
					Path:   "src/utils.ts",
					Status: "modified",
					Hunks: []git.Hunk{
						{
							OldStart: 10,
							OldCount: 3,
							NewStart: 10,
							NewCount: 1,
							Lines: []git.Line{
								{Type: "removed", Content: "export function oldHelper() {", OldNum: 10},
								{Type: "removed", Content: "  return true;", OldNum: 11},
								{Type: "removed", Content: "}", OldNum: 12},
								{Type: "added", Content: "// oldHelper removed", NewNum: 10},
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
					{Name: "newHelper", Kind: "function", Line: 5},
				},
				Symbols: []parser.Symbol{
					{Name: "newHelper", Kind: "function", Exported: true, Line: 5},
				},
			},
			"src/consumer.ts": {
				Path:     "src/consumer.ts",
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
		if issue.Category == "references" && issue.File == "src/consumer.ts" {
			found = true
			break
		}
	}
	if !found {
		t.Error("expected reference issue for deleted symbol still imported")
	}
}

func TestReferenceAnalyzerNoIssueForValidReferences(t *testing.T) {
	a := NewReferenceAnalyzer()
	ctx := &AnalysisContext{
		RepoPath: "/repo",
		Diff: &git.Diff{
			Files: []git.FileDiff{
				{Path: "src/utils.ts", Status: "modified"},
				{Path: "src/app.ts", Status: "modified"},
			},
			Mode: "staged",
		},
		ParsedFiles: map[string]*parser.ParsedFile{
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

	refIssues := filterIssues(issues, "references", "src/app.ts")
	if len(refIssues) != 0 {
		t.Errorf("expected 0 reference issues for valid references, got %d: %v", len(refIssues), refIssues)
	}
}

func TestReferenceAnalyzerDeletedGoSymbol(t *testing.T) {
	a := NewReferenceAnalyzer()
	ctx := &AnalysisContext{
		RepoPath: "/repo",
		Diff: &git.Diff{
			Files: []git.FileDiff{
				{
					Path:   "pkg/store/store.go",
					Status: "modified",
					Hunks: []git.Hunk{
						{
							OldStart: 20,
							OldCount: 3,
							NewStart: 20,
							NewCount: 1,
							Lines: []git.Line{
								{Type: "removed", Content: "func GetOldData() []byte {", OldNum: 20},
								{Type: "removed", Content: "  return nil", OldNum: 21},
								{Type: "removed", Content: "}", OldNum: 22},
								{Type: "added", Content: "// GetOldData removed", NewNum: 20},
							},
						},
					},
				},
			},
			Mode: "staged",
		},
		ParsedFiles: map[string]*parser.ParsedFile{
			"pkg/store/store.go": {
				Path:     "pkg/store/store.go",
				Language: "go",
				Imports:  make([]parser.Import, 0),
				Exports: []parser.Export{
					{Name: "GetNewData", Kind: "function", Line: 10},
				},
				Symbols: []parser.Symbol{
					{Name: "GetNewData", Kind: "function", Exported: true, Line: 10},
				},
			},
			"pkg/api/handler.go": {
				Path:     "pkg/api/handler.go",
				Language: "go",
				Imports: []parser.Import{
					{Path: "mymodule/pkg/store", Names: make([]string, 0), Line: 5},
				},
				Exports: make([]parser.Export, 0),
				Symbols: []parser.Symbol{
					{Name: "Handle", Kind: "function", Exported: true, Line: 10,
						Signature: "func Handle(GetOldData func())"},
				},
			},
		},
		Config: config.DefaultConfig(),
	}

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	// handler.go references GetOldData in its signature, which was removed from store.go
	found := false
	for _, issue := range issues {
		if issue.Category == "references" && issue.File == "pkg/api/handler.go" {
			found = true
			break
		}
	}
	if !found {
		t.Error("expected reference issue for deleted Go symbol still used in another file's signature")
	}
}

func TestReferenceAnalyzerExternalImportsIgnored(t *testing.T) {
	a := NewReferenceAnalyzer()
	ctx := &AnalysisContext{
		RepoPath: "/repo",
		Diff: &git.Diff{
			Files: []git.FileDiff{
				{Path: "src/app.ts", Status: "modified"},
				{Path: "src/deleted.ts", Status: "deleted"},
			},
			Mode: "staged",
		},
		ParsedFiles: map[string]*parser.ParsedFile{
			"src/app.ts": {
				Path:     "src/app.ts",
				Language: "typescript",
				Imports: []parser.Import{
					{Path: "react", Names: []string{"useState"}, Line: 1},
					{Path: "lodash", Names: []string{"debounce"}, Line: 2},
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

	// External imports should not be flagged
	refIssues := filterIssues(issues, "references", "src/app.ts")
	if len(refIssues) != 0 {
		t.Errorf("expected 0 reference issues for external imports, got %d", len(refIssues))
	}
}

func TestReferenceAnalyzerRenameWithSuggestion(t *testing.T) {
	a := NewReferenceAnalyzer()
	ctx := &AnalysisContext{
		RepoPath: "/repo",
		Diff: &git.Diff{
			Files: []git.FileDiff{
				{Path: "src/format-utils.ts", OldPath: "src/helpers.ts", Status: "renamed"},
			},
			Mode: "staged",
		},
		ParsedFiles: map[string]*parser.ParsedFile{
			"src/consumer.ts": {
				Path:     "src/consumer.ts",
				Language: "typescript",
				Imports: []parser.Import{
					{Path: "./helpers", Names: []string{"format"}, Line: 3},
				},
				Exports: make([]parser.Export, 0),
				Symbols: make([]parser.Symbol, 0),
			},
			"src/format-utils.ts": {
				Path:     "src/format-utils.ts",
				Language: "typescript",
				Imports:  make([]parser.Import, 0),
				Exports: []parser.Export{
					{Name: "format", Kind: "function", Line: 1},
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
		if issue.Category == "references" && issue.File == "src/consumer.ts" {
			found = true
			if issue.Suggestion == "" {
				t.Error("expected suggestion for renamed file reference")
			}
			break
		}
	}
	if !found {
		t.Error("expected reference issue for stale import after rename")
	}
}

func TestReferenceAnalyzerSymbolRenamedNotFlagged(t *testing.T) {
	// When a symbol is both removed and re-added (i.e., renamed), it should not
	// be flagged as "deleted".
	a := NewReferenceAnalyzer()
	ctx := &AnalysisContext{
		RepoPath: "/repo",
		Diff: &git.Diff{
			Files: []git.FileDiff{
				{
					Path:   "src/utils.ts",
					Status: "modified",
					Hunks: []git.Hunk{
						{
							OldStart: 1,
							OldCount: 1,
							NewStart: 1,
							NewCount: 1,
							Lines: []git.Line{
								{Type: "removed", Content: "export function helper() {", OldNum: 1},
								{Type: "added", Content: "export function helper() { // refactored", NewNum: 1},
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
					{Name: "helper", Kind: "function", Line: 1},
				},
				Symbols: []parser.Symbol{
					{Name: "helper", Kind: "function", Exported: true, Line: 1},
				},
			},
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

	// helper still exists in exports, so no "deleted symbol" reference issue should appear
	for _, issue := range issues {
		if issue.Category == "references" && issue.File == "src/app.ts" {
			t.Errorf("should not flag reference when symbol still exists: %v", issue)
		}
	}
}

// --- Helper function tests ---

func TestMatchesDeletedFile(t *testing.T) {
	tests := []struct {
		resolved string
		deleted  string
		language string
		want     bool
	}{
		{"src/helpers", "src/helpers.ts", "typescript", true},
		{"src/helpers", "src/helpers.tsx", "typescript", true},
		{"src/helpers", "src/helpers.js", "typescript", true},
		{"src/components", "src/components/index.ts", "typescript", true},
		{"src/helpers", "src/other.ts", "typescript", false},
		{"pkg/utils", "pkg/utils.py", "python", true},
		{"pkg/utils", "pkg/utils/__init__.py", "python", true},
		{"pkg/utils", "pkg/other.py", "python", false},
		{"src/same", "src/same", "typescript", true},
	}

	for _, tt := range tests {
		got := matchesDeletedFile(tt.resolved, tt.deleted, tt.language)
		if got != tt.want {
			t.Errorf("matchesDeletedFile(%q, %q, %q) = %v, want %v",
				tt.resolved, tt.deleted, tt.language, got, tt.want)
		}
	}
}

func TestOldBaseName(t *testing.T) {
	tests := []struct {
		path string
		want string
	}{
		{"src/helpers.ts", "helpers"},
		{"deep/path/file.tsx", "file"},
		{"noext", "noext"},
		{"file.test.ts", "file.test"},
	}

	for _, tt := range tests {
		got := oldBaseName(tt.path)
		if got != tt.want {
			t.Errorf("oldBaseName(%q) = %q, want %q", tt.path, got, tt.want)
		}
	}
}

func TestExtractRemovedSymbols(t *testing.T) {
	fd := git.FileDiff{
		Path:   "src/utils.ts",
		Status: "modified",
		Hunks: []git.Hunk{
			{
				Lines: []git.Line{
					{Type: "removed", Content: "export function foo() {"},
					{Type: "removed", Content: "export class Bar {"},
					{Type: "added", Content: "export function newFoo() {"},
					{Type: "context", Content: "// something"},
				},
			},
		},
	}

	symbols := extractRemovedSymbols(fd, "typescript")
	if len(symbols) != 2 {
		t.Fatalf("expected 2 removed symbols, got %d: %v", len(symbols), symbols)
	}

	found := make(map[string]bool)
	for _, s := range symbols {
		found[s] = true
	}
	if !found["foo"] {
		t.Error("expected 'foo' in removed symbols")
	}
	if !found["Bar"] {
		t.Error("expected 'Bar' in removed symbols")
	}
}

func TestExtractRemovedSymbolsGo(t *testing.T) {
	fd := git.FileDiff{
		Path:   "store.go",
		Status: "modified",
		Hunks: []git.Hunk{
			{
				Lines: []git.Line{
					{Type: "removed", Content: "func GetOld() string {"},
					{Type: "removed", Content: "type OldType struct {"},
					{Type: "removed", Content: "func privateFunc() {"},
					{Type: "added", Content: "func GetNew() string {"},
				},
			},
		},
	}

	symbols := extractRemovedSymbols(fd, "go")
	found := make(map[string]bool)
	for _, s := range symbols {
		found[s] = true
	}

	if !found["GetOld"] {
		t.Error("expected 'GetOld' in removed symbols")
	}
	if !found["OldType"] {
		t.Error("expected 'OldType' in removed symbols")
	}
	if found["privateFunc"] {
		t.Error("privateFunc should not be in removed symbols (not exported)")
	}
}

func TestFindSymbolUsageLine(t *testing.T) {
	pf := &parser.ParsedFile{
		Imports: []parser.Import{
			{Path: "./utils", Names: []string{"helper"}, Line: 2},
		},
		Symbols: []parser.Symbol{
			{Name: "doStuff", Kind: "function", Line: 10, Signature: "function doStuff(h: helper)"},
		},
	}

	// Find via import
	line := findSymbolUsageLine(pf, "helper")
	if line != 2 {
		t.Errorf("expected line 2 for 'helper', got %d", line)
	}

	// Find via signature
	line = findSymbolUsageLine(pf, "helper")
	if line == 0 {
		t.Error("expected non-zero line for 'helper' found in signature")
	}

	// Not found
	line = findSymbolUsageLine(pf, "nonexistent")
	if line != 0 {
		t.Errorf("expected line 0 for 'nonexistent', got %d", line)
	}
}
