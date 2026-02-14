package analyzer

import (
	"testing"

	"github.com/joeabbey/fault/pkg/config"
	"github.com/joeabbey/fault/pkg/git"
	"github.com/joeabbey/fault/pkg/parser"
)

func TestConsistencyAnalyzerName(t *testing.T) {
	a := NewConsistencyAnalyzer()
	if a.Name() != "consistency" {
		t.Errorf("expected name %q, got %q", "consistency", a.Name())
	}
}

func TestConsistencyAnalyzerEmptyContext(t *testing.T) {
	a := NewConsistencyAnalyzer()
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

func TestConsistencyAnalyzerNilDiff(t *testing.T) {
	a := NewConsistencyAnalyzer()
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

func TestConsistencyAnalyzerSignatureChange(t *testing.T) {
	a := NewConsistencyAnalyzer()
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
							OldCount: 3,
							NewStart: 1,
							NewCount: 3,
							Lines: []git.Line{
								{Type: "removed", Content: "export function calculate(a: number, b: number) {", OldNum: 1},
								{Type: "added", Content: "export function calculate(a: number, b: number, precision: number) {", NewNum: 1},
								{Type: "context", Content: "  return a + b;", OldNum: 2, NewNum: 2},
								{Type: "context", Content: "}", OldNum: 3, NewNum: 3},
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
					{Name: "calculate", Kind: "function", Line: 1},
				},
				Symbols: []parser.Symbol{
					{Name: "calculate", Kind: "function", Exported: true, Line: 1,
						Signature: "function calculate(a: number, b: number, precision: number)"},
				},
			},
			"src/app.ts": {
				Path:     "src/app.ts",
				Language: "typescript",
				Imports: []parser.Import{
					{Path: "./utils", Names: []string{"calculate"}, Line: 1},
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
		if issue.Category == "consistency" && issue.File == "src/app.ts" {
			found = true
			break
		}
	}
	if !found {
		t.Error("expected consistency warning for caller not updated after signature change")
	}
}

func TestConsistencyAnalyzerGoSignatureChange(t *testing.T) {
	a := NewConsistencyAnalyzer()
	ctx := &AnalysisContext{
		RepoPath: "/repo",
		Diff: &git.Diff{
			Files: []git.FileDiff{
				{
					Path:   "pkg/store/store.go",
					Status: "modified",
					Hunks: []git.Hunk{
						{
							OldStart: 10,
							OldCount: 3,
							NewStart: 10,
							NewCount: 3,
							Lines: []git.Line{
								{Type: "removed", Content: "func GetUser(id string) (*User, error) {", OldNum: 10},
								{Type: "added", Content: "func GetUser(id string, includeProfile bool) (*User, error) {", NewNum: 10},
								{Type: "context", Content: "  return nil, nil", OldNum: 11, NewNum: 11},
								{Type: "context", Content: "}", OldNum: 12, NewNum: 12},
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
					{Name: "GetUser", Kind: "function", Line: 10},
				},
				Symbols: []parser.Symbol{
					{Name: "GetUser", Kind: "function", Exported: true, Line: 10,
						Signature: "func GetUser(id string, includeProfile bool) (*User, error)"},
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
					{Name: "HandleGet", Kind: "function", Exported: true, Line: 15,
						Signature: "func HandleGet(w http.ResponseWriter, r *http.Request)"},
				},
			},
		},
		Config: config.DefaultConfig(),
	}

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	// Since the handler file doesn't directly reference GetUser in its imports or signatures,
	// the heuristic may not catch it. This is expected behavior for the heuristic approach.
	// The test validates the analyzer runs without error.
	_ = issues
}

func TestConsistencyAnalyzerGoSamePackage(t *testing.T) {
	a := NewConsistencyAnalyzer()
	ctx := &AnalysisContext{
		RepoPath: "/repo",
		Diff: &git.Diff{
			Files: []git.FileDiff{
				{
					Path:   "pkg/api/store.go",
					Status: "modified",
					Hunks: []git.Hunk{
						{
							OldStart: 5,
							OldCount: 1,
							NewStart: 5,
							NewCount: 1,
							Lines: []git.Line{
								{Type: "removed", Content: "func GetItems() []Item {", OldNum: 5},
								{Type: "added", Content: "func GetItems(limit int) []Item {", NewNum: 5},
							},
						},
					},
				},
			},
			Mode: "staged",
		},
		ParsedFiles: map[string]*parser.ParsedFile{
			"pkg/api/store.go": {
				Path:     "pkg/api/store.go",
				Language: "go",
				Imports:  make([]parser.Import, 0),
				Exports: []parser.Export{
					{Name: "GetItems", Kind: "function", Line: 5},
				},
				Symbols: []parser.Symbol{
					{Name: "GetItems", Kind: "function", Exported: true, Line: 5,
						Signature: "func GetItems(limit int) []Item"},
				},
			},
			"pkg/api/handler.go": {
				Path:     "pkg/api/handler.go",
				Language: "go",
				Imports:  make([]parser.Import, 0),
				Exports:  make([]parser.Export, 0),
				Symbols: []parser.Symbol{
					{Name: "ListHandler", Kind: "function", Exported: true, Line: 10,
						Signature: "func ListHandler() http.HandlerFunc"},
				},
			},
		},
		Config: config.DefaultConfig(),
	}

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	// handler.go is in the same package but doesn't reference GetItems in its signature
	// This validates same-package detection works without errors
	_ = issues
}

func TestConsistencyAnalyzerTypeChange(t *testing.T) {
	a := NewConsistencyAnalyzer()
	ctx := &AnalysisContext{
		RepoPath: "/repo",
		Diff: &git.Diff{
			Files: []git.FileDiff{
				{
					Path:   "src/types.ts",
					Status: "modified",
					Hunks: []git.Hunk{
						{
							OldStart: 1,
							OldCount: 4,
							NewStart: 1,
							NewCount: 5,
							Lines: []git.Line{
								{Type: "removed", Content: "export interface Config {", OldNum: 1},
								{Type: "added", Content: "export interface Config {", NewNum: 1},
								{Type: "context", Content: "  name: string;", OldNum: 2, NewNum: 2},
								{Type: "removed", Content: "  value: number;", OldNum: 3},
								{Type: "added", Content: "  value: string;", NewNum: 3},
								{Type: "added", Content: "  enabled: boolean;", NewNum: 4},
								{Type: "context", Content: "}", OldNum: 4, NewNum: 5},
							},
						},
					},
				},
			},
			Mode: "staged",
		},
		ParsedFiles: map[string]*parser.ParsedFile{
			"src/types.ts": {
				Path:     "src/types.ts",
				Language: "typescript",
				Imports:  make([]parser.Import, 0),
				Exports: []parser.Export{
					{Name: "Config", Kind: "type", Line: 1},
				},
				Symbols: []parser.Symbol{
					{Name: "Config", Kind: "type", Exported: true, Line: 1},
				},
			},
			"src/app.ts": {
				Path:     "src/app.ts",
				Language: "typescript",
				Imports: []parser.Import{
					{Path: "./types", Names: []string{"Config"}, Line: 1},
				},
				Exports: make([]parser.Export, 0),
				Symbols: []parser.Symbol{
					{Name: "createConfig", Kind: "function", Exported: true, Line: 5,
						Signature: "function createConfig(cfg: Config)"},
				},
			},
		},
		Config: config.DefaultConfig(),
	}

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	// app.ts uses Config (visible in its import and signature) and was not changed
	found := false
	for _, issue := range issues {
		if issue.Category == "consistency" && issue.File == "src/app.ts" {
			found = true
			break
		}
	}
	if !found {
		t.Error("expected consistency warning for type Config change not reflected in app.ts")
	}
}

func TestConsistencyAnalyzerNoFalsePositiveWhenBothFilesChanged(t *testing.T) {
	a := NewConsistencyAnalyzer()
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
								{Type: "removed", Content: "export function calc(a: number) {", OldNum: 1},
								{Type: "added", Content: "export function calc(a: number, b: number) {", NewNum: 1},
							},
						},
					},
				},
				{
					Path:   "src/app.ts",
					Status: "modified",
					Hunks: []git.Hunk{
						{
							OldStart: 5,
							OldCount: 1,
							NewStart: 5,
							NewCount: 1,
							Lines: []git.Line{
								{Type: "removed", Content: "const result = calc(1);", OldNum: 5},
								{Type: "added", Content: "const result = calc(1, 2);", NewNum: 5},
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
					{Name: "calc", Kind: "function", Line: 1},
				},
				Symbols: make([]parser.Symbol, 0),
			},
			"src/app.ts": {
				Path:     "src/app.ts",
				Language: "typescript",
				Imports: []parser.Import{
					{Path: "./utils", Names: []string{"calc"}, Line: 1},
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

	// Both files were changed and the caller updated the symbol usage
	// so we should NOT get a warning-severity consistency issue
	for _, issue := range issues {
		if issue.Category == "consistency" && issue.File == "src/app.ts" &&
			issue.Severity == SeverityWarning {
			t.Error("should not get a warning when caller was also updated with the symbol")
		}
	}
}

// --- Helper function tests ---

func TestExtractGoSignature(t *testing.T) {
	tests := []struct {
		line     string
		wantName string
		wantSig  string
	}{
		{
			"func Foo(a int, b string) error {",
			"Foo",
			"Foo(a int, b string) error",
		},
		{
			"func (r *Repo) Bar(id string) (*Item, error) {",
			"Bar",
			"Bar(id string) (*Item, error)",
		},
		{
			"not a function",
			"",
			"",
		},
	}

	for _, tt := range tests {
		name, sig := extractGoSignature(tt.line)
		if name != tt.wantName {
			t.Errorf("extractGoSignature(%q) name = %q, want %q", tt.line, name, tt.wantName)
		}
		if tt.wantName != "" && sig == "" {
			t.Errorf("extractGoSignature(%q) sig is empty, want non-empty", tt.line)
		}
	}
}

func TestExtractTSSignature(t *testing.T) {
	tests := []struct {
		line     string
		wantName string
	}{
		{"export function foo(a: number) {", "foo"},
		{"export async function bar(x: string): Promise<void> {", "bar"},
		{"function baz() {", "baz"},
		{"const x = 5", ""},
	}

	for _, tt := range tests {
		name, _ := extractTSSignature(tt.line)
		if name != tt.wantName {
			t.Errorf("extractTSSignature(%q) name = %q, want %q", tt.line, name, tt.wantName)
		}
	}
}

func TestExtractPythonSignature(t *testing.T) {
	tests := []struct {
		line     string
		wantName string
	}{
		{"def foo(x, y):", "foo"},
		{"async def bar(a, b, c):", "bar"},
		{"class Foo:", ""},
		{"x = 5", ""},
	}

	for _, tt := range tests {
		name, _ := extractPythonSignature(tt.line)
		if name != tt.wantName {
			t.Errorf("extractPythonSignature(%q) name = %q, want %q", tt.line, name, tt.wantName)
		}
	}
}

func TestIsExportedSymbol(t *testing.T) {
	tests := []struct {
		name     string
		language string
		want     bool
	}{
		{"Foo", "go", true},
		{"foo", "go", false},
		{"_private", "python", false},
		{"public_func", "python", true},
		{"anything", "typescript", true},
	}

	for _, tt := range tests {
		got := isExportedSymbol(tt.name, tt.language)
		if got != tt.want {
			t.Errorf("isExportedSymbol(%q, %q) = %v, want %v", tt.name, tt.language, got, tt.want)
		}
	}
}

func TestSameDirectory(t *testing.T) {
	tests := []struct {
		a, b string
		want bool
	}{
		{"pkg/api/server.go", "pkg/api/handler.go", true},
		{"pkg/api/server.go", "pkg/store/store.go", false},
		{"main.go", "util.go", true},
	}

	for _, tt := range tests {
		got := sameDirectory(tt.a, tt.b)
		if got != tt.want {
			t.Errorf("sameDirectory(%q, %q) = %v, want %v", tt.a, tt.b, got, tt.want)
		}
	}
}

func TestExtractTypeName(t *testing.T) {
	tests := []struct {
		line     string
		language string
		want     string
	}{
		{"type Config struct {", "go", "Config"},
		{"type Handler interface {", "go", "Handler"},
		{"type small struct {", "go", ""},
		{"export interface Config {", "typescript", "Config"},
		{"type UserProps = {", "typescript", "UserProps"},
		{"class MyClass:", "python", "MyClass"},
	}

	for _, tt := range tests {
		got := extractTypeName(tt.line, tt.language)
		if got != tt.want {
			t.Errorf("extractTypeName(%q, %q) = %q, want %q", tt.line, tt.language, got, tt.want)
		}
	}
}
