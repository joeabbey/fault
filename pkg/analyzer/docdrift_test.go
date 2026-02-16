package analyzer

import (
	"testing"

	"github.com/joeabbey/fault/pkg/config"
	"github.com/joeabbey/fault/pkg/git"
	"github.com/joeabbey/fault/pkg/parser"
)

func TestDocDriftAnalyzerName(t *testing.T) {
	a := NewDocDriftAnalyzer()
	if a.Name() != "docdrift" {
		t.Errorf("expected name %q, got %q", "docdrift", a.Name())
	}
}

func TestDocDriftNilDiff(t *testing.T) {
	a := NewDocDriftAnalyzer()
	ctx := &AnalysisContext{
		RepoPath:    "/repo",
		Diff:        nil,
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

// TestDocDriftGoParamChange: Go func params changed, doc unchanged -> flagged
func TestDocDriftGoParamChange(t *testing.T) {
	a := NewDocDriftAnalyzer()
	ctx := &AnalysisContext{
		RepoPath: "/repo",
		Diff: &git.Diff{
			Files: []git.FileDiff{
				{
					Path:   "pkg/store/store.go",
					Status: "modified",
					Hunks: []git.Hunk{
						{
							OldStart: 8,
							OldCount: 8,
							NewStart: 8,
							NewCount: 8,
							Lines: []git.Line{
								// Unchanged doc comment
								{Type: "context", Content: "// GetUser retrieves a user by their ID.", OldNum: 8, NewNum: 8},
								{Type: "context", Content: "// The id parameter specifies the unique user identifier.", OldNum: 9, NewNum: 9},
								// Signature change: added includeProfile param
								{Type: "removed", Content: "func GetUser(id string) (*User, error) {", OldNum: 10},
								{Type: "added", Content: "func GetUser(id string, includeProfile bool) (*User, error) {", NewNum: 10},
								// Body
								{Type: "context", Content: "	return nil, nil", OldNum: 11, NewNum: 11},
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
			},
		},
		Config: config.DefaultConfig(),
	}

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	if len(issues) == 0 {
		t.Fatal("expected at least 1 issue for stale doc, got 0")
	}

	foundStaleDoc := false
	for _, issue := range issues {
		if issue.Category == "docdrift" {
			foundStaleDoc = true
			if issue.File != "pkg/store/store.go" {
				t.Errorf("expected file %q, got %q", "pkg/store/store.go", issue.File)
			}
			break
		}
	}
	if !foundStaleDoc {
		t.Error("expected a docdrift issue for unchanged doc above changed signature")
	}
}

// TestDocDriftGoNewFunc: entirely new function with doc -> NOT flagged
func TestDocDriftGoNewFunc(t *testing.T) {
	a := NewDocDriftAnalyzer()
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
							OldCount: 0,
							NewStart: 20,
							NewCount: 5,
							Lines: []git.Line{
								// New doc + function (all added lines)
								{Type: "added", Content: "// NewFunc does something new.", NewNum: 20},
								{Type: "added", Content: "// It takes a context and returns an error.", NewNum: 21},
								{Type: "added", Content: "func NewFunc(ctx context.Context) error {", NewNum: 22},
								{Type: "added", Content: "	return nil", NewNum: 23},
								{Type: "added", Content: "}", NewNum: 24},
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
			},
		},
		Config: config.DefaultConfig(),
	}

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	for _, issue := range issues {
		if issue.Category == "docdrift" {
			t.Errorf("should not flag brand new function, got issue: %s", issue.Message)
		}
	}
}

// TestDocDriftGoDocUpdated: both signature and doc changed -> NOT flagged
func TestDocDriftGoDocUpdated(t *testing.T) {
	a := NewDocDriftAnalyzer()
	ctx := &AnalysisContext{
		RepoPath: "/repo",
		Diff: &git.Diff{
			Files: []git.FileDiff{
				{
					Path:   "pkg/store/store.go",
					Status: "modified",
					Hunks: []git.Hunk{
						{
							OldStart: 8,
							OldCount: 6,
							NewStart: 8,
							NewCount: 6,
							Lines: []git.Line{
								// Doc was ALSO changed (removed and added)
								{Type: "removed", Content: "// GetUser retrieves a user by their ID.", OldNum: 8},
								{Type: "added", Content: "// GetUser retrieves a user by their ID, optionally including profile.", NewNum: 8},
								{Type: "removed", Content: "// The id parameter specifies the unique user identifier.", OldNum: 9},
								{Type: "added", Content: "// The id parameter specifies the user, includeProfile includes profile data.", NewNum: 9},
								// Signature change
								{Type: "removed", Content: "func GetUser(id string) (*User, error) {", OldNum: 10},
								{Type: "added", Content: "func GetUser(id string, includeProfile bool) (*User, error) {", NewNum: 10},
								// Body
								{Type: "context", Content: "	return nil, nil", OldNum: 11, NewNum: 11},
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
			},
		},
		Config: config.DefaultConfig(),
	}

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	for _, issue := range issues {
		if issue.Category == "docdrift" {
			t.Errorf("should not flag when doc was also updated, got issue: %s", issue.Message)
		}
	}
}

// TestDocDriftPythonDocstring: Python def changed, docstring stale -> flagged
func TestDocDriftPythonDocstring(t *testing.T) {
	a := NewDocDriftAnalyzer()
	ctx := &AnalysisContext{
		RepoPath: "/repo",
		Diff: &git.Diff{
			Files: []git.FileDiff{
				{
					Path:   "src/service.py",
					Status: "modified",
					Hunks: []git.Hunk{
						{
							OldStart: 10,
							OldCount: 6,
							NewStart: 10,
							NewCount: 6,
							Lines: []git.Line{
								// Unchanged docstring-style comment
								{Type: "context", Content: "# Fetch data from the given url.", OldNum: 10, NewNum: 10},
								{Type: "context", Content: "# Args: url - the endpoint to query", OldNum: 11, NewNum: 11},
								// Signature change: added timeout param
								{Type: "removed", Content: "def fetch_data(url):", OldNum: 12},
								{Type: "added", Content: "def fetch_data(url, timeout=30):", NewNum: 12},
								// Body
								{Type: "context", Content: "    pass", OldNum: 13, NewNum: 13},
							},
						},
					},
				},
			},
			Mode: "staged",
		},
		ParsedFiles: map[string]*parser.ParsedFile{
			"src/service.py": {
				Path:     "src/service.py",
				Language: "python",
			},
		},
		Config: config.DefaultConfig(),
	}

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	foundDocDrift := false
	for _, issue := range issues {
		if issue.Category == "docdrift" && issue.File == "src/service.py" {
			foundDocDrift = true
			break
		}
	}
	if !foundDocDrift {
		t.Error("expected docdrift issue for Python function with stale docstring")
	}
}

// TestDocDriftTypeScript: TS function changed, JSDoc stale -> flagged
func TestDocDriftTypeScript(t *testing.T) {
	a := NewDocDriftAnalyzer()
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
							OldCount: 8,
							NewStart: 5,
							NewCount: 8,
							Lines: []git.Line{
								// Unchanged JSDoc
								{Type: "context", Content: "/**", OldNum: 5, NewNum: 5},
								{Type: "context", Content: " * Calculate the sum of two numbers.", OldNum: 6, NewNum: 6},
								{Type: "context", Content: " * @param a - first number", OldNum: 7, NewNum: 7},
								{Type: "context", Content: " * @param b - second number", OldNum: 8, NewNum: 8},
								{Type: "context", Content: " */", OldNum: 9, NewNum: 9},
								// Signature change
								{Type: "removed", Content: "export function calculate(a: number, b: number): number {", OldNum: 10},
								{Type: "added", Content: "export function calculate(a: number, b: number, precision: number): number {", NewNum: 10},
								// Body
								{Type: "context", Content: "  return a + b;", OldNum: 11, NewNum: 11},
								{Type: "context", Content: "}", OldNum: 12, NewNum: 12},
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
			},
		},
		Config: config.DefaultConfig(),
	}

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	foundDocDrift := false
	for _, issue := range issues {
		if issue.Category == "docdrift" && issue.File == "src/utils.ts" {
			foundDocDrift = true
			break
		}
	}
	if !foundDocDrift {
		t.Error("expected docdrift issue for TypeScript function with stale JSDoc")
	}
}

// TestDocDriftNoDoc: function changed but had no doc -> NOT flagged
func TestDocDriftNoDoc(t *testing.T) {
	a := NewDocDriftAnalyzer()
	ctx := &AnalysisContext{
		RepoPath: "/repo",
		Diff: &git.Diff{
			Files: []git.FileDiff{
				{
					Path:   "pkg/handler.go",
					Status: "modified",
					Hunks: []git.Hunk{
						{
							OldStart: 5,
							OldCount: 4,
							NewStart: 5,
							NewCount: 4,
							Lines: []git.Line{
								// No comment above — just code context
								{Type: "context", Content: "", OldNum: 5, NewNum: 5},
								// Signature change
								{Type: "removed", Content: "func Handle(w http.ResponseWriter, r *http.Request) {", OldNum: 6},
								{Type: "added", Content: "func Handle(w http.ResponseWriter, r *http.Request, db *DB) {", NewNum: 6},
								// Body
								{Type: "context", Content: "	w.WriteHeader(200)", OldNum: 7, NewNum: 7},
								{Type: "context", Content: "}", OldNum: 8, NewNum: 8},
							},
						},
					},
				},
			},
			Mode: "staged",
		},
		ParsedFiles: map[string]*parser.ParsedFile{
			"pkg/handler.go": {
				Path:     "pkg/handler.go",
				Language: "go",
			},
		},
		Config: config.DefaultConfig(),
	}

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	for _, issue := range issues {
		if issue.Category == "docdrift" {
			t.Errorf("should not flag function with no doc comment, got: %s", issue.Message)
		}
	}
}

// TestDocDriftOnlyBodyChange: function body changed but signature same -> NOT flagged
func TestDocDriftOnlyBodyChange(t *testing.T) {
	a := NewDocDriftAnalyzer()
	ctx := &AnalysisContext{
		RepoPath: "/repo",
		Diff: &git.Diff{
			Files: []git.FileDiff{
				{
					Path:   "pkg/store/store.go",
					Status: "modified",
					Hunks: []git.Hunk{
						{
							OldStart: 8,
							OldCount: 6,
							NewStart: 8,
							NewCount: 7,
							Lines: []git.Line{
								// Doc comment unchanged
								{Type: "context", Content: "// GetUser retrieves a user by their ID.", OldNum: 8, NewNum: 8},
								// Signature unchanged
								{Type: "context", Content: "func GetUser(id string) (*User, error) {", OldNum: 9, NewNum: 9},
								// Body changed
								{Type: "removed", Content: "	return db.Query(id)", OldNum: 10},
								{Type: "added", Content: "	user, err := db.Query(id)", NewNum: 10},
								{Type: "added", Content: "	if err != nil { return nil, err }", NewNum: 11},
								{Type: "added", Content: "	return user, nil", NewNum: 12},
								{Type: "context", Content: "}", OldNum: 11, NewNum: 13},
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
			},
		},
		Config: config.DefaultConfig(),
	}

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	for _, issue := range issues {
		if issue.Category == "docdrift" {
			t.Errorf("should not flag when only body changed (signature unchanged), got: %s", issue.Message)
		}
	}
}

// TestDocDriftGoParamMismatch: old param name referenced in doc but no longer in signature
func TestDocDriftGoParamMismatch(t *testing.T) {
	a := NewDocDriftAnalyzer()
	ctx := &AnalysisContext{
		RepoPath: "/repo",
		Diff: &git.Diff{
			Files: []git.FileDiff{
				{
					Path:   "pkg/api/handler.go",
					Status: "modified",
					Hunks: []git.Hunk{
						{
							OldStart: 15,
							OldCount: 6,
							NewStart: 15,
							NewCount: 6,
							Lines: []git.Line{
								// Doc references "userID" which will be renamed to "id"
								{Type: "context", Content: "// FetchProfile fetches the profile for the given userID.", OldNum: 15, NewNum: 15},
								{Type: "context", Content: "// The userID must be a valid UUID string.", OldNum: 16, NewNum: 16},
								// Param renamed from userID to id
								{Type: "removed", Content: "func FetchProfile(userID string) (*Profile, error) {", OldNum: 17},
								{Type: "added", Content: "func FetchProfile(id string) (*Profile, error) {", NewNum: 17},
								{Type: "context", Content: "	return nil, nil", OldNum: 18, NewNum: 18},
								{Type: "context", Content: "}", OldNum: 19, NewNum: 19},
							},
						},
					},
				},
			},
			Mode: "staged",
		},
		ParsedFiles: map[string]*parser.ParsedFile{
			"pkg/api/handler.go": {
				Path:     "pkg/api/handler.go",
				Language: "go",
			},
		},
		Config: config.DefaultConfig(),
	}

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	foundParamMismatch := false
	for _, issue := range issues {
		if issue.Category == "docdrift" && issue.Severity == SeverityWarning {
			if issue.File == "pkg/api/handler.go" {
				foundParamMismatch = true
				break
			}
		}
	}
	if !foundParamMismatch {
		t.Error("expected docdrift param-mismatch issue when doc references old param name")
	}
}

// TestDocDriftRustFuncChange: Rust fn changed, doc unchanged -> flagged
func TestDocDriftRustFuncChange(t *testing.T) {
	a := NewDocDriftAnalyzer()
	ctx := &AnalysisContext{
		RepoPath: "/repo",
		Diff: &git.Diff{
			Files: []git.FileDiff{
				{
					Path:   "src/lib.rs",
					Status: "modified",
					Hunks: []git.Hunk{
						{
							OldStart: 10,
							OldCount: 6,
							NewStart: 10,
							NewCount: 6,
							Lines: []git.Line{
								{Type: "context", Content: "/// Processes the given input string.", OldNum: 10, NewNum: 10},
								{Type: "context", Content: "/// Returns the processed result.", OldNum: 11, NewNum: 11},
								{Type: "removed", Content: "pub fn process(input: &str) -> Result<String, Error> {", OldNum: 12},
								{Type: "added", Content: "pub fn process(input: &str, opts: &Options) -> Result<String, Error> {", NewNum: 12},
								{Type: "context", Content: "    Ok(input.to_string())", OldNum: 13, NewNum: 13},
								{Type: "context", Content: "}", OldNum: 14, NewNum: 14},
							},
						},
					},
				},
			},
			Mode: "staged",
		},
		ParsedFiles: map[string]*parser.ParsedFile{
			"src/lib.rs": {
				Path:     "src/lib.rs",
				Language: "rust",
			},
		},
		Config: config.DefaultConfig(),
	}

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	foundDocDrift := false
	for _, issue := range issues {
		if issue.Category == "docdrift" && issue.File == "src/lib.rs" {
			foundDocDrift = true
			break
		}
	}
	if !foundDocDrift {
		t.Error("expected docdrift issue for Rust function with stale doc comment")
	}
}

// TestDocDriftReturnTypeChange: return type changed but doc not updated
func TestDocDriftReturnTypeChange(t *testing.T) {
	a := NewDocDriftAnalyzer()
	ctx := &AnalysisContext{
		RepoPath: "/repo",
		Diff: &git.Diff{
			Files: []git.FileDiff{
				{
					Path:   "pkg/store/store.go",
					Status: "modified",
					Hunks: []git.Hunk{
						{
							OldStart: 8,
							OldCount: 5,
							NewStart: 8,
							NewCount: 5,
							Lines: []git.Line{
								{Type: "context", Content: "// ListUsers returns all users.", OldNum: 8, NewNum: 8},
								{Type: "removed", Content: "func ListUsers() []User {", OldNum: 9},
								{Type: "added", Content: "func ListUsers() ([]*User, error) {", NewNum: 9},
								{Type: "context", Content: "	return nil", OldNum: 10, NewNum: 10},
								{Type: "context", Content: "}", OldNum: 11, NewNum: 11},
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
			},
		},
		Config: config.DefaultConfig(),
	}

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	foundReturnChange := false
	foundStaleDoc := false
	for _, issue := range issues {
		if issue.Category == "docdrift" {
			if issue.ID == "docdrift-return-type-pkg/store/store.go-ListUsers-9" {
				foundReturnChange = true
			}
			if issue.ID == "docdrift-stale-doc-pkg/store/store.go-ListUsers-9" {
				foundStaleDoc = true
			}
		}
	}
	if !foundReturnChange {
		t.Error("expected docdrift return-type issue when return type changed")
	}
	if !foundStaleDoc {
		t.Error("expected docdrift stale-doc issue")
	}
}

// TestDocDriftDeletedFile: deleted files should be skipped
func TestDocDriftDeletedFile(t *testing.T) {
	a := NewDocDriftAnalyzer()
	ctx := &AnalysisContext{
		RepoPath: "/repo",
		Diff: &git.Diff{
			Files: []git.FileDiff{
				{
					Path:   "pkg/old.go",
					Status: "deleted",
					Hunks: []git.Hunk{
						{
							OldStart: 1,
							OldCount: 5,
							NewStart: 0,
							NewCount: 0,
							Lines: []git.Line{
								{Type: "removed", Content: "// OldFunc does old things.", OldNum: 1},
								{Type: "removed", Content: "func OldFunc() {}", OldNum: 2},
							},
						},
					},
				},
			},
			Mode: "staged",
		},
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

// TestDocDriftTestFile: test files should be skipped
func TestDocDriftTestFile(t *testing.T) {
	a := NewDocDriftAnalyzer()
	ctx := &AnalysisContext{
		RepoPath: "/repo",
		Diff: &git.Diff{
			Files: []git.FileDiff{
				{
					Path:   "pkg/store/store_test.go",
					Status: "modified",
					Hunks: []git.Hunk{
						{
							OldStart: 8,
							OldCount: 5,
							NewStart: 8,
							NewCount: 5,
							Lines: []git.Line{
								{Type: "context", Content: "// TestGetUser tests getting a user.", OldNum: 8, NewNum: 8},
								{Type: "removed", Content: "func TestGetUser(t *testing.T) {", OldNum: 9},
								{Type: "added", Content: "func TestGetUser(t *testing.T, db *DB) {", NewNum: 9},
								{Type: "context", Content: "}", OldNum: 10, NewNum: 10},
							},
						},
					},
				},
			},
			Mode: "staged",
		},
		ParsedFiles: make(map[string]*parser.ParsedFile),
		Config:      config.DefaultConfig(),
	}

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(issues) != 0 {
		t.Errorf("expected 0 issues for test file, got %d", len(issues))
	}
}

// TestDocDriftGoMethodReceiver: method with receiver changed, doc stale
func TestDocDriftGoMethodReceiver(t *testing.T) {
	a := NewDocDriftAnalyzer()
	ctx := &AnalysisContext{
		RepoPath: "/repo",
		Diff: &git.Diff{
			Files: []git.FileDiff{
				{
					Path:   "pkg/server/server.go",
					Status: "modified",
					Hunks: []git.Hunk{
						{
							OldStart: 20,
							OldCount: 5,
							NewStart: 20,
							NewCount: 5,
							Lines: []git.Line{
								{Type: "context", Content: "// Start starts the server on the given address.", OldNum: 20, NewNum: 20},
								{Type: "removed", Content: "func (s *Server) Start(addr string) error {", OldNum: 21},
								{Type: "added", Content: "func (s *Server) Start(addr string, tlsConfig *tls.Config) error {", NewNum: 21},
								{Type: "context", Content: "	return nil", OldNum: 22, NewNum: 22},
								{Type: "context", Content: "}", OldNum: 23, NewNum: 23},
							},
						},
					},
				},
			},
			Mode: "staged",
		},
		ParsedFiles: map[string]*parser.ParsedFile{
			"pkg/server/server.go": {
				Path:     "pkg/server/server.go",
				Language: "go",
			},
		},
		Config: config.DefaultConfig(),
	}

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	foundDocDrift := false
	for _, issue := range issues {
		if issue.Category == "docdrift" && issue.File == "pkg/server/server.go" {
			foundDocDrift = true
			break
		}
	}
	if !foundDocDrift {
		t.Error("expected docdrift issue for Go method with stale doc comment")
	}
}

// TestDocDriftMultipleFuncsInOneHunk: multiple functions changed in same hunk
func TestDocDriftMultipleFuncsInOneHunk(t *testing.T) {
	a := NewDocDriftAnalyzer()
	ctx := &AnalysisContext{
		RepoPath: "/repo",
		Diff: &git.Diff{
			Files: []git.FileDiff{
				{
					Path:   "pkg/api/api.go",
					Status: "modified",
					Hunks: []git.Hunk{
						{
							OldStart: 5,
							OldCount: 12,
							NewStart: 5,
							NewCount: 12,
							Lines: []git.Line{
								// First function: doc stale
								{Type: "context", Content: "// Create makes a new resource.", OldNum: 5, NewNum: 5},
								{Type: "removed", Content: "func Create(name string) error {", OldNum: 6},
								{Type: "added", Content: "func Create(ctx context.Context, name string) error {", NewNum: 6},
								{Type: "context", Content: "	return nil", OldNum: 7, NewNum: 7},
								{Type: "context", Content: "}", OldNum: 8, NewNum: 8},
								{Type: "context", Content: "", OldNum: 9, NewNum: 9},
								// Second function: doc also stale
								{Type: "context", Content: "// Delete removes a resource by name.", OldNum: 10, NewNum: 10},
								{Type: "removed", Content: "func Delete(name string) error {", OldNum: 11},
								{Type: "added", Content: "func Delete(ctx context.Context, name string) error {", NewNum: 11},
								{Type: "context", Content: "	return nil", OldNum: 12, NewNum: 12},
								{Type: "context", Content: "}", OldNum: 13, NewNum: 13},
							},
						},
					},
				},
			},
			Mode: "staged",
		},
		ParsedFiles: map[string]*parser.ParsedFile{
			"pkg/api/api.go": {
				Path:     "pkg/api/api.go",
				Language: "go",
			},
		},
		Config: config.DefaultConfig(),
	}

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	docDriftCount := 0
	for _, issue := range issues {
		if issue.Category == "docdrift" {
			docDriftCount++
		}
	}
	if docDriftCount < 2 {
		t.Errorf("expected at least 2 docdrift issues for two changed functions, got %d", docDriftCount)
	}
}

// --- Helper function tests ---

func TestExtractFuncName(t *testing.T) {
	tests := []struct {
		line string
		ext  string
		want string
	}{
		{"func Foo(a int) error {", ".go", "Foo"},
		{"func (s *Server) Bar(id string) {", ".go", "Bar"},
		{"func main() {", ".go", "main"},
		{"def fetch_data(url):", ".py", "fetch_data"},
		{"async def process(data):", ".py", "process"},
		{"export function calculate(a: number): number {", ".ts", "calculate"},
		{"function helper() {", ".js", "helper"},
		{"pub fn process(input: &str) -> Result<String> {", ".rs", "process"},
		{"fn internal() {", ".rs", "internal"},
		{"def my_method(arg)", ".rb", "my_method"},
		{"fun processData(input: String): String {", ".kt", "processData"},
		{"func swiftFunc(name: String) -> Bool {", ".swift", "swiftFunc"},
		{"public function handle($request) {", ".php", "handle"},
		{"not a function at all", ".go", ""},
	}

	for _, tt := range tests {
		got := extractFuncName(tt.line, tt.ext)
		if got != tt.want {
			t.Errorf("extractFuncName(%q, %q) = %q, want %q", tt.line, tt.ext, got, tt.want)
		}
	}
}

func TestExtractParams(t *testing.T) {
	tests := []struct {
		line string
		ext  string
		want []string
	}{
		{"func Foo(a int, b string) error {", ".go", []string{"a", "b"}},
		{"func (s *Server) Bar(ctx context.Context, id string) {", ".go", []string{"ctx", "id"}},
		{"func NoParams() {", ".go", []string{}},
		{"def fetch(url, timeout=30):", ".py", []string{"url", "timeout"}},
		{"def method(self, name: str):", ".py", []string{"self", "name"}},
		{"export function calc(a: number, b: number): number {", ".ts", []string{"a", "b"}},
		{"pub fn process(input: &str, opts: &Options) -> Result {", ".rs", []string{"input", "opts"}},
	}

	for _, tt := range tests {
		got := extractParams(tt.line, tt.ext)
		if len(got) != len(tt.want) {
			t.Errorf("extractParams(%q, %q) = %v (len %d), want %v (len %d)",
				tt.line, tt.ext, got, len(got), tt.want, len(tt.want))
			continue
		}
		for i := range got {
			if got[i] != tt.want[i] {
				t.Errorf("extractParams(%q, %q)[%d] = %q, want %q",
					tt.line, tt.ext, i, got[i], tt.want[i])
			}
		}
	}
}

func TestIsCommentLine(t *testing.T) {
	tests := []struct {
		line string
		ext  string
		want bool
	}{
		{"// This is a Go comment", ".go", true},
		{"  // Indented comment", ".go", true},
		{"/* Block comment */", ".go", true},
		{" * continuation", ".go", true},
		{"/// Doc comment", ".rs", true},
		{"# Python comment", ".py", true},
		{"func NotAComment() {", ".go", false},
		{"", ".go", false},
		{"  code here  ", ".go", false},
	}

	for _, tt := range tests {
		got := isDocComment(tt.line, tt.ext)
		if got != tt.want {
			t.Errorf("isDocComment(%q, %q) = %v, want %v", tt.line, tt.ext, got, tt.want)
		}
	}
}

func TestContainsWord(t *testing.T) {
	tests := []struct {
		text string
		word string
		want bool
	}{
		{"the userID is required", "userID", true},
		{"pass the id parameter", "id", true},
		{"invalid identifier", "id", false}, // "id" is part of "invalid" and "identifier"
		{"", "test", false},
		{"test this", "", false},
	}

	for _, tt := range tests {
		got := containsWord(tt.text, tt.word)
		if got != tt.want {
			t.Errorf("containsWord(%q, %q) = %v, want %v", tt.text, tt.word, got, tt.want)
		}
	}
}

func TestExtractReturnType(t *testing.T) {
	tests := []struct {
		line string
		ext  string
		want string
	}{
		{"func Foo(a int) error {", ".go", "error"},
		{"func Bar(id string) (*User, error) {", ".go", "(*User, error)"},
		{"func NoReturn() {", ".go", ""},
		{"pub fn process(input: &str) -> Result<String, Error> {", ".rs", "Result<String, Error>"},
		{"def foo(x) -> int:", ".py", "int"},
	}

	for _, tt := range tests {
		got := extractReturnType(tt.line, tt.ext)
		if got != tt.want {
			t.Errorf("extractReturnType(%q, %q) = %q, want %q", tt.line, tt.ext, got, tt.want)
		}
	}
}
