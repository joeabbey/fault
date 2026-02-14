package analyzer

import (
	"testing"

	"github.com/joeabbey/fault/pkg/config"
	"github.com/joeabbey/fault/pkg/git"
	"github.com/joeabbey/fault/pkg/parser"
)

func TestAntiPatternAnalyzerName(t *testing.T) {
	a := NewAntiPatternAnalyzer()
	if a.Name() != "patterns" {
		t.Errorf("expected name 'patterns', got %q", a.Name())
	}
}

func TestAntiPatternEmptyDiff(t *testing.T) {
	a := NewAntiPatternAnalyzer()
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

func TestAntiPatternNilDiff(t *testing.T) {
	a := NewAntiPatternAnalyzer()
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

// --- TODO/Placeholder tests ---

func TestAntiPatternTODOComment(t *testing.T) {
	a := NewAntiPatternAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "main.go", line: 10, content: "	// TODO: implement this"},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "patterns/todo-placeholder")
	if found == nil {
		t.Fatal("expected patterns/todo-placeholder issue, found none")
	}
	if found.Severity != SeverityWarning {
		t.Errorf("expected warning severity, got %q", found.Severity)
	}
}

func TestAntiPatternPythonTODO(t *testing.T) {
	a := NewAntiPatternAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "app.py", line: 5, content: "    # TODO fix this later"},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "patterns/todo-placeholder")
	if found == nil {
		t.Fatal("expected patterns/todo-placeholder issue")
	}
}

func TestAntiPatternRaiseNotImplemented(t *testing.T) {
	a := NewAntiPatternAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "service.py", line: 8, content: "        raise NotImplementedError"},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "patterns/todo-placeholder")
	if found == nil {
		t.Fatal("expected patterns/todo-placeholder issue for raise NotImplementedError")
	}
}

func TestAntiPatternThrowNotImplemented(t *testing.T) {
	a := NewAntiPatternAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "handler.ts", line: 15, content: `    throw new Error('not implemented');`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "patterns/todo-placeholder")
	if found == nil {
		t.Fatal("expected patterns/todo-placeholder issue for throw new Error")
	}
}

func TestAntiPatternPanicNotImplemented(t *testing.T) {
	a := NewAntiPatternAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "handler.go", line: 20, content: `	panic("not implemented")`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "patterns/todo-placeholder")
	if found == nil {
		t.Fatal("expected patterns/todo-placeholder issue for panic")
	}
}

func TestAntiPatternStandalonePythonPass(t *testing.T) {
	a := NewAntiPatternAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "service.py", line: 3, content: "    pass"},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "patterns/todo-placeholder")
	if found == nil {
		t.Fatal("expected patterns/todo-placeholder issue for standalone pass")
	}
}

func TestAntiPatternPassInNonPythonIgnored(t *testing.T) {
	a := NewAntiPatternAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "main.go", line: 3, content: "    pass"},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	// "pass" by itself in a Go file should not trigger.
	found := findIssueByID(issues, "patterns/todo-placeholder")
	if found != nil {
		t.Errorf("did not expect placeholder issue for 'pass' in .go file")
	}
}

// --- Hardcoded credential tests ---

func TestAntiPatternHardcodedSKKey(t *testing.T) {
	a := NewAntiPatternAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "config.ts", line: 3, content: `const apiKey = "sk-abcdefghijklmnopqrstuvwxyz1234";`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "patterns/hardcoded-credential")
	if found == nil {
		t.Fatal("expected hardcoded credential issue for sk- key")
	}
	if found.Severity != SeverityError {
		t.Errorf("expected error severity, got %q", found.Severity)
	}
}

func TestAntiPatternHardcodedAWSKey(t *testing.T) {
	a := NewAntiPatternAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "config.go", line: 7, content: `	awsKey := "AKIAIOSFODNN7EXAMPLE"`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "patterns/hardcoded-credential")
	if found == nil {
		t.Fatal("expected hardcoded credential issue for AKIA key")
	}
}

func TestAntiPatternHardcodedGitHubToken(t *testing.T) {
	a := NewAntiPatternAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "ci.go", line: 12, content: `	token := "ghp_ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijkl"`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "patterns/hardcoded-credential")
	if found == nil {
		t.Fatal("expected hardcoded credential issue for ghp_ token")
	}
}

func TestAntiPatternHardcodedPassword(t *testing.T) {
	a := NewAntiPatternAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "db.go", line: 5, content: `	password = "supersecret123"`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "patterns/hardcoded-credential")
	if found == nil {
		t.Fatal("expected hardcoded credential issue for password")
	}
}

func TestAntiPatternEmptyPasswordIgnored(t *testing.T) {
	a := NewAntiPatternAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "db.go", line: 5, content: `	password = ""`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "patterns/hardcoded-credential")
	if found != nil {
		t.Errorf("did not expect credential issue for empty password")
	}
}

// --- Console/debug artifact tests ---

func TestAntiPatternConsoleLog(t *testing.T) {
	a := NewAntiPatternAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "app.ts", line: 20, content: `  console.log("debug value", data);`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "patterns/console-debug")
	if found == nil {
		t.Fatal("expected console-debug issue for console.log")
	}
	if found.Severity != SeverityInfo {
		t.Errorf("expected info severity, got %q", found.Severity)
	}
}

func TestAntiPatternFmtPrintln(t *testing.T) {
	a := NewAntiPatternAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "main.go", line: 15, content: `	fmt.Println("debug:", value)`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "patterns/console-debug")
	if found == nil {
		t.Fatal("expected console-debug issue for fmt.Println")
	}
}

func TestAntiPatternPythonPrint(t *testing.T) {
	a := NewAntiPatternAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "app.py", line: 10, content: `    print("debug:", result)`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "patterns/console-debug")
	if found == nil {
		t.Fatal("expected console-debug issue for print()")
	}
}

func TestAntiPatternCommentedConsoleLogIgnored(t *testing.T) {
	a := NewAntiPatternAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "app.ts", line: 20, content: `  // console.log("debug value", data);`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "patterns/console-debug")
	if found != nil {
		t.Errorf("did not expect console-debug for commented-out console.log")
	}
}

// --- Commented-out code tests ---

func TestAntiPatternCommentedOutCode(t *testing.T) {
	a := NewAntiPatternAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "main.go", line: 10, content: "// oldFunc()"},
		{file: "main.go", line: 11, content: "// oldFunc2()"},
		{file: "main.go", line: 12, content: "// oldFunc3()"},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "patterns/commented-code")
	if found == nil {
		t.Fatal("expected commented-code issue for 3+ consecutive comment lines")
	}
	if found.Severity != SeverityWarning {
		t.Errorf("expected warning severity, got %q", found.Severity)
	}
	if found.Line != 10 {
		t.Errorf("expected issue at line 10, got %d", found.Line)
	}
}

func TestAntiPatternTwoCommentLinesNoIssue(t *testing.T) {
	a := NewAntiPatternAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "main.go", line: 10, content: "// comment 1"},
		{file: "main.go", line: 11, content: "// comment 2"},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "patterns/commented-code")
	if found != nil {
		t.Errorf("did not expect commented-code issue for only 2 comment lines")
	}
}

// --- Unreachable code tests ---

func TestAntiPatternUnreachableAfterReturn(t *testing.T) {
	a := NewAntiPatternAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "handler.go", line: 20, content: "	return nil"},
		{file: "handler.go", line: 21, content: "	doSomethingElse()"},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "patterns/unreachable-code")
	if found == nil {
		t.Fatal("expected unreachable-code issue after return")
	}
	if found.Line != 21 {
		t.Errorf("expected issue at line 21, got %d", found.Line)
	}
}

func TestAntiPatternUnreachableAfterThrow(t *testing.T) {
	a := NewAntiPatternAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "handler.ts", line: 10, content: "    throw new Error('bad');"},
		{file: "handler.ts", line: 11, content: "    doMore();"},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "patterns/unreachable-code")
	if found == nil {
		t.Fatal("expected unreachable-code issue after throw")
	}
}

func TestAntiPatternReturnFollowedByCloseBraceOK(t *testing.T) {
	a := NewAntiPatternAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "handler.go", line: 20, content: "	return nil"},
		{file: "handler.go", line: 21, content: "}"},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "patterns/unreachable-code")
	if found != nil {
		t.Errorf("did not expect unreachable-code when next line is closing brace")
	}
}

func TestAntiPatternDeletedFilesSkipped(t *testing.T) {
	a := NewAntiPatternAnalyzer()
	ctx := &AnalysisContext{
		RepoPath: "/nonexistent",
		Diff: &git.Diff{Files: []git.FileDiff{
			{
				Path:   "old.go",
				Status: "deleted",
				Hunks: []git.Hunk{{
					Lines: []git.Line{
						{Type: "added", Content: "	// TODO: fix", NewNum: 1},
					},
				}},
			},
		}},
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

func TestAntiPatternRemovedLinesIgnored(t *testing.T) {
	a := NewAntiPatternAnalyzer()
	ctx := &AnalysisContext{
		RepoPath: "/nonexistent",
		Diff: &git.Diff{Files: []git.FileDiff{
			{
				Path:   "main.go",
				Status: "modified",
				Hunks: []git.Hunk{{
					Lines: []git.Line{
						{Type: "removed", Content: "	// TODO: fix", OldNum: 1},
						{Type: "removed", Content: `	password = "secret123"`, OldNum: 2},
					},
				}},
			},
		}},
		ParsedFiles: make(map[string]*parser.ParsedFile),
		Config:      config.DefaultConfig(),
	}

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(issues) != 0 {
		t.Errorf("expected 0 issues for removed lines, got %d", len(issues))
	}
}

// --- Helpers ---

type addedLine struct {
	file    string
	line    int
	content string
}

// makePatternContext builds an AnalysisContext with added lines grouped by file.
func makePatternContext(lines []addedLine) *AnalysisContext {
	fileMap := make(map[string][]git.Line)
	for _, l := range lines {
		fileMap[l.file] = append(fileMap[l.file], git.Line{
			Type:    "added",
			Content: l.content,
			NewNum:  l.line,
		})
	}

	files := make([]git.FileDiff, 0, len(fileMap))
	for path, gitLines := range fileMap {
		files = append(files, git.FileDiff{
			Path:   path,
			Status: "modified",
			Hunks:  []git.Hunk{{Lines: gitLines}},
		})
	}

	return &AnalysisContext{
		RepoPath:    "/nonexistent",
		Diff:        &git.Diff{Files: files},
		ParsedFiles: make(map[string]*parser.ParsedFile),
		Config:      config.DefaultConfig(),
	}
}

func findIssueByID(issues []Issue, id string) *Issue {
	for i := range issues {
		if issues[i].ID == id {
			return &issues[i]
		}
	}
	return nil
}
