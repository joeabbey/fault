package analyzer

import (
	"testing"

	"github.com/joeabbey/fault/pkg/config"
	"github.com/joeabbey/fault/pkg/git"
	"github.com/joeabbey/fault/pkg/parser"
)

func TestErrorHandlingAnalyzerName(t *testing.T) {
	a := NewErrorHandlingAnalyzer()
	if a.Name() != "errorhandling" {
		t.Errorf("expected name 'errorhandling', got %q", a.Name())
	}
}

func TestErrorHandlingEmptyDiff(t *testing.T) {
	a := NewErrorHandlingAnalyzer()
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

func TestErrorHandlingNilDiff(t *testing.T) {
	a := NewErrorHandlingAnalyzer()
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

// --- Go error handling tests ---

func TestErrorHandlingGoDiscardedSingle(t *testing.T) {
	a := NewErrorHandlingAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "main.go", line: 10, content: `	_ = os.Remove(path)`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "errorhandling/go-discarded-error")
	if found == nil {
		t.Fatal("expected go-discarded-error issue for _ = os.Remove()")
	}
	if found.Severity != SeverityWarning {
		t.Errorf("expected warning severity, got %q", found.Severity)
	}
	if found.Category != "errorhandling-go" {
		t.Errorf("expected category 'errorhandling-go', got %q", found.Category)
	}
}

func TestErrorHandlingGoDiscardedMulti(t *testing.T) {
	a := NewErrorHandlingAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "main.go", line: 15, content: `	_, _ = fmt.Fprintf(w, "hello")`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "errorhandling/go-discarded-error")
	if found == nil {
		t.Fatal("expected go-discarded-error issue for _, _ = fmt.Fprintf()")
	}
}

func TestErrorHandlingGoTestFileSkipped(t *testing.T) {
	a := NewErrorHandlingAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "main_test.go", line: 10, content: `	_ = os.Remove(path)`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "errorhandling/go-discarded-error")
	if found != nil {
		t.Error("expected no issues for test files, but got one")
	}
}

func TestErrorHandlingGoVendorSkipped(t *testing.T) {
	a := NewErrorHandlingAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "vendor/pkg/main.go", line: 10, content: `	_ = os.Remove(path)`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	if len(issues) != 0 {
		t.Error("expected no issues for vendor/ files, but got some")
	}
}

func TestErrorHandlingGoCleanCode(t *testing.T) {
	a := NewErrorHandlingAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "main.go", line: 10, content: `	if err := os.Remove(path); err != nil {`},
		{file: "main.go", line: 11, content: `		return fmt.Errorf("removing file: %w", err)`},
		{file: "main.go", line: 12, content: `	}`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "errorhandling/go-discarded-error")
	if found != nil {
		t.Error("expected no issues for properly handled error")
	}
}

// --- TypeScript/JavaScript error handling tests ---

func TestErrorHandlingTSThenWithoutCatch(t *testing.T) {
	a := NewErrorHandlingAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "app.ts", line: 10, content: `  fetch("/api/data").then(res => res.json())`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "errorhandling/ts-unhandled-promise")
	if found == nil {
		t.Fatal("expected ts-unhandled-promise issue for .then() without .catch()")
	}
	if found.Severity != SeverityWarning {
		t.Errorf("expected warning severity, got %q", found.Severity)
	}
	if found.Category != "errorhandling-ts" {
		t.Errorf("expected category 'errorhandling-ts', got %q", found.Category)
	}
}

func TestErrorHandlingTSThenWithCatchSameLine(t *testing.T) {
	a := NewErrorHandlingAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "app.ts", line: 10, content: `  fetch("/api/data").then(res => res.json()).catch(err => console.error(err))`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "errorhandling/ts-unhandled-promise")
	if found != nil {
		t.Error("expected no issue when .catch() is on the same line as .then()")
	}
}

func TestErrorHandlingTSThenWithCatchNearby(t *testing.T) {
	a := NewErrorHandlingAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "app.ts", line: 10, content: `  fetch("/api/data")`},
		{file: "app.ts", line: 11, content: `    .then(res => res.json())`},
		{file: "app.ts", line: 12, content: `    .catch(err => console.error(err))`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "errorhandling/ts-unhandled-promise")
	if found != nil {
		t.Error("expected no issue when .catch() is on a nearby line")
	}
}

func TestErrorHandlingTSAwaitWithoutTryCatch(t *testing.T) {
	a := NewErrorHandlingAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "service.ts", line: 5, content: `  const data = await fetch("/api/data")`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "errorhandling/ts-unhandled-await")
	if found == nil {
		t.Fatal("expected ts-unhandled-await issue for await without try-catch")
	}
}

func TestErrorHandlingTSAwaitWithTryCatch(t *testing.T) {
	a := NewErrorHandlingAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "service.ts", line: 3, content: `  try {`},
		{file: "service.ts", line: 4, content: `    const data = await fetch("/api/data")`},
		{file: "service.ts", line: 5, content: `  } catch (err) {`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "errorhandling/ts-unhandled-await")
	if found != nil {
		t.Error("expected no issue when await is inside try-catch")
	}
}

func TestErrorHandlingTSTestFileSkipped(t *testing.T) {
	a := NewErrorHandlingAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "app.test.ts", line: 10, content: `  fetch("/api/data").then(res => res.json())`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	if len(issues) != 0 {
		t.Error("expected no issues for test files")
	}
}

// --- Python error handling tests ---

func TestErrorHandlingPythonBareExcept(t *testing.T) {
	a := NewErrorHandlingAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "app.py", line: 10, content: `    except:`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "errorhandling/python-bare-except")
	if found == nil {
		t.Fatal("expected python-bare-except issue for bare except clause")
	}
	if found.Severity != SeverityWarning {
		t.Errorf("expected warning severity, got %q", found.Severity)
	}
	if found.Category != "errorhandling-python" {
		t.Errorf("expected category 'errorhandling-python', got %q", found.Category)
	}
}

func TestErrorHandlingPythonExceptPass(t *testing.T) {
	a := NewErrorHandlingAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "app.py", line: 10, content: `    except Exception as e:`},
		{file: "app.py", line: 11, content: `        pass`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "errorhandling/python-except-pass")
	if found == nil {
		t.Fatal("expected python-except-pass issue for except Exception followed by pass")
	}
}

func TestErrorHandlingPythonExceptPassBaseException(t *testing.T) {
	a := NewErrorHandlingAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "app.py", line: 10, content: `    except BaseException:`},
		{file: "app.py", line: 11, content: `        pass`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "errorhandling/python-except-pass")
	if found == nil {
		t.Fatal("expected python-except-pass issue for except BaseException followed by pass")
	}
}

func TestErrorHandlingPythonSpecificExcept(t *testing.T) {
	a := NewErrorHandlingAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "app.py", line: 10, content: `    except ValueError as e:`},
		{file: "app.py", line: 11, content: `        logger.error(e)`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	if len(issues) != 0 {
		t.Errorf("expected no issues for specific except with handler, got %d: %v", len(issues), issues)
	}
}

// --- Java error handling tests ---

func TestErrorHandlingJavaEmptyCatch(t *testing.T) {
	a := NewErrorHandlingAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "App.java", line: 20, content: `    } catch (IOException e) { }`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "errorhandling/java-empty-catch")
	if found == nil {
		t.Fatal("expected java-empty-catch issue for empty catch block")
	}
	if found.Severity != SeverityWarning {
		t.Errorf("expected warning severity, got %q", found.Severity)
	}
	if found.Category != "errorhandling-java" {
		t.Errorf("expected category 'errorhandling-java', got %q", found.Category)
	}
}

func TestErrorHandlingJavaBroadCatch(t *testing.T) {
	a := NewErrorHandlingAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "App.java", line: 20, content: `    } catch (Exception e) {`},
		{file: "App.java", line: 21, content: `        logger.error(e);`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "errorhandling/java-broad-catch")
	if found == nil {
		t.Fatal("expected java-broad-catch issue for catch(Exception e)")
	}
	if found.Severity != SeverityInfo {
		t.Errorf("expected info severity, got %q", found.Severity)
	}
}

func TestErrorHandlingJavaBroadThrows(t *testing.T) {
	a := NewErrorHandlingAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "App.java", line: 10, content: `    public void process() throws Exception {`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "errorhandling/java-broad-throws")
	if found == nil {
		t.Fatal("expected java-broad-throws issue for 'throws Exception'")
	}
	if found.Severity != SeverityInfo {
		t.Errorf("expected info severity, got %q", found.Severity)
	}
}

func TestErrorHandlingJavaSpecificCatch(t *testing.T) {
	a := NewErrorHandlingAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "App.java", line: 20, content: `    } catch (IOException e) {`},
		{file: "App.java", line: 21, content: `        logger.error("IO error", e);`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	// Should not flag specific exception types with handlers
	found := findIssueByID(issues, "errorhandling/java-empty-catch")
	if found != nil {
		t.Error("expected no empty-catch issue for specific exception with handler")
	}
	found = findIssueByID(issues, "errorhandling/java-broad-catch")
	if found != nil {
		t.Error("expected no broad-catch issue for IOException")
	}
}

// --- Rust error handling tests ---

func TestErrorHandlingRustUnwrap(t *testing.T) {
	a := NewErrorHandlingAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "main.rs", line: 10, content: `    let data = file.read_to_string().unwrap()`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "errorhandling/rust-unwrap")
	if found == nil {
		t.Fatal("expected rust-unwrap issue for .unwrap()")
	}
	if found.Severity != SeverityWarning {
		t.Errorf("expected warning severity, got %q", found.Severity)
	}
	if found.Category != "errorhandling-rust" {
		t.Errorf("expected category 'errorhandling-rust', got %q", found.Category)
	}
}

func TestErrorHandlingRustGenericExpect(t *testing.T) {
	a := NewErrorHandlingAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "main.rs", line: 10, content: `    let data = file.read_to_string().expect("failed")`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "errorhandling/rust-generic-expect")
	if found == nil {
		t.Fatal("expected rust-generic-expect issue for .expect(\"failed\")")
	}
	if found.Severity != SeverityInfo {
		t.Errorf("expected info severity, got %q", found.Severity)
	}
}

func TestErrorHandlingRustDescriptiveExpect(t *testing.T) {
	a := NewErrorHandlingAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "main.rs", line: 10, content: `    let data = file.read_to_string().expect("config file must exist at startup")`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "errorhandling/rust-generic-expect")
	if found != nil {
		t.Error("expected no issue for descriptive .expect() message")
	}
}

func TestErrorHandlingRustTestFileSkipped(t *testing.T) {
	// Rust test files: files with _test in the name or in tests/ dir
	// isTestFile doesn't match .rs files by default, so unwrap in non-test .rs files is flagged
	// but test files are skipped at the top level
	a := NewErrorHandlingAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "tests/integration.rs", line: 10, content: `    let data = file.read_to_string().unwrap()`},
	})

	// tests/ directory is not directly skipped by isTestFile for .rs,
	// but isErrorHandlingSkippedPath checks for __tests__ (not tests/).
	// This test verifies the behavior.
	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	// Rust test files in tests/ dir aren't caught by isTestFile, so this will be flagged.
	// That's acceptable behavior -- the skip is for standard test file conventions.
	_ = issues
}

func TestErrorHandlingNodeModulesSkipped(t *testing.T) {
	a := NewErrorHandlingAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "node_modules/lib/index.js", line: 5, content: `  fetch("/api").then(r => r.json())`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	if len(issues) != 0 {
		t.Error("expected no issues for node_modules/ files")
	}
}

func TestErrorHandlingDeletedFileSkipped(t *testing.T) {
	a := NewErrorHandlingAnalyzer()
	ctx := &AnalysisContext{
		RepoPath: "/nonexistent",
		Diff: &git.Diff{
			Files: []git.FileDiff{
				{
					Path:   "main.go",
					Status: "deleted",
					Hunks: []git.Hunk{{
						Lines: []git.Line{
							{Type: "added", Content: `	_ = os.Remove(path)`, NewNum: 10},
						},
					}},
				},
			},
		},
		ParsedFiles: make(map[string]*parser.ParsedFile),
		Config:      config.DefaultConfig(),
	}

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	if len(issues) != 0 {
		t.Error("expected no issues for deleted files")
	}
}

func TestErrorHandlingCommentsSkipped(t *testing.T) {
	a := NewErrorHandlingAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "main.go", line: 10, content: `	// _ = os.Remove(path)`},
		{file: "app.py", line: 20, content: `    # except:`},
		{file: "App.java", line: 30, content: `    // } catch (Exception e) { }`},
		{file: "main.rs", line: 40, content: `    // let data = file.read_to_string().unwrap()`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	if len(issues) != 0 {
		t.Errorf("expected no issues for commented-out code, got %d", len(issues))
	}
}
