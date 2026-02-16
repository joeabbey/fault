package analyzer

import (
	"fmt"
	"strings"
	"testing"

	"github.com/joeabbey/fault/pkg/config"
	"github.com/joeabbey/fault/pkg/git"
	"github.com/joeabbey/fault/pkg/parser"
)

func TestComplexityAnalyzerName(t *testing.T) {
	a := NewComplexityAnalyzer()
	if a.Name() != "complexity" {
		t.Errorf("expected name %q, got %q", "complexity", a.Name())
	}
}

func TestComplexityFunctionTooLong(t *testing.T) {
	a := NewComplexityAnalyzer()

	// Create a function symbol spanning 100 lines (line 10 to 110).
	ctx := &AnalysisContext{
		RepoPath: "/repo",
		Diff: &git.Diff{
			Files: []git.FileDiff{
				{
					Path:   "pkg/server.go",
					Status: "modified",
					Hunks: []git.Hunk{
						{
							NewStart: 1,
							NewCount: 120,
							Lines:    makeAddedLines(1, 120),
						},
					},
				},
			},
		},
		ParsedFiles: map[string]*parser.ParsedFile{
			"pkg/server.go": {
				Path:     "pkg/server.go",
				Language: "go",
				Symbols: []parser.Symbol{
					{
						Name:      "HandleRequest",
						Kind:      "function",
						Exported:  true,
						Line:      10,
						EndLine:   110,
						Signature: "func HandleRequest(w http.ResponseWriter, r *http.Request)",
					},
				},
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
		if issue.FixID == "complexity-function-too-long" {
			found = true
			if issue.Severity != SeverityWarning {
				t.Errorf("expected warning for 100-line function, got %s", issue.Severity)
			}
			if !strings.Contains(issue.Message, "HandleRequest") {
				t.Errorf("expected message to mention function name, got %q", issue.Message)
			}
			break
		}
	}
	if !found {
		t.Errorf("expected a function-too-long issue for 100-line function, got %d issues: %v", len(issues), issues)
	}

	// Test error threshold: function spanning 200 lines.
	ctx.ParsedFiles["pkg/server.go"].Symbols[0].EndLine = 210
	ctx.Diff.Files[0].Hunks[0].NewCount = 220
	ctx.Diff.Files[0].Hunks[0].Lines = makeAddedLines(1, 220)

	issues, err = a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found = false
	for _, issue := range issues {
		if issue.FixID == "complexity-function-too-long" && issue.Severity == SeverityError {
			found = true
			break
		}
	}
	if !found {
		t.Errorf("expected an error for 200-line function")
	}
}

func TestComplexityTooManyParams(t *testing.T) {
	a := NewComplexityAnalyzer()

	ctx := &AnalysisContext{
		RepoPath: "/repo",
		Diff: &git.Diff{
			Files: []git.FileDiff{
				{
					Path:   "pkg/handler.go",
					Status: "modified",
					Hunks: []git.Hunk{
						{
							NewStart: 1,
							NewCount: 30,
							Lines:    makeAddedLines(1, 30),
						},
					},
				},
			},
		},
		ParsedFiles: map[string]*parser.ParsedFile{
			"pkg/handler.go": {
				Path:     "pkg/handler.go",
				Language: "go",
				Symbols: []parser.Symbol{
					{
						Name:      "ProcessData",
						Kind:      "function",
						Exported:  true,
						Line:      5,
						EndLine:   25,
						Signature: "func ProcessData(a int, b string, c bool, d float64, e []byte, f error, g context.Context)",
					},
				},
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
		if issue.FixID == "complexity-too-many-params" {
			found = true
			if issue.Severity != SeverityWarning {
				t.Errorf("expected warning for 7-param function, got %s", issue.Severity)
			}
			if !strings.Contains(issue.Message, "7 parameters") {
				t.Errorf("expected message to say 7 parameters, got %q", issue.Message)
			}
			break
		}
	}
	if !found {
		t.Errorf("expected a too-many-params issue for 7-param function, got %d issues: %v", len(issues), issues)
	}

	// Test error threshold: 9 params.
	ctx.ParsedFiles["pkg/handler.go"].Symbols[0].Signature = "func ProcessData(a int, b string, c bool, d float64, e []byte, f error, g context.Context, h int, i string)"
	issues, err = a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found = false
	for _, issue := range issues {
		if issue.FixID == "complexity-too-many-params" && issue.Severity == SeverityError {
			found = true
			break
		}
	}
	if !found {
		t.Errorf("expected an error for 9-param function")
	}
}

func TestComplexityDeepNesting(t *testing.T) {
	a := NewComplexityAnalyzer()

	// Build 5 levels of nested braces in added lines.
	lines := make([]git.Line, 0)
	lineNum := 1
	addLine := func(content string) {
		lines = append(lines, git.Line{
			Type:    "added",
			Content: content,
			NewNum:  lineNum,
		})
		lineNum++
	}

	addLine("func deep() {")
	addLine("    if true {")
	addLine("        for i := range items {")
	addLine("            if ok {")
	addLine("                if ready {")
	addLine("                    doSomething()") // depth 5
	addLine("                }")
	addLine("            }")
	addLine("        }")
	addLine("    }")
	addLine("}")

	ctx := &AnalysisContext{
		RepoPath: "/repo",
		Diff: &git.Diff{
			Files: []git.FileDiff{
				{
					Path:   "pkg/deep.go",
					Status: "modified",
					Hunks: []git.Hunk{
						{
							NewStart: 1,
							NewCount: lineNum - 1,
							Lines:    lines,
						},
					},
				},
			},
		},
		ParsedFiles: map[string]*parser.ParsedFile{},
		Config:      config.DefaultConfig(),
	}

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := false
	for _, issue := range issues {
		if issue.FixID == "complexity-deep-nesting" {
			found = true
			if issue.Severity != SeverityWarning {
				t.Errorf("expected warning for 5-level nesting, got %s", issue.Severity)
			}
			break
		}
	}
	if !found {
		t.Errorf("expected a deep-nesting issue, got %d issues: %v", len(issues), issues)
	}
}

func TestComplexityHighCyclomaticComplexity(t *testing.T) {
	a := NewComplexityAnalyzer()

	// Build a function with many if/else branches.
	lines := make([]git.Line, 0)
	lineNum := 1
	addLine := func(content string) {
		lines = append(lines, git.Line{
			Type:    "added",
			Content: content,
			NewNum:  lineNum,
		})
		lineNum++
	}

	addLine("func classify(x int) string {")
	// Add 12 if/else if branches to push complexity past the warning threshold.
	for i := 0; i < 12; i++ {
		if i == 0 {
			addLine(fmt.Sprintf("    if x == %d {", i))
		} else {
			addLine(fmt.Sprintf("    } else if x == %d {", i))
		}
		addLine(fmt.Sprintf(`        return "val%d"`, i))
	}
	addLine("    }")
	addLine("    return \"default\"")
	addLine("}")

	ctx := &AnalysisContext{
		RepoPath: "/repo",
		Diff: &git.Diff{
			Files: []git.FileDiff{
				{
					Path:   "pkg/classify.go",
					Status: "modified",
					Hunks: []git.Hunk{
						{
							NewStart: 1,
							NewCount: lineNum - 1,
							Lines:    lines,
						},
					},
				},
			},
		},
		ParsedFiles: map[string]*parser.ParsedFile{},
		Config:      config.DefaultConfig(),
	}

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := false
	for _, issue := range issues {
		if issue.FixID == "complexity-high-cyclomatic" {
			found = true
			if issue.Severity != SeverityWarning {
				t.Errorf("expected warning for high cyclomatic complexity, got %s", issue.Severity)
			}
			if !strings.Contains(issue.Message, "classify") {
				t.Errorf("expected message to mention function name, got %q", issue.Message)
			}
			break
		}
	}
	if !found {
		t.Errorf("expected a cyclomatic-complexity issue, got %d issues: %v", len(issues), issues)
	}
}

func TestComplexityCleanCode(t *testing.T) {
	a := NewComplexityAnalyzer()

	// A short, simple function with 2 parameters and shallow nesting.
	lines := make([]git.Line, 0)
	lineNum := 1
	addLine := func(content string) {
		lines = append(lines, git.Line{
			Type:    "added",
			Content: content,
			NewNum:  lineNum,
		})
		lineNum++
	}

	addLine("func Add(a int, b int) int {")
	addLine("    return a + b")
	addLine("}")

	ctx := &AnalysisContext{
		RepoPath: "/repo",
		Diff: &git.Diff{
			Files: []git.FileDiff{
				{
					Path:   "pkg/math.go",
					Status: "modified",
					Hunks: []git.Hunk{
						{
							NewStart: 1,
							NewCount: lineNum - 1,
							Lines:    lines,
						},
					},
				},
			},
		},
		ParsedFiles: map[string]*parser.ParsedFile{
			"pkg/math.go": {
				Path:     "pkg/math.go",
				Language: "go",
				Symbols: []parser.Symbol{
					{
						Name:      "Add",
						Kind:      "function",
						Exported:  true,
						Line:      1,
						EndLine:   3,
						Signature: "func Add(a int, b int)",
					},
				},
			},
		},
		Config: config.DefaultConfig(),
	}

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	if len(issues) != 0 {
		t.Errorf("expected 0 issues for clean code, got %d: %v", len(issues), issues)
	}
}

func TestComplexityOnlyFlagsChangedFunctions(t *testing.T) {
	a := NewComplexityAnalyzer()

	// Two functions: one inside the diff hunk, one outside.
	ctx := &AnalysisContext{
		RepoPath: "/repo",
		Diff: &git.Diff{
			Files: []git.FileDiff{
				{
					Path:   "pkg/big.go",
					Status: "modified",
					Hunks: []git.Hunk{
						{
							NewStart: 200,
							NewCount: 10,
							Lines:    makeAddedLines(200, 10),
						},
					},
				},
			},
		},
		ParsedFiles: map[string]*parser.ParsedFile{
			"pkg/big.go": {
				Path:     "pkg/big.go",
				Language: "go",
				Symbols: []parser.Symbol{
					{
						// This function is NOT in the diff (lines 10-120).
						Name:      "UnchangedLongFunc",
						Kind:      "function",
						Exported:  true,
						Line:      10,
						EndLine:   120,
						Signature: "func UnchangedLongFunc(a, b, c, d, e, f, g int)",
					},
					{
						// This function IS in the diff (lines 200-205).
						Name:      "ChangedSmallFunc",
						Kind:      "function",
						Exported:  true,
						Line:      200,
						EndLine:   205,
						Signature: "func ChangedSmallFunc(x int)",
					},
				},
			},
		},
		Config: config.DefaultConfig(),
	}

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	// UnchangedLongFunc (110 lines, 7 params) should NOT be flagged.
	for _, issue := range issues {
		if strings.Contains(issue.Message, "UnchangedLongFunc") {
			t.Errorf("did not expect issues for unchanged function, got: %q", issue.Message)
		}
	}
}

func TestComplexityPythonIndentNesting(t *testing.T) {
	a := NewComplexityAnalyzer()

	lines := make([]git.Line, 0)
	lineNum := 1
	addLine := func(content string) {
		lines = append(lines, git.Line{
			Type:    "added",
			Content: content,
			NewNum:  lineNum,
		})
		lineNum++
	}

	addLine("def deep():")
	addLine("    if True:")
	addLine("        for x in items:")
	addLine("            if ok:")
	addLine("                if ready:")
	addLine("                    if final:")                     // 24 spaces = 6 levels
	addLine("                        do_something()")           // 28 spaces = 7 levels

	ctx := &AnalysisContext{
		RepoPath: "/repo",
		Diff: &git.Diff{
			Files: []git.FileDiff{
				{
					Path:   "app/deep.py",
					Status: "modified",
					Hunks: []git.Hunk{
						{
							NewStart: 1,
							NewCount: lineNum - 1,
							Lines:    lines,
						},
					},
				},
			},
		},
		ParsedFiles: map[string]*parser.ParsedFile{},
		Config:      config.DefaultConfig(),
	}

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := false
	for _, issue := range issues {
		if issue.FixID == "complexity-deep-nesting" {
			found = true
			break
		}
	}
	if !found {
		t.Errorf("expected a nesting issue for deeply indented Python, got %d issues: %v", len(issues), issues)
	}
}

func TestComplexitySkipsTestFiles(t *testing.T) {
	a := NewComplexityAnalyzer()

	ctx := &AnalysisContext{
		RepoPath: "/repo",
		Diff: &git.Diff{
			Files: []git.FileDiff{
				{
					Path:   "pkg/handler_test.go",
					Status: "modified",
					Hunks: []git.Hunk{
						{
							NewStart: 1,
							NewCount: 200,
							Lines:    makeAddedLines(1, 200),
						},
					},
				},
			},
		},
		ParsedFiles: map[string]*parser.ParsedFile{
			"pkg/handler_test.go": {
				Path:     "pkg/handler_test.go",
				Language: "go",
				Symbols: []parser.Symbol{
					{
						Name:      "TestBigHandler",
						Kind:      "function",
						Exported:  true,
						Line:      1,
						EndLine:   195,
						Signature: "func TestBigHandler(t *testing.T)",
					},
				},
			},
		},
		Config: config.DefaultConfig(),
	}

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	if len(issues) != 0 {
		t.Errorf("expected 0 issues for test files, got %d: %v", len(issues), issues)
	}
}

func TestComplexityExcessiveReturns(t *testing.T) {
	a := NewComplexityAnalyzer()

	lines := make([]git.Line, 0)
	lineNum := 1
	addLine := func(content string) {
		lines = append(lines, git.Line{
			Type:    "added",
			Content: content,
			NewNum:  lineNum,
		})
		lineNum++
	}

	addLine("func manyReturns(x int) string {")
	for i := 0; i < 10; i++ {
		addLine(fmt.Sprintf("    if x == %d {", i))
		addLine(fmt.Sprintf(`        return "val%d"`, i))
		addLine("    }")
	}
	addLine(`    return "default"`)
	addLine("}")

	ctx := &AnalysisContext{
		RepoPath: "/repo",
		Diff: &git.Diff{
			Files: []git.FileDiff{
				{
					Path:   "pkg/returns.go",
					Status: "modified",
					Hunks: []git.Hunk{
						{
							NewStart: 1,
							NewCount: lineNum - 1,
							Lines:    lines,
						},
					},
				},
			},
		},
		ParsedFiles: map[string]*parser.ParsedFile{},
		Config:      config.DefaultConfig(),
	}

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := false
	for _, issue := range issues {
		if issue.FixID == "complexity-excessive-returns" {
			found = true
			if issue.Severity != SeverityWarning {
				t.Errorf("expected warning for excessive returns, got %s", issue.Severity)
			}
			break
		}
	}
	if !found {
		t.Errorf("expected an excessive-returns issue, got %d issues: %v", len(issues), issues)
	}
}

func TestCountParameters(t *testing.T) {
	tests := []struct {
		sig  string
		want int
	}{
		{"func Add(a int, b int)", 2},
		{"func NoParams()", 0},
		{"func Single(x string)", 1},
		{"func (*Server) Handle(w http.ResponseWriter, r *http.Request)", 2},
		{"func (s *Store) Get(ctx context.Context, id int64) (*Item, error)", 2},
		{"processData(a, b, c, d, e, f, g)", 7},
		{"", 0},
		{"noparens", 0},
		{"func Complex(a map[string]int, b []string, c func(int) bool)", 3},
	}

	for _, tt := range tests {
		got := countParameters(tt.sig)
		if got != tt.want {
			t.Errorf("countParameters(%q) = %d, want %d", tt.sig, got, tt.want)
		}
	}
}

func TestComplexityNilDiff(t *testing.T) {
	a := NewComplexityAnalyzer()
	ctx := &AnalysisContext{
		RepoPath:    "/repo",
		Diff:        nil,
		ParsedFiles: map[string]*parser.ParsedFile{},
		Config:      config.DefaultConfig(),
	}

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(issues) != 0 {
		t.Errorf("expected 0 issues with nil diff, got %d", len(issues))
	}
}

// makeAddedLines creates a slice of added diff lines starting at startLine.
func makeAddedLines(startLine, count int) []git.Line {
	lines := make([]git.Line, count)
	for i := 0; i < count; i++ {
		lines[i] = git.Line{
			Type:    "added",
			Content: fmt.Sprintf("    line %d", startLine+i),
			NewNum:  startLine + i,
		}
	}
	return lines
}
