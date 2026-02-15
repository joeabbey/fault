package reporter

import (
	"testing"

	"github.com/joeabbey/fault/pkg/git"
)

func TestExtractContext(t *testing.T) {
	diff := &git.Diff{
		Files: []git.FileDiff{
			{
				Path:   "pkg/api/server.go",
				Status: "modified",
				Hunks: []git.Hunk{
					{
						NewStart: 40,
						NewCount: 6,
						Lines: []git.Line{
							{Type: "context", Content: "import (", NewNum: 40, OldNum: 40},
							{Type: "context", Content: `    "fmt"`, NewNum: 41, OldNum: 41},
							{Type: "added", Content: `    "github.com/foo/bar"`, NewNum: 42},
							{Type: "context", Content: `    "net/http"`, NewNum: 43, OldNum: 42},
							{Type: "context", Content: ")", NewNum: 44, OldNum: 43},
						},
					},
				},
			},
		},
	}

	ctx := ExtractContext("pkg/api/server.go", 42, diff, 3)
	if ctx == nil {
		t.Fatal("expected non-nil context")
	}

	if ctx.IssueLine != 42 {
		t.Errorf("expected IssueLine=42, got %d", ctx.IssueLine)
	}

	// Should have all 5 lines (only 2 before, 2 after available)
	if len(ctx.Lines) != 5 {
		t.Errorf("expected 5 context lines, got %d", len(ctx.Lines))
	}

	// Check the issue line
	found := false
	for _, l := range ctx.Lines {
		if l.IsIssue {
			found = true
			if !l.IsAdded {
				t.Error("expected issue line to be marked as added")
			}
			if l.Number != 42 {
				t.Errorf("expected issue line number 42, got %d", l.Number)
			}
		}
	}
	if !found {
		t.Error("no line marked as IsIssue")
	}
}

func TestExtractContextNoMatch(t *testing.T) {
	diff := &git.Diff{
		Files: []git.FileDiff{
			{
				Path:   "pkg/api/server.go",
				Status: "modified",
				Hunks: []git.Hunk{
					{
						NewStart: 40,
						NewCount: 3,
						Lines: []git.Line{
							{Type: "context", Content: "import (", NewNum: 40, OldNum: 40},
							{Type: "added", Content: `    "fmt"`, NewNum: 41},
							{Type: "context", Content: ")", NewNum: 42, OldNum: 41},
						},
					},
				},
			},
		},
	}

	// Line 100 is not in any hunk
	ctx := ExtractContext("pkg/api/server.go", 100, diff, 3)
	if ctx != nil {
		t.Error("expected nil context for non-matching line")
	}

	// Different file entirely
	ctx = ExtractContext("other.go", 41, diff, 3)
	if ctx != nil {
		t.Error("expected nil context for non-matching file")
	}
}

func TestExtractContextEdgeAtStart(t *testing.T) {
	diff := &git.Diff{
		Files: []git.FileDiff{
			{
				Path:   "main.go",
				Status: "modified",
				Hunks: []git.Hunk{
					{
						NewStart: 1,
						NewCount: 4,
						Lines: []git.Line{
							{Type: "added", Content: "package main", NewNum: 1},
							{Type: "context", Content: "", NewNum: 2, OldNum: 1},
							{Type: "context", Content: "import \"fmt\"", NewNum: 3, OldNum: 2},
							{Type: "context", Content: "", NewNum: 4, OldNum: 3},
						},
					},
				},
			},
		},
	}

	ctx := ExtractContext("main.go", 1, diff, 3)
	if ctx == nil {
		t.Fatal("expected non-nil context")
	}

	// Issue is at index 0, so there are 0 lines above and up to 3 below
	if len(ctx.Lines) != 4 {
		t.Errorf("expected 4 context lines, got %d", len(ctx.Lines))
	}

	if !ctx.Lines[0].IsIssue {
		t.Error("expected first line to be the issue")
	}
}

func TestExtractContextEdgeAtEnd(t *testing.T) {
	diff := &git.Diff{
		Files: []git.FileDiff{
			{
				Path:   "main.go",
				Status: "modified",
				Hunks: []git.Hunk{
					{
						NewStart: 10,
						NewCount: 4,
						Lines: []git.Line{
							{Type: "context", Content: "func main() {", NewNum: 10, OldNum: 10},
							{Type: "context", Content: "    fmt.Println()", NewNum: 11, OldNum: 11},
							{Type: "context", Content: "}", NewNum: 12, OldNum: 12},
							{Type: "added", Content: "", NewNum: 13},
						},
					},
				},
			},
		},
	}

	ctx := ExtractContext("main.go", 13, diff, 3)
	if ctx == nil {
		t.Fatal("expected non-nil context")
	}

	// Issue is at index 3 (last), so there are 3 lines above and 0 below
	if len(ctx.Lines) != 4 {
		t.Errorf("expected 4 context lines, got %d", len(ctx.Lines))
	}

	last := ctx.Lines[len(ctx.Lines)-1]
	if !last.IsIssue {
		t.Error("expected last line to be the issue")
	}
}

func TestExtractContextNilDiff(t *testing.T) {
	ctx := ExtractContext("main.go", 10, nil, 3)
	if ctx != nil {
		t.Error("expected nil context for nil diff")
	}
}
