package reporter

import (
	"github.com/joeabbey/fault/pkg/git"
)

// CodeContext holds source lines surrounding an issue.
type CodeContext struct {
	Lines     []ContextLine
	StartLine int
	IssueLine int
}

// ContextLine represents a single line of code context.
type ContextLine struct {
	Number  int
	Content string
	IsAdded bool // from diff (line status "added")
	IsIssue bool // this is the flagged line
}

// ExtractContext pulls context lines above/below an issue's location from diff hunks.
// Returns nil if the issue's file/line can't be found in the diff.
func ExtractContext(file string, line int, diff *git.Diff, contextSize int) *CodeContext {
	if diff == nil || line <= 0 {
		return nil
	}

	// Find the matching FileDiff by path
	var fileDiff *git.FileDiff
	for i := range diff.Files {
		if diff.Files[i].Path == file {
			fileDiff = &diff.Files[i]
			break
		}
	}
	if fileDiff == nil {
		return nil
	}

	// Walk hunks to find one containing the target line
	for _, hunk := range fileDiff.Hunks {
		idx := -1
		for i, l := range hunk.Lines {
			lineNum := l.NewNum
			if l.Type == "removed" {
				// Removed lines only have OldNum, not NewNum
				continue
			}
			if lineNum == line {
				idx = i
				break
			}
		}
		if idx < 0 {
			continue
		}

		// Determine the range of lines to extract
		start := idx - contextSize
		if start < 0 {
			start = 0
		}
		end := idx + contextSize + 1
		if end > len(hunk.Lines) {
			end = len(hunk.Lines)
		}

		lines := make([]ContextLine, 0, end-start)
		startLine := 0
		for i := start; i < end; i++ {
			l := hunk.Lines[i]
			num := l.NewNum
			if l.Type == "removed" {
				num = l.OldNum
			}
			if startLine == 0 && num > 0 {
				startLine = num
			}
			cl := ContextLine{
				Number:  num,
				Content: l.Content,
				IsAdded: l.Type == "added",
				IsIssue: i == idx,
			}
			lines = append(lines, cl)
		}

		return &CodeContext{
			Lines:     lines,
			StartLine: startLine,
			IssueLine: line,
		}
	}

	return nil
}
