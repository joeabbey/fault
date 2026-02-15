package fixer

import (
	"fmt"
	"os"
	"path/filepath"
	"sort"
	"strings"

	"github.com/joeabbey/fault/pkg/analyzer"
)

// Fix describes a concrete code change that resolves an issue.
type Fix struct {
	IssueID     string // matches the original Issue.ID
	FixID       string // matches Issue.FixID category
	File        string // file to modify
	Description string // what this fix does
	Edits       []Edit // ordered edits to apply
}

// Edit is a single text replacement in a file.
type Edit struct {
	Line    int    // 1-based line number
	OldText string // text to find on that line (for verification)
	NewText string // replacement (empty string = delete line)
}

// Fixer generates fixes for specific issue categories.
type Fixer interface {
	FixIDs() []string                                         // which FixID values this fixer handles
	GenerateFix(issue analyzer.Issue, repoRoot string) *Fix   // nil if can't fix
}

// Registry manages fixers by FixID.
type Registry struct {
	fixers map[string]Fixer
}

// NewRegistry creates a new fixer registry.
func NewRegistry() *Registry {
	return &Registry{
		fixers: make(map[string]Fixer),
	}
}

// Register adds a fixer to the registry for all its FixIDs.
func (r *Registry) Register(f Fixer) {
	for _, id := range f.FixIDs() {
		r.fixers[id] = f
	}
}

// GenerateFix finds the appropriate fixer for the issue and generates a fix.
func (r *Registry) GenerateFix(issue analyzer.Issue, repoRoot string) *Fix {
	if issue.FixID == "" {
		return nil
	}
	f, ok := r.fixers[issue.FixID]
	if !ok {
		return nil
	}
	return f.GenerateFix(issue, repoRoot)
}

// Apply writes a Fix to disk. Reads the file, verifies OldText matches at the
// specified line, replaces with NewText, writes back. Returns error if verification fails.
func Apply(fix *Fix, repoRoot string) error {
	filePath := filepath.Join(repoRoot, fix.File)
	data, err := os.ReadFile(filePath)
	if err != nil {
		return fmt.Errorf("reading %s: %w", fix.File, err)
	}

	lines := strings.Split(string(data), "\n")

	// Sort edits by line number descending so we can apply bottom-up
	// to preserve line numbers for earlier edits.
	edits := make([]Edit, len(fix.Edits))
	copy(edits, fix.Edits)
	sort.Slice(edits, func(i, j int) bool {
		return edits[i].Line > edits[j].Line
	})

	for _, edit := range edits {
		idx := edit.Line - 1 // convert to 0-based
		if idx < 0 || idx >= len(lines) {
			return fmt.Errorf("line %d out of range (file has %d lines)", edit.Line, len(lines))
		}

		// Verify OldText matches
		if strings.TrimSpace(lines[idx]) != strings.TrimSpace(edit.OldText) {
			return fmt.Errorf("verification failed at %s:%d: expected %q, got %q",
				fix.File, edit.Line, strings.TrimSpace(edit.OldText), strings.TrimSpace(lines[idx]))
		}

		if edit.NewText == "" {
			// Delete line
			lines = append(lines[:idx], lines[idx+1:]...)
		} else {
			// Replace line, preserving original indentation
			indent := leadingWhitespace(lines[idx])
			newContent := strings.TrimSpace(edit.NewText)
			if newContent == "" {
				lines[idx] = ""
			} else {
				lines[idx] = indent + newContent
			}
		}
	}

	output := strings.Join(lines, "\n")
	if err := os.WriteFile(filePath, []byte(output), 0644); err != nil {
		return fmt.Errorf("writing %s: %w", fix.File, err)
	}

	return nil
}

// DryRun returns a human-readable diff-like preview of what Apply would do.
func DryRun(fix *Fix) string {
	var b strings.Builder
	fmt.Fprintf(&b, "--- %s\n", fix.File)
	fmt.Fprintf(&b, "+++ %s\n", fix.File)
	fmt.Fprintf(&b, "Fix: %s\n\n", fix.Description)

	for _, edit := range fix.Edits {
		if edit.NewText == "" {
			fmt.Fprintf(&b, "Line %d:\n", edit.Line)
			fmt.Fprintf(&b, "- %s\n", edit.OldText)
		} else {
			fmt.Fprintf(&b, "Line %d:\n", edit.Line)
			fmt.Fprintf(&b, "- %s\n", edit.OldText)
			fmt.Fprintf(&b, "+ %s\n", edit.NewText)
		}
	}

	return b.String()
}

// leadingWhitespace returns the leading whitespace of a string.
func leadingWhitespace(s string) string {
	for i, ch := range s {
		if ch != ' ' && ch != '\t' {
			return s[:i]
		}
	}
	return s
}
