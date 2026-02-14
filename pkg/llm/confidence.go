package llm

import (
	"context"
	"fmt"
	"strings"

	"github.com/joeabbey/fault/pkg/git"
	"github.com/joeabbey/fault/pkg/parser"
)

// ConfidenceResult holds the LLM's confidence assessment of a diff.
type ConfidenceResult struct {
	Overall   float64            `json:"overall"`   // 0.0-1.0
	PerFile   map[string]float64 `json:"per_file"`  // file path -> score
	Reasoning string             `json:"reasoning"` // explanation
}

// ScoreConfidence uses the LLM to assess confidence in the quality of a diff.
// It considers the diff content and parsed file structure to evaluate code quality.
func ScoreConfidence(ctx context.Context, client *Client, diff *git.Diff, parsedFiles map[string]*parser.ParsedFile) (*ConfidenceResult, error) {
	if client == nil {
		return nil, fmt.Errorf("LLM client is nil")
	}

	if diff == nil || len(diff.Files) == 0 {
		return &ConfidenceResult{
			Overall:   1.0,
			PerFile:   make(map[string]float64),
			Reasoning: "No changes to evaluate.",
		}, nil
	}

	// Build the user prompt with diff summary and parsed file info
	userPrompt := buildConfidencePrompt(diff, parsedFiles)

	// Load the system prompt
	systemPrompt := LoadPrompt("confidence")
	if systemPrompt == "" {
		return nil, fmt.Errorf("failed to load confidence prompt template")
	}

	// Send to LLM and extract JSON
	var result ConfidenceResult
	if err := client.ExtractJSON(ctx, systemPrompt, userPrompt, &result); err != nil {
		// Graceful degradation: return a mid-confidence default
		return defaultConfidence(diff, fmt.Sprintf("LLM analysis failed: %v", err)), nil
	}

	// Validate and clamp scores
	result = validateConfidenceResult(result, diff)

	return &result, nil
}

// buildConfidencePrompt constructs the user prompt for confidence scoring.
func buildConfidencePrompt(diff *git.Diff, parsedFiles map[string]*parser.ParsedFile) string {
	var b strings.Builder

	b.WriteString("## Changed Files\n\n")

	for _, fd := range diff.Files {
		b.WriteString(fmt.Sprintf("### %s (%s)\n", fd.Path, fd.Status))

		// Include parsed file info if available
		if pf, ok := parsedFiles[fd.Path]; ok {
			writeParsedFileInfo(&b, pf)
		}

		// Include a concise diff summary
		writeDiffSummary(&b, &fd)
		b.WriteString("\n")
	}

	return b.String()
}

// writeParsedFileInfo writes structural information about a parsed file.
func writeParsedFileInfo(b *strings.Builder, pf *parser.ParsedFile) {
	if len(pf.Imports) > 0 {
		b.WriteString("Imports: ")
		imports := make([]string, 0, len(pf.Imports))
		for _, imp := range pf.Imports {
			imports = append(imports, imp.Path)
		}
		b.WriteString(strings.Join(imports, ", "))
		b.WriteString("\n")
	}

	if len(pf.Exports) > 0 {
		b.WriteString("Exports: ")
		exports := make([]string, 0, len(pf.Exports))
		for _, exp := range pf.Exports {
			exports = append(exports, fmt.Sprintf("%s (%s)", exp.Name, exp.Kind))
		}
		b.WriteString(strings.Join(exports, ", "))
		b.WriteString("\n")
	}

	if len(pf.Symbols) > 0 {
		b.WriteString("Symbols: ")
		symbols := make([]string, 0, len(pf.Symbols))
		for _, sym := range pf.Symbols {
			symbols = append(symbols, fmt.Sprintf("%s (%s)", sym.Name, sym.Kind))
		}
		b.WriteString(strings.Join(symbols, ", "))
		b.WriteString("\n")
	}
}

// writeDiffSummary writes a concise summary of the diff hunks for a file.
func writeDiffSummary(b *strings.Builder, fd *git.FileDiff) {
	if fd.IsBinary {
		b.WriteString("(binary file)\n")
		return
	}

	added := 0
	removed := 0
	for _, hunk := range fd.Hunks {
		for _, line := range hunk.Lines {
			switch line.Type {
			case "added":
				added++
			case "removed":
				removed++
			}
		}
	}

	b.WriteString(fmt.Sprintf("Changes: +%d/-%d lines\n", added, removed))

	// Include actual diff content, truncated to keep token usage reasonable
	b.WriteString("```diff\n")
	lineCount := 0
	const maxLines = 100
	for _, hunk := range fd.Hunks {
		for _, line := range hunk.Lines {
			if lineCount >= maxLines {
				b.WriteString("... (truncated)\n")
				break
			}
			switch line.Type {
			case "added":
				b.WriteString("+" + line.Content + "\n")
			case "removed":
				b.WriteString("-" + line.Content + "\n")
			case "context":
				b.WriteString(" " + line.Content + "\n")
			}
			lineCount++
		}
		if lineCount >= maxLines {
			break
		}
	}
	b.WriteString("```\n")
}

// validateConfidenceResult clamps scores to valid ranges and ensures all files are covered.
func validateConfidenceResult(result ConfidenceResult, diff *git.Diff) ConfidenceResult {
	// Clamp overall score
	result.Overall = clampScore(result.Overall)

	// Ensure per_file map exists
	if result.PerFile == nil {
		result.PerFile = make(map[string]float64)
	}

	// Clamp per-file scores and ensure all diff files are represented
	for _, fd := range diff.Files {
		if score, ok := result.PerFile[fd.Path]; ok {
			result.PerFile[fd.Path] = clampScore(score)
		} else {
			// Default missing files to the overall score
			result.PerFile[fd.Path] = result.Overall
		}
	}

	return result
}

// defaultConfidence returns a mid-confidence result when LLM analysis fails.
func defaultConfidence(diff *git.Diff, reason string) *ConfidenceResult {
	perFile := make(map[string]float64)
	for _, fd := range diff.Files {
		perFile[fd.Path] = 0.5
	}

	return &ConfidenceResult{
		Overall:   0.5,
		PerFile:   perFile,
		Reasoning: reason,
	}
}

// clampScore ensures a score is within the 0.0-1.0 range.
func clampScore(score float64) float64 {
	if score < 0.0 {
		return 0.0
	}
	if score > 1.0 {
		return 1.0
	}
	return score
}

// BuildDiffSummary creates a concise textual summary of a diff for LLM consumption.
// Exported for use by other packages that need to summarize diffs.
func BuildDiffSummary(diff *git.Diff) string {
	var b strings.Builder

	for _, fd := range diff.Files {
		b.WriteString(fmt.Sprintf("File: %s (%s)\n", fd.Path, fd.Status))
		writeDiffSummary(&b, &fd)
		b.WriteString("\n")
	}

	return b.String()
}
