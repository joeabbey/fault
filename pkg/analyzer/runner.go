package analyzer

import (
	"fmt"
	"log"
	"sort"
	"sync"
	"time"

	"github.com/joeabbey/fault/pkg/config"
	"github.com/joeabbey/fault/pkg/git"
	"github.com/joeabbey/fault/pkg/index"
	"github.com/joeabbey/fault/pkg/parser"
)

// Runner orchestrates running all enabled analyzers.
type Runner struct {
	analyzers []Analyzer
	config    *config.Config
}

// NewRunner creates a runner with the given analyzers and config.
func NewRunner(cfg *config.Config, analyzers []Analyzer) *Runner {
	return &Runner{
		analyzers: analyzers,
		config:    cfg,
	}
}

// analyzerResult holds the output of a single analyzer run.
type analyzerResult struct {
	name     string
	issues   []Issue
	err      error
	duration time.Duration
}

// Run executes all enabled analyzers and returns the combined result.
func (r *Runner) Run(repoPath string, diff *git.Diff, parsedFiles map[string]*parser.ParsedFile, idx *index.Index) *AnalysisResult {
	startTime := time.Now()

	ctx := &AnalysisContext{
		RepoPath:    repoPath,
		Diff:        diff,
		ParsedFiles: parsedFiles,
		Config:      r.config,
		Index:       idx,
	}

	// Filter to enabled analyzers
	enabled := r.enabledAnalyzers()

	// Run analyzers in parallel
	results := r.runParallel(enabled, ctx)

	// Collect all issues
	allIssues := make([]Issue, 0)
	for _, res := range results {
		if res.err != nil {
			log.Printf("analyzer %s failed: %v", res.name, res.err)
			// Add an info issue about the failure
			allIssues = append(allIssues, Issue{
				ID:       fmt.Sprintf("internal-%s-error", res.name),
				Severity: SeverityInfo,
				Category: "internal",
				Message:  fmt.Sprintf("Analyzer %q failed: %v", res.name, res.err),
			})
			continue
		}
		allIssues = append(allIssues, res.issues...)
	}

	// Sort issues: errors first, then warnings, then info; within same severity, by file then line
	sortIssues(allIssues)

	duration := time.Since(startTime)

	result := &AnalysisResult{
		RepoPath:     repoPath,
		Timestamp:    startTime,
		Duration:     duration,
		FilesChanged: len(diff.Files),
		Issues:       allIssues,
	}

	// Build summary
	result.Summary = buildSummary(result)

	return result
}

// enabledAnalyzers filters analyzers based on config.
func (r *Runner) enabledAnalyzers() []Analyzer {
	enabled := make([]Analyzer, 0)
	for _, a := range r.analyzers {
		if r.isAnalyzerEnabled(a.Name()) {
			enabled = append(enabled, a)
		}
	}
	return enabled
}

// isAnalyzerEnabled checks if an analyzer is enabled in config.
func (r *Runner) isAnalyzerEnabled(name string) bool {
	switch name {
	case "imports":
		return r.config.Analyzers.Imports
	case "consistency":
		return r.config.Analyzers.Consistency
	case "references":
		return r.config.Analyzers.References
	case "tests":
		return r.config.Analyzers.Tests
	case "patterns":
		return r.config.Analyzers.Patterns
	case "security":
		return r.config.Analyzers.Security
	case "hallucination":
		return r.config.Analyzers.Hallucination
	case "errorhandling":
		return r.config.Analyzers.ErrorHandling
	case "depgraph":
		return r.config.Analyzers.DepGraph
	case "deadcode":
		return r.config.Analyzers.DeadCode
	case "complexity":
		return r.config.Analyzers.Complexity
	case "concurrency":
		return r.config.Analyzers.Concurrency
	case "resource":
		return r.config.Analyzers.Resource
	case "migration":
		return r.config.Analyzers.Migration
	case "docdrift":
		return r.config.Analyzers.DocDrift
	default:
		// Unknown analyzers are enabled by default
		return true
	}
}

// runParallel runs all analyzers concurrently and collects results.
func (r *Runner) runParallel(analyzers []Analyzer, ctx *AnalysisContext) []analyzerResult {
	var wg sync.WaitGroup
	results := make([]analyzerResult, len(analyzers))

	for i, a := range analyzers {
		wg.Add(1)
		go func(idx int, analyzer Analyzer) {
			defer wg.Done()
			start := time.Now()

			issues, err := analyzer.Analyze(ctx)
			if issues == nil {
				issues = make([]Issue, 0)
			}

			results[idx] = analyzerResult{
				name:     analyzer.Name(),
				issues:   issues,
				err:      err,
				duration: time.Since(start),
			}
		}(i, a)
	}

	wg.Wait()
	return results
}

// sortIssues sorts by severity (error > warning > info), then file, then line.
func sortIssues(issues []Issue) {
	severityOrder := map[Severity]int{
		SeverityError:   0,
		SeverityWarning: 1,
		SeverityInfo:    2,
	}

	sort.Slice(issues, func(i, j int) bool {
		si := severityOrder[issues[i].Severity]
		sj := severityOrder[issues[j].Severity]
		if si != sj {
			return si < sj
		}
		if issues[i].File != issues[j].File {
			return issues[i].File < issues[j].File
		}
		return issues[i].Line < issues[j].Line
	})
}

// buildSummary creates a human-readable summary string.
func buildSummary(result *AnalysisResult) string {
	errors := result.ErrorCount()
	warnings := result.WarningCount()
	infos := result.InfoCount()
	total := len(result.Issues)

	if total == 0 {
		return fmt.Sprintf("No issues found in %d files", result.FilesChanged)
	}

	parts := make([]string, 0)
	if errors > 0 {
		parts = append(parts, fmt.Sprintf("%d errors", errors))
	}
	if warnings > 0 {
		parts = append(parts, fmt.Sprintf("%d warnings", warnings))
	}
	if infos > 0 {
		parts = append(parts, fmt.Sprintf("%d info", infos))
	}

	summary := fmt.Sprintf("Found %d issues", total)
	if len(parts) > 0 {
		summary += " (" + joinParts(parts) + ")"
	}
	summary += fmt.Sprintf(" in %d files", result.FilesChanged)
	return summary
}

// joinParts joins strings with ", ".
func joinParts(parts []string) string {
	result := ""
	for i, p := range parts {
		if i > 0 {
			result += ", "
		}
		result += p
	}
	return result
}
