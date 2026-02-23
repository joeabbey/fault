package analyzer

import (
	"time"

	"github.com/joeabbey/fault/pkg/config"
	"github.com/joeabbey/fault/pkg/git"
	"github.com/joeabbey/fault/pkg/index"
	"github.com/joeabbey/fault/pkg/parser"
)

// Severity represents the severity level of an issue.
type Severity string

const (
	SeverityError   Severity = "error"
	SeverityWarning Severity = "warning"
	SeverityInfo    Severity = "info"
)

// AnalysisResult holds the complete output of a fault check run.
type AnalysisResult struct {
	RepoPath     string        `json:"repo_path"`
	Branch       string        `json:"branch"`
	CommitRange  string        `json:"commit_range"`
	Timestamp    time.Time     `json:"timestamp"`
	Duration     time.Duration `json:"duration"`
	FilesChanged int           `json:"files_changed"`
	Issues       []Issue       `json:"issues"`
	Confidence   *Confidence   `json:"confidence,omitempty"`
	Summary      string        `json:"summary,omitempty"`
}

// Issue represents a single problem found by an analyzer.
type Issue struct {
	ID           string   `json:"id"`
	FixID        string   `json:"fix_id,omitempty"` // stable category ID for auto-fix matching
	Severity     Severity `json:"severity"`
	Category     string   `json:"category"`
	File         string   `json:"file"`
	Line         int      `json:"line,omitempty"`
	EndLine      int      `json:"end_line,omitempty"`
	Message      string   `json:"message"`
	Suggestion   string   `json:"suggestion,omitempty"`
	RelatedFiles []string `json:"related_files,omitempty"`
}

// Confidence represents how confident the analysis is.
type Confidence struct {
	Score   float64            `json:"score"`
	Factors []string           `json:"factors,omitempty"`
	PerFile map[string]float64 `json:"per_file,omitempty"`
}

// Analyzer is the interface all analyzers must implement.
type Analyzer interface {
	// Name returns the unique name of this analyzer.
	Name() string
	// Analyze runs the analyzer on the given context and returns issues.
	Analyze(ctx *AnalysisContext) ([]Issue, error)
}

// AnalysisContext provides data to analyzers.
type AnalysisContext struct {
	RepoPath    string
	Diff        *git.Diff
	ParsedFiles map[string]*parser.ParsedFile
	Config      *config.Config
	Index       *index.Index // Full repo index (may be nil if index build fails)
}

// ErrorCount returns the number of error-severity issues.
func (r *AnalysisResult) ErrorCount() int {
	count := 0
	for _, issue := range r.Issues {
		if issue.Severity == SeverityError {
			count++
		}
	}
	return count
}

// WarningCount returns the number of warning-severity issues.
func (r *AnalysisResult) WarningCount() int {
	count := 0
	for _, issue := range r.Issues {
		if issue.Severity == SeverityWarning {
			count++
		}
	}
	return count
}

// InfoCount returns the number of info-severity issues.
func (r *AnalysisResult) InfoCount() int {
	count := 0
	for _, issue := range r.Issues {
		if issue.Severity == SeverityInfo {
			count++
		}
	}
	return count
}

// ShouldBlock determines if the result should cause a non-zero exit code.
func (r *AnalysisResult) ShouldBlock(blockOn string) bool {
	switch blockOn {
	case "error":
		return r.ErrorCount() > 0
	case "warning":
		return r.ErrorCount() > 0 || r.WarningCount() > 0
	default:
		return r.ErrorCount() > 0
	}
}
