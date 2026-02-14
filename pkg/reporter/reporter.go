package reporter

import (
	"github.com/joeabbey/fault/pkg/analyzer"
)

// Reporter formats and outputs analysis results.
type Reporter interface {
	// Report outputs the analysis result.
	// Returns the exit code: 0 for success, 1 for blocking issues.
	Report(result *analyzer.AnalysisResult, blockOn string) int
}
