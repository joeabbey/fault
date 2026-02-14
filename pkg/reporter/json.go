package reporter

import (
	"encoding/json"
	"fmt"
	"io"
	"os"

	"github.com/joeabbey/fault/pkg/analyzer"
)

// JSONReporter outputs analysis results as pretty-printed JSON to stdout.
type JSONReporter struct {
	out io.Writer
}

// NewJSONReporter creates a JSON reporter that writes to stdout.
func NewJSONReporter() *JSONReporter {
	return &JSONReporter{
		out: os.Stdout,
	}
}

// NewJSONReporterWithWriter creates a JSON reporter that writes to the given writer.
func NewJSONReporterWithWriter(w io.Writer) *JSONReporter {
	return &JSONReporter{
		out: w,
	}
}

// Report outputs the analysis result as pretty-printed JSON.
// Returns exit code 1 if the result should block based on blockOn setting, 0 otherwise.
func (r *JSONReporter) Report(result *analyzer.AnalysisResult, blockOn string) int {
	data, err := json.MarshalIndent(result, "", "  ")
	if err != nil {
		fmt.Fprintf(r.out, "{\"error\": %q}\n", err.Error())
		return 1
	}

	fmt.Fprintln(r.out, string(data))

	if result.ShouldBlock(blockOn) {
		return 1
	}
	return 0
}
