package reporter

import (
	"encoding/json"
	"fmt"
	"io"
	"os"

	"github.com/joeabbey/fault/pkg/analyzer"
)

// SARIFReporter outputs analysis results in SARIF v2.1.0 format.
type SARIFReporter struct {
	out io.Writer
}

// NewSARIFReporter creates a SARIF reporter that writes to stdout.
func NewSARIFReporter() *SARIFReporter {
	return &SARIFReporter{
		out: os.Stdout,
	}
}

// NewSARIFReporterWithWriter creates a SARIF reporter that writes to the given writer.
func NewSARIFReporterWithWriter(w io.Writer) *SARIFReporter {
	return &SARIFReporter{
		out: w,
	}
}

// Report outputs the analysis result in SARIF v2.1.0 format.
// Returns exit code 1 if the result should block based on blockOn setting, 0 otherwise.
func (r *SARIFReporter) Report(result *analyzer.AnalysisResult, blockOn string) int {
	sarif := buildSARIF(result)

	data, err := json.MarshalIndent(sarif, "", "  ")
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

// --- SARIF types ---

// sarifLog is the root SARIF v2.1.0 document.
type sarifLog struct {
	Schema  string     `json:"$schema"`
	Version string     `json:"version"`
	Runs    []sarifRun `json:"runs"`
}

// sarifRun represents a single analysis run.
type sarifRun struct {
	Tool    sarifTool     `json:"tool"`
	Results []sarifResult `json:"results"`
}

// sarifTool describes the analysis tool.
type sarifTool struct {
	Driver sarifDriver `json:"driver"`
}

// sarifDriver describes the tool's main component.
type sarifDriver struct {
	Name           string `json:"name"`
	Version        string `json:"version"`
	InformationURI string `json:"informationUri"`
}

// sarifResult is a single finding.
type sarifResult struct {
	RuleID    string          `json:"ruleId"`
	Level     string          `json:"level"`
	Message   sarifMessage    `json:"message"`
	Locations []sarifLocation `json:"locations,omitempty"`
}

// sarifMessage holds the result message.
type sarifMessage struct {
	Text string `json:"text"`
}

// sarifLocation is a physical location in source.
type sarifLocation struct {
	PhysicalLocation sarifPhysicalLocation `json:"physicalLocation"`
}

// sarifPhysicalLocation points to a file and region.
type sarifPhysicalLocation struct {
	ArtifactLocation sarifArtifactLocation `json:"artifactLocation"`
	Region           *sarifRegion          `json:"region,omitempty"`
}

// sarifArtifactLocation identifies the file.
type sarifArtifactLocation struct {
	URI string `json:"uri"`
}

// sarifRegion identifies the line range.
type sarifRegion struct {
	StartLine int `json:"startLine"`
	EndLine   int `json:"endLine,omitempty"`
}

// --- SARIF construction ---

const (
	sarifSchemaURI = "https://raw.githubusercontent.com/oasis-tcs/sarif-spec/master/Schemata/sarif-schema-2.1.0.json"
	sarifVersion   = "2.1.0"
	toolName       = "fault"
	toolVersion    = "0.1.0"
	toolURI        = "https://github.com/joeabbey/fault"
)

// buildSARIF converts an AnalysisResult into a SARIF log.
func buildSARIF(result *analyzer.AnalysisResult) sarifLog {
	results := make([]sarifResult, 0, len(result.Issues))

	for _, issue := range result.Issues {
		sr := sarifResult{
			RuleID:  "fault/" + issue.ID,
			Level:   severityToSARIFLevel(issue.Severity),
			Message: sarifMessage{Text: issue.Message},
		}

		if issue.File != "" {
			loc := sarifLocation{
				PhysicalLocation: sarifPhysicalLocation{
					ArtifactLocation: sarifArtifactLocation{
						URI: issue.File,
					},
				},
			}

			if issue.Line > 0 {
				region := &sarifRegion{
					StartLine: issue.Line,
				}
				if issue.EndLine > 0 {
					region.EndLine = issue.EndLine
				}
				loc.PhysicalLocation.Region = region
			}

			sr.Locations = []sarifLocation{loc}
		}

		results = append(results, sr)
	}

	return sarifLog{
		Schema:  sarifSchemaURI,
		Version: sarifVersion,
		Runs: []sarifRun{
			{
				Tool: sarifTool{
					Driver: sarifDriver{
						Name:           toolName,
						Version:        toolVersion,
						InformationURI: toolURI,
					},
				},
				Results: results,
			},
		},
	}
}

// severityToSARIFLevel maps fault severity levels to SARIF levels.
func severityToSARIFLevel(s analyzer.Severity) string {
	switch s {
	case analyzer.SeverityError:
		return "error"
	case analyzer.SeverityWarning:
		return "warning"
	case analyzer.SeverityInfo:
		return "note"
	default:
		return "none"
	}
}
