package reporter

import (
	"encoding/json"
	"fmt"
	"io"
	"os"
	"sort"

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
	Tool       sarifTool       `json:"tool"`
	Results    []sarifResult   `json:"results"`
	Taxonomies []sarifTaxonomy `json:"taxonomies,omitempty"`
}

// sarifTool describes the analysis tool.
type sarifTool struct {
	Driver sarifDriver `json:"driver"`
}

// sarifDriver describes the tool's main component.
type sarifDriver struct {
	Name           string      `json:"name"`
	Version        string      `json:"version"`
	InformationURI string      `json:"informationUri"`
	Rules          []sarifRule `json:"rules,omitempty"`
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

// sarifRule describes an analysis rule.
type sarifRule struct {
	ID               string              `json:"id"`
	Name             string              `json:"name,omitempty"`
	ShortDescription sarifMessage        `json:"shortDescription,omitempty"`
	HelpURI          string              `json:"helpUri,omitempty"`
	Relationships    []sarifRelationship `json:"relationships,omitempty"`
}

// sarifRelationship links a rule to a taxonomy entry.
type sarifRelationship struct {
	Target sarifRelationshipTarget `json:"target"`
}

// sarifRelationshipTarget identifies the taxonomy entry.
type sarifRelationshipTarget struct {
	ID            string             `json:"id"`
	ToolComponent sarifToolComponent `json:"toolComponent"`
}

// sarifToolComponent references a taxonomy by index.
type sarifToolComponent struct {
	Name  string `json:"name"`
	Index int    `json:"index"`
}

// sarifTaxonomy represents an external standard like CWE.
type sarifTaxonomy struct {
	Name            string       `json:"name"`
	Version         string       `json:"version"`
	InformationURI  string       `json:"informationUri"`
	IsComprehensive bool         `json:"isComprehensive"`
	Taxa            []sarifTaxon `json:"taxa"`
}

// sarifTaxon represents a single entry in a taxonomy.
type sarifTaxon struct {
	ID               string       `json:"id"`
	Name             string       `json:"name"`
	ShortDescription sarifMessage `json:"shortDescription"`
	HelpURI          string       `json:"helpUri,omitempty"`
}

// --- SARIF construction ---

const (
	sarifSchemaURI = "https://raw.githubusercontent.com/oasis-tcs/sarif-spec/master/Schemata/sarif-schema-2.1.0.json"
	sarifVersion   = "2.1.0"
	toolName       = "fault"
	toolVersion    = "0.1.0"
	toolURI        = "https://github.com/joeabbey/fault"
)

// cweEntry maps a Fault issue ID to a CWE weakness.
type cweEntry struct {
	ID   string // CWE-NNN
	Name string // Short name
}

// cweMap maps Fault security/analyzer issue IDs to CWE entries.
var cweMap = map[string]cweEntry{
	// Security analyzer issues
	"security-sql-injection":         {ID: "CWE-89", Name: "SQL Injection"},
	"security-xss":                   {ID: "CWE-79", Name: "Cross-site Scripting (XSS)"},
	"security-path-traversal":        {ID: "CWE-22", Name: "Path Traversal"},
	"security-hardcoded-credentials": {ID: "CWE-798", Name: "Use of Hard-coded Credentials"},
	"security-weak-crypto":           {ID: "CWE-327", Name: "Use of a Broken or Risky Cryptographic Algorithm"},
	"security-command-injection":     {ID: "CWE-78", Name: "OS Command Injection"},
	"security-code-injection":        {ID: "CWE-94", Name: "Code Injection"},
	"security-xxe":                   {ID: "CWE-611", Name: "XML External Entity Reference"},
	"security-deserialization":       {ID: "CWE-502", Name: "Deserialization of Untrusted Data"},
	"security-ssrf":                  {ID: "CWE-918", Name: "Server-Side Request Forgery (SSRF)"},
	"security-open-redirect":         {ID: "CWE-601", Name: "URL Redirection to Untrusted Site"},
	"security-weak-random":           {ID: "CWE-330", Name: "Use of Insufficiently Random Values"},
	"security-insecure-tls":          {ID: "CWE-295", Name: "Improper Certificate Validation"},
	"security-sensitive-log":         {ID: "CWE-532", Name: "Insertion of Sensitive Information into Log File"},
	"security-eval":                  {ID: "CWE-95", Name: "Eval Injection"},
	"security-prototype-pollution":   {ID: "CWE-1321", Name: "Improperly Controlled Modification of Object Prototype Attributes"},
	"security-regex-dos":             {ID: "CWE-1333", Name: "Inefficient Regular Expression Complexity"},
	"security-unsafe-reflection":     {ID: "CWE-470", Name: "Use of Externally-Controlled Input to Select Classes or Code"},
	// Cross-cutting analyzer issues
	"concurrency-race":             {ID: "CWE-362", Name: "Race Condition"},
	"concurrency-deadlock":         {ID: "CWE-833", Name: "Deadlock"},
	"resource-leak":                {ID: "CWE-404", Name: "Improper Resource Shutdown or Release"},
	"resource-unclosed":            {ID: "CWE-404", Name: "Improper Resource Shutdown or Release"},
	"error-handling-unchecked":     {ID: "CWE-252", Name: "Unchecked Return Value"},
	"error-handling-swallowed":     {ID: "CWE-390", Name: "Detection of Error Condition Without Action"},
	"import-nonexistent":           {ID: "CWE-829", Name: "Inclusion of Functionality from Untrusted Control Sphere"},
	"complexity-high":              {ID: "CWE-1121", Name: "Excessive McCabe Cyclomatic Complexity"},
	"hallucination-nonexistent-api": {ID: "CWE-476", Name: "NULL Pointer Dereference"},
}

// buildSARIF converts an AnalysisResult into a SARIF log.
func buildSARIF(result *analyzer.AnalysisResult) sarifLog {
	results := make([]sarifResult, 0, len(result.Issues))
	ruleMap := make(map[string]*sarifRule) // dedup rules by ID
	cweRefs := make(map[string]cweEntry)   // track referenced CWEs

	for _, issue := range result.Issues {
		ruleID := "fault/" + issue.ID

		// Build or reuse rule
		if _, exists := ruleMap[ruleID]; !exists {
			rule := &sarifRule{
				ID:               ruleID,
				Name:             issue.ID,
				ShortDescription: sarifMessage{Text: issue.Category + " issue"},
			}

			// Check for CWE mapping
			if cwe, ok := cweMap[issue.ID]; ok {
				rule.HelpURI = "https://cwe.mitre.org/data/definitions/" + cwe.ID[4:] + ".html"
				rule.Relationships = []sarifRelationship{
					{
						Target: sarifRelationshipTarget{
							ID: cwe.ID,
							ToolComponent: sarifToolComponent{
								Name:  "CWE",
								Index: 0,
							},
						},
					},
				}
				cweRefs[cwe.ID] = cwe
			}

			ruleMap[ruleID] = rule
		}

		sr := sarifResult{
			RuleID:  ruleID,
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

	// Build sorted rules list
	rules := make([]sarifRule, 0, len(ruleMap))
	for _, rule := range ruleMap {
		rules = append(rules, *rule)
	}
	sort.Slice(rules, func(i, j int) bool {
		return rules[i].ID < rules[j].ID
	})

	// Build CWE taxonomy if we have any CWE references
	var taxonomies []sarifTaxonomy
	if len(cweRefs) > 0 {
		taxa := make([]sarifTaxon, 0, len(cweRefs))
		for _, cwe := range cweRefs {
			taxa = append(taxa, sarifTaxon{
				ID:               cwe.ID,
				Name:             cwe.Name,
				ShortDescription: sarifMessage{Text: cwe.Name},
				HelpURI:          "https://cwe.mitre.org/data/definitions/" + cwe.ID[4:] + ".html",
			})
		}
		sort.Slice(taxa, func(i, j int) bool {
			return taxa[i].ID < taxa[j].ID
		})

		taxonomies = []sarifTaxonomy{
			{
				Name:            "CWE",
				Version:         "4.13",
				InformationURI:  "https://cwe.mitre.org/data/published/cwe_v4.13.pdf",
				IsComprehensive: false,
				Taxa:            taxa,
			},
		}
	}

	run := sarifRun{
		Tool: sarifTool{
			Driver: sarifDriver{
				Name:           toolName,
				Version:        toolVersion,
				InformationURI: toolURI,
				Rules:          rules,
			},
		},
		Results: results,
	}
	if len(taxonomies) > 0 {
		run.Taxonomies = taxonomies
	}

	return sarifLog{
		Schema:  sarifSchemaURI,
		Version: sarifVersion,
		Runs:    []sarifRun{run},
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
