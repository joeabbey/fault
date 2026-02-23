package reporter

import (
	"bytes"
	"encoding/json"
	"testing"
	"time"

	"github.com/joeabbey/fault/pkg/analyzer"
)

func TestSARIFReporterNoIssues(t *testing.T) {
	var buf bytes.Buffer
	r := NewSARIFReporterWithWriter(&buf)

	result := &analyzer.AnalysisResult{
		RepoPath:     "/test/repo",
		FilesChanged: 3,
		Issues:       make([]analyzer.Issue, 0),
		Duration:     100 * time.Millisecond,
		Timestamp:    time.Date(2025, 1, 1, 0, 0, 0, 0, time.UTC),
	}

	exitCode := r.Report(result, "error")
	if exitCode != 0 {
		t.Errorf("expected exit code 0, got %d", exitCode)
	}

	var sarif sarifLog
	if err := json.Unmarshal(buf.Bytes(), &sarif); err != nil {
		t.Fatalf("output is not valid JSON: %v\nOutput: %s", err, buf.String())
	}

	if sarif.Version != "2.1.0" {
		t.Errorf("expected SARIF version 2.1.0, got %q", sarif.Version)
	}
	if sarif.Schema != sarifSchemaURI {
		t.Errorf("expected schema URI %q, got %q", sarifSchemaURI, sarif.Schema)
	}
	if len(sarif.Runs) != 1 {
		t.Fatalf("expected 1 run, got %d", len(sarif.Runs))
	}
	if sarif.Runs[0].Tool.Driver.Name != "fault" {
		t.Errorf("expected tool name 'fault', got %q", sarif.Runs[0].Tool.Driver.Name)
	}
	if len(sarif.Runs[0].Results) != 0 {
		t.Errorf("expected 0 results, got %d", len(sarif.Runs[0].Results))
	}
}

func TestSARIFReporterWithIssues(t *testing.T) {
	var buf bytes.Buffer
	r := NewSARIFReporterWithWriter(&buf)

	result := &analyzer.AnalysisResult{
		RepoPath:     "/test/repo",
		FilesChanged: 2,
		Issues: []analyzer.Issue{
			{
				ID:       "import/broken-import",
				Severity: analyzer.SeverityError,
				Category: "imports",
				File:     "path/to/file.go",
				Line:     10,
				Message:  "Broken import 'missing/pkg'",
			},
			{
				ID:       "tests/no-test",
				Severity: analyzer.SeverityWarning,
				Category: "tests",
				File:     "pkg/service.go",
				Line:     0,
				Message:  "No test file for service.go",
			},
			{
				ID:       "patterns/console-debug",
				Severity: analyzer.SeverityInfo,
				Category: "patterns",
				File:     "app.ts",
				Line:     25,
				EndLine:  27,
				Message:  "Debug output left in code",
			},
		},
		Duration:  50 * time.Millisecond,
		Timestamp: time.Date(2025, 1, 1, 0, 0, 0, 0, time.UTC),
	}

	exitCode := r.Report(result, "error")
	if exitCode != 1 {
		t.Errorf("expected exit code 1 with errors, got %d", exitCode)
	}

	var sarif sarifLog
	if err := json.Unmarshal(buf.Bytes(), &sarif); err != nil {
		t.Fatalf("output is not valid JSON: %v", err)
	}

	results := sarif.Runs[0].Results
	if len(results) != 3 {
		t.Fatalf("expected 3 results, got %d", len(results))
	}

	// Check first result — error severity with line.
	r0 := results[0]
	if r0.RuleID != "fault/import/broken-import" {
		t.Errorf("expected ruleId 'fault/import/broken-import', got %q", r0.RuleID)
	}
	if r0.Level != "error" {
		t.Errorf("expected level 'error', got %q", r0.Level)
	}
	if r0.Message.Text != "Broken import 'missing/pkg'" {
		t.Errorf("expected message text, got %q", r0.Message.Text)
	}
	if len(r0.Locations) != 1 {
		t.Fatalf("expected 1 location, got %d", len(r0.Locations))
	}
	loc0 := r0.Locations[0].PhysicalLocation
	if loc0.ArtifactLocation.URI != "path/to/file.go" {
		t.Errorf("expected URI 'path/to/file.go', got %q", loc0.ArtifactLocation.URI)
	}
	if loc0.Region == nil {
		t.Fatal("expected region for issue with line")
	}
	if loc0.Region.StartLine != 10 {
		t.Errorf("expected startLine 10, got %d", loc0.Region.StartLine)
	}

	// Check second result — warning, no line.
	r1 := results[1]
	if r1.RuleID != "fault/tests/no-test" {
		t.Errorf("expected ruleId 'fault/tests/no-test', got %q", r1.RuleID)
	}
	if r1.Level != "warning" {
		t.Errorf("expected level 'warning', got %q", r1.Level)
	}
	if len(r1.Locations) != 1 {
		t.Fatalf("expected 1 location, got %d", len(r1.Locations))
	}
	loc1 := r1.Locations[0].PhysicalLocation
	if loc1.Region != nil {
		t.Errorf("expected no region for issue without line, got %+v", loc1.Region)
	}

	// Check third result — info mapped to note, with endLine.
	r2 := results[2]
	if r2.Level != "note" {
		t.Errorf("expected level 'note' for info severity, got %q", r2.Level)
	}
	if len(r2.Locations) != 1 {
		t.Fatalf("expected 1 location, got %d", len(r2.Locations))
	}
	loc2 := r2.Locations[0].PhysicalLocation
	if loc2.Region == nil {
		t.Fatal("expected region for issue with line")
	}
	if loc2.Region.StartLine != 25 {
		t.Errorf("expected startLine 25, got %d", loc2.Region.StartLine)
	}
	if loc2.Region.EndLine != 27 {
		t.Errorf("expected endLine 27, got %d", loc2.Region.EndLine)
	}
}

func TestSARIFReporterExitCodes(t *testing.T) {
	tests := []struct {
		name    string
		issues  []analyzer.Issue
		blockOn string
		want    int
	}{
		{
			name:    "no issues = 0",
			issues:  make([]analyzer.Issue, 0),
			blockOn: "error",
			want:    0,
		},
		{
			name:    "errors with block_on=error = 1",
			issues:  []analyzer.Issue{{ID: "a", Severity: analyzer.SeverityError, Category: "a", File: "a.go", Message: "x"}},
			blockOn: "error",
			want:    1,
		},
		{
			name:    "warnings with block_on=error = 0",
			issues:  []analyzer.Issue{{ID: "a", Severity: analyzer.SeverityWarning, Category: "a", File: "a.go", Message: "x"}},
			blockOn: "error",
			want:    0,
		},
		{
			name:    "warnings with block_on=warning = 1",
			issues:  []analyzer.Issue{{ID: "a", Severity: analyzer.SeverityWarning, Category: "a", File: "a.go", Message: "x"}},
			blockOn: "warning",
			want:    1,
		},
		{
			name:    "info only = 0",
			issues:  []analyzer.Issue{{ID: "a", Severity: analyzer.SeverityInfo, Category: "a", File: "a.go", Message: "x"}},
			blockOn: "warning",
			want:    0,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			var buf bytes.Buffer
			r := NewSARIFReporterWithWriter(&buf)
			result := &analyzer.AnalysisResult{
				Issues:       tt.issues,
				FilesChanged: 1,
				Duration:     time.Millisecond,
				Timestamp:    time.Date(2025, 1, 1, 0, 0, 0, 0, time.UTC),
			}
			got := r.Report(result, tt.blockOn)
			if got != tt.want {
				t.Errorf("exit code = %d, want %d", got, tt.want)
			}
		})
	}
}

func TestSARIFReporterSchema(t *testing.T) {
	var buf bytes.Buffer
	r := NewSARIFReporterWithWriter(&buf)

	result := &analyzer.AnalysisResult{
		Issues:    make([]analyzer.Issue, 0),
		Duration:  time.Millisecond,
		Timestamp: time.Date(2025, 1, 1, 0, 0, 0, 0, time.UTC),
	}

	r.Report(result, "error")

	// Parse and check all required SARIF fields.
	var raw map[string]interface{}
	if err := json.Unmarshal(buf.Bytes(), &raw); err != nil {
		t.Fatalf("invalid JSON: %v", err)
	}

	if raw["$schema"] != sarifSchemaURI {
		t.Errorf("expected $schema = %q, got %v", sarifSchemaURI, raw["$schema"])
	}
	if raw["version"] != "2.1.0" {
		t.Errorf("expected version = 2.1.0, got %v", raw["version"])
	}

	runs, ok := raw["runs"].([]interface{})
	if !ok || len(runs) != 1 {
		t.Fatalf("expected 1 run, got %v", raw["runs"])
	}

	run := runs[0].(map[string]interface{})
	tool := run["tool"].(map[string]interface{})
	driver := tool["driver"].(map[string]interface{})

	if driver["name"] != "fault" {
		t.Errorf("expected driver name 'fault', got %v", driver["name"])
	}
	if driver["version"] != "0.1.0" {
		t.Errorf("expected driver version '0.1.0', got %v", driver["version"])
	}
	if driver["informationUri"] != "https://github.com/joeabbey/fault" {
		t.Errorf("expected informationUri, got %v", driver["informationUri"])
	}
}

func TestSARIFReporterIssueWithNoFile(t *testing.T) {
	var buf bytes.Buffer
	r := NewSARIFReporterWithWriter(&buf)

	result := &analyzer.AnalysisResult{
		Issues: []analyzer.Issue{
			{
				ID:       "internal/error",
				Severity: analyzer.SeverityInfo,
				Category: "internal",
				Message:  "Analyzer crashed",
			},
		},
		Duration:  time.Millisecond,
		Timestamp: time.Date(2025, 1, 1, 0, 0, 0, 0, time.UTC),
	}

	exitCode := r.Report(result, "error")
	if exitCode != 0 {
		t.Errorf("expected exit code 0, got %d", exitCode)
	}

	var sarif sarifLog
	if err := json.Unmarshal(buf.Bytes(), &sarif); err != nil {
		t.Fatalf("invalid JSON: %v", err)
	}

	results := sarif.Runs[0].Results
	if len(results) != 1 {
		t.Fatalf("expected 1 result, got %d", len(results))
	}

	// No file means no locations.
	if len(results[0].Locations) != 0 {
		t.Errorf("expected 0 locations for issue with no file, got %d", len(results[0].Locations))
	}
}

func TestSARIFReporterCWEMapping(t *testing.T) {
	var buf bytes.Buffer
	r := NewSARIFReporterWithWriter(&buf)

	result := &analyzer.AnalysisResult{
		RepoPath:     "/test/repo",
		FilesChanged: 1,
		Issues: []analyzer.Issue{
			{
				ID:       "security-sql-injection",
				Severity: analyzer.SeverityError,
				Category: "security",
				File:     "db/query.go",
				Line:     42,
				Message:  "Possible SQL injection",
			},
			{
				ID:       "security-xss",
				Severity: analyzer.SeverityError,
				Category: "security",
				File:     "web/handler.go",
				Line:     15,
				Message:  "Potential XSS vulnerability",
			},
			{
				ID:       "patterns/console-debug",
				Severity: analyzer.SeverityInfo,
				Category: "patterns",
				File:     "app.ts",
				Line:     10,
				Message:  "Debug output left in code",
			},
		},
		Duration:  time.Millisecond,
		Timestamp: time.Date(2025, 1, 1, 0, 0, 0, 0, time.UTC),
	}

	r.Report(result, "error")

	var sarif sarifLog
	if err := json.Unmarshal(buf.Bytes(), &sarif); err != nil {
		t.Fatalf("invalid JSON: %v\nOutput: %s", err, buf.String())
	}

	run := sarif.Runs[0]

	// Should have 3 rules (one per unique issue ID).
	if len(run.Tool.Driver.Rules) != 3 {
		t.Fatalf("expected 3 rules, got %d", len(run.Tool.Driver.Rules))
	}

	// Find the sql-injection rule and verify CWE relationship.
	var sqlRule *sarifRule
	for i := range run.Tool.Driver.Rules {
		if run.Tool.Driver.Rules[i].ID == "fault/security-sql-injection" {
			sqlRule = &run.Tool.Driver.Rules[i]
			break
		}
	}
	if sqlRule == nil {
		t.Fatal("expected to find rule fault/security-sql-injection")
	}
	if sqlRule.HelpURI != "https://cwe.mitre.org/data/definitions/89.html" {
		t.Errorf("expected helpUri for CWE-89, got %q", sqlRule.HelpURI)
	}
	if len(sqlRule.Relationships) != 1 {
		t.Fatalf("expected 1 relationship, got %d", len(sqlRule.Relationships))
	}
	if sqlRule.Relationships[0].Target.ID != "CWE-89" {
		t.Errorf("expected relationship target CWE-89, got %q", sqlRule.Relationships[0].Target.ID)
	}
	if sqlRule.Relationships[0].Target.ToolComponent.Name != "CWE" {
		t.Errorf("expected toolComponent name CWE, got %q", sqlRule.Relationships[0].Target.ToolComponent.Name)
	}

	// The non-security rule should have no relationships.
	var patternRule *sarifRule
	for i := range run.Tool.Driver.Rules {
		if run.Tool.Driver.Rules[i].ID == "fault/patterns/console-debug" {
			patternRule = &run.Tool.Driver.Rules[i]
			break
		}
	}
	if patternRule == nil {
		t.Fatal("expected to find rule fault/patterns/console-debug")
	}
	if len(patternRule.Relationships) != 0 {
		t.Errorf("expected 0 relationships for non-CWE rule, got %d", len(patternRule.Relationships))
	}

	// Taxonomy should exist with 2 CWE entries (89 and 79).
	if len(run.Taxonomies) != 1 {
		t.Fatalf("expected 1 taxonomy, got %d", len(run.Taxonomies))
	}
	taxonomy := run.Taxonomies[0]
	if taxonomy.Name != "CWE" {
		t.Errorf("expected taxonomy name CWE, got %q", taxonomy.Name)
	}
	if len(taxonomy.Taxa) != 2 {
		t.Fatalf("expected 2 taxa, got %d", len(taxonomy.Taxa))
	}
	// Taxa should be sorted by ID.
	if taxonomy.Taxa[0].ID != "CWE-79" {
		t.Errorf("expected first taxon CWE-79, got %q", taxonomy.Taxa[0].ID)
	}
	if taxonomy.Taxa[1].ID != "CWE-89" {
		t.Errorf("expected second taxon CWE-89, got %q", taxonomy.Taxa[1].ID)
	}
}

func TestSARIFReporterNoTaxonomyWithoutCWE(t *testing.T) {
	var buf bytes.Buffer
	r := NewSARIFReporterWithWriter(&buf)

	result := &analyzer.AnalysisResult{
		RepoPath:     "/test/repo",
		FilesChanged: 1,
		Issues: []analyzer.Issue{
			{
				ID:       "patterns/console-debug",
				Severity: analyzer.SeverityInfo,
				Category: "patterns",
				File:     "app.ts",
				Line:     10,
				Message:  "Debug output left in code",
			},
		},
		Duration:  time.Millisecond,
		Timestamp: time.Date(2025, 1, 1, 0, 0, 0, 0, time.UTC),
	}

	r.Report(result, "error")

	var sarif sarifLog
	if err := json.Unmarshal(buf.Bytes(), &sarif); err != nil {
		t.Fatalf("invalid JSON: %v", err)
	}

	run := sarif.Runs[0]

	// Should have 1 rule with no CWE relationships.
	if len(run.Tool.Driver.Rules) != 1 {
		t.Fatalf("expected 1 rule, got %d", len(run.Tool.Driver.Rules))
	}
	if len(run.Tool.Driver.Rules[0].Relationships) != 0 {
		t.Errorf("expected 0 relationships, got %d", len(run.Tool.Driver.Rules[0].Relationships))
	}

	// No taxonomy when no CWE mappings are used.
	if len(run.Taxonomies) != 0 {
		t.Errorf("expected 0 taxonomies when no CWE issues, got %d", len(run.Taxonomies))
	}
}

func TestSeverityToSARIFLevel(t *testing.T) {
	tests := []struct {
		severity analyzer.Severity
		want     string
	}{
		{analyzer.SeverityError, "error"},
		{analyzer.SeverityWarning, "warning"},
		{analyzer.SeverityInfo, "note"},
		{analyzer.Severity("unknown"), "none"},
	}

	for _, tt := range tests {
		got := severityToSARIFLevel(tt.severity)
		if got != tt.want {
			t.Errorf("severityToSARIFLevel(%q) = %q, want %q", tt.severity, got, tt.want)
		}
	}
}
