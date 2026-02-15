package analyzer

import (
	"encoding/json"
	"os"
	"path/filepath"
	"testing"
)

func TestSaveBaselineCreatesValidJSON(t *testing.T) {
	dir := t.TempDir()
	path := filepath.Join(dir, ".fault-baseline.json")

	result := &AnalysisResult{
		Issues: []Issue{
			{ID: "broken-import-1", Category: "imports", File: "src/app.ts", Line: 5, Message: "import './utils' not found"},
			{ID: "unused-var-1", Category: "consistency", File: "src/lib.ts", Line: 10, Message: "unused variable 'x'"},
		},
	}

	if err := SaveBaseline(path, result); err != nil {
		t.Fatalf("SaveBaseline failed: %v", err)
	}

	data, err := os.ReadFile(path)
	if err != nil {
		t.Fatalf("reading baseline file: %v", err)
	}

	var baseline Baseline
	if err := json.Unmarshal(data, &baseline); err != nil {
		t.Fatalf("invalid JSON in baseline: %v", err)
	}

	if baseline.Version != 1 {
		t.Errorf("expected version 1, got %d", baseline.Version)
	}
	if len(baseline.Issues) != 2 {
		t.Errorf("expected 2 issues, got %d", len(baseline.Issues))
	}
}

func TestLoadBaselineReadsBack(t *testing.T) {
	dir := t.TempDir()
	path := filepath.Join(dir, ".fault-baseline.json")

	original := &AnalysisResult{
		Issues: []Issue{
			{ID: "broken-import-1", Category: "imports", File: "src/app.ts", Line: 5, Message: "import './utils' not found"},
		},
	}

	if err := SaveBaseline(path, original); err != nil {
		t.Fatalf("SaveBaseline failed: %v", err)
	}

	baseline, err := LoadBaseline(path)
	if err != nil {
		t.Fatalf("LoadBaseline failed: %v", err)
	}

	if len(baseline.Issues) != 1 {
		t.Fatalf("expected 1 issue, got %d", len(baseline.Issues))
	}

	entry := baseline.Issues[0]
	if entry.ID != "broken-import-1" {
		t.Errorf("expected ID %q, got %q", "broken-import-1", entry.ID)
	}
	if entry.Category != "imports" {
		t.Errorf("expected category %q, got %q", "imports", entry.Category)
	}
	if entry.File != "src/app.ts" {
		t.Errorf("expected file %q, got %q", "src/app.ts", entry.File)
	}
	if entry.Message != "import './utils' not found" {
		t.Errorf("expected message %q, got %q", "import './utils' not found", entry.Message)
	}
}

func TestFilterBaselineRemovesMatching(t *testing.T) {
	baseline := &Baseline{
		Version: 1,
		Issues: []BaselineEntry{
			{ID: "broken-import-1", Category: "import", File: "src/app.ts", Message: "import './utils' not found"},
		},
	}

	issues := []Issue{
		{ID: "broken-import-1", Category: "imports", File: "src/app.ts", Line: 5, Message: "import './utils' not found"},
		{ID: "unused-var-1", Category: "consistency", File: "src/lib.ts", Line: 10, Message: "unused variable 'x'"},
	}

	filtered := FilterBaseline(issues, baseline)
	if len(filtered) != 1 {
		t.Fatalf("expected 1 issue after filtering, got %d", len(filtered))
	}
	if filtered[0].ID != "unused-var-1" {
		t.Errorf("expected remaining issue %q, got %q", "unused-var-1", filtered[0].ID)
	}
}

func TestFilterBaselineKeepsNewIssues(t *testing.T) {
	baseline := &Baseline{
		Version: 1,
		Issues: []BaselineEntry{
			{ID: "broken-import-1", Category: "imports", File: "src/app.ts", Message: "import './utils' not found"},
		},
	}

	issues := []Issue{
		{ID: "new-import-1", Category: "imports", File: "src/new.ts", Line: 3, Message: "import './missing' not found"},
	}

	filtered := FilterBaseline(issues, baseline)
	if len(filtered) != 1 {
		t.Fatalf("expected 1 issue (new), got %d", len(filtered))
	}
}

func TestFilterBaselineHandlesLineNumberChanges(t *testing.T) {
	baseline := &Baseline{
		Version: 1,
		Issues: []BaselineEntry{
			{ID: "broken-import-1", Category: "imports", File: "src/app.ts", Message: "import './utils' not found"},
		},
	}

	// Same issue but ID and line moved (baseline doesn't store line; IDs shouldn't need to match).
	issues := []Issue{
		{ID: "broken-import-2", Category: "imports", File: "src/app.ts", Line: 15, Message: "import './utils' not found"},
	}

	filtered := FilterBaseline(issues, baseline)
	if len(filtered) != 0 {
		t.Errorf("expected 0 issues (baseline should match regardless of line), got %d", len(filtered))
	}
}

func TestFilterBaselineFuzzyMessageMatch(t *testing.T) {
	baseline := &Baseline{
		Version: 1,
		Issues: []BaselineEntry{
			{ID: "broken-import-1", Category: "imports", File: "src/app.ts", Message: "import './utils' not found"},
		},
	}

	// Message is a superset (contains the baseline message)
	issues := []Issue{
		{ID: "broken-import-1", Category: "imports", File: "src/app.ts", Line: 5, Message: "import './utils' not found in project"},
	}

	filtered := FilterBaseline(issues, baseline)
	if len(filtered) != 0 {
		t.Errorf("expected 0 issues (fuzzy message match), got %d", len(filtered))
	}
}

func TestFilterBaselineNilBaseline(t *testing.T) {
	issues := []Issue{
		{ID: "test-1", Category: "imports", File: "a.ts", Line: 1, Message: "broken"},
	}

	filtered := FilterBaseline(issues, nil)
	if len(filtered) != 1 {
		t.Errorf("expected 1 issue with nil baseline, got %d", len(filtered))
	}
}

func TestFilterBaselineEmptyBaseline(t *testing.T) {
	baseline := &Baseline{Version: 1, Issues: []BaselineEntry{}}

	issues := []Issue{
		{ID: "test-1", Category: "imports", File: "a.ts", Line: 1, Message: "broken"},
	}

	filtered := FilterBaseline(issues, baseline)
	if len(filtered) != 1 {
		t.Errorf("expected 1 issue with empty baseline, got %d", len(filtered))
	}
}

func TestParseSuppressionCommentsSingleCategory(t *testing.T) {
	cats := ParseSuppressionComments("// fault:ignore imports")
	if len(cats) != 1 || cats[0] != "imports" {
		t.Errorf("expected [imports], got %v", cats)
	}
}

func TestParseSuppressionCommentsMultipleCategories(t *testing.T) {
	cats := ParseSuppressionComments("// fault:ignore imports,consistency")
	if len(cats) != 2 {
		t.Fatalf("expected 2 categories, got %d: %v", len(cats), cats)
	}
	if cats[0] != "imports" || cats[1] != "consistency" {
		t.Errorf("expected [imports, consistency], got %v", cats)
	}
}

func TestParseSuppressionCommentsWildcard(t *testing.T) {
	cats := ParseSuppressionComments("// fault:ignore *")
	if len(cats) != 1 || cats[0] != "*" {
		t.Errorf("expected [*], got %v", cats)
	}
}

func TestParseSuppressionCommentsBareDirective(t *testing.T) {
	cats := ParseSuppressionComments("// fault:ignore")
	if len(cats) != 1 || cats[0] != "*" {
		t.Errorf("expected [*] for bare directive, got %v", cats)
	}
}

func TestParseSuppressionCommentsNoDirective(t *testing.T) {
	cats := ParseSuppressionComments("// this is a normal comment")
	if cats != nil {
		t.Errorf("expected nil for normal comment, got %v", cats)
	}
}

func TestParseSuppressionCommentsPython(t *testing.T) {
	cats := ParseSuppressionComments("# fault:ignore imports")
	if len(cats) != 1 || cats[0] != "imports" {
		t.Errorf("expected [imports], got %v", cats)
	}
}

func TestParseSuppressionCommentsBlockComment(t *testing.T) {
	cats := ParseSuppressionComments("/* fault:ignore imports */")
	if len(cats) != 1 || cats[0] != "imports" {
		t.Errorf("expected [imports], got %v", cats)
	}
}

func TestFilterSuppressedInlineIgnore(t *testing.T) {
	issues := []Issue{
		{ID: "broken-import-1", Category: "imports", File: "src/app.ts", Line: 2, Message: "broken import"},
	}

	fileLines := map[string][]string{
		"src/app.ts": {
			"// fault:ignore imports",      // line 1 (above the issue)
			"import { x } from './gone';", // line 2 (the issue line)
			"console.log(x);",              // line 3
		},
	}

	filtered := FilterSuppressed(issues, fileLines)
	if len(filtered) != 0 {
		t.Errorf("expected 0 issues (suppressed by line above), got %d", len(filtered))
	}
}

func TestFilterSuppressedSameLineIgnore(t *testing.T) {
	issues := []Issue{
		{ID: "broken-import-1", Category: "imports", File: "src/app.ts", Line: 1, Message: "broken import"},
	}

	fileLines := map[string][]string{
		"src/app.ts": {
			"import { x } from './gone'; // fault:ignore imports", // line 1
		},
	}

	filtered := FilterSuppressed(issues, fileLines)
	if len(filtered) != 0 {
		t.Errorf("expected 0 issues (suppressed on same line), got %d", len(filtered))
	}
}

func TestFilterSuppressedWildcard(t *testing.T) {
	issues := []Issue{
		{ID: "broken-import-1", Category: "imports", File: "src/app.ts", Line: 2, Message: "broken import"},
		{ID: "unused-var-1", Category: "consistency", File: "src/app.ts", Line: 2, Message: "unused var"},
	}

	fileLines := map[string][]string{
		"src/app.ts": {
			"// fault:ignore *",           // line 1
			"import { x } from './gone';", // line 2
		},
	}

	filtered := FilterSuppressed(issues, fileLines)
	if len(filtered) != 0 {
		t.Errorf("expected 0 issues (wildcard suppression), got %d", len(filtered))
	}
}

func TestFilterSuppressedWrongCategory(t *testing.T) {
	issues := []Issue{
		{ID: "broken-import-1", Category: "imports", File: "src/app.ts", Line: 2, Message: "broken import"},
	}

	fileLines := map[string][]string{
		"src/app.ts": {
			"// fault:ignore consistency",  // line 1 (wrong category)
			"import { x } from './gone';", // line 2
		},
	}

	filtered := FilterSuppressed(issues, fileLines)
	if len(filtered) != 1 {
		t.Errorf("expected 1 issue (wrong category not suppressed), got %d", len(filtered))
	}
}

func TestFilterSuppressedNoFileLines(t *testing.T) {
	issues := []Issue{
		{ID: "test-1", Category: "imports", File: "src/app.ts", Line: 1, Message: "broken"},
	}

	filtered := FilterSuppressed(issues, nil)
	if len(filtered) != 1 {
		t.Errorf("expected 1 issue with nil fileLines, got %d", len(filtered))
	}
}

func TestFilterSuppressedNoLineNumber(t *testing.T) {
	issues := []Issue{
		{ID: "test-1", Category: "imports", File: "src/app.ts", Line: 0, Message: "broken"},
	}

	fileLines := map[string][]string{
		"src/app.ts": {"// fault:ignore imports", "code"},
	}

	filtered := FilterSuppressed(issues, fileLines)
	if len(filtered) != 1 {
		t.Errorf("expected 1 issue (no line number, can't suppress), got %d", len(filtered))
	}
}

func TestLoadBaselineFileNotFound(t *testing.T) {
	_, err := LoadBaseline("/nonexistent/.fault-baseline.json")
	if err == nil {
		t.Error("expected error for missing file")
	}
}

func TestSaveBaselineEmptyResult(t *testing.T) {
	dir := t.TempDir()
	path := filepath.Join(dir, ".fault-baseline.json")

	result := &AnalysisResult{Issues: []Issue{}}
	if err := SaveBaseline(path, result); err != nil {
		t.Fatalf("SaveBaseline failed: %v", err)
	}

	baseline, err := LoadBaseline(path)
	if err != nil {
		t.Fatalf("LoadBaseline failed: %v", err)
	}
	if len(baseline.Issues) != 0 {
		t.Errorf("expected 0 issues, got %d", len(baseline.Issues))
	}
}
