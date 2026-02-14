package analyzer

import (
	"os"
	"path/filepath"
	"strings"

	"github.com/joeabbey/fault/pkg/git"
	"github.com/joeabbey/fault/pkg/parser"
)

// TestImpactAnalyzer checks whether changed source files have corresponding test coverage.
type TestImpactAnalyzer struct{}

// NewTestImpactAnalyzer creates a new test impact analyzer.
func NewTestImpactAnalyzer() *TestImpactAnalyzer {
	return &TestImpactAnalyzer{}
}

// Name returns the analyzer name.
func (a *TestImpactAnalyzer) Name() string {
	return "tests"
}

// Analyze checks that source file changes have corresponding test file changes.
func (a *TestImpactAnalyzer) Analyze(ctx *AnalysisContext) ([]Issue, error) {
	issues := make([]Issue, 0)

	if ctx.Diff == nil || len(ctx.Diff.Files) == 0 {
		return issues, nil
	}

	// Build a set of all changed file paths for quick lookup.
	changedFiles := make(map[string]bool, len(ctx.Diff.Files))
	for _, f := range ctx.Diff.Files {
		changedFiles[f.Path] = true
	}

	for _, fileDiff := range ctx.Diff.Files {
		path := fileDiff.Path

		// Skip deleted files — no test needed for removed code.
		if fileDiff.Status == "deleted" {
			continue
		}

		// Skip non-source files (configs, docs, etc.)
		if !isSourceFile(path) {
			continue
		}

		// Determine if this is a test file or a source file.
		if isTestFile(path) {
			// Test-only change: test file modified but corresponding source wasn't.
			issues = append(issues, a.checkTestOnlyChange(path, changedFiles, ctx)...)
			continue
		}

		// This is a source file. Check for a corresponding test file.
		issues = append(issues, a.checkSourceHasTest(fileDiff, changedFiles, ctx)...)
	}

	return issues, nil
}

// checkSourceHasTest checks if a source file has a corresponding test file
// either in the diff or on disk.
func (a *TestImpactAnalyzer) checkSourceHasTest(fileDiff git.FileDiff, changedFiles map[string]bool, ctx *AnalysisContext) []Issue {
	issues := make([]Issue, 0)
	path := fileDiff.Path

	testPaths := possibleTestPaths(path)

	// Check if any test file is in the diff.
	for _, tp := range testPaths {
		if changedFiles[tp] {
			return issues // Test file was also changed — all good.
		}
	}

	// Check if any test file exists on disk.
	testExists := false
	for _, tp := range testPaths {
		if fileExistsOnDisk(ctx.RepoPath, tp) {
			testExists = true
			break
		}
	}

	if fileDiff.Status == "added" && !testExists {
		issues = append(issues, Issue{
			ID:         "tests/new-file-no-test",
			Severity:   SeverityWarning,
			Category:   "tests",
			File:       path,
			Message:    "New source file has no corresponding test file",
			Suggestion: "Add a test file: " + testPaths[0],
		})
	} else if fileDiff.Status == "modified" && !testExists {
		issues = append(issues, Issue{
			ID:         "tests/changed-no-test",
			Severity:   SeverityWarning,
			Category:   "tests",
			File:       path,
			Message:    "Changed source file has no corresponding test file",
			Suggestion: "Add a test file: " + testPaths[0],
		})
	}

	return issues
}

// checkTestOnlyChange reports an info-level issue when a test file is modified
// but the corresponding source file is not.
func (a *TestImpactAnalyzer) checkTestOnlyChange(testPath string, changedFiles map[string]bool, ctx *AnalysisContext) []Issue {
	issues := make([]Issue, 0)

	sourcePath := sourcePathFromTest(testPath)
	if sourcePath == "" {
		return issues
	}

	// If the corresponding source file was also changed, not a test-only change.
	if changedFiles[sourcePath] {
		return issues
	}

	// Verify the source file exists on disk (it might be a standalone test file).
	if !fileExistsOnDisk(ctx.RepoPath, sourcePath) {
		return issues
	}

	issues = append(issues, Issue{
		ID:           "tests/test-only-change",
		Severity:     SeverityInfo,
		Category:     "tests",
		File:         testPath,
		Message:      "Test file modified without corresponding source change",
		Suggestion:   "This may be intentional (fixing a flaky test), but verify the source doesn't need changes too",
		RelatedFiles: []string{sourcePath},
	})

	return issues
}

// isSourceFile returns true if the file is a recognized source file (not config, docs, etc.)
func isSourceFile(path string) bool {
	lang := parser.DetectLanguage(path)
	return lang != ""
}

// isTestFile returns true if the path looks like a test file.
func isTestFile(path string) bool {
	base := filepath.Base(path)
	ext := filepath.Ext(base)
	nameNoExt := strings.TrimSuffix(base, ext)

	switch ext {
	case ".go":
		return strings.HasSuffix(nameNoExt, "_test")
	case ".ts", ".tsx", ".js", ".jsx", ".mjs", ".cjs":
		return strings.HasSuffix(nameNoExt, ".test") ||
			strings.HasSuffix(nameNoExt, ".spec") ||
			isInTestsDir(path)
	case ".py":
		return strings.HasPrefix(nameNoExt, "test_") ||
			strings.HasSuffix(nameNoExt, "_test") ||
			isInTestsDir(path)
	}
	return false
}

// isInTestsDir checks if a path is inside a __tests__ or tests directory.
func isInTestsDir(path string) bool {
	parts := strings.Split(filepath.ToSlash(path), "/")
	for _, p := range parts {
		if p == "__tests__" || p == "tests" {
			return true
		}
	}
	return false
}

// possibleTestPaths returns the list of conventional test file paths for a source file.
func possibleTestPaths(sourcePath string) []string {
	dir := filepath.Dir(sourcePath)
	base := filepath.Base(sourcePath)
	ext := filepath.Ext(base)
	nameNoExt := strings.TrimSuffix(base, ext)

	paths := make([]string, 0, 4)

	switch ext {
	case ".go":
		paths = append(paths, filepath.Join(dir, nameNoExt+"_test.go"))
	case ".ts", ".tsx", ".js", ".jsx", ".mjs", ".cjs":
		// foo.ts -> foo.test.ts, foo.spec.ts, __tests__/foo.test.ts
		paths = append(paths, filepath.Join(dir, nameNoExt+".test"+ext))
		paths = append(paths, filepath.Join(dir, nameNoExt+".spec"+ext))
		paths = append(paths, filepath.Join(dir, "__tests__", nameNoExt+".test"+ext))
	case ".py":
		// foo.py -> test_foo.py, tests/test_foo.py
		paths = append(paths, filepath.Join(dir, "test_"+base))
		paths = append(paths, filepath.Join(dir, "tests", "test_"+base))
	}

	// Normalize to forward slashes for consistent matching.
	for i, p := range paths {
		paths[i] = filepath.ToSlash(p)
	}

	return paths
}

// sourcePathFromTest returns the likely source file path given a test file path.
// Returns empty string if the mapping can't be determined.
func sourcePathFromTest(testPath string) string {
	dir := filepath.Dir(testPath)
	base := filepath.Base(testPath)
	ext := filepath.Ext(base)
	nameNoExt := strings.TrimSuffix(base, ext)

	switch ext {
	case ".go":
		if strings.HasSuffix(nameNoExt, "_test") {
			srcName := strings.TrimSuffix(nameNoExt, "_test") + ext
			return filepath.ToSlash(filepath.Join(dir, srcName))
		}
	case ".ts", ".tsx", ".js", ".jsx", ".mjs", ".cjs":
		if strings.HasSuffix(nameNoExt, ".test") {
			srcName := strings.TrimSuffix(nameNoExt, ".test") + ext
			// If in __tests__ dir, go up one level.
			if filepath.Base(dir) == "__tests__" {
				return filepath.ToSlash(filepath.Join(filepath.Dir(dir), srcName))
			}
			return filepath.ToSlash(filepath.Join(dir, srcName))
		}
		if strings.HasSuffix(nameNoExt, ".spec") {
			srcName := strings.TrimSuffix(nameNoExt, ".spec") + ext
			return filepath.ToSlash(filepath.Join(dir, srcName))
		}
	case ".py":
		if strings.HasPrefix(nameNoExt, "test_") {
			srcName := strings.TrimPrefix(nameNoExt, "test_") + ext
			// If in tests/ dir, go up one level.
			if filepath.Base(dir) == "tests" {
				return filepath.ToSlash(filepath.Join(filepath.Dir(dir), srcName))
			}
			return filepath.ToSlash(filepath.Join(dir, srcName))
		}
	}

	return ""
}

// fileExistsOnDisk checks if a file exists relative to the repo root.
func fileExistsOnDisk(repoPath, relPath string) bool {
	absPath := filepath.Join(repoPath, relPath)
	_, err := os.Stat(absPath)
	return err == nil
}
