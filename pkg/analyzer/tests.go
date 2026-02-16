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

		// Skip files that don't conventionally need tests.
		if isTestExempt(path) {
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

// isTestExempt returns true for files that conventionally don't need test files.
// This includes CLI entry points, migration files, and scripts.
func isTestExempt(path string) bool {
	base := filepath.Base(path)

	// Go main.go files in cmd/ directories are entry points.
	if base == "main.go" && strings.Contains(filepath.ToSlash(path), "cmd/") {
		return true
	}

	// Migration files.
	if strings.Contains(filepath.ToSlash(path), "migration") {
		return true
	}

	// Script files in scripts/ directory.
	if strings.HasPrefix(filepath.ToSlash(path), "scripts/") {
		return true
	}

	return false
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
	case ".rb", ".rake":
		return strings.HasSuffix(nameNoExt, "_test") ||
			strings.HasSuffix(nameNoExt, "_spec") ||
			isInTestsDir(path) ||
			isInSpecDir(path)
	case ".kt", ".kts":
		return strings.HasSuffix(nameNoExt, "Test") ||
			strings.HasSuffix(nameNoExt, "Tests") ||
			strings.HasSuffix(nameNoExt, "Spec") ||
			isInTestsDir(path)
	case ".cs":
		return strings.HasSuffix(nameNoExt, "Test") ||
			strings.HasSuffix(nameNoExt, "Tests") ||
			isInTestsDir(path) ||
			isInCSharpTestDir(path)
	case ".php":
		return strings.HasSuffix(nameNoExt, "Test") ||
			strings.HasSuffix(nameNoExt, "_test") ||
			isInTestsDir(path)
	case ".swift":
		return strings.HasSuffix(nameNoExt, "Tests") ||
			strings.HasSuffix(nameNoExt, "Test") ||
			isInTestsDir(path)
	case ".c", ".h":
		return strings.HasSuffix(nameNoExt, "_test") ||
			strings.HasPrefix(nameNoExt, "test_") ||
			isInTestsDir(path)
	case ".cpp", ".cc", ".cxx", ".hpp", ".hxx":
		return strings.HasSuffix(nameNoExt, "_test") ||
			strings.HasPrefix(nameNoExt, "test_") ||
			isInTestsDir(path)
	case ".m", ".mm":
		return strings.HasSuffix(nameNoExt, "Tests") ||
			strings.HasSuffix(nameNoExt, "Test") ||
			isInTestsDir(path)
	case ".sh", ".bash":
		return strings.HasPrefix(nameNoExt, "test_") ||
			strings.HasSuffix(nameNoExt, "_test") ||
			isInTestsDir(path)
	case ".dart":
		return strings.HasSuffix(nameNoExt, "_test") ||
			isInTestsDir(path)
	case ".scala":
		return strings.HasSuffix(nameNoExt, "Spec") ||
			strings.HasSuffix(nameNoExt, "Test") ||
			strings.HasSuffix(nameNoExt, "Suite") ||
			isInTestsDir(path)
	case ".r":
		return strings.HasPrefix(nameNoExt, "test_") ||
			strings.HasPrefix(nameNoExt, "test-") ||
			isInTestsDir(path)
	case ".ex", ".exs":
		return strings.HasSuffix(nameNoExt, "_test") ||
			isInTestsDir(path)
	case ".lua":
		return strings.HasPrefix(nameNoExt, "test_") ||
			strings.HasSuffix(nameNoExt, "_test") ||
			strings.HasSuffix(nameNoExt, "_spec") ||
			isInTestsDir(path)
	}
	return false
}

// isInTestsDir checks if a path is inside a __tests__ or tests directory.
func isInTestsDir(path string) bool {
	parts := strings.Split(filepath.ToSlash(path), "/")
	for _, p := range parts {
		if p == "__tests__" || p == "tests" || p == "test" {
			return true
		}
	}
	return false
}

// isInSpecDir checks if a path is inside a spec directory (Ruby convention).
func isInSpecDir(path string) bool {
	parts := strings.Split(filepath.ToSlash(path), "/")
	for _, p := range parts {
		if p == "spec" {
			return true
		}
	}
	return false
}

// isInCSharpTestDir checks if a path is in a C# test directory convention.
func isInCSharpTestDir(path string) bool {
	parts := strings.Split(filepath.ToSlash(path), "/")
	for _, p := range parts {
		if p == "Tests" || p == "test" || strings.HasSuffix(p, ".Tests") || strings.HasSuffix(p, ".Test") {
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
	case ".kt", ".kts":
		// UserService.kt -> UserServiceTest.kt, UserServiceTests.kt
		paths = append(paths, filepath.Join(dir, nameNoExt+"Test"+ext))
		paths = append(paths, filepath.Join(dir, nameNoExt+"Tests"+ext))
	case ".cs":
		// Foo.cs -> FooTest.cs, FooTests.cs, Tests/FooTests.cs
		paths = append(paths, filepath.Join(dir, nameNoExt+"Test"+ext))
		paths = append(paths, filepath.Join(dir, nameNoExt+"Tests"+ext))
		paths = append(paths, filepath.Join(dir, "Tests", nameNoExt+"Tests"+ext))
	case ".php":
		// UserController.php -> UserControllerTest.php, tests/UserControllerTest.php
		paths = append(paths, filepath.Join(dir, nameNoExt+"Test"+ext))
		paths = append(paths, filepath.Join(dir, "tests", nameNoExt+"Test"+ext))
		paths = append(paths, filepath.Join("tests", nameNoExt+"Test"+ext))
	case ".swift":
		// Foo.swift -> FooTests.swift, FooTest.swift
		paths = append(paths, filepath.Join(dir, nameNoExt+"Tests.swift"))
		paths = append(paths, filepath.Join(dir, nameNoExt+"Test.swift"))
	case ".c", ".h":
		// foo.c -> foo_test.c, test_foo.c
		paths = append(paths, filepath.Join(dir, nameNoExt+"_test"+ext))
		paths = append(paths, filepath.Join(dir, "test_"+base))
	case ".cpp", ".cc", ".cxx", ".hpp", ".hxx":
		// foo.cpp -> foo_test.cpp, test_foo.cpp
		paths = append(paths, filepath.Join(dir, nameNoExt+"_test"+ext))
		paths = append(paths, filepath.Join(dir, "test_"+base))
	case ".m", ".mm":
		// Foo.m -> FooTests.m, FooTest.m
		paths = append(paths, filepath.Join(dir, nameNoExt+"Tests"+ext))
		paths = append(paths, filepath.Join(dir, nameNoExt+"Test"+ext))
	case ".sh", ".bash":
		// deploy.sh -> test_deploy.sh, deploy_test.sh
		paths = append(paths, filepath.Join(dir, "test_"+base))
		paths = append(paths, filepath.Join(dir, nameNoExt+"_test"+ext))
	case ".dart":
		// foo.dart -> foo_test.dart
		paths = append(paths, filepath.Join(dir, nameNoExt+"_test.dart"))
		paths = append(paths, filepath.Join(dir, "test", nameNoExt+"_test.dart"))
	case ".scala":
		// UserService.scala -> UserServiceSpec.scala, UserServiceTest.scala
		paths = append(paths, filepath.Join(dir, nameNoExt+"Spec.scala"))
		paths = append(paths, filepath.Join(dir, nameNoExt+"Test.scala"))
		paths = append(paths, filepath.Join(dir, nameNoExt+"Suite.scala"))
	case ".r":
		// analysis.r -> test_analysis.r, test-analysis.r
		paths = append(paths, filepath.Join(dir, "test_"+base))
		paths = append(paths, filepath.Join(dir, "test-"+base))
	case ".ex", ".exs":
		// foo.ex -> foo_test.exs
		paths = append(paths, filepath.Join(dir, nameNoExt+"_test.exs"))
		paths = append(paths, filepath.Join(dir, "test", nameNoExt+"_test.exs"))
	case ".lua":
		// foo.lua -> test_foo.lua, foo_test.lua, foo_spec.lua
		paths = append(paths, filepath.Join(dir, "test_"+base))
		paths = append(paths, filepath.Join(dir, nameNoExt+"_test.lua"))
		paths = append(paths, filepath.Join(dir, nameNoExt+"_spec.lua"))
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
	case ".kt", ".kts":
		if strings.HasSuffix(nameNoExt, "Test") {
			srcName := strings.TrimSuffix(nameNoExt, "Test") + ext
			return filepath.ToSlash(filepath.Join(dir, srcName))
		}
		if strings.HasSuffix(nameNoExt, "Tests") {
			srcName := strings.TrimSuffix(nameNoExt, "Tests") + ext
			return filepath.ToSlash(filepath.Join(dir, srcName))
		}
	case ".cs":
		if strings.HasSuffix(nameNoExt, "Tests") {
			srcName := strings.TrimSuffix(nameNoExt, "Tests") + ext
			if filepath.Base(dir) == "Tests" {
				return filepath.ToSlash(filepath.Join(filepath.Dir(dir), srcName))
			}
			return filepath.ToSlash(filepath.Join(dir, srcName))
		}
		if strings.HasSuffix(nameNoExt, "Test") {
			srcName := strings.TrimSuffix(nameNoExt, "Test") + ext
			return filepath.ToSlash(filepath.Join(dir, srcName))
		}
	case ".php":
		if strings.HasSuffix(nameNoExt, "Test") {
			srcName := strings.TrimSuffix(nameNoExt, "Test") + ext
			if filepath.Base(dir) == "tests" {
				return filepath.ToSlash(filepath.Join(filepath.Dir(dir), srcName))
			}
			return filepath.ToSlash(filepath.Join(dir, srcName))
		}
	case ".swift":
		if strings.HasSuffix(nameNoExt, "Tests") {
			srcName := strings.TrimSuffix(nameNoExt, "Tests") + ext
			return filepath.ToSlash(filepath.Join(dir, srcName))
		}
		if strings.HasSuffix(nameNoExt, "Test") {
			srcName := strings.TrimSuffix(nameNoExt, "Test") + ext
			return filepath.ToSlash(filepath.Join(dir, srcName))
		}
	case ".c", ".h", ".cpp", ".cc", ".cxx", ".hpp", ".hxx":
		if strings.HasSuffix(nameNoExt, "_test") {
			srcName := strings.TrimSuffix(nameNoExt, "_test") + ext
			return filepath.ToSlash(filepath.Join(dir, srcName))
		}
		if strings.HasPrefix(nameNoExt, "test_") {
			srcName := strings.TrimPrefix(nameNoExt, "test_") + ext
			return filepath.ToSlash(filepath.Join(dir, srcName))
		}
	case ".m", ".mm":
		if strings.HasSuffix(nameNoExt, "Tests") {
			srcName := strings.TrimSuffix(nameNoExt, "Tests") + ext
			return filepath.ToSlash(filepath.Join(dir, srcName))
		}
		if strings.HasSuffix(nameNoExt, "Test") {
			srcName := strings.TrimSuffix(nameNoExt, "Test") + ext
			return filepath.ToSlash(filepath.Join(dir, srcName))
		}
	case ".sh", ".bash":
		if strings.HasPrefix(nameNoExt, "test_") {
			srcName := strings.TrimPrefix(nameNoExt, "test_") + ext
			return filepath.ToSlash(filepath.Join(dir, srcName))
		}
		if strings.HasSuffix(nameNoExt, "_test") {
			srcName := strings.TrimSuffix(nameNoExt, "_test") + ext
			return filepath.ToSlash(filepath.Join(dir, srcName))
		}
	case ".dart":
		if strings.HasSuffix(nameNoExt, "_test") {
			srcName := strings.TrimSuffix(nameNoExt, "_test") + ext
			return filepath.ToSlash(filepath.Join(dir, srcName))
		}
	case ".scala":
		if strings.HasSuffix(nameNoExt, "Spec") {
			srcName := strings.TrimSuffix(nameNoExt, "Spec") + ext
			return filepath.ToSlash(filepath.Join(dir, srcName))
		}
		if strings.HasSuffix(nameNoExt, "Test") {
			srcName := strings.TrimSuffix(nameNoExt, "Test") + ext
			return filepath.ToSlash(filepath.Join(dir, srcName))
		}
		if strings.HasSuffix(nameNoExt, "Suite") {
			srcName := strings.TrimSuffix(nameNoExt, "Suite") + ext
			return filepath.ToSlash(filepath.Join(dir, srcName))
		}
	case ".r":
		if strings.HasPrefix(nameNoExt, "test_") {
			srcName := strings.TrimPrefix(nameNoExt, "test_") + ext
			return filepath.ToSlash(filepath.Join(dir, srcName))
		}
		if strings.HasPrefix(nameNoExt, "test-") {
			srcName := strings.TrimPrefix(nameNoExt, "test-") + ext
			return filepath.ToSlash(filepath.Join(dir, srcName))
		}
	case ".ex", ".exs":
		if strings.HasSuffix(nameNoExt, "_test") {
			srcName := strings.TrimSuffix(nameNoExt, "_test") + ext
			if filepath.Base(dir) == "test" {
				return filepath.ToSlash(filepath.Join(filepath.Dir(dir), srcName))
			}
			return filepath.ToSlash(filepath.Join(dir, srcName))
		}
	case ".lua":
		if strings.HasPrefix(nameNoExt, "test_") {
			srcName := strings.TrimPrefix(nameNoExt, "test_") + ext
			return filepath.ToSlash(filepath.Join(dir, srcName))
		}
		if strings.HasSuffix(nameNoExt, "_test") {
			srcName := strings.TrimSuffix(nameNoExt, "_test") + ext
			return filepath.ToSlash(filepath.Join(dir, srcName))
		}
		if strings.HasSuffix(nameNoExt, "_spec") {
			srcName := strings.TrimSuffix(nameNoExt, "_spec") + ext
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
