package analyzer

import (
	"fmt"
	"path/filepath"
	"strings"

	"github.com/joeabbey/fault/pkg/index"
	"github.com/joeabbey/fault/pkg/parser"
)

// DeadCodeAnalyzer detects exported symbols that are never imported anywhere
// in the codebase, leveraging the full repo Index.
type DeadCodeAnalyzer struct{}

// NewDeadCodeAnalyzer creates a new dead code analyzer.
func NewDeadCodeAnalyzer() *DeadCodeAnalyzer {
	return &DeadCodeAnalyzer{}
}

// Name returns the analyzer name.
func (a *DeadCodeAnalyzer) Name() string {
	return "deadcode"
}

// Analyze scans exported symbols in changed files and reports any that have
// no importers anywhere in the index.
func (a *DeadCodeAnalyzer) Analyze(ctx *AnalysisContext) ([]Issue, error) {
	issues := make([]Issue, 0)

	if ctx.Index == nil || ctx.Diff == nil {
		return issues, nil
	}

	for _, fd := range ctx.Diff.Files {
		if fd.Status == "deleted" {
			continue
		}

		if ctx.Config != nil && ctx.Config.IsIgnored(fd.Path) {
			continue
		}

		if shouldSkipFile(fd.Path) {
			continue
		}

		pf, ok := ctx.ParsedFiles[fd.Path]
		if !ok {
			continue
		}

		for _, exp := range pf.Exports {
			if isSymbolUsed(fd.Path, exp, pf.Language, ctx.Index) {
				continue
			}

			issues = append(issues, Issue{
				ID:       fmt.Sprintf("deadcode-%s-%s-%d", fd.Path, exp.Name, exp.Line),
				FixID:    "deadcode-unused-export",
				Severity: SeverityWarning,
				Category: "deadcode",
				File:     fd.Path,
				Line:     exp.Line,
				Message:  fmt.Sprintf("Exported symbol '%s' appears unused in the codebase", exp.Name),
				Suggestion: fmt.Sprintf(
					"Consider removing the export or making '%s' unexported if it is only used internally",
					exp.Name,
				),
			})
		}
	}

	return issues, nil
}

// shouldSkipFile returns true for files that should be excluded from dead code analysis.
func shouldSkipFile(path string) bool {
	// Skip vendor and node_modules
	if strings.Contains(path, "vendor/") || strings.Contains(path, "node_modules/") {
		return true
	}

	base := filepath.Base(path)

	// Skip Go test files
	if strings.HasSuffix(base, "_test.go") {
		return true
	}

	// Skip Go main packages (entry points)
	dir := filepath.Dir(path)
	dirBase := filepath.Base(dir)
	if strings.HasSuffix(path, ".go") && (dirBase == "main" || dirBase == "cmd") {
		return true
	}

	// Skip TS/JS barrel re-exports (index files)
	if isBarrelFile(base) {
		return true
	}

	// Skip Python __init__.py re-exports
	if base == "__init__.py" {
		return true
	}

	// Skip Python private symbols (handled at the symbol level, but skip private modules too)
	if strings.HasSuffix(path, ".py") && strings.HasPrefix(base, "_") && base != "__init__.py" {
		return true
	}

	return false
}

// isBarrelFile returns true for index/barrel files that aggregate re-exports.
func isBarrelFile(base string) bool {
	barrelNames := []string{
		"index.ts", "index.tsx", "index.js", "index.jsx", "index.mjs", "index.cjs",
	}
	for _, name := range barrelNames {
		if base == name {
			return true
		}
	}
	return false
}

// isSymbolUsed checks whether the given export from filePath is imported by any
// other file in the index.
func isSymbolUsed(filePath string, exp parser.Export, language string, idx *index.Index) bool {
	switch language {
	case "go":
		return isGoSymbolUsed(filePath, exp, idx)
	case "typescript":
		return isTSSymbolUsed(filePath, exp, idx)
	case "python":
		return isPythonSymbolUsed(filePath, exp, idx)
	default:
		// For unknown languages, assume used to avoid false positives
		return true
	}
}

// isGoSymbolUsed checks if a Go exported symbol is used. In Go, a symbol is
// "used" if any file outside its package imports the package containing it.
// Same-package usage means the symbol is also accessible.
func isGoSymbolUsed(filePath string, exp parser.Export, idx *index.Index) bool {
	pkgDir := filepath.Dir(filePath)

	// Check if this is a main package by looking at any file in the same directory
	if isGoMainPackage(pkgDir, idx) {
		return true
	}

	for _, entry := range idx.AllFiles() {
		if entry.Path == filePath {
			continue
		}

		// Same-package files can access all exported symbols
		if entry.Language == "go" && filepath.Dir(entry.Path) == pkgDir {
			continue // same package — doesn't prove external usage
		}

		// Check if this file imports the package containing our symbol
		for _, imp := range entry.Imports {
			if goImportMatchesDir(imp.Path, pkgDir) {
				return true
			}
		}
	}

	return false
}

// isGoMainPackage checks if a directory contains a Go main package by looking
// for "main" in file paths or common entry-point directories.
func isGoMainPackage(pkgDir string, idx *index.Index) bool {
	// Check common main package directories
	base := filepath.Base(pkgDir)
	if base == "main" || base == "cmd" {
		return true
	}

	// Also check parent directory pattern like cmd/myapp/
	parts := strings.Split(pkgDir, string(filepath.Separator))
	for _, p := range parts {
		if p == "cmd" {
			return true
		}
	}

	return false
}

// goImportMatchesDir checks if a Go import path corresponds to the given
// package directory. For example, "github.com/foo/pkg/store" matches "pkg/store".
func goImportMatchesDir(importPath, pkgDir string) bool {
	if importPath == pkgDir {
		return true
	}
	// The import path typically ends with the relative directory
	if strings.HasSuffix(importPath, "/"+pkgDir) {
		return true
	}
	// Handle case where pkgDir is just "pkg" and import is "something/pkg"
	if strings.HasSuffix(importPath, pkgDir) {
		return true
	}
	return false
}

// isTSSymbolUsed checks if a TypeScript/JavaScript exported symbol is used
// by any other file in the index.
func isTSSymbolUsed(filePath string, exp parser.Export, idx *index.Index) bool {
	for _, entry := range idx.AllFiles() {
		if entry.Path == filePath {
			continue
		}

		if entry.Language != "typescript" {
			continue
		}

		for _, imp := range entry.Imports {
			if !isRelativeImport(imp.Path) {
				continue
			}

			// Resolve the import to see if it points to our file
			resolved := resolveRelativeImport(entry.Path, imp.Path)
			target := findTSFileInIndex(resolved, idx)
			if target == "" {
				// Try matching with extensions directly
				target = matchTSPath(resolved, filePath)
			}

			if target != filePath {
				continue
			}

			// The file is imported. Check if our specific symbol is referenced.
			// If the import has no named imports, it might be a wildcard or default import.
			if len(imp.Names) == 0 {
				return true
			}

			for _, name := range imp.Names {
				if name == exp.Name {
					return true
				}
			}
		}
	}

	return false
}

// matchTSPath checks if a resolved import base path matches a file path,
// accounting for TypeScript extension resolution.
func matchTSPath(resolved, filePath string) string {
	if resolved == filePath {
		return filePath
	}

	extensions := []string{".ts", ".tsx", ".js", ".jsx", ".mjs", ".cjs"}
	for _, ext := range extensions {
		if resolved+ext == filePath {
			return filePath
		}
	}
	for _, ext := range extensions {
		if filepath.Join(resolved, "index"+ext) == filePath {
			return filePath
		}
	}

	return ""
}

// isPythonSymbolUsed checks if a Python exported symbol is used by any other
// file in the index.
func isPythonSymbolUsed(filePath string, exp parser.Export, idx *index.Index) bool {
	// Skip private symbols (convention: underscore prefix)
	if strings.HasPrefix(exp.Name, "_") {
		return true // not truly exported, skip reporting
	}

	for _, entry := range idx.AllFiles() {
		if entry.Path == filePath {
			continue
		}

		if entry.Language != "python" {
			continue
		}

		for _, imp := range entry.Imports {
			// Check relative imports
			if strings.HasPrefix(imp.Path, ".") {
				resolved := resolvePythonImport(entry.Path, imp.Path)
				target := matchPythonPath(resolved, filePath)
				if target != filePath {
					continue
				}

				// Check if the specific symbol is named in the import
				if len(imp.Names) == 0 {
					return true // bare import of the module
				}
				for _, name := range imp.Names {
					if name == exp.Name {
						return true
					}
				}
			}

			// Check absolute imports (e.g., "mypackage.module")
			// Convert dotted module path to file path
			modulePath := strings.ReplaceAll(imp.Path, ".", string(filepath.Separator))
			if matchPythonPath(modulePath, filePath) == filePath {
				if len(imp.Names) == 0 {
					return true
				}
				for _, name := range imp.Names {
					if name == exp.Name {
						return true
					}
				}
			}
		}
	}

	return false
}

// matchPythonPath checks if a resolved import path matches a Python file path.
func matchPythonPath(resolved, filePath string) string {
	if resolved == filePath {
		return filePath
	}
	if resolved+".py" == filePath {
		return filePath
	}
	if filepath.Join(resolved, "__init__.py") == filePath {
		return filePath
	}
	return ""
}
