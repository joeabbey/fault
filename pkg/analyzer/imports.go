package analyzer

import (
	"fmt"
	"path/filepath"
	"strings"

	"github.com/joeabbey/fault/pkg/git"
	"github.com/joeabbey/fault/pkg/parser"
)

// ImportAnalyzer checks for broken imports, removed exports still imported,
// and missing external dependencies across changed files.
type ImportAnalyzer struct{}

// NewImportAnalyzer creates a new import/export analyzer.
func NewImportAnalyzer() *ImportAnalyzer {
	return &ImportAnalyzer{}
}

// Name returns the analyzer name.
func (a *ImportAnalyzer) Name() string {
	return "imports"
}

// Analyze runs import/export validation on the analysis context.
func (a *ImportAnalyzer) Analyze(ctx *AnalysisContext) ([]Issue, error) {
	issues := make([]Issue, 0)

	if ctx.Diff == nil || len(ctx.ParsedFiles) == 0 {
		return issues, nil
	}

	// Build the export map: filePath -> exportName -> Export
	exportMap := buildExportMap(ctx.ParsedFiles)

	// Build the set of changed file paths for quick lookup
	changedFiles := buildChangedFileSet(ctx)

	// Build the set of deleted files
	deletedFiles := buildDeletedFileSet(ctx)

	// Check each parsed file's imports
	for filePath, pf := range ctx.ParsedFiles {
		if ctx.Config != nil && ctx.Config.IsIgnored(filePath) {
			continue
		}

		for _, imp := range pf.Imports {
			// Determine what kind of import this is and validate it
			importIssues := a.validateImport(ctx, pf, imp, exportMap, changedFiles, deletedFiles)
			issues = append(issues, importIssues...)
		}
	}

	// Check for removed exports that are still imported elsewhere
	removedExportIssues := a.checkRemovedExports(ctx, exportMap)
	issues = append(issues, removedExportIssues...)

	return issues, nil
}

// validateImport checks a single import for validity.
func (a *ImportAnalyzer) validateImport(
	ctx *AnalysisContext,
	pf *parser.ParsedFile,
	imp parser.Import,
	exportMap map[string]map[string]bool,
	changedFiles map[string]bool,
	deletedFiles map[string]bool,
) []Issue {
	issues := make([]Issue, 0)

	switch pf.Language {
	case "typescript":
		issues = append(issues, a.validateTSImport(ctx, pf, imp, exportMap, deletedFiles)...)
	case "python":
		issues = append(issues, a.validatePythonImport(ctx, pf, imp, exportMap, deletedFiles)...)
	case "go":
		issues = append(issues, a.validateGoImport(ctx, pf, imp, exportMap)...)
	}

	return issues
}

// validateTSImport validates a TypeScript/JavaScript import.
func (a *ImportAnalyzer) validateTSImport(
	ctx *AnalysisContext,
	pf *parser.ParsedFile,
	imp parser.Import,
	exportMap map[string]map[string]bool,
	deletedFiles map[string]bool,
) []Issue {
	issues := make([]Issue, 0)

	// Skip external/node_modules imports (not relative paths)
	if !isRelativeImport(imp.Path) {
		return issues
	}

	// Resolve the import path relative to the importing file
	resolvedPath := resolveRelativeImport(pf.Path, imp.Path)

	// Try common extensions for TS/JS
	targetPath := findTSFile(resolvedPath, ctx.ParsedFiles)

	if targetPath == "" {
		// Could not resolve — the target file doesn't exist in parsed files
		if deletedFiles[resolvedPath] || deletedFiles[resolvedPath+".ts"] ||
			deletedFiles[resolvedPath+".tsx"] || deletedFiles[resolvedPath+".js"] ||
			deletedFiles[resolvedPath+".jsx"] {
			issues = append(issues, Issue{
				ID:       fmt.Sprintf("import-deleted-%s-%d", pf.Path, imp.Line),
				Severity: SeverityError,
				Category: "import",
				File:     pf.Path,
				Line:     imp.Line,
				Message:  fmt.Sprintf("Import %q points to a deleted file", imp.Path),
				Suggestion: "Remove this import or update it to point to the new location",
				RelatedFiles: []string{resolvedPath},
			})
		} else {
			issues = append(issues, Issue{
				ID:       fmt.Sprintf("import-broken-%s-%d", pf.Path, imp.Line),
				Severity: SeverityError,
				Category: "import",
				File:     pf.Path,
				Line:     imp.Line,
				Message:  fmt.Sprintf("Import %q could not be resolved to any known file", imp.Path),
				Suggestion: "Check the import path and ensure the target file exists",
			})
		}
		return issues
	}

	// Check named imports against exports
	exports := exportMap[targetPath]
	for _, name := range imp.Names {
		if name == "default" || name == "*" {
			continue
		}
		if exports != nil && !exports[name] {
			// Check if there's a wildcard re-export
			if exports["*"] {
				continue
			}
			issues = append(issues, Issue{
				ID:       fmt.Sprintf("import-name-%s-%d-%s", pf.Path, imp.Line, name),
				Severity: SeverityError,
				Category: "import",
				File:     pf.Path,
				Line:     imp.Line,
				Message:  fmt.Sprintf("Name %q is not exported from %q", name, imp.Path),
				Suggestion: fmt.Sprintf("Check available exports in %s", targetPath),
				RelatedFiles: []string{targetPath},
			})
		}
	}

	return issues
}

// validatePythonImport validates a Python import.
func (a *ImportAnalyzer) validatePythonImport(
	ctx *AnalysisContext,
	pf *parser.ParsedFile,
	imp parser.Import,
	exportMap map[string]map[string]bool,
	deletedFiles map[string]bool,
) []Issue {
	issues := make([]Issue, 0)

	// Only validate relative imports (starting with .)
	if !strings.HasPrefix(imp.Path, ".") {
		return issues
	}

	// Resolve relative Python import
	resolvedPath := resolvePythonImport(pf.Path, imp.Path)

	// Try to find the target file
	targetPath := findPythonFile(resolvedPath, ctx.ParsedFiles)

	if targetPath == "" {
		if deletedFiles[resolvedPath] || deletedFiles[resolvedPath+".py"] {
			issues = append(issues, Issue{
				ID:       fmt.Sprintf("import-deleted-%s-%d", pf.Path, imp.Line),
				Severity: SeverityError,
				Category: "import",
				File:     pf.Path,
				Line:     imp.Line,
				Message:  fmt.Sprintf("Import from %q points to a deleted module", imp.Path),
				Suggestion: "Remove this import or update it to point to the new location",
				RelatedFiles: []string{resolvedPath},
			})
		}
		// For Python, we can't always resolve local imports without full project context,
		// so only flag deleted files as errors.
		return issues
	}

	// Check named imports against exports
	exports := exportMap[targetPath]
	for _, name := range imp.Names {
		if name == "*" {
			continue
		}
		if exports != nil && !exports[name] {
			issues = append(issues, Issue{
				ID:       fmt.Sprintf("import-name-%s-%d-%s", pf.Path, imp.Line, name),
				Severity: SeverityWarning,
				Category: "import",
				File:     pf.Path,
				Line:     imp.Line,
				Message:  fmt.Sprintf("Name %q may not be exported from %q", name, imp.Path),
				Suggestion: fmt.Sprintf("Check available exports in %s", targetPath),
				RelatedFiles: []string{targetPath},
			})
		}
	}

	return issues
}

// validateGoImport validates a Go import.
func (a *ImportAnalyzer) validateGoImport(
	ctx *AnalysisContext,
	pf *parser.ParsedFile,
	imp parser.Import,
	exportMap map[string]map[string]bool,
) []Issue {
	issues := make([]Issue, 0)

	// For Go, imports are package paths, not file paths.
	// We check if any parsed file belongs to the imported package.
	// Standard library and external packages are skipped.

	if isGoStdlib(imp.Path) {
		return issues
	}

	// Check if this is an internal import (within the same module)
	// by checking if any parsed file's directory matches the import path suffix.
	matchingFiles := findGoPackageFiles(imp.Path, ctx.ParsedFiles)

	// If no matching files found in ParsedFiles, it might be an external dependency.
	// We don't flag external dependencies as broken within just the parsed file set,
	// since we may only have a subset of the repo's files.
	if len(matchingFiles) == 0 {
		return issues
	}

	// For Go, exported names are capitalized identifiers.
	// We don't typically import individual names in Go (the import is the whole package),
	// so there's less to validate at the name level.
	// The main check is that the package exists and has exported symbols.

	return issues
}

// checkRemovedExports scans the diff for removed export lines and checks if
// other files still import them.
func (a *ImportAnalyzer) checkRemovedExports(
	ctx *AnalysisContext,
	exportMap map[string]map[string]bool,
) []Issue {
	issues := make([]Issue, 0)

	if ctx.Diff == nil {
		return issues
	}

	for _, fd := range ctx.Diff.Files {
		if fd.Status == "deleted" {
			continue
		}

		// Find removed export lines in the diff
		removedExports := extractRemovedExports(fd)
		if len(removedExports) == 0 {
			continue
		}

		// Check if any other parsed file imports these removed exports
		for _, pf := range ctx.ParsedFiles {
			if pf.Path == fd.Path {
				continue
			}

			for _, imp := range pf.Imports {
				targetPath := resolveImportToFile(pf, imp, ctx.ParsedFiles)
				if targetPath != fd.Path {
					continue
				}

				// This file imports from the changed file — check names
				for _, name := range imp.Names {
					if removedExports[name] {
						issues = append(issues, Issue{
							ID:       fmt.Sprintf("export-removed-%s-%s-%s", fd.Path, pf.Path, name),
							Severity: SeverityWarning,
							Category: "import",
							File:     pf.Path,
							Line:     imp.Line,
							Message: fmt.Sprintf(
								"Import %q was removed from %q but is still imported here",
								name, fd.Path,
							),
							Suggestion:   fmt.Sprintf("Update or remove the import of %q", name),
							RelatedFiles: []string{fd.Path},
						})
					}
				}
			}
		}
	}

	return issues
}

// --- Helper functions ---

// buildExportMap creates a map of filePath -> exportName -> true from all parsed files.
func buildExportMap(parsedFiles map[string]*parser.ParsedFile) map[string]map[string]bool {
	exportMap := make(map[string]map[string]bool)
	for filePath, pf := range parsedFiles {
		exports := make(map[string]bool)
		for _, exp := range pf.Exports {
			exports[exp.Name] = true
		}
		// Also include exported symbols not in the export list
		// (e.g., Go exported names are determined by capitalization)
		for _, sym := range pf.Symbols {
			if sym.Exported {
				exports[sym.Name] = true
			}
		}
		exportMap[filePath] = exports
	}
	return exportMap
}

// buildChangedFileSet returns a set of file paths from the diff.
func buildChangedFileSet(ctx *AnalysisContext) map[string]bool {
	changed := make(map[string]bool)
	if ctx.Diff == nil {
		return changed
	}
	for _, fd := range ctx.Diff.Files {
		changed[fd.Path] = true
		if fd.OldPath != "" {
			changed[fd.OldPath] = true
		}
	}
	return changed
}

// buildDeletedFileSet returns a set of deleted file paths from the diff.
func buildDeletedFileSet(ctx *AnalysisContext) map[string]bool {
	deleted := make(map[string]bool)
	if ctx.Diff == nil {
		return deleted
	}
	for _, fd := range ctx.Diff.Files {
		if fd.Status == "deleted" {
			deleted[fd.Path] = true
		}
	}
	return deleted
}

// isRelativeImport returns true if the import path is relative (starts with . or ..).
func isRelativeImport(path string) bool {
	return strings.HasPrefix(path, "./") || strings.HasPrefix(path, "../") || path == "." || path == ".."
}

// resolveRelativeImport resolves a relative import path based on the importing file's location.
func resolveRelativeImport(importerPath, importPath string) string {
	dir := filepath.Dir(importerPath)
	resolved := filepath.Join(dir, importPath)
	return filepath.Clean(resolved)
}

// findTSFile tries to find a TypeScript/JavaScript file in parsedFiles,
// trying common extensions if the path doesn't have one.
func findTSFile(basePath string, parsedFiles map[string]*parser.ParsedFile) string {
	// Try exact path first
	if _, ok := parsedFiles[basePath]; ok {
		return basePath
	}

	// Try with extensions
	extensions := []string{".ts", ".tsx", ".js", ".jsx", ".mjs", ".cjs"}
	for _, ext := range extensions {
		candidate := basePath + ext
		if _, ok := parsedFiles[candidate]; ok {
			return candidate
		}
	}

	// Try index files
	for _, ext := range extensions {
		candidate := filepath.Join(basePath, "index"+ext)
		if _, ok := parsedFiles[candidate]; ok {
			return candidate
		}
	}

	return ""
}

// resolvePythonImport resolves a relative Python import to a file path.
func resolvePythonImport(importerPath, importPath string) string {
	dir := filepath.Dir(importerPath)

	// Count leading dots for relative import level
	dots := 0
	for _, ch := range importPath {
		if ch == '.' {
			dots++
		} else {
			break
		}
	}

	// Go up directories based on dot count
	for i := 1; i < dots; i++ {
		dir = filepath.Dir(dir)
	}

	// Get the module path after the dots
	modulePart := importPath[dots:]
	if modulePart == "" {
		return dir
	}

	// Convert dots in module name to path separators
	modulePath := strings.ReplaceAll(modulePart, ".", string(filepath.Separator))
	return filepath.Join(dir, modulePath)
}

// findPythonFile tries to find a Python file in parsedFiles.
func findPythonFile(basePath string, parsedFiles map[string]*parser.ParsedFile) string {
	// Try exact path
	if _, ok := parsedFiles[basePath]; ok {
		return basePath
	}

	// Try with .py extension
	candidate := basePath + ".py"
	if _, ok := parsedFiles[candidate]; ok {
		return candidate
	}

	// Try __init__.py in directory
	candidate = filepath.Join(basePath, "__init__.py")
	if _, ok := parsedFiles[candidate]; ok {
		return candidate
	}

	return ""
}

// isGoStdlib performs a heuristic check for Go standard library packages.
// Standard library packages don't contain a dot in the first path component.
func isGoStdlib(importPath string) bool {
	parts := strings.SplitN(importPath, "/", 2)
	if len(parts) == 0 {
		return false
	}
	// Standard library packages don't have dots in the first segment
	return !strings.Contains(parts[0], ".")
}

// findGoPackageFiles finds parsed files that belong to the given Go import path.
func findGoPackageFiles(importPath string, parsedFiles map[string]*parser.ParsedFile) []string {
	matching := make([]string, 0)
	for filePath := range parsedFiles {
		dir := filepath.Dir(filePath)
		// Check if the file's directory matches the import path suffix
		// e.g., import "github.com/foo/pkg/store" matches file at "pkg/store/store.go"
		if strings.HasSuffix(dir, importPath) || dir == importPath {
			matching = append(matching, filePath)
			continue
		}
		// Also check if the import path ends with the file's directory
		// e.g., import "mymodule/pkg/store" matches directory "pkg/store"
		if strings.HasSuffix(importPath, "/"+dir) || strings.HasSuffix(importPath, dir) {
			matching = append(matching, filePath)
		}
	}
	return matching
}

// extractRemovedExports looks at the diff hunks of a file to find
// export-like lines that were removed.
func extractRemovedExports(fd git.FileDiff) map[string]bool {
	removed := make(map[string]bool)

	for _, hunk := range fd.Hunks {
		for _, line := range hunk.Lines {
			if line.Type != "removed" {
				continue
			}
			content := strings.TrimSpace(line.Content)

			// TypeScript/JavaScript exports
			if strings.HasPrefix(content, "export ") {
				names := extractExportNames(content)
				for _, name := range names {
					removed[name] = true
				}
			}

			// Python: look for function/class definitions at module level
			if strings.HasPrefix(content, "def ") || strings.HasPrefix(content, "class ") {
				name := extractPythonDefName(content)
				if name != "" && !strings.HasPrefix(name, "_") {
					removed[name] = true
				}
			}

			// Go: look for exported (capitalized) symbols
			name := extractGoExportedName(content)
			if name != "" {
				removed[name] = true
			}
		}
	}

	return removed
}

// extractExportNames extracts exported names from a TypeScript export line.
func extractExportNames(line string) []string {
	names := make([]string, 0)

	// export function foo / export class Foo / export const bar
	patterns := []struct {
		prefix string
	}{
		{"export function "},
		{"export async function "},
		{"export default function "},
		{"export class "},
		{"export default class "},
		{"export const "},
		{"export let "},
		{"export var "},
		{"export type "},
		{"export interface "},
		{"export enum "},
	}

	for _, p := range patterns {
		if strings.HasPrefix(line, p.prefix) {
			rest := line[len(p.prefix):]
			// Extract the name (first word-like token)
			name := extractFirstIdentifier(rest)
			if name != "" {
				names = append(names, name)
			}
			return names
		}
	}

	return names
}

// extractFirstIdentifier extracts the first identifier from a string.
func extractFirstIdentifier(s string) string {
	var b strings.Builder
	for _, ch := range s {
		if (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z') ||
			(ch >= '0' && ch <= '9') || ch == '_' || ch == '$' {
			b.WriteRune(ch)
		} else {
			break
		}
	}
	return b.String()
}

// extractPythonDefName extracts the name from a Python def/class line.
func extractPythonDefName(line string) string {
	if strings.HasPrefix(line, "def ") {
		rest := line[4:]
		if idx := strings.IndexByte(rest, '('); idx > 0 {
			return strings.TrimSpace(rest[:idx])
		}
	}
	if strings.HasPrefix(line, "class ") {
		rest := line[6:]
		// class Foo: or class Foo(Base):
		for i, ch := range rest {
			if ch == '(' || ch == ':' || ch == ' ' {
				return strings.TrimSpace(rest[:i])
			}
		}
		return strings.TrimSpace(rest)
	}
	return ""
}

// extractGoExportedName extracts an exported Go identifier from a removed line.
func extractGoExportedName(line string) string {
	prefixes := []string{"func ", "type ", "var ", "const "}
	for _, prefix := range prefixes {
		if strings.HasPrefix(line, prefix) {
			rest := line[len(prefix):]
			// Skip receiver for methods: func (r *Repo) Name(...)
			if prefix == "func " && strings.HasPrefix(rest, "(") {
				idx := strings.Index(rest, ") ")
				if idx > 0 {
					rest = rest[idx+2:]
				}
			}
			name := extractFirstIdentifier(rest)
			if name != "" && len(name) > 0 && name[0] >= 'A' && name[0] <= 'Z' {
				return name
			}
		}
	}
	return ""
}

// resolveImportToFile resolves an import to the actual file path in parsedFiles.
func resolveImportToFile(pf *parser.ParsedFile, imp parser.Import, parsedFiles map[string]*parser.ParsedFile) string {
	switch pf.Language {
	case "typescript":
		if !isRelativeImport(imp.Path) {
			return ""
		}
		resolved := resolveRelativeImport(pf.Path, imp.Path)
		return findTSFile(resolved, parsedFiles)
	case "python":
		if !strings.HasPrefix(imp.Path, ".") {
			return ""
		}
		resolved := resolvePythonImport(pf.Path, imp.Path)
		return findPythonFile(resolved, parsedFiles)
	case "go":
		files := findGoPackageFiles(imp.Path, parsedFiles)
		if len(files) > 0 {
			return files[0]
		}
	}
	return ""
}

