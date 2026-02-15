package analyzer

import (
	"fmt"
	"strings"

	"github.com/joeabbey/fault/pkg/git"
	"github.com/joeabbey/fault/pkg/parser"
)

// ReferenceAnalyzer checks for broken references caused by deleted or renamed
// files and removed symbols.
type ReferenceAnalyzer struct{}

// NewReferenceAnalyzer creates a new broken reference analyzer.
func NewReferenceAnalyzer() *ReferenceAnalyzer {
	return &ReferenceAnalyzer{}
}

// Name returns the analyzer name.
func (a *ReferenceAnalyzer) Name() string {
	return "references"
}

// Analyze runs broken reference checks on the analysis context.
func (a *ReferenceAnalyzer) Analyze(ctx *AnalysisContext) ([]Issue, error) {
	issues := make([]Issue, 0)

	if ctx.Diff == nil || len(ctx.ParsedFiles) == 0 {
		return issues, nil
	}

	// 1. Check for deleted files still referenced
	deletedIssues := a.checkDeletedFileReferences(ctx)
	issues = append(issues, deletedIssues...)

	// 2. Check for renamed files with stale references
	renamedIssues := a.checkRenamedFileReferences(ctx)
	issues = append(issues, renamedIssues...)

	// 3. Check for deleted symbols still referenced
	symbolIssues := a.checkDeletedSymbolReferences(ctx)
	issues = append(issues, symbolIssues...)

	return issues, nil
}

// checkDeletedFileReferences finds files that were deleted in the diff but are
// still imported by other parsed files.
func (a *ReferenceAnalyzer) checkDeletedFileReferences(ctx *AnalysisContext) []Issue {
	issues := make([]Issue, 0)

	// Collect deleted file paths
	deletedPaths := make([]string, 0)
	for _, fd := range ctx.Diff.Files {
		if fd.Status == "deleted" {
			deletedPaths = append(deletedPaths, fd.Path)
		}
	}

	if len(deletedPaths) == 0 {
		return issues
	}

	// For each parsed file, check if it imports a deleted file
	for filePath, pf := range ctx.ParsedFiles {
		if ctx.Config != nil && ctx.Config.IsIgnored(filePath) {
			continue
		}

		for _, imp := range pf.Imports {
			resolvedTarget := resolveImportTarget(pf, imp, ctx.ParsedFiles)

			for _, deletedPath := range deletedPaths {
				if matchesDeletedFile(resolvedTarget, deletedPath, pf.Language) {
					issues = append(issues, Issue{
						ID:       fmt.Sprintf("ref-deleted-%s-%d-%s", filePath, imp.Line, deletedPath),
						Severity: SeverityError,
						Category: "reference",
						File:     filePath,
						Line:     imp.Line,
						Message: fmt.Sprintf(
							"Import %q references deleted file %q",
							imp.Path, deletedPath,
						),
						Suggestion:   "Remove this import or update it to point to a replacement file",
						RelatedFiles: []string{deletedPath},
					})
				}
			}
		}
	}

	// Use the index to find non-changed files that also import deleted files
	if ctx.Index != nil {
		issues = append(issues, a.checkDeletedFileReferencesViaIndex(ctx, deletedPaths)...)
	}

	return issues
}

// checkDeletedFileReferencesViaIndex uses the repo index to find files outside
// the changeset that import deleted files.
func (a *ReferenceAnalyzer) checkDeletedFileReferencesViaIndex(ctx *AnalysisContext, deletedPaths []string) []Issue {
	issues := make([]Issue, 0)

	for _, entry := range ctx.Index.AllFiles() {
		// Skip files already in ParsedFiles (checked above)
		if _, inParsed := ctx.ParsedFiles[entry.Path]; inParsed {
			continue
		}

		for _, imp := range entry.Imports {
			var resolvedTarget string
			switch entry.Language {
			case "typescript":
				if isRelativeImport(imp.Path) {
					resolvedTarget = resolveRelativeImport(entry.Path, imp.Path)
				}
			case "python":
				if strings.HasPrefix(imp.Path, ".") {
					resolvedTarget = resolvePythonImport(entry.Path, imp.Path)
				}
			}
			if resolvedTarget == "" {
				continue
			}

			for _, deletedPath := range deletedPaths {
				if matchesDeletedFile(resolvedTarget, deletedPath, entry.Language) {
					issues = append(issues, Issue{
						ID:       fmt.Sprintf("ref-deleted-%s-index-%s", entry.Path, deletedPath),
						Severity: SeverityError,
						Category: "reference",
						File:     entry.Path,
						Message: fmt.Sprintf(
							"Import references deleted file %q (found via repo index)",
							deletedPath,
						),
						Suggestion:   "Remove this import or update it to point to a replacement file",
						RelatedFiles: []string{deletedPath},
					})
				}
			}
		}
	}

	return issues
}

// checkRenamedFileReferences finds files that were renamed in the diff but
// imports still point to the old path.
func (a *ReferenceAnalyzer) checkRenamedFileReferences(ctx *AnalysisContext) []Issue {
	issues := make([]Issue, 0)

	// Collect renames: old path -> new path
	renames := make(map[string]string)
	for _, fd := range ctx.Diff.Files {
		if fd.Status == "renamed" && fd.OldPath != "" {
			renames[fd.OldPath] = fd.Path
		}
	}

	if len(renames) == 0 {
		return issues
	}

	// For each parsed file, check if it imports from an old (renamed) path
	for filePath, pf := range ctx.ParsedFiles {
		if ctx.Config != nil && ctx.Config.IsIgnored(filePath) {
			continue
		}

		for _, imp := range pf.Imports {
			resolvedTarget := resolveImportTarget(pf, imp, ctx.ParsedFiles)

			for oldPath, newPath := range renames {
				if matchesRenamedFile(resolvedTarget, oldPath, pf.Language) {
					issues = append(issues, Issue{
						ID:       fmt.Sprintf("ref-renamed-%s-%d-%s", filePath, imp.Line, oldPath),
						Severity: SeverityWarning,
						Category: "reference",
						File:     filePath,
						Line:     imp.Line,
						Message: fmt.Sprintf(
							"Import %q references old path %q which was renamed to %q",
							imp.Path, oldPath, newPath,
						),
						Suggestion: fmt.Sprintf(
							"Update the import to reference %q",
							suggestNewImportPath(pf, imp.Path, oldPath, newPath),
						),
						RelatedFiles: []string{oldPath, newPath},
					})
				}
			}
		}
	}

	return issues
}

// checkDeletedSymbolReferences finds symbols that were removed from changed files
// but are still referenced by other changed files.
func (a *ReferenceAnalyzer) checkDeletedSymbolReferences(ctx *AnalysisContext) []Issue {
	issues := make([]Issue, 0)

	// For each changed file, find symbols that were removed (in diff, removed lines only)
	for _, fd := range ctx.Diff.Files {
		if fd.Status == "deleted" || fd.Status == "added" {
			continue
		}

		pf, ok := ctx.ParsedFiles[fd.Path]
		if !ok {
			continue
		}

		removedSymbols := extractRemovedSymbols(fd, pf.Language)
		if len(removedSymbols) == 0 {
			continue
		}

		// Filter out symbols that were also re-added (renamed/refactored)
		currentExports := make(map[string]bool)
		if currentPf, ok := ctx.ParsedFiles[fd.Path]; ok {
			for _, exp := range currentPf.Exports {
				currentExports[exp.Name] = true
			}
			for _, sym := range currentPf.Symbols {
				if sym.Exported {
					currentExports[sym.Name] = true
				}
			}
		}

		trulyRemoved := make([]string, 0)
		for _, name := range removedSymbols {
			if !currentExports[name] {
				trulyRemoved = append(trulyRemoved, name)
			}
		}

		if len(trulyRemoved) == 0 {
			continue
		}

		// Check all other parsed files for references to the removed symbols
		for otherPath, otherPf := range ctx.ParsedFiles {
			if otherPath == fd.Path {
				continue
			}

			// Check if the other file imports from this file
			importsFromFile := false
			importLine := 0
			for _, imp := range otherPf.Imports {
				target := resolveImportTarget(otherPf, imp, ctx.ParsedFiles)
				if target == fd.Path {
					importsFromFile = true
					importLine = imp.Line
					break
				}
			}

			// For Go: same-package files
			if !importsFromFile && pf.Language == "go" && sameDirectory(otherPath, fd.Path) {
				importsFromFile = true
			}

			if !importsFromFile {
				continue
			}

			// Check named imports
			for _, imp := range otherPf.Imports {
				for _, name := range imp.Names {
					for _, removed := range trulyRemoved {
						if name == removed {
							issues = append(issues, Issue{
								ID: fmt.Sprintf(
									"ref-symbol-deleted-%s-%s-%s",
									fd.Path, otherPath, removed,
								),
								Severity: SeverityWarning,
								Category: "reference",
								File:     otherPath,
								Line:     imp.Line,
								Message: fmt.Sprintf(
									"Symbol %q was removed from %q but is still imported here",
									removed, fd.Path,
								),
								Suggestion: fmt.Sprintf(
									"Remove or replace the reference to %q", removed,
								),
								RelatedFiles: []string{fd.Path},
							})
						}
					}
				}
			}

			// Check for usage in symbols (signature references)
			for _, removed := range trulyRemoved {
				if fileReferencesSymbol(otherPf, removed) {
					// Avoid duplicate if already flagged via named import
					alreadyFlagged := false
					for _, imp := range otherPf.Imports {
						for _, name := range imp.Names {
							if name == removed {
								alreadyFlagged = true
								break
							}
						}
						if alreadyFlagged {
							break
						}
					}

					if !alreadyFlagged {
						line := importLine
						if line == 0 {
							line = findSymbolUsageLine(otherPf, removed)
						}
						issues = append(issues, Issue{
							ID: fmt.Sprintf(
								"ref-symbol-usage-%s-%s-%s",
								fd.Path, otherPath, removed,
							),
							Severity: SeverityWarning,
							Category: "reference",
							File:     otherPath,
							Line:     line,
							Message: fmt.Sprintf(
								"Symbol %q was removed from %q but may still be used in this file",
								removed, fd.Path,
							),
							Suggestion: fmt.Sprintf(
								"Check for usages of %q and update or remove them", removed,
							),
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

// resolveImportTarget resolves an import to a file path string
// (not necessarily a key in parsedFiles).
func resolveImportTarget(pf *parser.ParsedFile, imp parser.Import, parsedFiles map[string]*parser.ParsedFile) string {
	switch pf.Language {
	case "typescript":
		if !isRelativeImport(imp.Path) {
			return ""
		}
		resolved := resolveRelativeImport(pf.Path, imp.Path)
		// Try to find the actual file
		found := findTSFile(resolved, parsedFiles)
		if found != "" {
			return found
		}
		// Return the resolved base path even if not found
		return resolved
	case "python":
		if !strings.HasPrefix(imp.Path, ".") {
			return ""
		}
		resolved := resolvePythonImport(pf.Path, imp.Path)
		found := findPythonFile(resolved, parsedFiles)
		if found != "" {
			return found
		}
		return resolved
	case "go":
		files := findGoPackageFiles(imp.Path, parsedFiles)
		if len(files) > 0 {
			return files[0]
		}
		return imp.Path
	}
	return ""
}

// matchesDeletedFile checks if a resolved import target matches a deleted file path.
func matchesDeletedFile(resolvedTarget, deletedPath, language string) bool {
	if resolvedTarget == "" {
		return false
	}

	if resolvedTarget == deletedPath {
		return true
	}

	// For TS/JS: the import might not include the extension
	if language == "typescript" {
		extensions := []string{".ts", ".tsx", ".js", ".jsx", ".mjs", ".cjs"}
		for _, ext := range extensions {
			if resolvedTarget+ext == deletedPath {
				return true
			}
		}
		// Check index files
		for _, ext := range extensions {
			if resolvedTarget+"/index"+ext == deletedPath {
				return true
			}
		}
	}

	// For Python
	if language == "python" {
		if resolvedTarget+".py" == deletedPath {
			return true
		}
		if resolvedTarget+"/__init__.py" == deletedPath {
			return true
		}
	}

	return false
}

// matchesRenamedFile checks if a resolved import target matches a renamed file's old path.
func matchesRenamedFile(resolvedTarget, oldPath, language string) bool {
	return matchesDeletedFile(resolvedTarget, oldPath, language)
}

// suggestNewImportPath generates a suggested new import path after a rename.
func suggestNewImportPath(pf *parser.ParsedFile, currentImport, oldPath, newPath string) string {
	// Simple approach: replace the old filename with the new one in the import
	if strings.Contains(currentImport, oldBaseName(oldPath)) {
		return strings.Replace(currentImport, oldBaseName(oldPath), oldBaseName(newPath), 1)
	}
	return newPath
}

// oldBaseName extracts the base name without extension from a path.
func oldBaseName(path string) string {
	// Get just the filename
	idx := strings.LastIndexByte(path, '/')
	name := path
	if idx >= 0 {
		name = path[idx+1:]
	}

	// Remove extension
	if dotIdx := strings.LastIndexByte(name, '.'); dotIdx > 0 {
		name = name[:dotIdx]
	}

	return name
}

// extractRemovedSymbols extracts symbol names that were removed (only in removed lines)
// from a file diff.
func extractRemovedSymbols(fd git.FileDiff, language string) []string {
	removed := make([]string, 0)
	seen := make(map[string]bool)

	for _, hunk := range fd.Hunks {
		for _, line := range hunk.Lines {
			if line.Type != "removed" {
				continue
			}
			content := strings.TrimSpace(line.Content)

			var name string
			switch language {
			case "go":
				name = extractGoExportedName(content)
			case "typescript":
				names := extractExportNames(content)
				if len(names) > 0 {
					name = names[0]
				}
			case "python":
				name = extractPythonDefName(content)
				if name != "" && strings.HasPrefix(name, "_") {
					name = "" // Not exported
				}
			}

			if name != "" && !seen[name] {
				seen[name] = true
				removed = append(removed, name)
			}
		}
	}

	return removed
}

// findSymbolUsageLine attempts to find the line number where a symbol is used in a parsed file.
func findSymbolUsageLine(pf *parser.ParsedFile, symbolName string) int {
	// Check import lines first (most direct reference)
	for _, imp := range pf.Imports {
		for _, name := range imp.Names {
			if name == symbolName {
				return imp.Line
			}
		}
	}

	// Check symbol signatures
	for _, sym := range pf.Symbols {
		if strings.Contains(sym.Signature, symbolName) {
			return sym.Line
		}
	}

	return 0
}
