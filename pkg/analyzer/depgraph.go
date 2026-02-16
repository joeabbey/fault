package analyzer

import (
	"bufio"
	"encoding/json"
	"os"
	"path/filepath"
	"regexp"
	"strings"

	"github.com/joeabbey/fault/pkg/git"
	"github.com/joeabbey/fault/pkg/index"
)

// DepGraphAnalyzer detects circular dependencies and unused manifest dependencies.
type DepGraphAnalyzer struct{}

// NewDepGraphAnalyzer creates a new dependency graph analyzer.
func NewDepGraphAnalyzer() *DepGraphAnalyzer {
	return &DepGraphAnalyzer{}
}

// Name returns the analyzer name.
func (a *DepGraphAnalyzer) Name() string {
	return "depgraph"
}

// Analyze builds a dependency graph from the index and checks for circular
// dependencies and unused manifest dependencies.
func (a *DepGraphAnalyzer) Analyze(ctx *AnalysisContext) ([]Issue, error) {
	issues := make([]Issue, 0)

	if ctx.Index == nil || ctx.Diff == nil || len(ctx.Diff.Files) == 0 {
		return issues, nil
	}

	// Build set of files in the diff for filtering.
	diffFiles := make(map[string]bool)
	for _, f := range ctx.Diff.Files {
		diffFiles[f.Path] = true
	}

	// Check circular dependencies.
	issues = append(issues, a.checkCircularDeps(ctx.Index, diffFiles)...)

	// Check unused manifest dependencies.
	issues = append(issues, a.checkUnusedDeps(ctx)...)

	return issues, nil
}

// --- Circular Dependency Detection ---

// checkCircularDeps builds a directed graph from imports and detects cycles
// involving files present in the diff.
func (a *DepGraphAnalyzer) checkCircularDeps(idx *index.Index, diffFiles map[string]bool) []Issue {
	issues := make([]Issue, 0)

	// Build adjacency list: file -> list of files it imports.
	graph := buildImportGraph(idx)

	// Track which cycles we've already reported to avoid duplicates.
	reported := make(map[string]bool)

	// Run DFS from each diff file.
	for file := range diffFiles {
		if _, ok := idx.Files[file]; !ok {
			continue
		}
		if isDepGraphSkippedPath(file) {
			continue
		}

		cycles := detectCycles(graph, file)
		for _, cycle := range cycles {
			// Create a canonical key for the cycle to avoid duplicates.
			key := canonicalCycleKey(cycle)
			if reported[key] {
				continue
			}
			reported[key] = true

			cyclePath := strings.Join(cycle, " -> ")
			issues = append(issues, Issue{
				ID:       "depgraph/circular-dependency",
				Severity: SeverityWarning,
				Category: "depgraph",
				File:     file,
				Message:  "Circular dependency detected: " + cyclePath,
				Suggestion: "Break the cycle by extracting shared types into a separate module, " +
					"using dependency injection, or restructuring imports",
			})
		}
	}

	return issues
}

// buildImportGraph constructs a directed graph from the index.
// Each key is a file path, and the value is the list of file paths it imports.
func buildImportGraph(idx *index.Index) map[string][]string {
	graph := make(map[string][]string)

	for filePath, entry := range idx.Files {
		if isDepGraphSkippedPath(filePath) {
			continue
		}

		targets := make([]string, 0)
		for _, imp := range entry.Imports {
			// Try to resolve the import to a file in the index.
			resolved := depGraphResolveImport(idx, filePath, imp.Path, entry.Language)
			if resolved != "" {
				targets = append(targets, resolved)
			}
		}

		if len(targets) > 0 {
			graph[filePath] = targets
		}
	}

	return graph
}

// depGraphResolveImport tries to find the file in the index that corresponds
// to an import path. Returns empty string if not resolvable.
func depGraphResolveImport(idx *index.Index, fromFile, importPath, lang string) string {
	// Handle relative imports (./foo, ../bar).
	if strings.HasPrefix(importPath, ".") {
		dir := filepath.Dir(fromFile)
		target := filepath.Join(dir, importPath)
		target = filepath.Clean(target)

		// Try exact match.
		if _, ok := idx.Files[target]; ok {
			return target
		}

		// Try with common extensions based on language.
		exts := extensionsForLanguage(lang)
		for _, ext := range exts {
			candidate := target + ext
			if _, ok := idx.Files[candidate]; ok {
				return candidate
			}
		}

		// Try index files for directories.
		for _, ext := range exts {
			candidate := filepath.Join(target, "index"+ext)
			if _, ok := idx.Files[candidate]; ok {
				return candidate
			}
		}
	}

	// For Go: import paths are module-relative. Match against directory names in the index.
	if lang == "go" {
		// Go imports are packages, not files — find files in matching directories.
		// Look for files in a directory whose suffix matches the import path.
		for filePath := range idx.Files {
			dir := filepath.Dir(filePath)
			if strings.HasSuffix(filepath.ToSlash(dir), filepath.ToSlash(importPath)) {
				return filePath
			}
		}
	}

	return ""
}

// extensionsForLanguage returns file extensions to try for a given language.
func extensionsForLanguage(lang string) []string {
	switch lang {
	case "typescript":
		return []string{".ts", ".tsx", ".js", ".jsx", ".mjs"}
	case "javascript":
		return []string{".js", ".jsx", ".mjs", ".ts", ".tsx"}
	case "python":
		return []string{".py"}
	case "rust":
		return []string{".rs"}
	default:
		return nil
	}
}

// detectCycles finds all cycles that include the start node using DFS.
func detectCycles(graph map[string][]string, start string) [][]string {
	cycles := make([][]string, 0)

	// visiting tracks nodes currently in the DFS path.
	visiting := make(map[string]bool)
	// path tracks the current DFS path.
	path := make([]string, 0)

	var dfs func(node string)
	dfs = func(node string) {
		if visiting[node] {
			// Found a cycle. Extract it from the path.
			cycleStart := -1
			for i, p := range path {
				if p == node {
					cycleStart = i
					break
				}
			}
			if cycleStart >= 0 {
				cycle := make([]string, len(path)-cycleStart+1)
				copy(cycle, path[cycleStart:])
				cycle[len(cycle)-1] = node // close the cycle
				cycles = append(cycles, cycle)
			}
			return
		}

		visiting[node] = true
		path = append(path, node)

		for _, neighbor := range graph[node] {
			dfs(neighbor)
			if len(cycles) >= 5 {
				// Cap at 5 cycles per start node to avoid explosion.
				break
			}
		}

		path = path[:len(path)-1]
		visiting[node] = false
	}

	dfs(start)
	return cycles
}

// canonicalCycleKey creates a canonical string for a cycle so we can deduplicate.
// It rotates the cycle to start from the lexicographically smallest element.
func canonicalCycleKey(cycle []string) string {
	if len(cycle) <= 1 {
		return strings.Join(cycle, "->")
	}

	// Remove the closing element (which duplicates the first).
	nodes := cycle[:len(cycle)-1]

	// Find the lexicographically smallest element.
	minIdx := 0
	for i := 1; i < len(nodes); i++ {
		if nodes[i] < nodes[minIdx] {
			minIdx = i
		}
	}

	// Rotate to start from the smallest element.
	rotated := make([]string, len(nodes))
	for i := 0; i < len(nodes); i++ {
		rotated[i] = nodes[(minIdx+i)%len(nodes)]
	}

	return strings.Join(rotated, "->")
}

// --- Unused Dependency Detection ---

// checkUnusedDeps checks for manifest dependencies not used in source imports.
func (a *DepGraphAnalyzer) checkUnusedDeps(ctx *AnalysisContext) []Issue {
	issues := make([]Issue, 0)

	// Collect all import paths from the index.
	allImports := collectAllImports(ctx.Index)

	for _, fileDiff := range ctx.Diff.Files {
		if fileDiff.Status == "deleted" {
			continue
		}

		base := filepath.Base(fileDiff.Path)

		switch base {
		case "package.json":
			issues = append(issues, checkUnusedNodeDeps(ctx, fileDiff, allImports)...)
		case "go.mod":
			issues = append(issues, checkUnusedGoDeps(ctx, fileDiff, allImports)...)
		case "requirements.txt":
			issues = append(issues, checkUnusedPythonDeps(ctx, fileDiff, allImports)...)
		case "Cargo.toml":
			issues = append(issues, checkUnusedRustDeps(ctx, fileDiff, allImports)...)
		}
	}

	return issues
}

// collectAllImports gathers all import paths from every file in the index.
func collectAllImports(idx *index.Index) map[string]bool {
	imports := make(map[string]bool)
	for _, entry := range idx.Files {
		for _, imp := range entry.Imports {
			imports[imp.Path] = true
			// Also add the top-level package name for scoped and nested imports.
			if strings.HasPrefix(imp.Path, "@") {
				parts := strings.SplitN(imp.Path, "/", 3)
				if len(parts) >= 2 {
					imports[parts[0]+"/"+parts[1]] = true
				}
			} else if !strings.HasPrefix(imp.Path, ".") && !strings.HasPrefix(imp.Path, "/") {
				if idx := strings.Index(imp.Path, "/"); idx != -1 {
					imports[imp.Path[:idx]] = true
				}
			}
		}
	}
	return imports
}

// --- Node.js (package.json) ---

func checkUnusedNodeDeps(ctx *AnalysisContext, fileDiff git.FileDiff, allImports map[string]bool) []Issue {
	issues := make([]Issue, 0)

	fullPath := filepath.Join(ctx.RepoPath, fileDiff.Path)
	deps, err := depGraphParsePackageJSON(fullPath)
	if err != nil {
		return issues
	}

	for depName := range deps {
		if isNodeDepUsed(depName, allImports) {
			continue
		}

		issues = append(issues, Issue{
			ID:         "depgraph/unused-dependency",
			Severity:   SeverityInfo,
			Category:   "depgraph",
			File:       fileDiff.Path,
			Message:    "Dependency appears unused: " + depName,
			Suggestion: "Remove with 'npm uninstall " + depName + "' if no longer needed",
		})
	}

	return issues
}

// isNodeDepUsed checks if a Node dependency is referenced in any import.
func isNodeDepUsed(depName string, allImports map[string]bool) bool {
	// Direct match.
	if allImports[depName] {
		return true
	}

	// Check if any import starts with the dep name (subpath imports).
	for imp := range allImports {
		if strings.HasPrefix(imp, depName+"/") {
			return true
		}
	}

	return false
}

// depGraphParsePackageJSON parses package.json for dependency names.
// Uses only the "dependencies" section (not devDependencies) for unused-dep checks.
func depGraphParsePackageJSON(path string) (map[string]bool, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}

	var pkg struct {
		Dependencies map[string]string `json:"dependencies"`
	}
	if err := json.Unmarshal(data, &pkg); err != nil {
		return nil, err
	}

	deps := make(map[string]bool)
	for name := range pkg.Dependencies {
		deps[name] = true
	}
	return deps, nil
}

// --- Go (go.mod) ---

func checkUnusedGoDeps(ctx *AnalysisContext, fileDiff git.FileDiff, allImports map[string]bool) []Issue {
	issues := make([]Issue, 0)

	fullPath := filepath.Join(ctx.RepoPath, fileDiff.Path)
	_, requires, err := depGraphParseGoMod(fullPath)
	if err != nil {
		return issues
	}

	for _, req := range requires {
		if isGoDependencyUsed(req, allImports) {
			continue
		}

		issues = append(issues, Issue{
			ID:         "depgraph/unused-dependency",
			Severity:   SeverityInfo,
			Category:   "depgraph",
			File:       fileDiff.Path,
			Message:    "Dependency appears unused: " + req,
			Suggestion: "Remove with 'go mod tidy' if no longer needed",
		})
	}

	return issues
}

// isGoDependencyUsed checks if a Go module dependency is referenced in any import.
func isGoDependencyUsed(modulePath string, allImports map[string]bool) bool {
	for imp := range allImports {
		if strings.HasPrefix(imp, modulePath) {
			return true
		}
	}
	return false
}

// depGraphParseGoMod parses go.mod for module path and require directives.
func depGraphParseGoMod(path string) (string, []string, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return "", nil, err
	}

	var modulePath string
	requires := make([]string, 0)

	lines := strings.Split(string(data), "\n")
	inRequireBlock := false
	for _, line := range lines {
		line = strings.TrimSpace(line)

		if strings.HasPrefix(line, "module ") {
			modulePath = strings.TrimSpace(strings.TrimPrefix(line, "module"))
			continue
		}

		if line == "require (" {
			inRequireBlock = true
			continue
		}
		if inRequireBlock && line == ")" {
			inRequireBlock = false
			continue
		}

		if inRequireBlock {
			parts := strings.Fields(line)
			if len(parts) >= 2 && !strings.HasPrefix(line, "//") {
				// Skip indirect dependencies.
				if !strings.Contains(line, "// indirect") {
					requires = append(requires, parts[0])
				}
			}
			continue
		}

		if strings.HasPrefix(line, "require ") && !strings.Contains(line, "(") {
			rest := strings.TrimPrefix(line, "require ")
			parts := strings.Fields(rest)
			if len(parts) >= 2 && !strings.Contains(line, "// indirect") {
				requires = append(requires, parts[0])
			}
		}
	}

	return modulePath, requires, nil
}

// --- Python (requirements.txt) ---

func checkUnusedPythonDeps(ctx *AnalysisContext, fileDiff git.FileDiff, allImports map[string]bool) []Issue {
	issues := make([]Issue, 0)

	fullPath := filepath.Join(ctx.RepoPath, fileDiff.Path)
	deps, err := parseRequirementsTxt(fullPath)
	if err != nil {
		return issues
	}

	for _, dep := range deps {
		if isPythonDepUsed(dep, allImports) {
			continue
		}

		issues = append(issues, Issue{
			ID:         "depgraph/unused-dependency",
			Severity:   SeverityInfo,
			Category:   "depgraph",
			File:       fileDiff.Path,
			Message:    "Dependency appears unused: " + dep,
			Suggestion: "Remove from requirements.txt if no longer needed",
		})
	}

	return issues
}

// isPythonDepUsed checks if a Python dependency name is referenced in any import.
// Python package names often differ from import names (e.g., "Pillow" -> "PIL"),
// so we do a case-insensitive check and also try replacing hyphens with underscores.
func isPythonDepUsed(depName string, allImports map[string]bool) bool {
	normalized := strings.ToLower(strings.ReplaceAll(depName, "-", "_"))

	for imp := range allImports {
		impNorm := strings.ToLower(strings.ReplaceAll(imp, "-", "_"))
		if impNorm == normalized || strings.HasPrefix(impNorm, normalized+".") {
			return true
		}
	}
	return false
}

var requirementsVersionPattern = regexp.MustCompile(`^([a-zA-Z0-9_.-]+)`)

// parseRequirementsTxt parses a requirements.txt file for package names.
func parseRequirementsTxt(path string) ([]string, error) {
	f, err := os.Open(path)
	if err != nil {
		return nil, err
	}
	defer f.Close()

	deps := make([]string, 0)
	scanner := bufio.NewScanner(f)
	for scanner.Scan() {
		line := strings.TrimSpace(scanner.Text())
		if line == "" || strings.HasPrefix(line, "#") || strings.HasPrefix(line, "-") {
			continue
		}

		if m := requirementsVersionPattern.FindStringSubmatch(line); len(m) >= 2 {
			deps = append(deps, m[1])
		}
	}

	return deps, nil
}

// --- Rust (Cargo.toml) ---

func checkUnusedRustDeps(ctx *AnalysisContext, fileDiff git.FileDiff, allImports map[string]bool) []Issue {
	issues := make([]Issue, 0)

	fullPath := filepath.Join(ctx.RepoPath, fileDiff.Path)
	deps, err := depGraphParseCargoToml(fullPath)
	if err != nil {
		return issues
	}

	for _, dep := range deps {
		if isRustDepUsed(dep, allImports) {
			continue
		}

		issues = append(issues, Issue{
			ID:         "depgraph/unused-dependency",
			Severity:   SeverityInfo,
			Category:   "depgraph",
			File:       fileDiff.Path,
			Message:    "Dependency appears unused: " + dep,
			Suggestion: "Remove from Cargo.toml if no longer needed",
		})
	}

	return issues
}

// isRustDepUsed checks if a Rust crate is referenced in any import (use statement).
// Crate names with hyphens become underscores in Rust imports.
func isRustDepUsed(depName string, allImports map[string]bool) bool {
	normalized := strings.ReplaceAll(depName, "-", "_")

	for imp := range allImports {
		impNorm := strings.ReplaceAll(imp, "-", "_")
		if impNorm == normalized || strings.HasPrefix(impNorm, normalized+"::") {
			return true
		}
	}
	return false
}

var cargoDepPattern = regexp.MustCompile(`^\s*([a-zA-Z0-9_-]+)\s*=`)

// parseCargoToml parses Cargo.toml for dependency names in the [dependencies] section.
func depGraphParseCargoToml(path string) ([]string, error) {
	f, err := os.Open(path)
	if err != nil {
		return nil, err
	}
	defer f.Close()

	deps := make([]string, 0)
	scanner := bufio.NewScanner(f)
	inDepsSection := false
	sectionPattern := regexp.MustCompile(`^\s*\[([^\]]+)\]`)

	for scanner.Scan() {
		line := scanner.Text()
		trimmed := strings.TrimSpace(line)

		// Check for section headers.
		if m := sectionPattern.FindStringSubmatch(trimmed); len(m) >= 2 {
			section := strings.TrimSpace(m[1])
			inDepsSection = section == "dependencies"
			continue
		}

		if !inDepsSection {
			continue
		}

		// Skip comments and empty lines.
		if trimmed == "" || strings.HasPrefix(trimmed, "#") {
			continue
		}

		if m := cargoDepPattern.FindStringSubmatch(line); len(m) >= 2 {
			deps = append(deps, m[1])
		}
	}

	return deps, nil
}

// --- Shared helpers ---

// isDepGraphSkippedPath returns true for vendored/generated directories.
func isDepGraphSkippedPath(path string) bool {
	normalized := filepath.ToSlash(path)
	parts := strings.Split(normalized, "/")
	for _, p := range parts {
		switch p {
		case "vendor", "node_modules", "testdata":
			return true
		}
	}
	return false
}
