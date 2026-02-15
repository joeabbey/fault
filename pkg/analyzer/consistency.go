package analyzer

import (
	"fmt"
	"strings"

	"github.com/joeabbey/fault/pkg/git"
	"github.com/joeabbey/fault/pkg/parser"
)

// ConsistencyAnalyzer checks for cross-file consistency issues such as
// changed function signatures without updated callers, changed type definitions,
// and interface/implementation gaps.
type ConsistencyAnalyzer struct{}

// NewConsistencyAnalyzer creates a new cross-file consistency analyzer.
func NewConsistencyAnalyzer() *ConsistencyAnalyzer {
	return &ConsistencyAnalyzer{}
}

// Name returns the analyzer name.
func (a *ConsistencyAnalyzer) Name() string {
	return "consistency"
}

// signatureChange represents a detected change in an exported symbol's signature.
type signatureChange struct {
	File         string
	SymbolName   string
	OldSignature string
	NewSignature string
	Kind         string
	Line         int
}

// Analyze runs cross-file consistency checks on the analysis context.
func (a *ConsistencyAnalyzer) Analyze(ctx *AnalysisContext) ([]Issue, error) {
	issues := make([]Issue, 0)

	if ctx.Diff == nil || len(ctx.ParsedFiles) == 0 {
		return issues, nil
	}

	// Build the set of changed files
	changedFiles := make(map[string]bool)
	for _, fd := range ctx.Diff.Files {
		if fd.Status != "deleted" {
			changedFiles[fd.Path] = true
		}
	}

	// 1. Detect signature changes from diff hunks
	sigChanges := a.detectSignatureChanges(ctx)

	// 2. For each changed signature, check if callers are updated
	for _, sc := range sigChanges {
		callerIssues := a.checkCallers(ctx, sc, changedFiles)
		issues = append(issues, callerIssues...)

		// Also check non-changed files across the repo via index
		if ctx.Index != nil {
			indexIssues := a.checkCallersViaIndex(ctx, sc, changedFiles)
			issues = append(issues, indexIssues...)
		}
	}

	// 3. Check for type/struct definition changes with stale usages
	typeIssues := a.checkTypeChanges(ctx, changedFiles)
	issues = append(issues, typeIssues...)

	// 4. Check for interface/implementation gaps (Go-specific)
	interfaceIssues := a.checkInterfaceGaps(ctx, changedFiles)
	issues = append(issues, interfaceIssues...)

	return issues, nil
}

// detectSignatureChanges looks at diff hunks to find exported symbols whose
// signatures were changed (removed old line, added new line with same name but different signature).
func (a *ConsistencyAnalyzer) detectSignatureChanges(ctx *AnalysisContext) []signatureChange {
	changes := make([]signatureChange, 0)

	for _, fd := range ctx.Diff.Files {
		if fd.Status == "deleted" || fd.Status == "added" {
			continue
		}

		pf, ok := ctx.ParsedFiles[fd.Path]
		if !ok {
			continue
		}

		hunkChanges := extractSignatureChangesFromHunks(fd, pf.Language)
		changes = append(changes, hunkChanges...)
	}

	return changes
}

// extractSignatureChangesFromHunks analyzes diff hunks to find removed and added
// lines that represent signature changes for the same symbol.
func extractSignatureChangesFromHunks(fd git.FileDiff, language string) []signatureChange {
	changes := make([]signatureChange, 0)

	for _, hunk := range fd.Hunks {
		// Collect removed and added function/method declarations
		removedSigs := make(map[string]string) // name -> signature
		addedSigs := make(map[string]signatureInfo)

		for _, line := range hunk.Lines {
			content := strings.TrimSpace(line.Content)

			if line.Type == "removed" {
				name, sig := extractSignatureFromLine(content, language)
				if name != "" && isExportedSymbol(name, language) {
					removedSigs[name] = sig
				}
			}

			if line.Type == "added" {
				name, sig := extractSignatureFromLine(content, language)
				if name != "" && isExportedSymbol(name, language) {
					addedSigs[name] = signatureInfo{signature: sig, line: line.NewNum}
				}
			}
		}

		// Find symbols that appear in both removed and added with different signatures
		for name, oldSig := range removedSigs {
			if newInfo, ok := addedSigs[name]; ok {
				if oldSig != newInfo.signature {
					changes = append(changes, signatureChange{
						File:         fd.Path,
						SymbolName:   name,
						OldSignature: oldSig,
						NewSignature: newInfo.signature,
						Kind:         "function",
						Line:         newInfo.line,
					})
				}
			}
		}
	}

	return changes
}

type signatureInfo struct {
	signature string
	line      int
}

// extractSignatureFromLine extracts a function/method name and signature from a source line.
func extractSignatureFromLine(line, language string) (string, string) {
	switch language {
	case "go":
		return extractGoSignature(line)
	case "typescript":
		return extractTSSignature(line)
	case "python":
		return extractPythonSignature(line)
	}
	return "", ""
}

// extractGoSignature extracts name and signature from a Go function declaration line.
func extractGoSignature(line string) (string, string) {
	if !strings.HasPrefix(line, "func ") {
		return "", ""
	}

	rest := line[5:]

	// Skip receiver: func (r *Repo) Name(...)
	if strings.HasPrefix(rest, "(") {
		idx := strings.Index(rest, ") ")
		if idx < 0 {
			return "", ""
		}
		rest = rest[idx+2:]
	}

	// Extract name
	parenIdx := strings.IndexByte(rest, '(')
	if parenIdx < 0 {
		return "", ""
	}
	name := strings.TrimSpace(rest[:parenIdx])
	if name == "" {
		return "", ""
	}

	// The signature is everything from name to end of line (or opening brace)
	sig := rest
	if braceIdx := strings.IndexByte(sig, '{'); braceIdx > 0 {
		sig = strings.TrimSpace(sig[:braceIdx])
	}

	return name, sig
}

// extractTSSignature extracts name and signature from a TypeScript function line.
func extractTSSignature(line string) (string, string) {
	// Handle: export function foo(args): return / export async function foo(args)
	// Also: function foo(args)
	prefixes := []string{
		"export default async function ",
		"export default function ",
		"export async function ",
		"export function ",
		"async function ",
		"function ",
	}

	for _, prefix := range prefixes {
		if strings.HasPrefix(line, prefix) {
			rest := line[len(prefix):]
			parenIdx := strings.IndexByte(rest, '(')
			if parenIdx < 0 {
				continue
			}
			name := strings.TrimSpace(rest[:parenIdx])
			if name == "" {
				continue
			}

			sig := rest
			if braceIdx := strings.IndexByte(sig, '{'); braceIdx > 0 {
				sig = strings.TrimSpace(sig[:braceIdx])
			}
			return name, sig
		}
	}

	return "", ""
}

// extractPythonSignature extracts name and signature from a Python def line.
func extractPythonSignature(line string) (string, string) {
	prefixes := []string{"async def ", "def "}
	for _, prefix := range prefixes {
		if strings.HasPrefix(line, prefix) {
			rest := line[len(prefix):]
			parenIdx := strings.IndexByte(rest, '(')
			if parenIdx < 0 {
				continue
			}
			name := strings.TrimSpace(rest[:parenIdx])
			if name == "" {
				continue
			}

			// Signature up to the colon
			sig := rest
			if colonIdx := strings.LastIndexByte(sig, ':'); colonIdx > 0 {
				sig = strings.TrimSpace(sig[:colonIdx])
			}
			return name, sig
		}
	}
	return "", ""
}

// isExportedSymbol checks if a symbol name is exported in the given language.
func isExportedSymbol(name, language string) bool {
	switch language {
	case "go":
		return len(name) > 0 && name[0] >= 'A' && name[0] <= 'Z'
	case "typescript":
		// In TS context, we only flag signatures we see in export lines.
		// The caller already extracted from export-prefixed lines.
		return true
	case "python":
		return !strings.HasPrefix(name, "_")
	}
	return true
}

// checkCallers checks if any other changed file references a symbol whose
// signature was changed, but may not have been updated.
func (a *ConsistencyAnalyzer) checkCallers(
	ctx *AnalysisContext,
	sc signatureChange,
	changedFiles map[string]bool,
) []Issue {
	issues := make([]Issue, 0)

	for filePath, pf := range ctx.ParsedFiles {
		if filePath == sc.File {
			continue
		}

		// Check if this file imports from the file that changed
		importsFrom := false
		importLine := 0
		for _, imp := range pf.Imports {
			targetPath := resolveImportToFile(pf, imp, ctx.ParsedFiles)
			if targetPath == sc.File {
				importsFrom = true
				importLine = imp.Line
				break
			}
		}

		if !importsFrom {
			// For Go: check if the file is in the same package (no import needed)
			if pf.Language == "go" {
				// Files in the same directory share a package
				if sameDirectory(filePath, sc.File) {
					importsFrom = true
				}
			}
		}

		if !importsFrom {
			continue
		}

		// This file imports from the changed file.
		// Check if the symbol name appears in the file's content (heuristic).
		// If the file is in the diff, check whether it was also modified.
		usesSymbol := fileReferencesSymbol(pf, sc.SymbolName)
		if !usesSymbol {
			continue
		}

		// The file references the changed symbol. If it wasn't also changed, flag it.
		if !changedFiles[filePath] {
			issues = append(issues, Issue{
				ID:       fmt.Sprintf("sig-change-%s-%s-%s", sc.File, filePath, sc.SymbolName),
				Severity: SeverityWarning,
				Category: "consistency",
				File:     filePath,
				Line:     importLine,
				Message: fmt.Sprintf(
					"Function %q signature changed in %s but this file was not updated",
					sc.SymbolName, sc.File,
				),
				Suggestion: fmt.Sprintf(
					"Review usages of %s: was %q, now %q",
					sc.SymbolName, sc.OldSignature, sc.NewSignature,
				),
				RelatedFiles: []string{sc.File},
			})
		} else {
			// File was changed, but double-check if the usage lines were actually modified
			if !fileModifiedSymbolUsage(ctx.Diff, filePath, sc.SymbolName) {
				issues = append(issues, Issue{
					ID:       fmt.Sprintf("sig-change-partial-%s-%s-%s", sc.File, filePath, sc.SymbolName),
					Severity: SeverityInfo,
					Category: "consistency",
					File:     filePath,
					Line:     importLine,
					Message: fmt.Sprintf(
						"Function %q signature changed in %s; verify call sites in this file are updated",
						sc.SymbolName, sc.File,
					),
					Suggestion: fmt.Sprintf(
						"Signature changed from %q to %q",
						sc.OldSignature, sc.NewSignature,
					),
					RelatedFiles: []string{sc.File},
				})
			}
		}
	}

	return issues
}

// checkCallersViaIndex uses the repo index to find files outside the changeset
// that import a file whose signature changed and may reference the symbol.
func (a *ConsistencyAnalyzer) checkCallersViaIndex(
	ctx *AnalysisContext,
	sc signatureChange,
	changedFiles map[string]bool,
) []Issue {
	issues := make([]Issue, 0)

	for _, entry := range ctx.Index.AllFiles() {
		// Skip files already in ParsedFiles (handled by checkCallers)
		if _, inParsed := ctx.ParsedFiles[entry.Path]; inParsed {
			continue
		}
		// Skip changed files
		if changedFiles[entry.Path] {
			continue
		}
		// Skip the changed file itself
		if entry.Path == sc.File {
			continue
		}

		// Check if this index file imports from the changed file
		importsFrom := false
		for _, imp := range entry.Imports {
			switch entry.Language {
			case "typescript":
				if isRelativeImport(imp.Path) {
					resolved := resolveRelativeImport(entry.Path, imp.Path)
					if matchesTSPath(resolved, sc.File) {
						importsFrom = true
					}
				}
			case "python":
				if strings.HasPrefix(imp.Path, ".") {
					resolved := resolvePythonImport(entry.Path, imp.Path)
					if matchesPythonPath(resolved, sc.File) {
						importsFrom = true
					}
				}
			case "go":
				// For Go, check same directory (same package)
				if sameDirectory(entry.Path, sc.File) {
					importsFrom = true
				}
			}
			if importsFrom {
				break
			}
		}

		// For Go: same-package files
		if !importsFrom && entry.Language == "go" && sameDirectory(entry.Path, sc.File) {
			importsFrom = true
		}

		if !importsFrom {
			continue
		}

		// Check if any import references the changed symbol by name
		referencesSymbol := false
		for _, imp := range entry.Imports {
			for _, name := range imp.Names {
				if name == sc.SymbolName {
					referencesSymbol = true
					break
				}
			}
			if referencesSymbol {
				break
			}
		}

		// For Go: check exports list for references to the symbol
		if !referencesSymbol && entry.Language == "go" {
			for _, exp := range entry.Exports {
				if strings.Contains(exp.Name, sc.SymbolName) {
					referencesSymbol = true
					break
				}
			}
		}

		if !referencesSymbol {
			continue
		}

		issues = append(issues, Issue{
			ID:       fmt.Sprintf("sig-change-index-%s-%s-%s", sc.File, entry.Path, sc.SymbolName),
			Severity: SeverityWarning,
			Category: "consistency",
			File:     entry.Path,
			Message: fmt.Sprintf(
				"Function %q signature changed in %s but this file was not updated (found via repo index)",
				sc.SymbolName, sc.File,
			),
			Suggestion: fmt.Sprintf(
				"Review usages of %s: was %q, now %q",
				sc.SymbolName, sc.OldSignature, sc.NewSignature,
			),
			RelatedFiles: []string{sc.File},
		})
	}

	return issues
}

// checkTypeChanges detects when a type/struct definition changed but files using it were not updated.
func (a *ConsistencyAnalyzer) checkTypeChanges(
	ctx *AnalysisContext,
	changedFiles map[string]bool,
) []Issue {
	issues := make([]Issue, 0)

	// Find type definitions that changed (appeared in removed + added lines)
	for _, fd := range ctx.Diff.Files {
		if fd.Status == "deleted" || fd.Status == "added" {
			continue
		}

		pf, ok := ctx.ParsedFiles[fd.Path]
		if !ok {
			continue
		}

		changedTypes := detectChangedTypes(fd, pf.Language)
		if len(changedTypes) == 0 {
			continue
		}

		// For each changed type, check other files that reference it
		for _, typeName := range changedTypes {
			for otherPath, otherPf := range ctx.ParsedFiles {
				if otherPath == fd.Path {
					continue
				}

				// Check if the other file imports from this file
				importsFrom := false
				for _, imp := range otherPf.Imports {
					targetPath := resolveImportToFile(otherPf, imp, ctx.ParsedFiles)
					if targetPath == fd.Path {
						importsFrom = true
						break
					}
				}

				// For Go: same package files don't need explicit imports
				if !importsFrom && pf.Language == "go" && sameDirectory(otherPath, fd.Path) {
					importsFrom = true
				}

				if !importsFrom {
					continue
				}

				if fileReferencesSymbol(otherPf, typeName) && !changedFiles[otherPath] {
					issues = append(issues, Issue{
						ID:       fmt.Sprintf("type-change-%s-%s-%s", fd.Path, otherPath, typeName),
						Severity: SeverityWarning,
						Category: "consistency",
						File:     otherPath,
						Message: fmt.Sprintf(
							"Type %q was modified in %s but this file using it was not updated",
							typeName, fd.Path,
						),
						Suggestion:   fmt.Sprintf("Review usages of %s in this file", typeName),
						RelatedFiles: []string{fd.Path},
					})
				}
			}
		}
	}

	return issues
}

// checkInterfaceGaps checks for Go interface methods that were added or changed
// without corresponding updates to implementation files.
func (a *ConsistencyAnalyzer) checkInterfaceGaps(
	ctx *AnalysisContext,
	changedFiles map[string]bool,
) []Issue {
	issues := make([]Issue, 0)

	// Collect all interfaces and their methods from parsed files
	interfaceMethods := make(map[string][]string) // interfaceName -> method names

	for filePath, pf := range ctx.ParsedFiles {
		if pf.Language != "go" {
			continue
		}
		if !changedFiles[filePath] {
			continue
		}

		for _, sym := range pf.Symbols {
			if sym.Kind == "interface" && sym.Exported {
				// Extract interface method names from diff additions
				methods := extractInterfaceMethodsFromDiff(ctx.Diff, filePath, sym)
				if len(methods) > 0 {
					interfaceMethods[sym.Name] = methods
				}
			}
		}
	}

	if len(interfaceMethods) == 0 {
		return issues
	}

	// Check implementation files: look for types that have methods matching the interface
	for filePath, pf := range ctx.ParsedFiles {
		if pf.Language != "go" {
			continue
		}

		for _, sym := range pf.Symbols {
			if sym.Kind != "method" {
				continue
			}

			// For each interface, check if this file's type might implement it
			for ifaceName, methods := range interfaceMethods {
				for _, methodName := range methods {
					if sym.Name == methodName && !changedFiles[filePath] {
						issues = append(issues, Issue{
							ID:       fmt.Sprintf("iface-gap-%s-%s-%s", ifaceName, filePath, methodName),
							Severity: SeverityWarning,
							Category: "consistency",
							File:     filePath,
							Line:     sym.Line,
							Message: fmt.Sprintf(
								"Interface %q method %q was changed but this implementation was not updated",
								ifaceName, methodName,
							),
							Suggestion:   "Verify this method still satisfies the interface contract",
							RelatedFiles: make([]string, 0),
						})
					}
				}
			}
		}
	}

	return issues
}

// --- Helper functions ---

// sameDirectory returns true if two file paths are in the same directory.
func sameDirectory(a, b string) bool {
	return dirOf(a) == dirOf(b)
}

// dirOf returns the directory portion of a path.
func dirOf(path string) string {
	idx := strings.LastIndexByte(path, '/')
	if idx < 0 {
		return "."
	}
	return path[:idx]
}

// fileReferencesSymbol checks if a parsed file likely references a symbol by name.
// It looks at imports (named imports) and the symbol list for matching names.
func fileReferencesSymbol(pf *parser.ParsedFile, symbolName string) bool {
	// Check named imports
	for _, imp := range pf.Imports {
		for _, name := range imp.Names {
			if name == symbolName {
				return true
			}
		}
	}

	// Check if any symbol in this file references it (e.g., in signature)
	for _, sym := range pf.Symbols {
		if strings.Contains(sym.Signature, symbolName) {
			return true
		}
	}

	return false
}

// fileModifiedSymbolUsage checks if any diff lines in a file mention the symbol name.
func fileModifiedSymbolUsage(diff *git.Diff, filePath, symbolName string) bool {
	if diff == nil {
		return false
	}

	for _, fd := range diff.Files {
		if fd.Path != filePath {
			continue
		}

		for _, hunk := range fd.Hunks {
			for _, line := range hunk.Lines {
				if line.Type == "added" || line.Type == "removed" {
					if strings.Contains(line.Content, symbolName) {
						return true
					}
				}
			}
		}
	}

	return false
}

// detectChangedTypes finds type/struct/class names that were modified in a diff.
func detectChangedTypes(fd git.FileDiff, language string) []string {
	changed := make([]string, 0)
	removedTypes := make(map[string]bool)
	addedTypes := make(map[string]bool)

	for _, hunk := range fd.Hunks {
		for _, line := range hunk.Lines {
			content := strings.TrimSpace(line.Content)
			name := extractTypeName(content, language)
			if name == "" {
				continue
			}

			if line.Type == "removed" {
				removedTypes[name] = true
			}
			if line.Type == "added" {
				addedTypes[name] = true
			}
		}
	}

	// Types that appear in both removed and added are "changed"
	for name := range removedTypes {
		if addedTypes[name] {
			changed = append(changed, name)
		}
	}

	return changed
}

// extractTypeName extracts a type/struct/class name from a source line.
func extractTypeName(line, language string) string {
	switch language {
	case "go":
		if strings.HasPrefix(line, "type ") {
			rest := line[5:]
			name := extractFirstIdentifier(rest)
			if name != "" && name[0] >= 'A' && name[0] <= 'Z' {
				return name
			}
		}
	case "typescript":
		prefixes := []string{"export type ", "export interface ", "type ", "interface "}
		for _, prefix := range prefixes {
			if strings.HasPrefix(line, prefix) {
				rest := line[len(prefix):]
				return extractFirstIdentifier(rest)
			}
		}
	case "python":
		if strings.HasPrefix(line, "class ") {
			rest := line[6:]
			name := extractFirstIdentifier(rest)
			if name != "" {
				return name
			}
		}
	}
	return ""
}

// extractInterfaceMethodsFromDiff extracts method names added to a Go interface in the diff.
func extractInterfaceMethodsFromDiff(diff *git.Diff, filePath string, sym parser.Symbol) []string {
	methods := make([]string, 0)

	if diff == nil {
		return methods
	}

	for _, fd := range diff.Files {
		if fd.Path != filePath {
			continue
		}

		for _, hunk := range fd.Hunks {
			// Check if this hunk overlaps with the interface definition
			if sym.EndLine > 0 && (hunk.NewStart > sym.EndLine || hunk.NewStart+hunk.NewCount < sym.Line) {
				continue
			}

			for _, line := range hunk.Lines {
				if line.Type != "added" {
					continue
				}
				content := strings.TrimSpace(line.Content)
				// Go interface method: Name(args) (returns)
				if len(content) > 0 && content[0] >= 'A' && content[0] <= 'Z' {
					name := extractFirstIdentifier(content)
					if name != "" && strings.Contains(content, "(") {
						methods = append(methods, name)
					}
				}
			}
		}
	}

	return methods
}
