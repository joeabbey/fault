package analyzer

import (
	"fmt"
	"path/filepath"
	"regexp"
	"strings"

	"github.com/joeabbey/fault/pkg/git"
)

// DocDriftAnalyzer detects stale comments and documentation that no longer
// match the function signatures they describe. This is one of the most common
// AI agent tells: modifying function signatures while leaving the docstring
// unchanged.
type DocDriftAnalyzer struct{}

// NewDocDriftAnalyzer creates a new doc drift analyzer.
func NewDocDriftAnalyzer() *DocDriftAnalyzer {
	return &DocDriftAnalyzer{}
}

// Name returns the analyzer name.
func (a *DocDriftAnalyzer) Name() string {
	return "docdrift"
}

// Analyze runs doc drift detection on the analysis context.
func (a *DocDriftAnalyzer) Analyze(ctx *AnalysisContext) ([]Issue, error) {
	issues := make([]Issue, 0)

	if ctx.Diff == nil {
		return issues, nil
	}

	for _, fileDiff := range ctx.Diff.Files {
		if fileDiff.Status == "deleted" || fileDiff.IsBinary {
			continue
		}
		if isTestFile(fileDiff.Path) {
			continue
		}
		if ddIsVendorOrNodeModules(fileDiff.Path) {
			continue
		}

		ext := strings.ToLower(filepath.Ext(fileDiff.Path))
		issues = append(issues, a.checkDocDrift(fileDiff, ext)...)
	}

	return issues, nil
}

// ddIsVendorOrNodeModules returns true if the path is in vendor/ or node_modules/.
func ddIsVendorOrNodeModules(path string) bool {
	return strings.Contains(path, "vendor/") || strings.Contains(path, "node_modules/")
}

// sigPair represents a matched pair of old (removed) and new (added) function signatures.
type sigPair struct {
	funcName  string
	oldSig    string
	newSig    string
	oldParams []string
	newParams []string
	oldReturn string
	newReturn string
	newLine   int // line number in the new file
}

// commentBlock represents a contiguous block of unchanged comment lines
// found immediately above a signature change.
type commentBlock struct {
	lines    []string
	startNum int // line number of the first comment line
	endNum   int // line number of the last comment line
}

// checkDocDrift scans diff hunks for signature changes with stale comments.
func (a *DocDriftAnalyzer) checkDocDrift(fd git.FileDiff, ext string) []Issue {
	issues := make([]Issue, 0)

	for _, hunk := range fd.Hunks {
		issues = append(issues, a.checkHunkForDocDrift(fd.Path, hunk, ext)...)
	}

	return issues
}

// checkHunkForDocDrift analyzes a single diff hunk for doc drift.
func (a *DocDriftAnalyzer) checkHunkForDocDrift(filePath string, hunk git.Hunk, ext string) []Issue {
	issues := make([]Issue, 0)
	lines := hunk.Lines

	// Find signature pairs: removed func signatures followed by added func
	// signatures with the same name but different content.
	pairs := a.findSignaturePairs(lines, ext)

	for _, pair := range pairs {
		// Look backwards from the signature change for an unchanged comment block
		cb := a.findPrecedingCommentBlock(lines, pair, ext)
		if cb == nil || len(cb.lines) == 0 {
			// No doc comment above this function — nothing to drift
			continue
		}

		// We have an unchanged comment above a changed signature. Emit issues.

		// Check for param mismatch: old param names in the comment that aren't in the new signature
		staleParams := a.findStaleParamReferences(cb, pair)
		if len(staleParams) > 0 {
			issues = append(issues, Issue{
				ID:       fmt.Sprintf("docdrift-param-mismatch-%s-%s-%d", filePath, pair.funcName, pair.newLine),
				Severity: SeverityWarning,
				Category: "docdrift",
				File:     filePath,
				Line:     cb.startNum,
				EndLine:  cb.endNum,
				Message: fmt.Sprintf(
					"Documentation for %s references parameters %s that no longer exist in the function signature",
					pair.funcName, formatParamList(staleParams),
				),
				Suggestion: "Update the documentation to reflect the current function parameters.",
			})
		}

		// Check for return type change
		if pair.oldReturn != "" && pair.newReturn != "" && pair.oldReturn != pair.newReturn {
			issues = append(issues, Issue{
				ID:       fmt.Sprintf("docdrift-return-type-%s-%s-%d", filePath, pair.funcName, pair.newLine),
				Severity: SeverityInfo,
				Category: "docdrift",
				File:     filePath,
				Line:     cb.startNum,
				EndLine:  cb.endNum,
				Message: fmt.Sprintf(
					"Return type of %s changed from %s to %s but documentation was not updated",
					pair.funcName, pair.oldReturn, pair.newReturn,
				),
				Suggestion: "Review the documentation to ensure it describes the new return type.",
			})
		}

		// Check for stale code examples in the comment
		if a.hasStaleExample(cb, pair) {
			issues = append(issues, Issue{
				ID:       fmt.Sprintf("docdrift-stale-example-%s-%s-%d", filePath, pair.funcName, pair.newLine),
				Severity: SeverityInfo,
				Category: "docdrift",
				File:     filePath,
				Line:     cb.startNum,
				EndLine:  cb.endNum,
				Message: fmt.Sprintf(
					"Documentation for %s contains code examples that may reference outdated parameter patterns",
					pair.funcName,
				),
				Suggestion: "Update code examples in the documentation to match the current function signature.",
			})
		}

		// General stale-doc: signature changed, doc completely unchanged
		// Only emit if we didn't already emit param-mismatch (avoid double-flagging)
		if len(staleParams) == 0 {
			issues = append(issues, Issue{
				ID:       fmt.Sprintf("docdrift-stale-doc-%s-%s-%d", filePath, pair.funcName, pair.newLine),
				Severity: SeverityWarning,
				Category: "docdrift",
				File:     filePath,
				Line:     cb.startNum,
				EndLine:  cb.endNum,
				Message: fmt.Sprintf(
					"Function %s signature changed but documentation was not updated",
					pair.funcName,
				),
				Suggestion: "Function signature changed but documentation was not updated. Review the comment to ensure it still accurately describes the function's behavior.",
			})
		}
	}

	return issues
}

// findSignaturePairs scans hunk lines to find removed + added function signature
// pairs with the same function name but different content.
func (a *DocDriftAnalyzer) findSignaturePairs(lines []git.Line, ext string) []sigPair {
	pairs := make([]sigPair, 0)

	// Collect removed and added function signatures with their positions
	type sigEntry struct {
		index  int
		name   string
		sig    string
		line   git.Line
		params []string
		ret    string
	}

	removedSigs := make([]sigEntry, 0)
	addedSigs := make([]sigEntry, 0)

	for i, line := range lines {
		content := strings.TrimSpace(line.Content)
		name := extractFuncName(content, ext)
		if name == "" {
			continue
		}

		params := extractParams(content, ext)
		ret := extractReturnType(content, ext)

		if line.Type == "removed" {
			removedSigs = append(removedSigs, sigEntry{
				index: i, name: name, sig: content, line: line,
				params: params, ret: ret,
			})
		} else if line.Type == "added" {
			addedSigs = append(addedSigs, sigEntry{
				index: i, name: name, sig: content, line: line,
				params: params, ret: ret,
			})
		}
	}

	// Match removed + added by function name
	for _, removed := range removedSigs {
		for _, added := range addedSigs {
			if removed.name == added.name && removed.sig != added.sig {
				pairs = append(pairs, sigPair{
					funcName:  removed.name,
					oldSig:    removed.sig,
					newSig:    added.sig,
					oldParams: removed.params,
					newParams: added.params,
					oldReturn: removed.ret,
					newReturn: added.ret,
					newLine:   added.line.NewNum,
				})
				break // one match per removed sig
			}
		}
	}

	return pairs
}

// findPrecedingCommentBlock looks backwards from a signature pair's position
// in the hunk lines to find an unchanged (context) comment block.
func (a *DocDriftAnalyzer) findPrecedingCommentBlock(lines []git.Line, pair sigPair, ext string) *commentBlock {
	// Find the position of the removed signature line in the hunk
	removedIdx := -1
	for i, line := range lines {
		if line.Type == "removed" {
			name := extractFuncName(strings.TrimSpace(line.Content), ext)
			if name == pair.funcName {
				removedIdx = i
				break
			}
		}
	}
	if removedIdx < 0 {
		return nil
	}

	// Walk backwards from removedIdx collecting context comment lines
	cb := &commentBlock{
		lines: make([]string, 0),
	}

	for i := removedIdx - 1; i >= 0; i-- {
		line := lines[i]

		// Only context (unchanged) lines count as stale docs
		if line.Type != "context" {
			break
		}

		content := line.Content
		if !isDocComment(content, ext) {
			// If we hit a blank context line between comments, keep going
			if strings.TrimSpace(content) == "" && i > 0 {
				// Peek further back to see if there are more comment lines
				peekLine := lines[i-1]
				if peekLine.Type == "context" && isDocComment(peekLine.Content, ext) {
					continue
				}
			}
			break
		}

		cb.lines = append([]string{content}, cb.lines...)
		if cb.endNum == 0 {
			cb.endNum = line.OldNum
		}
		cb.startNum = line.OldNum
	}

	if len(cb.lines) == 0 {
		return nil
	}

	return cb
}

// findStaleParamReferences checks if the unchanged comment references parameter
// names from the old signature that are not in the new signature.
func (a *DocDriftAnalyzer) findStaleParamReferences(cb *commentBlock, pair sigPair) []string {
	// Build set of new param names
	newParamSet := make(map[string]bool)
	for _, p := range pair.newParams {
		newParamSet[p] = true
	}

	// Find old param names that are NOT in the new params
	removedParams := make([]string, 0)
	for _, p := range pair.oldParams {
		if !newParamSet[p] {
			removedParams = append(removedParams, p)
		}
	}

	if len(removedParams) == 0 {
		return nil
	}

	// Check if any removed param name appears in the comment text
	commentText := strings.Join(cb.lines, " ")
	staleParams := make([]string, 0)
	for _, p := range removedParams {
		if containsWord(commentText, p) {
			staleParams = append(staleParams, p)
		}
	}

	return staleParams
}

// hasStaleExample checks if the comment block contains code examples that
// reference old parameter patterns.
func (a *DocDriftAnalyzer) hasStaleExample(cb *commentBlock, pair sigPair) bool {
	commentText := strings.Join(cb.lines, "\n")

	// Look for function call patterns with old param count
	// e.g., "FuncName(arg1, arg2)" when new sig has 3 params
	if len(pair.oldParams) != len(pair.newParams) {
		// Build a rough regex for old call pattern: funcName( ... , ... )
		// with the old number of commas
		pattern := regexp.QuoteMeta(pair.funcName) + `\s*\(`
		if matched, _ := regexp.MatchString(pattern, commentText); matched {
			// Count commas in the example call vs expected
			return true
		}
	}

	return false
}

// extractFuncName extracts a function name from a source code line.
func extractFuncName(line, ext string) string {
	switch ext {
	case ".go":
		return ddExtractGoFuncName(line)
	case ".py":
		return extractPythonFuncName(line)
	case ".ts", ".tsx", ".js", ".jsx", ".mjs", ".cjs":
		return extractTSFuncName(line)
	case ".java":
		return extractJavaFuncName(line)
	case ".rs":
		return ddExtractRustFuncName(line)
	case ".rb":
		return ddExtractRubyFuncName(line)
	case ".c", ".cpp", ".h", ".hpp", ".cc":
		return extractCFuncName(line)
	case ".kt", ".kts":
		return extractKotlinFuncName(line)
	case ".cs":
		return extractCSharpFuncName(line)
	case ".swift":
		return extractSwiftFuncName(line)
	case ".php":
		return ddExtractPHPFuncName(line)
	case ".dart":
		return extractDartFuncName(line)
	case ".scala":
		return extractScalaFuncName(line)
	case ".ex", ".exs":
		return ddExtractElixirFuncName(line)
	case ".zig":
		return extractZigFuncName(line)
	}
	return ""
}

// ddExtractGoFuncName extracts a Go function name.
func ddExtractGoFuncName(line string) string {
	if !strings.HasPrefix(line, "func ") {
		return ""
	}
	rest := line[5:]
	// Skip receiver: func (r *Repo) Name(...)
	if strings.HasPrefix(rest, "(") {
		idx := strings.Index(rest, ") ")
		if idx < 0 {
			return ""
		}
		rest = rest[idx+2:]
	}
	parenIdx := strings.IndexByte(rest, '(')
	if parenIdx < 0 {
		return ""
	}
	name := strings.TrimSpace(rest[:parenIdx])
	if name == "" {
		return ""
	}
	return name
}

// extractPythonFuncName extracts a Python function name.
func extractPythonFuncName(line string) string {
	for _, prefix := range []string{"async def ", "def "} {
		if strings.HasPrefix(line, prefix) {
			rest := line[len(prefix):]
			parenIdx := strings.IndexByte(rest, '(')
			if parenIdx < 0 {
				continue
			}
			name := strings.TrimSpace(rest[:parenIdx])
			if name != "" {
				return name
			}
		}
	}
	return ""
}

// extractTSFuncName extracts a TypeScript/JavaScript function name.
func extractTSFuncName(line string) string {
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
			if name != "" {
				return name
			}
		}
	}

	// Handle: const name = (...) => or const name = function(
	if strings.HasPrefix(line, "const ") || strings.HasPrefix(line, "let ") || strings.HasPrefix(line, "var ") {
		eqIdx := strings.Index(line, "=")
		if eqIdx < 0 {
			return ""
		}
		// Get name between keyword and =
		keyword := "const "
		if strings.HasPrefix(line, "let ") {
			keyword = "let "
		} else if strings.HasPrefix(line, "var ") {
			keyword = "var "
		}
		name := strings.TrimSpace(line[len(keyword):eqIdx])
		rhs := strings.TrimSpace(line[eqIdx+1:])
		// Check if RHS is a function expression or arrow
		if strings.HasPrefix(rhs, "(") || strings.HasPrefix(rhs, "async (") ||
			strings.HasPrefix(rhs, "function") || strings.HasPrefix(rhs, "async function") {
			if name != "" {
				return name
			}
		}
	}

	// Handle method declarations: name(...) { or async name(...) {
	methodLine := line
	if strings.HasPrefix(methodLine, "async ") {
		methodLine = methodLine[6:]
	}
	if len(methodLine) > 0 && (isIdentStart(methodLine[0])) {
		parenIdx := strings.IndexByte(methodLine, '(')
		if parenIdx > 0 {
			name := strings.TrimSpace(methodLine[:parenIdx])
			if isValidIdentifier(name) {
				return name
			}
		}
	}

	return ""
}

// extractJavaFuncName extracts a Java method name.
func extractJavaFuncName(line string) string {
	// Java method patterns: [modifiers] returnType name(params)
	// Look for identifier followed by ( that isn't a control structure
	controlKeywords := map[string]bool{
		"if": true, "else": true, "for": true, "while": true, "switch": true,
		"catch": true, "class": true, "interface": true, "enum": true,
		"new": true, "return": true, "throw": true, "import": true, "package": true,
	}

	parenIdx := strings.IndexByte(line, '(')
	if parenIdx <= 0 {
		return ""
	}

	before := strings.TrimSpace(line[:parenIdx])
	parts := strings.Fields(before)
	if len(parts) == 0 {
		return ""
	}

	name := parts[len(parts)-1]
	if controlKeywords[name] {
		return ""
	}
	if !isValidIdentifier(name) {
		return ""
	}

	// Must have at least a return type before the name (or be a constructor)
	if len(parts) >= 2 || (len(parts) == 1 && isUpperStart(name)) {
		return name
	}

	return ""
}

// ddExtractRustFuncName extracts a Rust function name.
func ddExtractRustFuncName(line string) string {
	// pub fn name( or fn name( or pub async fn name(
	stripped := line
	for _, prefix := range []string{"pub ", "pub(crate) ", "pub(super) ", "async ", "unsafe ", "const ", "extern \"C\" "} {
		stripped = strings.TrimPrefix(stripped, prefix)
	}
	if !strings.HasPrefix(stripped, "fn ") {
		return ""
	}
	rest := stripped[3:]
	parenIdx := strings.IndexByte(rest, '(')
	if parenIdx <= 0 {
		return ""
	}
	// Handle generics: fn name<T>(
	angleIdx := strings.IndexByte(rest, '<')
	if angleIdx > 0 && angleIdx < parenIdx {
		return strings.TrimSpace(rest[:angleIdx])
	}
	return strings.TrimSpace(rest[:parenIdx])
}

// ddExtractRubyFuncName extracts a Ruby method name.
func ddExtractRubyFuncName(line string) string {
	if strings.HasPrefix(line, "def ") {
		rest := line[4:]
		// def self.name or def name
		rest = strings.TrimPrefix(rest, "self.")
		// Find end of name: ( or space or newline
		for i, ch := range rest {
			if ch == '(' || ch == ' ' || ch == '\n' {
				name := rest[:i]
				if name != "" {
					return name
				}
				break
			}
		}
		// No delimiter found, whole rest is the name
		name := strings.TrimSpace(rest)
		if name != "" {
			return name
		}
	}
	return ""
}

// extractCFuncName extracts a C/C++ function name.
func extractCFuncName(line string) string {
	parenIdx := strings.IndexByte(line, '(')
	if parenIdx <= 0 {
		return ""
	}
	before := strings.TrimSpace(line[:parenIdx])
	parts := strings.Fields(before)
	if len(parts) < 2 {
		return ""
	}
	name := parts[len(parts)-1]
	// Remove pointer/reference markers
	name = strings.TrimLeft(name, "*&")
	if !isValidIdentifier(name) {
		return ""
	}
	// Ignore preprocessor directives, control flow, etc.
	if strings.HasPrefix(line, "#") || strings.HasPrefix(line, "if") ||
		strings.HasPrefix(line, "for") || strings.HasPrefix(line, "while") {
		return ""
	}
	return name
}

// extractKotlinFuncName extracts a Kotlin function name.
func extractKotlinFuncName(line string) string {
	stripped := line
	for _, prefix := range []string{"override ", "open ", "private ", "internal ", "protected ", "public ", "suspend ", "inline "} {
		stripped = strings.TrimPrefix(stripped, prefix)
	}
	if !strings.HasPrefix(stripped, "fun ") {
		return ""
	}
	rest := stripped[4:]
	// Handle generics: fun <T> name(
	if strings.HasPrefix(rest, "<") {
		closeIdx := strings.IndexByte(rest, '>')
		if closeIdx > 0 {
			rest = strings.TrimSpace(rest[closeIdx+1:])
		}
	}
	// Handle extension functions: Type.name(
	parenIdx := strings.IndexByte(rest, '(')
	if parenIdx <= 0 {
		return ""
	}
	nameStr := strings.TrimSpace(rest[:parenIdx])
	if dotIdx := strings.LastIndexByte(nameStr, '.'); dotIdx >= 0 {
		nameStr = nameStr[dotIdx+1:]
	}
	return nameStr
}

// extractCSharpFuncName extracts a C# method name.
func extractCSharpFuncName(line string) string {
	// Similar to Java: [modifiers] returnType Name(params)
	parenIdx := strings.IndexByte(line, '(')
	if parenIdx <= 0 {
		return ""
	}
	before := strings.TrimSpace(line[:parenIdx])
	parts := strings.Fields(before)
	if len(parts) < 2 {
		return ""
	}
	name := parts[len(parts)-1]
	controlKeywords := map[string]bool{
		"if": true, "else": true, "for": true, "foreach": true, "while": true,
		"switch": true, "catch": true, "class": true, "interface": true,
		"new": true, "return": true, "throw": true, "using": true, "namespace": true,
	}
	if controlKeywords[name] {
		return ""
	}
	if !isValidIdentifier(name) {
		return ""
	}
	return name
}

// extractSwiftFuncName extracts a Swift function name.
func extractSwiftFuncName(line string) string {
	stripped := line
	for _, prefix := range []string{"override ", "open ", "private ", "internal ", "public ", "static ", "@objc ", "mutating "} {
		stripped = strings.TrimPrefix(stripped, prefix)
	}
	if !strings.HasPrefix(stripped, "func ") {
		return ""
	}
	rest := stripped[5:]
	parenIdx := strings.IndexByte(rest, '(')
	if parenIdx <= 0 {
		return ""
	}
	// Handle generics
	angleIdx := strings.IndexByte(rest, '<')
	if angleIdx > 0 && angleIdx < parenIdx {
		return strings.TrimSpace(rest[:angleIdx])
	}
	return strings.TrimSpace(rest[:parenIdx])
}

// ddExtractPHPFuncName extracts a PHP function/method name.
func ddExtractPHPFuncName(line string) string {
	stripped := line
	for _, prefix := range []string{"public ", "private ", "protected ", "static ", "final ", "abstract "} {
		stripped = strings.TrimPrefix(stripped, prefix)
	}
	if !strings.HasPrefix(stripped, "function ") {
		return ""
	}
	rest := stripped[9:]
	parenIdx := strings.IndexByte(rest, '(')
	if parenIdx <= 0 {
		return ""
	}
	return strings.TrimSpace(rest[:parenIdx])
}

// extractDartFuncName extracts a Dart function name.
func extractDartFuncName(line string) string {
	// Dart: returnType name(params) or void name(params)
	parenIdx := strings.IndexByte(line, '(')
	if parenIdx <= 0 {
		return ""
	}
	before := strings.TrimSpace(line[:parenIdx])
	parts := strings.Fields(before)
	if len(parts) < 2 {
		return ""
	}
	name := parts[len(parts)-1]
	if !isValidIdentifier(name) {
		return ""
	}
	controlKeywords := map[string]bool{
		"if": true, "for": true, "while": true, "switch": true, "catch": true,
		"class": true, "new": true, "return": true, "import": true,
	}
	if controlKeywords[name] {
		return ""
	}
	return name
}

// extractScalaFuncName extracts a Scala function name.
func extractScalaFuncName(line string) string {
	stripped := line
	for _, prefix := range []string{"override ", "private ", "protected ", "final ", "implicit ", "lazy "} {
		stripped = strings.TrimPrefix(stripped, prefix)
	}
	if !strings.HasPrefix(stripped, "def ") {
		return ""
	}
	rest := stripped[4:]
	parenIdx := strings.IndexByte(rest, '(')
	if parenIdx <= 0 {
		// Scala allows parameterless defs
		return ""
	}
	// Handle type params
	bracketIdx := strings.IndexByte(rest, '[')
	if bracketIdx > 0 && bracketIdx < parenIdx {
		return strings.TrimSpace(rest[:bracketIdx])
	}
	return strings.TrimSpace(rest[:parenIdx])
}

// ddExtractElixirFuncName extracts an Elixir function name.
func ddExtractElixirFuncName(line string) string {
	for _, prefix := range []string{"defp ", "def "} {
		if strings.HasPrefix(line, prefix) {
			rest := line[len(prefix):]
			parenIdx := strings.IndexByte(rest, '(')
			if parenIdx <= 0 {
				// May be a no-paren function like `def name do`
				spaceIdx := strings.IndexByte(rest, ' ')
				if spaceIdx > 0 {
					return strings.TrimSpace(rest[:spaceIdx])
				}
				return ""
			}
			return strings.TrimSpace(rest[:parenIdx])
		}
	}
	return ""
}

// extractZigFuncName extracts a Zig function name.
func extractZigFuncName(line string) string {
	stripped := line
	for _, prefix := range []string{"pub ", "export "} {
		stripped = strings.TrimPrefix(stripped, prefix)
	}
	if !strings.HasPrefix(stripped, "fn ") {
		return ""
	}
	rest := stripped[3:]
	parenIdx := strings.IndexByte(rest, '(')
	if parenIdx <= 0 {
		return ""
	}
	return strings.TrimSpace(rest[:parenIdx])
}

// extractParams extracts parameter names from a function signature line.
func extractParams(line, ext string) []string {
	params := make([]string, 0)

	paramLine := line

	// For Go, skip the receiver to find the actual parameter list
	if ext == ".go" && strings.HasPrefix(strings.TrimSpace(line), "func ") {
		rest := strings.TrimSpace(line)[5:]
		if strings.HasPrefix(rest, "(") {
			// Method with receiver: func (r *Type) Name(params)
			// Skip past the receiver paren
			closeIdx := strings.Index(rest, ") ")
			if closeIdx >= 0 {
				// Now find the function name and its params
				afterReceiver := rest[closeIdx+2:]
				nameParenIdx := strings.IndexByte(afterReceiver, '(')
				if nameParenIdx >= 0 {
					paramLine = afterReceiver[nameParenIdx:]
				} else {
					return params
				}
			}
		}
	}

	parenStart := strings.IndexByte(paramLine, '(')
	if parenStart < 0 {
		return params
	}

	// Find matching close paren
	depth := 0
	parenEnd := -1
	for i := parenStart; i < len(paramLine); i++ {
		switch paramLine[i] {
		case '(':
			depth++
		case ')':
			depth--
			if depth == 0 {
				parenEnd = i
			}
		}
		if parenEnd >= 0 {
			break
		}
	}

	if parenEnd < 0 {
		return params
	}

	paramStr := paramLine[parenStart+1 : parenEnd]
	if strings.TrimSpace(paramStr) == "" {
		return params
	}

	// Split on commas (simplified — doesn't handle nested generics perfectly)
	parts := splitParams(paramStr)

	for _, part := range parts {
		part = strings.TrimSpace(part)
		if part == "" {
			continue
		}

		name := extractParamName(part, ext)
		if name != "" {
			params = append(params, name)
		}
	}

	return params
}

// splitParams splits a parameter string by commas, respecting nesting.
func splitParams(s string) []string {
	parts := make([]string, 0)
	depth := 0
	start := 0

	for i := 0; i < len(s); i++ {
		switch s[i] {
		case '(', '<', '[', '{':
			depth++
		case ')', '>', ']', '}':
			depth--
		case ',':
			if depth == 0 {
				parts = append(parts, s[start:i])
				start = i + 1
			}
		}
	}
	if start < len(s) {
		parts = append(parts, s[start:])
	}

	return parts
}

// extractParamName extracts a parameter name from a single parameter declaration.
func extractParamName(param, ext string) string {
	param = strings.TrimSpace(param)

	switch ext {
	case ".go":
		// Go: name type or name, name type
		fields := strings.Fields(param)
		if len(fields) >= 1 {
			name := fields[0]
			// Skip variadic
			name = strings.TrimPrefix(name, "...")
			if isValidIdentifier(name) {
				return name
			}
		}
	case ".py":
		// Python: name, name: type, name=default, *args, **kwargs, self
		if strings.HasPrefix(param, "**") {
			return strings.TrimPrefix(param, "**")
		}
		if strings.HasPrefix(param, "*") {
			rest := strings.TrimPrefix(param, "*")
			if rest == "" {
				return ""
			}
			return rest
		}
		// name: type = default
		colonIdx := strings.IndexByte(param, ':')
		eqIdx := strings.IndexByte(param, '=')
		if colonIdx > 0 {
			return strings.TrimSpace(param[:colonIdx])
		}
		if eqIdx > 0 {
			return strings.TrimSpace(param[:eqIdx])
		}
		return strings.TrimSpace(param)
	case ".ts", ".tsx", ".js", ".jsx", ".mjs", ".cjs":
		// TS: name: type, name?: type, name = default, ...name: type
		param = strings.TrimPrefix(param, "...")
		colonIdx := strings.IndexByte(param, ':')
		eqIdx := strings.IndexByte(param, '=')
		qIdx := strings.IndexByte(param, '?')
		endIdx := len(param)
		if colonIdx > 0 && colonIdx < endIdx {
			endIdx = colonIdx
		}
		if eqIdx > 0 && eqIdx < endIdx {
			endIdx = eqIdx
		}
		if qIdx > 0 && qIdx < endIdx {
			endIdx = qIdx
		}
		return strings.TrimSpace(param[:endIdx])
	case ".rs":
		// Rust: name: Type, mut name: Type, &self, &mut self
		if param == "self" || param == "&self" || param == "&mut self" {
			return "self"
		}
		param = strings.TrimPrefix(param, "mut ")
		colonIdx := strings.IndexByte(param, ':')
		if colonIdx > 0 {
			return strings.TrimSpace(param[:colonIdx])
		}
	case ".java", ".kt", ".kts", ".cs", ".dart", ".scala":
		// Java/Kotlin/C#/Dart: Type name or name: Type
		colonIdx := strings.IndexByte(param, ':')
		if colonIdx > 0 {
			// Kotlin/Scala style: name: Type
			return strings.TrimSpace(param[:colonIdx])
		}
		fields := strings.Fields(param)
		if len(fields) >= 2 {
			return fields[len(fields)-1]
		}
	case ".rb":
		// Ruby: name, name:, name = default, *name, **name
		param = strings.TrimPrefix(param, "**")
		param = strings.TrimPrefix(param, "*")
		param = strings.TrimSuffix(param, ":")
		eqIdx := strings.IndexByte(param, '=')
		if eqIdx > 0 {
			param = param[:eqIdx]
		}
		return strings.TrimSpace(param)
	case ".swift":
		// Swift: externalName internalName: Type
		colonIdx := strings.IndexByte(param, ':')
		if colonIdx > 0 {
			before := strings.TrimSpace(param[:colonIdx])
			fields := strings.Fields(before)
			if len(fields) >= 2 {
				return fields[1] // internal name
			}
			if len(fields) == 1 && fields[0] != "_" {
				return fields[0]
			}
		}
	case ".php":
		// PHP: Type $name or $name
		for _, field := range strings.Fields(param) {
			if strings.HasPrefix(field, "$") {
				return strings.TrimPrefix(field, "$")
			}
		}
	case ".c", ".cpp", ".h", ".hpp", ".cc":
		// C/C++: type name or type *name
		fields := strings.Fields(param)
		if len(fields) >= 2 {
			name := fields[len(fields)-1]
			name = strings.TrimLeft(name, "*&")
			if isValidIdentifier(name) {
				return name
			}
		}
	case ".ex", ".exs":
		// Elixir: name or name \\ default
		slashIdx := strings.Index(param, "\\\\")
		if slashIdx > 0 {
			return strings.TrimSpace(param[:slashIdx])
		}
		return strings.TrimSpace(param)
	case ".zig":
		// Zig: name: type
		colonIdx := strings.IndexByte(param, ':')
		if colonIdx > 0 {
			return strings.TrimSpace(param[:colonIdx])
		}
	}

	return ""
}

// extractReturnType extracts the return type from a function signature line.
func extractReturnType(line, ext string) string {
	switch ext {
	case ".go":
		// After the closing paren of params, everything before { is the return type
		parenStart := strings.IndexByte(line, '(')
		if parenStart < 0 {
			return ""
		}
		// Find closing paren of params
		depth := 0
		closeIdx := -1
		for i := parenStart; i < len(line); i++ {
			switch line[i] {
			case '(':
				depth++
			case ')':
				depth--
				if depth == 0 {
					closeIdx = i
				}
			}
			if closeIdx >= 0 {
				break
			}
		}
		if closeIdx < 0 || closeIdx >= len(line)-1 {
			return ""
		}
		rest := line[closeIdx+1:]
		if braceIdx := strings.IndexByte(rest, '{'); braceIdx >= 0 {
			rest = rest[:braceIdx]
		}
		return strings.TrimSpace(rest)

	case ".ts", ".tsx", ".js", ".jsx", ".mjs", ".cjs":
		// After ): returnType {
		lastParen := strings.LastIndexByte(line, ')')
		if lastParen < 0 {
			return ""
		}
		rest := line[lastParen+1:]
		if colonIdx := strings.IndexByte(rest, ':'); colonIdx >= 0 {
			ret := rest[colonIdx+1:]
			if braceIdx := strings.IndexByte(ret, '{'); braceIdx >= 0 {
				ret = ret[:braceIdx]
			}
			return strings.TrimSpace(ret)
		}

	case ".py":
		// -> ReturnType:
		arrowIdx := strings.Index(line, "->")
		if arrowIdx < 0 {
			return ""
		}
		ret := line[arrowIdx+2:]
		if colonIdx := strings.LastIndexByte(ret, ':'); colonIdx >= 0 {
			ret = ret[:colonIdx]
		}
		return strings.TrimSpace(ret)

	case ".rs":
		// -> ReturnType {
		arrowIdx := strings.Index(line, "->")
		if arrowIdx < 0 {
			return ""
		}
		ret := line[arrowIdx+2:]
		if braceIdx := strings.IndexByte(ret, '{'); braceIdx >= 0 {
			ret = ret[:braceIdx]
		}
		if whereIdx := strings.Index(ret, "where"); whereIdx >= 0 {
			ret = ret[:whereIdx]
		}
		return strings.TrimSpace(ret)
	}

	return ""
}

// isDocComment returns true if the line looks like a comment in the given language.
func isDocComment(line, ext string) bool {
	trimmed := strings.TrimSpace(line)
	if trimmed == "" {
		return false
	}

	switch ext {
	case ".go", ".rs", ".swift", ".kt", ".kts", ".java", ".ts", ".tsx", ".js", ".jsx",
		".mjs", ".cjs", ".c", ".cpp", ".h", ".hpp", ".cc", ".cs", ".dart", ".scala",
		".zig", ".php":
		return strings.HasPrefix(trimmed, "//") ||
			strings.HasPrefix(trimmed, "/*") ||
			strings.HasPrefix(trimmed, "*") ||
			strings.HasPrefix(trimmed, "/**") ||
			strings.HasPrefix(trimmed, "///")
	case ".py":
		return strings.HasPrefix(trimmed, "#") ||
			strings.Contains(trimmed, `"""`) ||
			strings.Contains(trimmed, `'''`)
	case ".rb", ".pl", ".r", ".jl", ".cr", ".nim", ".ex", ".exs":
		return strings.HasPrefix(trimmed, "#")
	}

	// Default: check common comment prefixes
	return strings.HasPrefix(trimmed, "//") || strings.HasPrefix(trimmed, "#") ||
		strings.HasPrefix(trimmed, "/*") || strings.HasPrefix(trimmed, "*")
}

// containsWord checks if text contains a word as a standalone token (not as
// part of another word). Uses word boundary matching.
func containsWord(text, word string) bool {
	if word == "" {
		return false
	}
	pattern := `\b` + regexp.QuoteMeta(word) + `\b`
	matched, _ := regexp.MatchString(pattern, text)
	return matched
}

// formatParamList formats a list of param names for display.
func formatParamList(params []string) string {
	quoted := make([]string, len(params))
	for i, p := range params {
		quoted[i] = fmt.Sprintf("%q", p)
	}
	return strings.Join(quoted, ", ")
}

// isIdentStart checks if a byte is a valid identifier start character.
func isIdentStart(b byte) bool {
	return (b >= 'a' && b <= 'z') || (b >= 'A' && b <= 'Z') || b == '_' || b == '$'
}

// isValidIdentifier checks if a string is a valid identifier.
func isValidIdentifier(s string) bool {
	if s == "" {
		return false
	}
	if !isIdentStart(s[0]) {
		return false
	}
	for i := 1; i < len(s); i++ {
		ch := s[i]
		if !((ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z') ||
			(ch >= '0' && ch <= '9') || ch == '_' || ch == '$') {
			return false
		}
	}
	return true
}

// isUpperStart checks if a string starts with an uppercase letter.
func isUpperStart(s string) bool {
	return len(s) > 0 && s[0] >= 'A' && s[0] <= 'Z'
}
