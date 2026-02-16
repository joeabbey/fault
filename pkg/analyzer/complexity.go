package analyzer

import (
	"fmt"
	"path/filepath"
	"strings"

	"github.com/joeabbey/fault/pkg/git"
	"github.com/joeabbey/fault/pkg/parser"
)

// ComplexityAnalyzer detects overly complex code that AI agents tend to generate:
// long functions, too many parameters, deep nesting, high cyclomatic complexity,
// and excessive return statements.
type ComplexityAnalyzer struct{}

// NewComplexityAnalyzer creates a new complexity analyzer.
func NewComplexityAnalyzer() *ComplexityAnalyzer {
	return &ComplexityAnalyzer{}
}

// Name returns the analyzer name.
func (a *ComplexityAnalyzer) Name() string {
	return "complexity"
}

// Thresholds for complexity checks.
const (
	funcLenWarn  = 80
	funcLenError = 150

	paramWarn  = 5
	paramError = 8

	nestingWarn  = 4
	nestingError = 6

	cyclomaticWarn  = 10
	cyclomaticError = 20

	returnWarn = 8
)

// Analyze runs all complexity checks on changed files.
func (a *ComplexityAnalyzer) Analyze(ctx *AnalysisContext) ([]Issue, error) {
	issues := make([]Issue, 0)

	if ctx.Diff == nil {
		return issues, nil
	}

	// Build a lookup from file path to FileDiff for overlap checks.
	diffByPath := make(map[string]git.FileDiff, len(ctx.Diff.Files))
	for _, fd := range ctx.Diff.Files {
		diffByPath[fd.Path] = fd
	}

	// Phase 1: ParsedFile analysis (function length, parameter count).
	for path, pf := range ctx.ParsedFiles {
		if ctx.Config != nil && ctx.Config.IsIgnored(path) {
			continue
		}
		if isTestFile(path) {
			continue
		}
		if isVendorOrNodeModules(path) {
			continue
		}

		fd, inDiff := diffByPath[path]
		if !inDiff {
			continue
		}

		for _, sym := range pf.Symbols {
			if sym.Kind != "function" && sym.Kind != "method" {
				continue
			}
			if !symbolOverlapsDiff(sym, fd) {
				continue
			}

			issues = append(issues, a.checkFunctionLength(path, sym)...)
			issues = append(issues, a.checkParameterCount(path, sym)...)
		}
	}

	// Phase 2: Diff-based analysis (nesting depth, cyclomatic complexity, return count).
	for _, fd := range ctx.Diff.Files {
		if fd.Status == "deleted" {
			continue
		}
		if ctx.Config != nil && ctx.Config.IsIgnored(fd.Path) {
			continue
		}
		if isTestFile(fd.Path) {
			continue
		}
		if isVendorOrNodeModules(fd.Path) {
			continue
		}

		lang := languageFromPath(fd.Path)
		issues = append(issues, a.checkNestingDepth(fd, lang)...)
		issues = append(issues, a.checkCyclomaticComplexity(fd, lang)...)
		issues = append(issues, a.checkExcessiveReturns(fd, lang)...)
	}

	return issues, nil
}

// checkFunctionLength flags functions that are too long.
func (a *ComplexityAnalyzer) checkFunctionLength(path string, sym parser.Symbol) []Issue {
	issues := make([]Issue, 0)

	if sym.EndLine <= 0 || sym.Line <= 0 {
		return issues
	}

	length := sym.EndLine - sym.Line
	if length > funcLenError {
		issues = append(issues, Issue{
			ID:         fmt.Sprintf("complexity-func-length-%s-%d", path, sym.Line),
			FixID:      "complexity-function-too-long",
			Severity:   SeverityError,
			Category:   "complexity",
			File:       path,
			Line:       sym.Line,
			EndLine:    sym.EndLine,
			Message:    fmt.Sprintf("Function %q is %d lines long (max recommended: %d)", sym.Name, length, funcLenError),
			Suggestion: "Split this function into smaller, focused helper functions",
		})
	} else if length > funcLenWarn {
		issues = append(issues, Issue{
			ID:         fmt.Sprintf("complexity-func-length-%s-%d", path, sym.Line),
			FixID:      "complexity-function-too-long",
			Severity:   SeverityWarning,
			Category:   "complexity",
			File:       path,
			Line:       sym.Line,
			EndLine:    sym.EndLine,
			Message:    fmt.Sprintf("Function %q is %d lines long (consider splitting at %d+)", sym.Name, length, funcLenWarn),
			Suggestion: "Consider breaking this function into smaller pieces for readability",
		})
	}

	return issues
}

// checkParameterCount flags functions with too many parameters.
func (a *ComplexityAnalyzer) checkParameterCount(path string, sym parser.Symbol) []Issue {
	issues := make([]Issue, 0)

	count := countParameters(sym.Signature)
	if count <= 0 {
		return issues
	}

	if count > paramError {
		issues = append(issues, Issue{
			ID:         fmt.Sprintf("complexity-params-%s-%d", path, sym.Line),
			FixID:      "complexity-too-many-params",
			Severity:   SeverityError,
			Category:   "complexity",
			File:       path,
			Line:       sym.Line,
			Message:    fmt.Sprintf("Function %q has %d parameters (max recommended: %d)", sym.Name, count, paramError),
			Suggestion: "Consider using an options struct/object to reduce parameter count",
		})
	} else if count > paramWarn {
		issues = append(issues, Issue{
			ID:         fmt.Sprintf("complexity-params-%s-%d", path, sym.Line),
			FixID:      "complexity-too-many-params",
			Severity:   SeverityWarning,
			Category:   "complexity",
			File:       path,
			Line:       sym.Line,
			Message:    fmt.Sprintf("Function %q has %d parameters (consider reducing at %d+)", sym.Name, count, paramWarn),
			Suggestion: "Consider using an options struct/object to reduce parameter count",
		})
	}

	return issues
}

// checkNestingDepth scans added diff lines for deep nesting.
func (a *ComplexityAnalyzer) checkNestingDepth(fd git.FileDiff, lang string) []Issue {
	issues := make([]Issue, 0)

	if isBraceLanguage(lang) {
		issues = append(issues, a.checkBraceNesting(fd)...)
	} else if lang == "python" {
		issues = append(issues, a.checkIndentNesting(fd)...)
	}

	return issues
}

// checkBraceNesting tracks brace depth in added lines for C-family languages.
func (a *ComplexityAnalyzer) checkBraceNesting(fd git.FileDiff) []Issue {
	issues := make([]Issue, 0)
	reported := make(map[int]bool) // avoid duplicate reports per line

	for _, hunk := range fd.Hunks {
		depth := 0
		for _, line := range hunk.Lines {
			if line.Type != "added" {
				continue
			}
			content := line.Content

			// Track brace depth changes.
			for _, ch := range content {
				switch ch {
				case '{':
					depth++
				case '}':
					if depth > 0 {
						depth--
					}
				}
			}

			if reported[line.NewNum] {
				continue
			}

			if depth > nestingError {
				issues = append(issues, Issue{
					ID:         fmt.Sprintf("complexity-nesting-%s-%d", fd.Path, line.NewNum),
					FixID:      "complexity-deep-nesting",
					Severity:   SeverityError,
					Category:   "complexity",
					File:       fd.Path,
					Line:       line.NewNum,
					Message:    fmt.Sprintf("Code is nested %d levels deep (max recommended: %d)", depth, nestingError),
					Suggestion: "Extract inner logic into helper functions or use early returns to reduce nesting",
				})
				reported[line.NewNum] = true
			} else if depth > nestingWarn {
				issues = append(issues, Issue{
					ID:         fmt.Sprintf("complexity-nesting-%s-%d", fd.Path, line.NewNum),
					FixID:      "complexity-deep-nesting",
					Severity:   SeverityWarning,
					Category:   "complexity",
					File:       fd.Path,
					Line:       line.NewNum,
					Message:    fmt.Sprintf("Code is nested %d levels deep (consider flattening at %d+)", depth, nestingWarn),
					Suggestion: "Consider using early returns, guard clauses, or extracting helper functions",
				})
				reported[line.NewNum] = true
			}
		}
	}

	return issues
}

// checkIndentNesting tracks indentation-based nesting for Python.
func (a *ComplexityAnalyzer) checkIndentNesting(fd git.FileDiff) []Issue {
	issues := make([]Issue, 0)

	for _, hunk := range fd.Hunks {
		for _, line := range hunk.Lines {
			if line.Type != "added" {
				continue
			}
			content := line.Content
			trimmed := strings.TrimLeft(content, " \t")
			if trimmed == "" {
				continue
			}

			// Count indentation: each 4 spaces or 1 tab = 1 level.
			indent := len(content) - len(trimmed)
			depth := 0
			for i := 0; i < len(content) && i < indent; i++ {
				if content[i] == '\t' {
					depth++
				}
			}
			if depth == 0 {
				depth = indent / 4
			}

			if depth > nestingError {
				issues = append(issues, Issue{
					ID:         fmt.Sprintf("complexity-nesting-%s-%d", fd.Path, line.NewNum),
					FixID:      "complexity-deep-nesting",
					Severity:   SeverityError,
					Category:   "complexity",
					File:       fd.Path,
					Line:       line.NewNum,
					Message:    fmt.Sprintf("Code is nested %d levels deep (max recommended: %d)", depth, nestingError),
					Suggestion: "Extract inner logic into helper functions or use early returns to reduce nesting",
				})
			} else if depth > nestingWarn {
				issues = append(issues, Issue{
					ID:         fmt.Sprintf("complexity-nesting-%s-%d", fd.Path, line.NewNum),
					FixID:      "complexity-deep-nesting",
					Severity:   SeverityWarning,
					Category:   "complexity",
					File:       fd.Path,
					Line:       line.NewNum,
					Message:    fmt.Sprintf("Code is nested %d levels deep (consider flattening at %d+)", depth, nestingWarn),
					Suggestion: "Consider using early returns, guard clauses, or extracting helper functions",
				})
			}
		}
	}

	return issues
}

// checkCyclomaticComplexity counts decision points in added diff lines
// within function boundaries.
func (a *ComplexityAnalyzer) checkCyclomaticComplexity(fd git.FileDiff, lang string) []Issue {
	issues := make([]Issue, 0)

	// Collect all added lines grouped by approximate function boundaries.
	// We use a simple approach: track function starts/ends via braces or indentation,
	// and count decision points within each function region.
	type funcRegion struct {
		name    string
		line    int
		endLine int
		count   int
	}

	// Simple approach: count decision points across all added lines per hunk.
	// For each hunk, we track brace-delimited function regions.
	decisionKeywords := decisionKeywordsForLang(lang)
	if len(decisionKeywords) == 0 {
		return issues
	}

	for _, hunk := range fd.Hunks {
		complexity := 0
		funcStartLine := 0
		funcName := ""

		for _, line := range hunk.Lines {
			if line.Type != "added" {
				continue
			}
			content := strings.TrimSpace(line.Content)
			if content == "" {
				continue
			}

			// Detect function start.
			if name := detectFuncStart(content, lang); name != "" {
				if funcStartLine > 0 && complexity > 0 {
					// Emit issue for previous function.
					issues = append(issues, a.emitCyclomaticIssue(fd.Path, funcName, funcStartLine, complexity)...)
				}
				funcStartLine = line.NewNum
				funcName = name
				complexity = 1 // base complexity
				continue
			}

			if funcStartLine == 0 {
				continue
			}

			// Count decision points.
			for _, kw := range decisionKeywords {
				complexity += countKeywordOccurrences(content, kw)
			}
		}

		// Emit for the last function in this hunk.
		if funcStartLine > 0 && complexity > 0 {
			issues = append(issues, a.emitCyclomaticIssue(fd.Path, funcName, funcStartLine, complexity)...)
		}
	}

	return issues
}

func (a *ComplexityAnalyzer) emitCyclomaticIssue(path, funcName string, line, complexity int) []Issue {
	issues := make([]Issue, 0)

	if funcName == "" {
		funcName = "<anonymous>"
	}

	if complexity > cyclomaticError {
		issues = append(issues, Issue{
			ID:         fmt.Sprintf("complexity-cyclomatic-%s-%d", path, line),
			FixID:      "complexity-high-cyclomatic",
			Severity:   SeverityError,
			Category:   "complexity",
			File:       path,
			Line:       line,
			Message:    fmt.Sprintf("Function %q has cyclomatic complexity %d (max recommended: %d)", funcName, complexity, cyclomaticError),
			Suggestion: "Decompose this function into smaller functions with clearer control flow",
		})
	} else if complexity > cyclomaticWarn {
		issues = append(issues, Issue{
			ID:         fmt.Sprintf("complexity-cyclomatic-%s-%d", path, line),
			FixID:      "complexity-high-cyclomatic",
			Severity:   SeverityWarning,
			Category:   "complexity",
			File:       path,
			Line:       line,
			Message:    fmt.Sprintf("Function %q has cyclomatic complexity %d (consider simplifying at %d+)", funcName, complexity, cyclomaticWarn),
			Suggestion: "Consider reducing branching by extracting helper functions or using lookup tables",
		})
	}

	return issues
}

// checkExcessiveReturns counts return statements in function regions of added lines.
func (a *ComplexityAnalyzer) checkExcessiveReturns(fd git.FileDiff, lang string) []Issue {
	issues := make([]Issue, 0)

	returnKw := returnKeywordForLang(lang)
	if returnKw == "" {
		return issues
	}

	for _, hunk := range fd.Hunks {
		funcStartLine := 0
		funcName := ""
		returnCount := 0

		for _, line := range hunk.Lines {
			if line.Type != "added" {
				continue
			}
			content := strings.TrimSpace(line.Content)
			if content == "" {
				continue
			}

			if name := detectFuncStart(content, lang); name != "" {
				// Emit for previous function.
				if funcStartLine > 0 {
					issues = append(issues, a.emitReturnIssue(fd.Path, funcName, funcStartLine, returnCount)...)
				}
				funcStartLine = line.NewNum
				funcName = name
				returnCount = 0
				continue
			}

			if funcStartLine == 0 {
				continue
			}

			if countKeywordOccurrences(content, returnKw) > 0 {
				returnCount++
			}
		}

		if funcStartLine > 0 {
			issues = append(issues, a.emitReturnIssue(fd.Path, funcName, funcStartLine, returnCount)...)
		}
	}

	return issues
}

func (a *ComplexityAnalyzer) emitReturnIssue(path, funcName string, line, count int) []Issue {
	issues := make([]Issue, 0)

	if funcName == "" {
		funcName = "<anonymous>"
	}

	if count > returnWarn {
		issues = append(issues, Issue{
			ID:         fmt.Sprintf("complexity-returns-%s-%d", path, line),
			FixID:      "complexity-excessive-returns",
			Severity:   SeverityWarning,
			Category:   "complexity",
			File:       path,
			Line:       line,
			Message:    fmt.Sprintf("Function %q has %d return statements (consider reducing at %d+)", funcName, count, returnWarn),
			Suggestion: "Consider decomposing this function — many return paths make logic hard to follow",
		})
	}

	return issues
}

// --- Helper functions ---

// symbolOverlapsDiff checks if a parsed symbol overlaps with any hunk in the file diff.
func symbolOverlapsDiff(sym parser.Symbol, fileDiff git.FileDiff) bool {
	for _, hunk := range fileDiff.Hunks {
		hunkStart := hunk.NewStart
		hunkEnd := hunk.NewStart + hunk.NewCount
		if sym.Line <= hunkEnd && sym.EndLine >= hunkStart {
			return true
		}
	}
	return false
}

// countParameters extracts parameter count from a signature string like
// "func Name(a int, b string, c bool)" or "function(x, y, z)".
func countParameters(sig string) int {
	if sig == "" {
		return 0
	}

	// Find the first parenthesized parameter list.
	start := strings.Index(sig, "(")
	if start == -1 {
		return 0
	}

	// For method signatures like "func (*T) Name(params)", skip the receiver.
	rest := sig[start+1:]
	end := strings.Index(rest, ")")
	if end == -1 {
		return 0
	}
	params := rest[:end]

	// Check if this looks like a receiver (Go methods): single type, no commas,
	// and there's another parenthesized group after it.
	afterClose := rest[end+1:]
	if nextOpen := strings.Index(afterClose, "("); nextOpen != -1 {
		// This was a receiver; use the next parenthesized group instead.
		afterOpen := afterClose[nextOpen+1:]
		nextClose := strings.Index(afterOpen, ")")
		if nextClose != -1 {
			params = afterOpen[:nextClose]
		}
	}

	params = strings.TrimSpace(params)
	if params == "" {
		return 0
	}

	// Count comma-separated parameters, respecting nested generics/parens.
	count := 1
	depth := 0
	for _, ch := range params {
		switch ch {
		case '(', '<', '[':
			depth++
		case ')', '>', ']':
			depth--
		case ',':
			if depth == 0 {
				count++
			}
		}
	}

	return count
}

// isVendorOrNodeModules returns true if the path is inside vendor/ or node_modules/.
func isVendorOrNodeModules(path string) bool {
	parts := strings.Split(filepath.ToSlash(path), "/")
	for _, part := range parts {
		if part == "vendor" || part == "node_modules" {
			return true
		}
	}
	return false
}

// isBraceLanguage returns true if the language uses braces for block scoping.
func isBraceLanguage(lang string) bool {
	switch lang {
	case "go", "java", "typescript", "rust", "c", "cpp", "csharp", "php",
		"swift", "kotlin", "dart", "scala", "groovy", "zig", "crystal", "dlang",
		"javascript":
		return true
	}
	return false
}

// languageFromPath infers language from file extension.
func languageFromPath(path string) string {
	ext := strings.ToLower(filepath.Ext(path))
	switch ext {
	case ".go":
		return "go"
	case ".ts", ".tsx":
		return "typescript"
	case ".js", ".jsx", ".mjs", ".cjs":
		return "javascript"
	case ".py":
		return "python"
	case ".rs":
		return "rust"
	case ".java":
		return "java"
	case ".rb":
		return "ruby"
	case ".kt", ".kts":
		return "kotlin"
	case ".cs":
		return "csharp"
	case ".php":
		return "php"
	case ".swift":
		return "swift"
	case ".c", ".h":
		return "c"
	case ".cpp", ".cc", ".cxx", ".hpp":
		return "cpp"
	case ".dart":
		return "dart"
	case ".scala":
		return "scala"
	case ".groovy":
		return "groovy"
	case ".zig":
		return "zig"
	case ".cr":
		return "crystal"
	case ".d":
		return "dlang"
	case ".ex", ".exs":
		return "elixir"
	default:
		return ""
	}
}

// decisionKeywordsForLang returns keywords that represent decision points for cyclomatic complexity.
func decisionKeywordsForLang(lang string) []string {
	switch lang {
	case "go":
		return []string{"if ", "else if ", "case ", "for ", "&&", "||"}
	case "typescript", "javascript":
		return []string{"if ", "else if ", "case ", "for ", "while ", "catch ", "&&", "||", "?"}
	case "python":
		return []string{"if ", "elif ", "for ", "while ", "except ", "and ", "or "}
	case "rust":
		return []string{"if ", "else if ", "match ", "for ", "while ", "&&", "||", "?"}
	case "java", "kotlin", "csharp", "dart", "scala", "groovy":
		return []string{"if ", "else if ", "case ", "for ", "while ", "catch ", "&&", "||", "?"}
	case "ruby":
		return []string{"if ", "elsif ", "when ", "unless ", "while ", "until ", "rescue ", "&&", "||"}
	case "elixir":
		return []string{"if ", "cond ", "case ", "with ", "rescue ", "and ", "or "}
	case "c", "cpp":
		return []string{"if ", "else if ", "case ", "for ", "while ", "catch ", "&&", "||", "?"}
	case "php":
		return []string{"if ", "elseif ", "case ", "for ", "while ", "foreach ", "catch ", "&&", "||", "?"}
	case "swift":
		return []string{"if ", "else if ", "case ", "for ", "while ", "catch ", "&&", "||", "?"}
	default:
		return nil
	}
}

// returnKeywordForLang returns the return keyword for the language.
func returnKeywordForLang(lang string) string {
	switch lang {
	case "go", "typescript", "javascript", "rust", "java", "kotlin", "csharp",
		"dart", "scala", "groovy", "c", "cpp", "php", "swift", "ruby", "elixir":
		return "return"
	case "python":
		return "return"
	default:
		return ""
	}
}

// detectFuncStart checks if a line looks like the start of a function definition
// and returns the function name (or empty string if not a function start).
func detectFuncStart(line, lang string) string {
	switch lang {
	case "go":
		if strings.HasPrefix(line, "func ") {
			return extractGoFuncName(line)
		}
	case "typescript", "javascript":
		return extractJSFuncName(line)
	case "python":
		if strings.HasPrefix(line, "def ") || strings.HasPrefix(line, "async def ") {
			return extractPyFuncName(line)
		}
	case "rust":
		if strings.HasPrefix(line, "fn ") || strings.HasPrefix(line, "pub fn ") ||
			strings.HasPrefix(line, "pub(crate) fn ") || strings.HasPrefix(line, "async fn ") ||
			strings.HasPrefix(line, "pub async fn ") {
			return extractRustFuncName(line)
		}
	case "java", "kotlin", "csharp", "dart", "scala", "groovy", "swift":
		return extractCFamilyFuncName(line)
	case "c", "cpp":
		return extractCFamilyFuncName(line)
	case "php":
		if strings.Contains(line, "function ") {
			return extractPHPFuncName(line)
		}
	case "ruby":
		if strings.HasPrefix(line, "def ") {
			return extractRubyFuncName(line)
		}
	case "elixir":
		if strings.HasPrefix(line, "def ") || strings.HasPrefix(line, "defp ") {
			return extractElixirFuncName(line)
		}
	}
	return ""
}

func extractGoFuncName(line string) string {
	rest := strings.TrimPrefix(line, "func ")
	// Skip receiver: func (r *T) Name(...)
	if strings.HasPrefix(rest, "(") {
		idx := strings.Index(rest, ") ")
		if idx == -1 {
			return ""
		}
		rest = rest[idx+2:]
	}
	return extractFirstIdentifier(rest)
}

func extractJSFuncName(line string) string {
	// function name(...) or async function name(...)
	if strings.HasPrefix(line, "function ") {
		rest := strings.TrimPrefix(line, "function ")
		return extractFirstIdentifier(rest)
	}
	if strings.HasPrefix(line, "async function ") {
		rest := strings.TrimPrefix(line, "async function ")
		return extractFirstIdentifier(rest)
	}
	// export function name(...)
	if strings.HasPrefix(line, "export function ") {
		rest := strings.TrimPrefix(line, "export function ")
		return extractFirstIdentifier(rest)
	}
	if strings.HasPrefix(line, "export async function ") {
		rest := strings.TrimPrefix(line, "export async function ")
		return extractFirstIdentifier(rest)
	}
	return ""
}

func extractPyFuncName(line string) string {
	rest := line
	if strings.HasPrefix(rest, "async ") {
		rest = rest[6:]
	}
	rest = strings.TrimPrefix(rest, "def ")
	if idx := strings.IndexByte(rest, '('); idx > 0 {
		return strings.TrimSpace(rest[:idx])
	}
	return extractFirstIdentifier(rest)
}

func extractRustFuncName(line string) string {
	idx := strings.Index(line, "fn ")
	if idx == -1 {
		return ""
	}
	rest := line[idx+3:]
	return extractFirstIdentifier(rest)
}

func extractCFamilyFuncName(line string) string {
	// Look for pattern: ... name(
	parenIdx := strings.IndexByte(line, '(')
	if parenIdx <= 0 {
		return ""
	}
	before := strings.TrimSpace(line[:parenIdx])
	// Extract last word before the paren.
	parts := strings.Fields(before)
	if len(parts) == 0 {
		return ""
	}
	candidate := parts[len(parts)-1]
	// Strip pointer/reference markers.
	candidate = strings.TrimLeft(candidate, "*&")
	if candidate == "" || candidate == "if" || candidate == "for" || candidate == "while" ||
		candidate == "switch" || candidate == "catch" || candidate == "return" {
		return ""
	}
	return candidate
}

func extractPHPFuncName(line string) string {
	idx := strings.Index(line, "function ")
	if idx == -1 {
		return ""
	}
	rest := line[idx+9:]
	return extractFirstIdentifier(rest)
}

func extractRubyFuncName(line string) string {
	rest := strings.TrimPrefix(line, "def ")
	if idx := strings.IndexAny(rest, "(; "); idx > 0 {
		return strings.TrimSpace(rest[:idx])
	}
	return extractFirstIdentifier(rest)
}

func extractElixirFuncName(line string) string {
	rest := line
	if strings.HasPrefix(rest, "defp ") {
		rest = rest[5:]
	} else {
		rest = strings.TrimPrefix(rest, "def ")
	}
	if idx := strings.IndexAny(rest, "(, "); idx > 0 {
		return strings.TrimSpace(rest[:idx])
	}
	return extractFirstIdentifier(rest)
}

// countKeywordOccurrences counts non-overlapping occurrences of a keyword in a line.
// For word-boundary keywords (ending with space), it checks that the character before
// the keyword is also a word boundary. For operator keywords (&&, ||, ?), it counts
// raw occurrences.
// Special case: "if " does not match when preceded by "else " to avoid double-counting
// "else if" branches.
func countKeywordOccurrences(line, keyword string) int {
	count := 0
	offset := 0

	for offset < len(line) {
		search := line[offset:]
		idx := strings.Index(search, keyword)
		if idx == -1 {
			break
		}

		absIdx := offset + idx

		// For word-like keywords, ensure we're at a word boundary.
		valid := true
		if strings.HasSuffix(keyword, " ") || isAlpha(keyword[0]) {
			if absIdx > 0 {
				prev := line[absIdx-1]
				if isAlpha(prev) || prev == '_' {
					valid = false
				}
			}
		}

		// Avoid double-counting "else if" as both "else if " and "if ".
		if valid && keyword == "if " && absIdx >= 5 && line[absIdx-5:absIdx] == "else " {
			valid = false
		}

		if valid {
			count++
		}
		offset = absIdx + len(keyword)
	}

	return count
}

func isAlpha(b byte) bool {
	return (b >= 'a' && b <= 'z') || (b >= 'A' && b <= 'Z')
}
