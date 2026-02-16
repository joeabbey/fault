package analyzer

import (
	"path/filepath"
	"regexp"
	"strings"

	"github.com/joeabbey/fault/pkg/git"
)

// AntiPatternAnalyzer detects common anti-patterns in added code.
type AntiPatternAnalyzer struct{}

// NewAntiPatternAnalyzer creates a new anti-pattern analyzer.
func NewAntiPatternAnalyzer() *AntiPatternAnalyzer {
	return &AntiPatternAnalyzer{}
}

// Name returns the analyzer name.
func (a *AntiPatternAnalyzer) Name() string {
	return "patterns"
}

// Analyze scans diff content for anti-patterns in added lines.
func (a *AntiPatternAnalyzer) Analyze(ctx *AnalysisContext) ([]Issue, error) {
	issues := make([]Issue, 0)

	if ctx.Diff == nil || len(ctx.Diff.Files) == 0 {
		return issues, nil
	}

	for _, fileDiff := range ctx.Diff.Files {
		if fileDiff.Status == "deleted" || fileDiff.IsBinary {
			continue
		}

		// Skip non-source files (docs, config, data files).
		if !isAnalyzableFile(fileDiff.Path) {
			continue
		}

		testFile := isTestFile(fileDiff.Path)

		// Credentials are checked in all source files including tests,
		// but test files get extra exclusions applied.
		issues = append(issues, checkHardcodedCredentials(fileDiff, testFile)...)

		// Skip remaining pattern checks for test files — test data
		// legitimately contains TODOs, console.log strings, etc.
		if testFile {
			continue
		}

		issues = append(issues, checkTODOPlaceholders(fileDiff)...)
		issues = append(issues, checkConsoleDebug(fileDiff)...)
		issues = append(issues, checkCommentedOutCode(fileDiff)...)
		issues = append(issues, checkUnreachableCode(fileDiff)...)
		issues = append(issues, checkRustAntiPatterns(fileDiff)...)
		issues = append(issues, checkTypeScriptPatterns(fileDiff)...)
		issues = append(issues, checkSwiftAntiPatterns(fileDiff)...)

		// Java-specific patterns
		if isJavaFile(fileDiff.Path) {
			issues = append(issues, checkJavaEmptyCatch(fileDiff)...)
			issues = append(issues, checkJavaStringEquals(fileDiff)...)
		}

		// C#-specific patterns
		if isCSharpFile(fileDiff.Path) {
			issues = append(issues, checkCSharpPatterns(fileDiff)...)
		}

		// PHP-specific patterns
		if isPHPFile(fileDiff.Path) {
			issues = append(issues, checkPHPPatterns(fileDiff)...)
		}

		// C/C++-specific patterns
		if isCOrCppFile(fileDiff.Path) {
			issues = append(issues, checkCAntiPatterns(fileDiff)...)
		}

		// Bash-specific patterns
		if isBashFile(fileDiff.Path) {
			issues = append(issues, checkBashAntiPatterns(fileDiff)...)
		}

		// Perl-specific patterns
		if isPerlFile(fileDiff.Path) {
			issues = append(issues, checkPerlPatterns(fileDiff)...)
		}

		// PowerShell-specific patterns
		if isPowershellFile(fileDiff.Path) {
			issues = append(issues, checkPowershellPatterns(fileDiff)...)
		}

		// Groovy-specific patterns
		if isGroovyFile(fileDiff.Path) {
			issues = append(issues, checkGroovyPatterns(fileDiff)...)
		}

		// Modern systems languages
		if isZigFile(fileDiff.Path) {
			issues = append(issues, checkZigAntiPatterns(fileDiff)...)
		}
		if isNimFile(fileDiff.Path) {
			issues = append(issues, checkNimAntiPatterns(fileDiff)...)
		}
		if isCrystalFile(fileDiff.Path) {
			issues = append(issues, checkCrystalAntiPatterns(fileDiff)...)
		}
		if isVlangFile(fileDiff.Path) {
			issues = append(issues, checkVlangAntiPatterns(fileDiff)...)
		}
		if isDlangFile(fileDiff.Path) {
			issues = append(issues, checkDlangAntiPatterns(fileDiff)...)
		}

		// Functional languages
		if isHaskellFile(fileDiff.Path) {
			issues = append(issues, checkHaskellPatterns(fileDiff)...)
		}
		if isClojureFile(fileDiff.Path) {
			issues = append(issues, checkClojurePatterns(fileDiff)...)
		}
		if isErlangFile(fileDiff.Path) {
			issues = append(issues, checkErlangPatterns(fileDiff)...)
		}
		if isFsharpFile(fileDiff.Path) {
			issues = append(issues, checkFsharpPatterns(fileDiff)...)
		}
		if isOcamlFile(fileDiff.Path) {
			issues = append(issues, checkOcamlPatterns(fileDiff)...)
		}

		// Domain languages
		if isJuliaFile(fileDiff.Path) {
			issues = append(issues, checkJuliaPatterns(fileDiff)...)
		}
		if isFortranFile(fileDiff.Path) {
			issues = append(issues, checkFortranPatterns(fileDiff)...)
		}
		if isSolidityFile(fileDiff.Path) {
			issues = append(issues, checkSolidityPatterns(fileDiff)...)
		}
		if isTerraformFile(fileDiff.Path) {
			issues = append(issues, checkTerraformPatterns(fileDiff)...)
		}
		if isProtobufFile(fileDiff.Path) {
			issues = append(issues, checkProtobufPatterns(fileDiff)...)
		}

		// Legacy languages
		if isVisualBasicFile(fileDiff.Path) {
			issues = append(issues, checkVisualBasicPatterns(fileDiff)...)
		}
		if isCobolFile(fileDiff.Path) {
			issues = append(issues, checkCobolPatterns(fileDiff)...)
		}
		if isAdaFile(fileDiff.Path) {
			issues = append(issues, checkAdaPatterns(fileDiff)...)
		}
		if isPascalFile(fileDiff.Path) {
			issues = append(issues, checkPascalPatterns(fileDiff)...)
		}
	}

	return issues, nil
}

// --- TODO / Placeholder detection ---

var todoPatterns = []*regexp.Regexp{
	regexp.MustCompile(`(?i)//\s*TODO\b`),
	regexp.MustCompile(`(?i)#\s*TODO\b`),
	regexp.MustCompile(`(?i)raise\s+NotImplementedError`),
	regexp.MustCompile(`(?i)throw\s+new\s+Error\(\s*['"]not\s+implemented`),
	regexp.MustCompile(`panic\(\s*"not implemented"`),
	regexp.MustCompile(`\btodo!\(`),
	regexp.MustCompile(`\bunimplemented!\(`),
}

// standalonePythonPass matches "pass" as a standalone statement (the only non-whitespace on the line).
var standalonePythonPass = regexp.MustCompile(`^\s*pass\s*$`)

func checkTODOPlaceholders(fileDiff git.FileDiff) []Issue {
	issues := make([]Issue, 0)

	for _, hunk := range fileDiff.Hunks {
		for _, line := range hunk.Lines {
			if line.Type != "added" {
				continue
			}

			content := line.Content

			for _, pat := range todoPatterns {
				if pat.MatchString(content) {
					issues = append(issues, Issue{
						ID:         "patterns/todo-placeholder",
						Severity:   SeverityWarning,
						Category:   "patterns",
						File:       fileDiff.Path,
						Line:       line.NewNum,
						Message:    "TODO or placeholder left in code: " + strings.TrimSpace(content),
						Suggestion: "Implement the functionality or remove the placeholder before committing",
					})
					break // Only report once per line even if multiple patterns match.
				}
			}

			// Check for standalone Python pass (only for .py files).
			if isPythonFile(fileDiff.Path) && standalonePythonPass.MatchString(content) {
				issues = append(issues, Issue{
					ID:         "patterns/todo-placeholder",
					Severity:   SeverityWarning,
					Category:   "patterns",
					File:       fileDiff.Path,
					Line:       line.NewNum,
					Message:    "Standalone 'pass' statement may be a placeholder",
					Suggestion: "Implement the function body or add a comment explaining why it's empty",
				})
			}
		}
	}

	return issues
}

// --- Hardcoded credential detection ---

var credentialPatterns = []*regexp.Regexp{
	regexp.MustCompile(`(?i)["']sk-[a-zA-Z0-9]{20,}["']`),
	regexp.MustCompile(`(?i)["']AKIA[A-Z0-9]{16}["']`),
	regexp.MustCompile(`(?i)["']ghp_[a-zA-Z0-9]{36,}["']`),
	regexp.MustCompile(`(?i)password\s*=\s*"[^"]+"`),
	regexp.MustCompile(`(?i)secret\s*=\s*"[^"]+"`),
}

// credentialExclusions filters out common false positives.
var credentialExclusions = []*regexp.Regexp{
	regexp.MustCompile(`(?i)password\s*=\s*""\s*$`),                  // empty string
	regexp.MustCompile(`(?i)secret\s*=\s*""\s*$`),                    // empty string
	regexp.MustCompile(`(?i)os\.Getenv\(.*password`),                 // env var lookup
	regexp.MustCompile(`(?i)os\.Getenv\(.*secret`),                   // env var lookup
	regexp.MustCompile(`(?i)password\s*=\s*"\$\{`),                   // template variable
	regexp.MustCompile(`(?i)secret\s*=\s*"\$\{`),                     // template variable
	regexp.MustCompile(`(?i)password\s*=\s*"<[^>]*>"`),               // placeholder like <password>
	regexp.MustCompile(`(?i)secret\s*=\s*"<[^>]*>"`),                 // placeholder like <secret>
	regexp.MustCompile(`(?i)password\s*=\s*"(test|example|dummy)"`),   // obvious test values
}

func checkHardcodedCredentials(fileDiff git.FileDiff, testFile bool) []Issue {
	// Don't flag credentials in test files — they contain fake test data.
	if testFile {
		return make([]Issue, 0)
	}

	issues := make([]Issue, 0)

	for _, hunk := range fileDiff.Hunks {
		for _, line := range hunk.Lines {
			if line.Type != "added" {
				continue
			}

			content := line.Content

			for _, pat := range credentialPatterns {
				if pat.MatchString(content) {
					// Check exclusions.
					excluded := false
					for _, excl := range credentialExclusions {
						if excl.MatchString(content) {
							excluded = true
							break
						}
					}
					if excluded {
						continue
					}

					issues = append(issues, Issue{
						ID:         "patterns/hardcoded-credential",
						Severity:   SeverityError,
						Category:   "patterns",
						File:       fileDiff.Path,
						Line:       line.NewNum,
						Message:    "Possible hardcoded credential detected",
						Suggestion: "Use environment variables or a secrets manager instead of hardcoding credentials",
					})
					break // Only report once per line.
				}
			}
		}
	}

	return issues
}

// --- Console/debug artifact detection ---

type consoleRule struct {
	pattern *regexp.Regexp
	fixID   string
}

var consolePatterns = []consoleRule{
	{regexp.MustCompile(`\bconsole\.log\(`), "anti-console-log"},
	{regexp.MustCompile(`\bfmt\.Println\(`), "anti-debug-print"},
	{regexp.MustCompile(`\bprint\(`), "anti-debug-print"},
	{regexp.MustCompile(`\bSystem\.out\.println\(`), "anti-sysout"},
	{regexp.MustCompile(`\bSystem\.err\.println\(`), "anti-sysout"},
	{regexp.MustCompile(`\.printStackTrace\(\)`), "anti-printstacktrace"},
	{regexp.MustCompile(`\bprintln!\(`), "anti-debug-macro"},
	{regexp.MustCompile(`\beprintln!\(`), "anti-debug-macro"},
	{regexp.MustCompile(`\bdbg!\(`), "anti-debug-macro"},
	// Ruby
	{regexp.MustCompile(`\bputs\s+`), "anti-debug-print"},
	{regexp.MustCompile(`\b(?:^|\s)p\s+[^=]`), "anti-debug-print"},
	{regexp.MustCompile(`\bpp\s+`), "anti-debug-print"},
	{regexp.MustCompile(`\bbinding\.pry\b`), "anti-debugger"},
	{regexp.MustCompile(`\bbyebug\b`), "anti-debugger"},
	{regexp.MustCompile(`\bdebugger\b`), "anti-debugger"},
	// Kotlin
	{regexp.MustCompile(`\bprintln\(`), "anti-kotlin-println"},
	// C#
	{regexp.MustCompile(`\bConsole\.Write(Line)?\(`), "anti-console-write"},
	// PHP
	{regexp.MustCompile(`\bvar_dump\(`), "anti-debug-print"},
	{regexp.MustCompile(`\bprint_r\(`), "anti-debug-print"},
	{regexp.MustCompile(`\bdd\(`), "anti-debug-print"},
	// Swift / Dart
	{regexp.MustCompile(`\bdebugPrint\(`), "anti-debug-print"},
	// C/C++
	{regexp.MustCompile(`\bprintf\(`), "anti-debug-print"},
	{regexp.MustCompile(`\bstd::cout\b`), "anti-debug-print"},
	{regexp.MustCompile(`\bstd::cerr\b`), "anti-debug-print"},
	{regexp.MustCompile(`\bfprintf\s*\(\s*stderr\s*,`), "anti-debug-print"},
	// Elixir
	{regexp.MustCompile(`\bIO\.puts\b`), "anti-debug-print"},
	{regexp.MustCompile(`\bIO\.inspect\b`), "anti-debug-print"},
	{regexp.MustCompile(`\bdbg\(`), "anti-debug-macro"},
	// R
	{regexp.MustCompile(`\bcat\(`), "anti-debug-print"},
	{regexp.MustCompile(`\bbrowser\(\)`), "anti-debugger"},
}

// isCLIEntryPoint returns true for files that legitimately use print/Println for
// user-facing output (CLI entry points, scripts).
func isCLIEntryPoint(path string) bool {
	normalized := filepath.ToSlash(path)

	// Scripts in scripts/ directory.
	if strings.HasPrefix(normalized, "scripts/") {
		return true
	}

	// Go main packages in cmd/ directories.
	if strings.Contains(normalized, "cmd/") && filepath.Base(path) == "main.go" {
		return true
	}

	return false
}

func checkConsoleDebug(fileDiff git.FileDiff) []Issue {
	issues := make([]Issue, 0)

	// CLI tools and scripts legitimately use print for user output.
	if isCLIEntryPoint(fileDiff.Path) {
		return issues
	}

	for _, hunk := range fileDiff.Hunks {
		for _, line := range hunk.Lines {
			if line.Type != "added" {
				continue
			}

			content := line.Content

			// Skip comments — they're not debug artifacts.
			trimmed := strings.TrimSpace(content)
			if strings.HasPrefix(trimmed, "//") || strings.HasPrefix(trimmed, "#") || strings.HasPrefix(trimmed, "/*") {
				continue
			}

			for _, rule := range consolePatterns {
				if rule.pattern.MatchString(content) {
					issues = append(issues, Issue{
						ID:         "patterns/console-debug",
						FixID:      rule.fixID,
						Severity:   SeverityInfo,
						Category:   "patterns",
						File:       fileDiff.Path,
						Line:       line.NewNum,
						Message:    "Debug/console output left in code: " + strings.TrimSpace(content),
						Suggestion: "Remove debug output or replace with proper logging",
					})
					break
				}
			}
		}
	}

	return issues
}

// --- Commented-out code detection ---

// commentLinePattern matches lines that are just comments (for Go, JS/TS, Python).
var commentLinePatterns = []*regexp.Regexp{
	regexp.MustCompile(`^\s*//`),  // Go, JS, TS
	regexp.MustCompile(`^\s*#`),   // Python, shell
	regexp.MustCompile(`^\s*/\*`), // Block comment start
	regexp.MustCompile(`^\s*\*`),  // Block comment continuation
}

func isCommentLine(content string) bool {
	for _, pat := range commentLinePatterns {
		if pat.MatchString(content) {
			return true
		}
	}
	return false
}

// codeLikePatterns detect whether a comment line contains code-like constructs
// (assignments, function calls, braces, etc.) as opposed to natural language.
var codeLikePatterns = []*regexp.Regexp{
	regexp.MustCompile(`[{;]`),                     // braces, semicolons (but not plain parens — common in prose)
	regexp.MustCompile(`\w+\.\w+\(`),               // method calls: foo.bar(
	regexp.MustCompile(`\w+\s*:=\s*`),               // short assignments: x :=
	regexp.MustCompile(`^\s*(if|for|while|switch|case)\s`), // control flow keywords
	regexp.MustCompile(`^\s*(return|import|from|var|let|const|func|def|class)\b`), // declaration keywords
}

// looksLikeCode checks if a comment body (after stripping the comment prefix)
// contains code-like patterns rather than just natural language prose.
func looksLikeCode(content string) bool {
	// Strip comment prefix.
	body := strings.TrimSpace(content)
	for _, prefix := range []string{"//", "#", "/*", "*", "* "} {
		body = strings.TrimPrefix(body, prefix)
	}
	body = strings.TrimSpace(body)

	// Empty line inside comment block isn't code.
	if body == "" {
		return false
	}

	for _, pat := range codeLikePatterns {
		if pat.MatchString(body) {
			return true
		}
	}
	return false
}

func checkCommentedOutCode(fileDiff git.FileDiff) []Issue {
	issues := make([]Issue, 0)

	for _, hunk := range fileDiff.Hunks {
		consecutiveComments := 0
		codeLikeCount := 0
		blockStartLine := 0

		emitIfNeeded := func(endLine int) {
			// Only flag if at least 3 consecutive comment lines AND
			// at least one line looks like code (not just prose/docs).
			if consecutiveComments >= 3 && codeLikeCount > 0 {
				issues = append(issues, Issue{
					ID:         "patterns/commented-code",
					Severity:   SeverityWarning,
					Category:   "patterns",
					File:       fileDiff.Path,
					Line:       blockStartLine,
					EndLine:    endLine,
					Message:    "Large block of commented-out code detected",
					Suggestion: "Remove commented-out code; use version control to recover it if needed",
				})
			}
			consecutiveComments = 0
			codeLikeCount = 0
		}

		for _, line := range hunk.Lines {
			if line.Type != "added" {
				emitIfNeeded(line.NewNum - 1)
				continue
			}

			if isCommentLine(line.Content) {
				if consecutiveComments == 0 {
					blockStartLine = line.NewNum
				}
				consecutiveComments++
				if looksLikeCode(line.Content) {
					codeLikeCount++
				}
			} else {
				emitIfNeeded(line.NewNum - 1)
			}
		}

		// Check at end of hunk.
		if consecutiveComments >= 3 && codeLikeCount > 0 {
			lastLine := hunk.Lines[len(hunk.Lines)-1]
			issues = append(issues, Issue{
				ID:         "patterns/commented-code",
				Severity:   SeverityWarning,
				Category:   "patterns",
				File:       fileDiff.Path,
				Line:       blockStartLine,
				EndLine:    lastLine.NewNum,
				Message:    "Large block of commented-out code detected",
				Suggestion: "Remove commented-out code; use version control to recover it if needed",
			})
		}
	}

	return issues
}

// --- Unreachable code detection ---

// returnPatterns match lines that are return/throw/panic statements.
var returnPatterns = []*regexp.Regexp{
	regexp.MustCompile(`^\s*return\b`),
	regexp.MustCompile(`^\s*throw\b`),
	regexp.MustCompile(`^\s*panic\(`),
}

func isReturnLike(content string) bool {
	for _, pat := range returnPatterns {
		if pat.MatchString(content) {
			return true
		}
	}
	return false
}

// isBlockEnd matches closing braces/brackets or blank lines that end a block scope.
func isBlockEnd(content string) bool {
	trimmed := strings.TrimSpace(content)
	return trimmed == "}" || trimmed == "})" || trimmed == ""
}

// indentLevel returns the number of leading whitespace characters (tabs count as 1).
func indentLevel(content string) int {
	for i, ch := range content {
		if ch != ' ' && ch != '\t' {
			return i
		}
	}
	return len(content)
}

func checkUnreachableCode(fileDiff git.FileDiff) []Issue {
	issues := make([]Issue, 0)

	for _, hunk := range fileDiff.Hunks {
		// Use ALL lines in the hunk (not just added) to understand block structure.
		// We only flag added lines, but we need context lines too.
		allLines := hunk.Lines

		for i := 0; i < len(allLines)-1; i++ {
			line := allLines[i]
			// Only check return statements that are in added lines.
			if line.Type != "added" {
				continue
			}

			if !isReturnLike(line.Content) {
				continue
			}

			returnIndent := indentLevel(line.Content)

			// Scan forward to find the next meaningful line (skip blank, closing braces, comments).
			foundUnreachable := false
			for j := i + 1; j < len(allLines); j++ {
				next := allLines[j]
				nextTrimmed := strings.TrimSpace(next.Content)

				// Skip blank lines.
				if nextTrimmed == "" {
					continue
				}

				// A closing brace at same or lower indent means we exited the block scope.
				// Code after is reachable (it's in the enclosing scope).
				if nextTrimmed == "}" || nextTrimmed == "})" {
					break
				}

				// Skip comments.
				if isCommentLine(next.Content) {
					continue
				}

				// Skip case/default labels.
				if strings.HasPrefix(nextTrimmed, "case ") || strings.HasPrefix(nextTrimmed, "default:") {
					break
				}

				nextIndent := indentLevel(next.Content)

				// If the next code line has LESS indent than the return, we've
				// exited the block scope — this code is reachable.
				if nextIndent < returnIndent {
					break
				}

				// If at the same indent and this is an added line, it may be unreachable.
				// But only if it's at the SAME indent level as the return (same block).
				if nextIndent == returnIndent && next.Type == "added" {
					foundUnreachable = true
					issues = append(issues, Issue{
						ID:         "patterns/unreachable-code",
						Severity:   SeverityWarning,
						Category:   "patterns",
						File:       fileDiff.Path,
						Line:       next.NewNum,
						Message:    "Potentially unreachable code after return/throw/panic",
						Suggestion: "Remove unreachable code or restructure the control flow",
					})
				}
				break
			}
			_ = foundUnreachable
		}
	}

	return issues
}

// --- Helpers ---

// nonSourceExtensions are file extensions that should not be analyzed for code patterns.
var nonSourceExtensions = map[string]bool{
	".md":       true,
	".markdown": true,
	".txt":      true,
	".rst":      true,
	".yaml":     true,
	".yml":      true,
	".json":     true,
	".toml":     true,
	".xml":      true,
	".html":     true,
	".css":      true,
	".scss":     true,
	".less":     true,
	".svg":      true,
	".csv":      true,
	".lock":     true,
	".sum":      true,
	".mod":      true,
	".cfg":      true,
	".ini":      true,
	".env":      true,
}

// isAnalyzableFile returns true if the file is source code that should be
// checked for anti-patterns. Returns false for documentation, config, and
// data files where patterns like "#" headers or "TODO" text are normal.
func isAnalyzableFile(path string) bool {
	ext := strings.ToLower(filepath.Ext(path))
	return !nonSourceExtensions[ext]
}

func isPythonFile(path string) bool {
	return strings.HasSuffix(path, ".py")
}

func isJavaFile(path string) bool {
	return strings.HasSuffix(path, ".java")
}

func isRustFile(path string) bool {
	return strings.HasSuffix(path, ".rs")
}

func isRubyFile(path string) bool {
	return strings.HasSuffix(path, ".rb") || strings.HasSuffix(path, ".rake")
}

func isKotlinFile(path string) bool {
	ext := strings.ToLower(filepath.Ext(path))
	return ext == ".kt" || ext == ".kts"
}

func isCSharpFile(path string) bool {
	return strings.HasSuffix(path, ".cs")
}

func isPHPFile(path string) bool {
	return strings.HasSuffix(path, ".php")
}

func isSwiftFile(path string) bool {
	return strings.HasSuffix(path, ".swift")
}

func isTypeScriptFile(path string) bool {
	ext := strings.ToLower(filepath.Ext(path))
	return ext == ".ts" || ext == ".tsx" || ext == ".js" || ext == ".jsx" || ext == ".mjs"
}

// --- TypeScript-specific pattern detection ---

// anyTypePatterns detect usage of the `any` type in TypeScript.
var anyTypePatterns = []*regexp.Regexp{
	regexp.MustCompile(`:\s*any\b`),         // param: any, let x: any
	regexp.MustCompile(`\bas\s+any\b`),       // value as any
	regexp.MustCompile(`<any>`),              // <any> cast
	regexp.MustCompile(`:\s*any\s*[,)]`),     // function(x: any, y: any)
	regexp.MustCompile(`:\s*any\s*;`),        // property: any;
	regexp.MustCompile(`:\s*any\[\]`),        // array: any[]
}

// nonNullAssertionPattern detects !. or similar non-null assertion abuse.
var nonNullAssertionPattern = regexp.MustCompile(`\w+!\.[a-zA-Z]`)

// emptyInterfacePattern detects interface Foo {} (should be Record<string, never>).
var emptyInterfacePattern = regexp.MustCompile(`interface\s+\w+\s*\{\s*\}`)

// tsIgnorePatterns detect @ts-ignore and @ts-nocheck directives.
var tsIgnorePatterns = []*regexp.Regexp{
	regexp.MustCompile(`@ts-ignore`),
	regexp.MustCompile(`@ts-nocheck`),
}

// tripleSlashPattern detects triple-slash directives that should be imports.
var tripleSlashDirectivePattern = regexp.MustCompile(`^///\s*<reference\s+`)

func checkTypeScriptPatterns(fileDiff git.FileDiff) []Issue {
	issues := make([]Issue, 0)

	if !isTypeScriptFile(fileDiff.Path) {
		return issues
	}

	for _, hunk := range fileDiff.Hunks {
		for _, line := range hunk.Lines {
			if line.Type != "added" {
				continue
			}

			content := line.Content
			trimmed := strings.TrimSpace(content)

			// Skip comments (except for @ts-ignore detection).
			isComment := strings.HasPrefix(trimmed, "//") || strings.HasPrefix(trimmed, "/*") || strings.HasPrefix(trimmed, "*")

			// Check @ts-ignore / @ts-nocheck in comments.
			if isComment {
				for _, pat := range tsIgnorePatterns {
					if pat.MatchString(content) {
						issues = append(issues, Issue{
							ID:         "patterns/ts-ignore",
							FixID:      "ts-remove-ignore",
							Severity:   SeverityWarning,
							Category:   "patterns",
							File:       fileDiff.Path,
							Line:       line.NewNum,
							Message:    "TypeScript suppression directive: " + strings.TrimSpace(content),
							Suggestion: "Fix the underlying type error instead of suppressing it",
						})
						break
					}
				}

				// Triple-slash directives.
				if tripleSlashDirectivePattern.MatchString(trimmed) {
					issues = append(issues, Issue{
						ID:         "patterns/ts-triple-slash",
						Severity:   SeverityInfo,
						Category:   "patterns",
						File:       fileDiff.Path,
						Line:       line.NewNum,
						Message:    "Triple-slash directive should be an import: " + strings.TrimSpace(content),
						Suggestion: "Use ES module imports instead of triple-slash reference directives",
					})
				}
				continue
			}

			// Check `any` type usage.
			for _, pat := range anyTypePatterns {
				if pat.MatchString(content) {
					issues = append(issues, Issue{
						ID:         "patterns/ts-any-type",
						FixID:      "ts-any-to-unknown",
						Severity:   SeverityWarning,
						Category:   "patterns",
						File:       fileDiff.Path,
						Line:       line.NewNum,
						Message:    "Usage of `any` type reduces type safety: " + strings.TrimSpace(content),
						Suggestion: "Use `unknown` for type-safe alternatives, or define a specific type",
					})
					break
				}
			}

			// Check non-null assertion abuse.
			if nonNullAssertionPattern.MatchString(content) {
				issues = append(issues, Issue{
					ID:         "patterns/ts-non-null-assertion",
					Severity:   SeverityWarning,
					Category:   "patterns",
					File:       fileDiff.Path,
					Line:       line.NewNum,
					Message:    "Non-null assertion operator (!) may hide null safety issues: " + strings.TrimSpace(content),
					Suggestion: "Use optional chaining (?.) or add a proper null check",
				})
			}

			// Check empty interfaces.
			if emptyInterfacePattern.MatchString(content) {
				issues = append(issues, Issue{
					ID:         "patterns/ts-empty-interface",
					Severity:   SeverityInfo,
					Category:   "patterns",
					File:       fileDiff.Path,
					Line:       line.NewNum,
					Message:    "Empty interface: " + strings.TrimSpace(content),
					Suggestion: "Use `type Foo = Record<string, never>` or `type Foo = object` instead of an empty interface",
				})
			}
		}
	}

	return issues
}

// --- Java-specific pattern detection ---

// emptyCatchPattern matches catch blocks that are empty or contain only a comment.
var emptyCatchPattern = regexp.MustCompile(`\bcatch\s*\([^)]*\)\s*\{\s*\}`)

func checkJavaEmptyCatch(fileDiff git.FileDiff) []Issue {
	issues := make([]Issue, 0)

	for _, hunk := range fileDiff.Hunks {
		for i, line := range hunk.Lines {
			if line.Type != "added" {
				continue
			}

			content := line.Content

			// Check for empty catch on a single line: catch (Exception e) {}
			if emptyCatchPattern.MatchString(content) {
				issues = append(issues, Issue{
					ID:         "patterns/empty-catch",
					Severity:   SeverityWarning,
					Category:   "patterns",
					File:       fileDiff.Path,
					Line:       line.NewNum,
					Message:    "Empty catch block silently swallows exception",
					Suggestion: "At minimum, log the exception. Empty catch blocks hide errors and make debugging difficult",
				})
				continue
			}

			// Check for catch followed by empty block across lines
			trimmed := strings.TrimSpace(content)
			if strings.Contains(trimmed, "catch") && strings.Contains(trimmed, "{") && !strings.Contains(trimmed, "}") {
				// Look ahead for immediate closing brace
				if i+1 < len(hunk.Lines) {
					next := hunk.Lines[i+1]
					nextTrimmed := strings.TrimSpace(next.Content)
					if nextTrimmed == "}" && next.Type == "added" {
						issues = append(issues, Issue{
							ID:         "patterns/empty-catch",
							Severity:   SeverityWarning,
							Category:   "patterns",
							File:       fileDiff.Path,
							Line:       line.NewNum,
							Message:    "Empty catch block silently swallows exception",
							Suggestion: "At minimum, log the exception. Empty catch blocks hide errors and make debugging difficult",
						})
					}
				}
			}
		}
	}

	return issues
}

// javaStringEqualsPattern matches == or != comparisons with String variables
// that should use .equals() instead.
var javaStringEqualsPattern = regexp.MustCompile(`\b(\w+)\s*[!=]=\s*"[^"]*"`)

func checkJavaStringEquals(fileDiff git.FileDiff) []Issue {
	issues := make([]Issue, 0)

	for _, hunk := range fileDiff.Hunks {
		for _, line := range hunk.Lines {
			if line.Type != "added" {
				continue
			}

			content := line.Content
			trimmed := strings.TrimSpace(content)

			// Skip comments
			if strings.HasPrefix(trimmed, "//") || strings.HasPrefix(trimmed, "/*") || strings.HasPrefix(trimmed, "*") {
				continue
			}

			if javaStringEqualsPattern.MatchString(content) {
				// Exclude null comparisons and common non-string patterns
				if strings.Contains(content, "== null") || strings.Contains(content, "!= null") {
					continue
				}

				issues = append(issues, Issue{
					ID:         "patterns/string-equals",
					Severity:   SeverityWarning,
					Category:   "patterns",
					File:       fileDiff.Path,
					Line:       line.NewNum,
					Message:    "String comparison using == instead of .equals(): " + strings.TrimSpace(content),
					Suggestion: "Use .equals() for String comparison in Java; == compares references, not values",
				})
			}
		}
	}

	return issues
}

// --- Rust-specific anti-pattern detection ---

var rustAntiPatterns = []struct {
	pattern *regexp.Regexp
	id      string
	message string
	suggest string
}{
	{
		pattern: regexp.MustCompile(`\.unwrap\(\)`),
		id:      "patterns/rust-unwrap",
		message: "Calling .unwrap() may panic at runtime",
		suggest: "Use the ? operator or match/if-let to handle errors properly",
	},
	{
		pattern: regexp.MustCompile(`\.expect\("[^"]*"\)`),
		id:      "patterns/rust-expect",
		message: "Calling .expect() may panic at runtime",
		suggest: "Use the ? operator or match/if-let to handle errors, or ensure the message is descriptive",
	},
	{
		pattern: regexp.MustCompile(`\.clone\(\)`),
		id:      "patterns/rust-unnecessary-clone",
		message: "Potentially unnecessary .clone() call",
		suggest: "Consider using references instead of cloning to avoid unnecessary allocations",
	},
}

func checkRustAntiPatterns(fileDiff git.FileDiff) []Issue {
	issues := make([]Issue, 0)

	if !isRustFile(fileDiff.Path) {
		return issues
	}

	for _, hunk := range fileDiff.Hunks {
		for _, line := range hunk.Lines {
			if line.Type != "added" {
				continue
			}

			content := line.Content
			trimmed := strings.TrimSpace(content)

			// Skip comments
			if strings.HasPrefix(trimmed, "//") || strings.HasPrefix(trimmed, "/*") || strings.HasPrefix(trimmed, "*") {
				continue
			}

			for _, rule := range rustAntiPatterns {
				if rule.pattern.MatchString(content) {
					issues = append(issues, Issue{
						ID:         rule.id,
						Severity:   SeverityWarning,
						Category:   "patterns",
						File:       fileDiff.Path,
						Line:       line.NewNum,
						Message:    rule.message + ": " + strings.TrimSpace(content),
						Suggestion: rule.suggest,
					})
					break
				}
			}
		}
	}

	return issues
}

// --- C#-specific pattern detection ---

var csharpAntiPatterns = []struct {
	pattern *regexp.Regexp
	id      string
	message string
	suggest string
}{
	{
		pattern: regexp.MustCompile(`\bConsole\.Write(Line)?\(`),
		id:      "patterns/console-debug",
		message: "Console.Write/Console.WriteLine left in code",
		suggest: "Remove debug output or replace with proper logging (ILogger, Serilog, etc.)",
	},
	{
		pattern: regexp.MustCompile(`\bSystem\.Diagnostics\.Debug\.Write(Line)?\(`),
		id:      "patterns/console-debug",
		message: "Debug.WriteLine left in code",
		suggest: "Remove debug output or replace with proper logging",
	},
	{
		pattern: regexp.MustCompile(`#pragma\s+warning\s+disable\b`),
		id:      "patterns/pragma-disable",
		message: "#pragma warning disable without corresponding restore",
		suggest: "Add a matching #pragma warning restore, or fix the underlying warning instead of suppressing it",
	},
}

func checkCSharpPatterns(fileDiff git.FileDiff) []Issue {
	issues := make([]Issue, 0)

	for _, hunk := range fileDiff.Hunks {
		for _, line := range hunk.Lines {
			if line.Type != "added" {
				continue
			}

			content := line.Content
			trimmed := strings.TrimSpace(content)

			// Skip comments
			if strings.HasPrefix(trimmed, "//") || strings.HasPrefix(trimmed, "/*") || strings.HasPrefix(trimmed, "*") {
				continue
			}

			for _, rule := range csharpAntiPatterns {
				if rule.pattern.MatchString(content) {
					issues = append(issues, Issue{
						ID:         rule.id,
						Severity:   SeverityWarning,
						Category:   "patterns",
						File:       fileDiff.Path,
						Line:       line.NewNum,
						Message:    rule.message + ": " + strings.TrimSpace(content),
						Suggestion: rule.suggest,
					})
					break
				}
			}
		}
	}

	return issues
}

// --- PHP-specific pattern detection ---

var phpAntiPatterns = []struct {
	pattern *regexp.Regexp
	id      string
	message string
	suggest string
}{
	{
		pattern: regexp.MustCompile(`\berror_reporting\s*\(\s*0\s*\)`),
		id:      "patterns/php-error-suppression",
		message: "Error reporting disabled with error_reporting(0)",
		suggest: "Use proper error handling instead of suppressing all errors",
	},
	{
		pattern: regexp.MustCompile(`@\$`),
		id:      "patterns/php-error-suppression",
		message: "Error suppression operator (@) hides errors",
		suggest: "Use proper error handling (try/catch) instead of the @ operator",
	},
}

func checkPHPPatterns(fileDiff git.FileDiff) []Issue {
	issues := make([]Issue, 0)

	for _, hunk := range fileDiff.Hunks {
		for _, line := range hunk.Lines {
			if line.Type != "added" {
				continue
			}

			content := line.Content
			trimmed := strings.TrimSpace(content)

			// Skip comments
			if strings.HasPrefix(trimmed, "//") || strings.HasPrefix(trimmed, "/*") ||
				strings.HasPrefix(trimmed, "*") || strings.HasPrefix(trimmed, "#") {
				continue
			}

			for _, rule := range phpAntiPatterns {
				if rule.pattern.MatchString(content) {
					issues = append(issues, Issue{
						ID:         rule.id,
						Severity:   SeverityWarning,
						Category:   "patterns",
						File:       fileDiff.Path,
						Line:       line.NewNum,
						Message:    rule.message + ": " + strings.TrimSpace(content),
						Suggestion: rule.suggest,
					})
					break
				}
			}
		}
	}

	return issues
}

// --- Swift-specific anti-pattern detection ---

var swiftAntiPatterns = []struct {
	pattern *regexp.Regexp
	id      string
	message string
	suggest string
}{
	{
		pattern: regexp.MustCompile(`\bfatalError\(`),
		id:      "patterns/swift-fatal-error",
		message: "fatalError() will crash the application at runtime",
		suggest: "Use proper error handling with throws/Result instead of fatalError()",
	},
	{
		pattern: regexp.MustCompile(`#if\s+DEBUG`),
		id:      "patterns/swift-debug-block",
		message: "#if DEBUG block may contain code not intended for production",
		suggest: "Review and ensure debug-only code is appropriate, or use runtime configuration instead",
	},
}

func checkSwiftAntiPatterns(fileDiff git.FileDiff) []Issue {
	issues := make([]Issue, 0)

	if !isSwiftFile(fileDiff.Path) {
		return issues
	}

	for _, hunk := range fileDiff.Hunks {
		for _, line := range hunk.Lines {
			if line.Type != "added" {
				continue
			}

			content := line.Content
			trimmed := strings.TrimSpace(content)

			// Skip comments
			if strings.HasPrefix(trimmed, "//") || strings.HasPrefix(trimmed, "/*") || strings.HasPrefix(trimmed, "*") {
				continue
			}

			for _, rule := range swiftAntiPatterns {
				if rule.pattern.MatchString(content) {
					issues = append(issues, Issue{
						ID:         rule.id,
						Severity:   SeverityWarning,
						Category:   "patterns",
						File:       fileDiff.Path,
						Line:       line.NewNum,
						Message:    rule.message + ": " + strings.TrimSpace(content),
						Suggestion: rule.suggest,
					})
					break
				}
			}
		}
	}

	return issues
}

// --- C/C++ helper ---

func isCOrCppFile(path string) bool {
	ext := strings.ToLower(filepath.Ext(path))
	return ext == ".c" || ext == ".h" || ext == ".cpp" || ext == ".cc" || ext == ".cxx" || ext == ".hpp" || ext == ".hxx"
}

func isBashFile(path string) bool {
	ext := strings.ToLower(filepath.Ext(path))
	return ext == ".sh" || ext == ".bash"
}

func isPerlFile(path string) bool {
	ext := strings.ToLower(filepath.Ext(path))
	return ext == ".pl" || ext == ".pm"
}

func isPowershellFile(path string) bool {
	ext := strings.ToLower(filepath.Ext(path))
	return ext == ".ps1" || ext == ".psm1"
}

func isGroovyFile(path string) bool {
	ext := strings.ToLower(filepath.Ext(path))
	return ext == ".groovy" || ext == ".gvy"
}

// Modern systems language helpers
func isZigFile(path string) bool {
	return strings.ToLower(filepath.Ext(path)) == ".zig"
}
func isNimFile(path string) bool {
	return strings.ToLower(filepath.Ext(path)) == ".nim"
}
func isCrystalFile(path string) bool {
	return strings.ToLower(filepath.Ext(path)) == ".cr"
}
func isVlangFile(path string) bool {
	return strings.ToLower(filepath.Ext(path)) == ".v"
}
func isDlangFile(path string) bool {
	return strings.ToLower(filepath.Ext(path)) == ".d"
}

// Functional language helpers
func isHaskellFile(path string) bool {
	ext := strings.ToLower(filepath.Ext(path))
	return ext == ".hs" || ext == ".lhs"
}
func isClojureFile(path string) bool {
	ext := strings.ToLower(filepath.Ext(path))
	return ext == ".clj" || ext == ".cljs" || ext == ".cljc" || ext == ".edn"
}
func isErlangFile(path string) bool {
	ext := strings.ToLower(filepath.Ext(path))
	return ext == ".erl" || ext == ".hrl"
}
func isFsharpFile(path string) bool {
	ext := strings.ToLower(filepath.Ext(path))
	return ext == ".fs" || ext == ".fsx" || ext == ".fsi"
}
func isOcamlFile(path string) bool {
	ext := strings.ToLower(filepath.Ext(path))
	return ext == ".ml" || ext == ".mli"
}

// Domain language helpers
func isJuliaFile(path string) bool {
	return strings.ToLower(filepath.Ext(path)) == ".jl"
}
func isFortranFile(path string) bool {
	ext := strings.ToLower(filepath.Ext(path))
	return ext == ".f90" || ext == ".f95" || ext == ".f03" || ext == ".f08" || ext == ".f" || ext == ".for"
}
func isSolidityFile(path string) bool {
	return strings.ToLower(filepath.Ext(path)) == ".sol"
}
func isTerraformFile(path string) bool {
	return strings.ToLower(filepath.Ext(path)) == ".tf"
}
func isProtobufFile(path string) bool {
	return strings.ToLower(filepath.Ext(path)) == ".proto"
}

// Legacy language helpers
func isVisualBasicFile(path string) bool {
	ext := strings.ToLower(filepath.Ext(path))
	return ext == ".vb" || ext == ".vbs"
}
func isCobolFile(path string) bool {
	ext := strings.ToLower(filepath.Ext(path))
	return ext == ".cob" || ext == ".cbl" || ext == ".cpy"
}
func isAdaFile(path string) bool {
	ext := strings.ToLower(filepath.Ext(path))
	return ext == ".adb" || ext == ".ads"
}
func isPascalFile(path string) bool {
	ext := strings.ToLower(filepath.Ext(path))
	return ext == ".pas" || ext == ".pp" || ext == ".dpr" || ext == ".lpr"
}

// --- C/C++-specific anti-pattern detection ---

var cAntiPatterns = []struct {
	pattern *regexp.Regexp
	id      string
	message string
	suggest string
}{
	{
		pattern: regexp.MustCompile(`\bgoto\s+`),
		id:      "patterns/c-goto",
		message: "goto statement detected — reduces code readability and maintainability",
		suggest: "Restructure control flow using loops, conditionals, or functions instead of goto",
	},
}

func checkCAntiPatterns(fileDiff git.FileDiff) []Issue {
	issues := make([]Issue, 0)

	for _, hunk := range fileDiff.Hunks {
		for _, line := range hunk.Lines {
			if line.Type != "added" {
				continue
			}

			content := line.Content
			trimmed := strings.TrimSpace(content)

			// Skip comments
			if strings.HasPrefix(trimmed, "//") || strings.HasPrefix(trimmed, "/*") || strings.HasPrefix(trimmed, "*") {
				continue
			}

			for _, rule := range cAntiPatterns {
				if rule.pattern.MatchString(content) {
					issues = append(issues, Issue{
						ID:         rule.id,
						Severity:   SeverityWarning,
						Category:   "patterns",
						File:       fileDiff.Path,
						Line:       line.NewNum,
						Message:    rule.message + ": " + strings.TrimSpace(content),
						Suggestion: rule.suggest,
					})
					break
				}
			}
		}
	}

	return issues
}

// --- Bash-specific anti-pattern detection ---

func checkBashAntiPatterns(fileDiff git.FileDiff) []Issue {
	issues := make([]Issue, 0)

	// Check if the first few added lines contain "set -e".
	hasSetE := false
	for _, hunk := range fileDiff.Hunks {
		for _, line := range hunk.Lines {
			if line.Type != "added" {
				continue
			}
			trimmed := strings.TrimSpace(line.Content)
			if trimmed == "" || strings.HasPrefix(trimmed, "#") {
				continue
			}
			// Check the first non-comment, non-blank added line (within first 10 lines).
			if line.NewNum <= 10 {
				if strings.Contains(trimmed, "set -e") || strings.Contains(trimmed, "set -euo") || strings.Contains(trimmed, "set -eu") {
					hasSetE = true
				}
			}
		}
	}

	if !hasSetE {
		// Only report if there are added lines (not just a deletion).
		for _, hunk := range fileDiff.Hunks {
			for _, line := range hunk.Lines {
				if line.Type == "added" {
					issues = append(issues, Issue{
						ID:         "patterns/bash-no-set-e",
						Severity:   SeverityInfo,
						Category:   "patterns",
						File:       fileDiff.Path,
						Line:       1,
						Message:    "Bash script missing 'set -e' — errors may be silently ignored",
						Suggestion: "Add 'set -euo pipefail' near the top of the script to fail on errors",
					})
					return issues
				}
			}
		}
	}

	return issues
}
