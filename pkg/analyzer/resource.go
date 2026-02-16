package analyzer

import (
	"path/filepath"
	"regexp"
	"strings"

	"github.com/joeabbey/fault/pkg/git"
)

// ResourceAnalyzer detects resource leaks in added code.
type ResourceAnalyzer struct{}

// NewResourceAnalyzer creates a new resource leak analyzer.
func NewResourceAnalyzer() *ResourceAnalyzer {
	return &ResourceAnalyzer{}
}

// Name returns the analyzer name.
func (a *ResourceAnalyzer) Name() string {
	return "resource"
}

// Analyze scans diff content for resource leak patterns in added lines.
func (a *ResourceAnalyzer) Analyze(ctx *AnalysisContext) ([]Issue, error) {
	issues := make([]Issue, 0)

	if ctx.Diff == nil || len(ctx.Diff.Files) == 0 {
		return issues, nil
	}

	for _, fileDiff := range ctx.Diff.Files {
		if fileDiff.Status == "deleted" || fileDiff.IsBinary {
			continue
		}

		if isResourceSkippedPath(fileDiff.Path) {
			continue
		}

		if isTestFile(fileDiff.Path) {
			continue
		}

		ext := strings.ToLower(filepath.Ext(fileDiff.Path))

		switch ext {
		case ".go":
			issues = append(issues, checkGoResources(fileDiff)...)
		case ".ts", ".tsx", ".js", ".jsx", ".mjs":
			issues = append(issues, checkTSResources(fileDiff)...)
		case ".py":
			issues = append(issues, checkPythonResources(fileDiff)...)
		case ".rs":
			issues = append(issues, checkRustResources(fileDiff)...)
		case ".java":
			issues = append(issues, checkJavaResources(fileDiff)...)
		case ".c", ".cpp", ".cc", ".h", ".hpp":
			issues = append(issues, checkCResources(fileDiff)...)
		case ".cs":
			issues = append(issues, checkCSharpResources(fileDiff)...)
		case ".rb":
			issues = append(issues, checkRubyResources(fileDiff)...)
		case ".php":
			issues = append(issues, checkPHPResources(fileDiff)...)
		case ".swift":
			issues = append(issues, checkSwiftResources(fileDiff)...)
		case ".kt", ".kts":
			issues = append(issues, checkKotlinResources(fileDiff)...)
		}
	}

	return issues, nil
}

// isResourceSkippedPath returns true for vendored/generated directories.
func isResourceSkippedPath(path string) bool {
	normalized := filepath.ToSlash(path)
	parts := strings.Split(normalized, "/")
	for _, p := range parts {
		switch p {
		case "vendor", "node_modules", "testdata", "__tests__":
			return true
		}
	}
	return false
}

// linesAfter returns the content of added/context lines within `count` lines
// after index `i` in the given hunk lines slice. Comment lines are excluded
// to avoid false matches on commented-out code.
func linesAfter(lines []git.Line, i int, count int) []string {
	result := make([]string, 0, count)
	for j := i + 1; j < len(lines) && j <= i+count; j++ {
		if lines[j].Type == "removed" {
			continue
		}
		trimmed := strings.TrimSpace(lines[j].Content)
		if strings.HasPrefix(trimmed, "//") || strings.HasPrefix(trimmed, "/*") ||
			strings.HasPrefix(trimmed, "*") || strings.HasPrefix(trimmed, "#") {
			continue
		}
		result = append(result, lines[j].Content)
	}
	return result
}

// anyLineContains checks if any of the given lines contain the substring.
func anyLineContains(lines []string, substr string) bool {
	for _, l := range lines {
		if strings.Contains(l, substr) {
			return true
		}
	}
	return false
}

// anyLineMatches checks if any of the given lines match the regex.
func anyLineMatches(lines []string, re *regexp.Regexp) bool {
	for _, l := range lines {
		if re.MatchString(l) {
			return true
		}
	}
	return false
}

// isComment returns true if the trimmed line is a comment for C-style languages.
func isResourceComment(trimmed string) bool {
	return strings.HasPrefix(trimmed, "//") ||
		strings.HasPrefix(trimmed, "/*") ||
		strings.HasPrefix(trimmed, "*") ||
		strings.HasPrefix(trimmed, "#")
}

// --- Go resource leak detection ---

var (
	goFileOpen    = regexp.MustCompile(`\b(os\.Open|os\.Create|os\.OpenFile)\s*\(`)
	goHTTPCall    = regexp.MustCompile(`\b(http\.Get|http\.Post|http\.Head|http\.PostForm|client\.Do|client\.Get|client\.Post|client\.Head)\s*\(`)
	goDBQuery     = regexp.MustCompile(`\b(\.Query|\.QueryContext|\.QueryRow|\.QueryRowContext)\s*\(`)
	goDeferClose  = regexp.MustCompile(`defer\b.*\.Close\(\)`)
	goBodyClose   = regexp.MustCompile(`defer\b.*\.Body\.Close\(\)`)
	goRowsClose   = regexp.MustCompile(`defer\b.*\.(Close|Scan)\(\)`)
	goDefer       = regexp.MustCompile(`\bdefer\b`)
	goNetListen   = regexp.MustCompile(`\b(net\.Listen|tls\.Listen|net\.ListenPacket)\s*\(`)
	goGRPCDial    = regexp.MustCompile(`\b(grpc\.Dial|grpc\.DialContext|grpc\.NewClient)\s*\(`)
	goSQLOpen     = regexp.MustCompile(`\b(sql\.Open|sqlx\.Open|sqlx\.Connect)\s*\(`)
)

func checkGoResources(fileDiff git.FileDiff) []Issue {
	issues := make([]Issue, 0)

	for _, hunk := range fileDiff.Hunks {
		lines := hunk.Lines

		for i, line := range lines {
			if line.Type != "added" {
				continue
			}

			content := line.Content
			trimmed := strings.TrimSpace(content)

			if strings.HasPrefix(trimmed, "//") || strings.HasPrefix(trimmed, "/*") || strings.HasPrefix(trimmed, "*") {
				continue
			}

			after := linesAfter(lines, i, 5)

			// File open without defer close
			if goFileOpen.MatchString(content) {
				if !anyLineMatches(after[:min(3, len(after))], goDeferClose) && !anyLineMatches(after, goDefer) {
					issues = append(issues, Issue{
						ID:         "resource/go-unclosed-file",
						FixID:      "unclosed-file",
						Severity:   SeverityWarning,
						Category:   "resource-go",
						File:       fileDiff.Path,
						Line:       line.NewNum,
						Message:    "File opened without defer Close() in nearby lines",
						Suggestion: "Add 'defer f.Close()' immediately after checking the error from os.Open/os.Create",
					})
				}
			}

			// HTTP response body not closed
			if goHTTPCall.MatchString(content) {
				if !anyLineMatches(after, goBodyClose) && !anyLineContains(after, ".Body.Close()") {
					issues = append(issues, Issue{
						ID:         "resource/go-unclosed-body",
						FixID:      "unclosed-body",
						Severity:   SeverityWarning,
						Category:   "resource-go",
						File:       fileDiff.Path,
						Line:       line.NewNum,
						Message:    "HTTP response body not closed; this leaks connections",
						Suggestion: "Add 'defer resp.Body.Close()' after checking the error from the HTTP call",
					})
				}
			}

			// DB query rows not closed
			if goDBQuery.MatchString(content) && !strings.Contains(content, "QueryRow") {
				if !anyLineMatches(after[:min(3, len(after))], goRowsClose) && !anyLineContains(after, ".Close()") {
					issues = append(issues, Issue{
						ID:         "resource/go-unclosed-rows",
						FixID:      "unclosed-rows",
						Severity:   SeverityWarning,
						Category:   "resource-go",
						File:       fileDiff.Path,
						Line:       line.NewNum,
						Message:    "Database rows not closed; this leaks database connections",
						Suggestion: "Add 'defer rows.Close()' immediately after checking the error from Query()",
					})
				}
			}

			// Net listener without defer close
			if goNetListen.MatchString(content) {
				if !anyLineMatches(after, goDeferClose) && !anyLineMatches(after, goDefer) {
					issues = append(issues, Issue{
						ID:         "resource/go-unclosed-listener",
						FixID:      "missing-defer",
						Severity:   SeverityWarning,
						Category:   "resource-go",
						File:       fileDiff.Path,
						Line:       line.NewNum,
						Message:    "Network listener opened without defer Close()",
						Suggestion: "Add 'defer ln.Close()' after checking the error from net.Listen",
					})
				}
			}

			// gRPC dial without close
			if goGRPCDial.MatchString(content) {
				if !anyLineMatches(after, goDeferClose) && !anyLineMatches(after, goDefer) {
					issues = append(issues, Issue{
						ID:         "resource/go-unclosed-grpc",
						FixID:      "missing-defer",
						Severity:   SeverityWarning,
						Category:   "resource-go",
						File:       fileDiff.Path,
						Line:       line.NewNum,
						Message:    "gRPC connection opened without defer Close()",
						Suggestion: "Add 'defer conn.Close()' after checking the error from grpc.Dial",
					})
				}
			}

			// sql.Open without close
			if goSQLOpen.MatchString(content) {
				if !anyLineMatches(after, goDeferClose) && !anyLineMatches(after, goDefer) {
					issues = append(issues, Issue{
						ID:         "resource/go-unclosed-db",
						FixID:      "missing-defer",
						Severity:   SeverityWarning,
						Category:   "resource-go",
						File:       fileDiff.Path,
						Line:       line.NewNum,
						Message:    "Database connection opened without defer Close()",
						Suggestion: "Add 'defer db.Close()' after checking the error from sql.Open",
					})
				}
			}
		}
	}

	return issues
}

// --- TypeScript/JavaScript resource leak detection ---

var (
	tsWebSocket      = regexp.MustCompile(`new\s+WebSocket\s*\(`)
	tsDBConnect      = regexp.MustCompile(`\b(createConnection|createPool|connect)\s*\(`)
	tsAddEventRe     = regexp.MustCompile(`\.addEventListener\s*\(`)
	tsRemoveEventRe  = regexp.MustCompile(`\.removeEventListener\s*\(`)
	tsCreateStream   = regexp.MustCompile(`\b(createReadStream|createWriteStream)\s*\(`)
	tsStreamCleanup  = regexp.MustCompile(`\.(destroy|close|end)\s*\(`)
	tsFetchCall      = regexp.MustCompile(`\bfetch\s*\(`)
	tsAbortController = regexp.MustCompile(`\bAbortController\b`)
	tsUseEffect      = regexp.MustCompile(`\buseEffect\s*\(`)
	tsOnMount        = regexp.MustCompile(`\bonMount\s*\(`)
	resSetInterval    = regexp.MustCompile(`\bsetInterval\s*\(`)
	resClearInterval  = regexp.MustCompile(`\bclearInterval\s*\(`)
)

func checkTSResources(fileDiff git.FileDiff) []Issue {
	issues := make([]Issue, 0)

	for _, hunk := range fileDiff.Hunks {
		lines := hunk.Lines

		for i, line := range lines {
			if line.Type != "added" {
				continue
			}

			content := line.Content
			trimmed := strings.TrimSpace(content)

			if strings.HasPrefix(trimmed, "//") || strings.HasPrefix(trimmed, "/*") || strings.HasPrefix(trimmed, "*") {
				continue
			}

			after := linesAfter(lines, i, 8)

			// WebSocket without close
			if tsWebSocket.MatchString(content) {
				if !anyLineContains(after, ".close()") && !anyLineContains(after, ".close(") {
					issues = append(issues, Issue{
						ID:         "resource/ts-unclosed-connection",
						FixID:      "unclosed-connection",
						Severity:   SeverityWarning,
						Category:   "resource-ts",
						File:       fileDiff.Path,
						Line:       line.NewNum,
						Message:    "WebSocket created without .close() in nearby code",
						Suggestion: "Ensure the WebSocket is closed when no longer needed, especially in component cleanup",
					})
				}
			}

			// DB connection without close
			if tsDBConnect.MatchString(content) {
				if !anyLineContains(after, ".close()") && !anyLineContains(after, ".end()") && !anyLineContains(after, ".destroy()") {
					issues = append(issues, Issue{
						ID:         "resource/ts-unclosed-db",
						FixID:      "unclosed-connection",
						Severity:   SeverityWarning,
						Category:   "resource-ts",
						File:       fileDiff.Path,
						Line:       line.NewNum,
						Message:    "Database connection created without cleanup in nearby code",
						Suggestion: "Close the connection with .close() or .end() when done, or use a connection pool",
					})
				}
			}

			// addEventListener without removeEventListener in same hunk
			if tsAddEventRe.MatchString(content) {
				allHunkLines := make([]string, 0, len(lines))
				for _, l := range lines {
					if l.Type != "removed" {
						allHunkLines = append(allHunkLines, l.Content)
					}
				}
				if !anyLineMatches(allHunkLines, tsRemoveEventRe) {
					issues = append(issues, Issue{
						ID:         "resource/ts-missing-cleanup",
						FixID:      "missing-cleanup",
						Severity:   SeverityWarning,
						Category:   "resource-ts",
						File:       fileDiff.Path,
						Line:       line.NewNum,
						Message:    "addEventListener without corresponding removeEventListener (potential memory leak)",
						Suggestion: "Add removeEventListener in cleanup/unmount to prevent memory leaks in long-lived components",
					})
				}
			}

			// Stream without cleanup
			if tsCreateStream.MatchString(content) {
				if !anyLineMatches(after, tsStreamCleanup) && !anyLineContains(after, ".pipe(") {
					issues = append(issues, Issue{
						ID:         "resource/ts-unclosed-stream",
						FixID:      "unclosed-stream",
						Severity:   SeverityWarning,
						Category:   "resource-ts",
						File:       fileDiff.Path,
						Line:       line.NewNum,
						Message:    "Stream created without .destroy(), .close(), or .pipe() handling",
						Suggestion: "Ensure streams are properly closed with .destroy() or piped to handle backpressure",
					})
				}
			}

			// fetch in useEffect/onMount without AbortController
			if tsFetchCall.MatchString(content) {
				// Look backwards for useEffect or onMount
				inEffect := false
				for j := i - 1; j >= 0 && j >= i-15; j-- {
					if lines[j].Type == "removed" {
						continue
					}
					lc := lines[j].Content
					if tsUseEffect.MatchString(lc) || tsOnMount.MatchString(lc) {
						inEffect = true
						break
					}
				}
				if inEffect {
					// Check surrounding lines for AbortController
					allNearby := make([]string, 0)
					for j := max(0, i-10); j < min(len(lines), i+10); j++ {
						if lines[j].Type != "removed" {
							allNearby = append(allNearby, lines[j].Content)
						}
					}
					if !anyLineMatches(allNearby, tsAbortController) {
						issues = append(issues, Issue{
							ID:         "resource/ts-missing-abort-controller",
							FixID:      "missing-abort-controller",
							Severity:   SeverityWarning,
							Category:   "resource-ts",
							File:       fileDiff.Path,
							Line:       line.NewNum,
							Message:    "fetch() in effect/lifecycle hook without AbortController for cancellation",
							Suggestion: "Use an AbortController to cancel fetch requests when the component unmounts",
						})
					}
				}
			}

			// setInterval without clearInterval
			if resSetInterval.MatchString(content) {
				allHunkLines := make([]string, 0, len(lines))
				for _, l := range lines {
					if l.Type != "removed" {
						allHunkLines = append(allHunkLines, l.Content)
					}
				}
				if !anyLineMatches(allHunkLines, resClearInterval) {
					issues = append(issues, Issue{
						ID:         "resource/ts-interval-leak",
						FixID:      "missing-cleanup",
						Severity:   SeverityWarning,
						Category:   "resource-ts",
						File:       fileDiff.Path,
						Line:       line.NewNum,
						Message:    "setInterval without clearInterval (timer leak)",
						Suggestion: "Store the interval ID and call clearInterval in cleanup/unmount",
					})
				}
			}
		}
	}

	return issues
}

// --- Python resource leak detection ---

var (
	pyOpen           = regexp.MustCompile(`\bopen\s*\(`)
	pyWithOpen       = regexp.MustCompile(`\bwith\b.*\bopen\s*\(`)
	pyDBConnect      = regexp.MustCompile(`\b(sqlite3\.connect|psycopg2\.connect|pymysql\.connect|MySQLdb\.connect|cx_Oracle\.connect)\s*\(`)
	pyWithStmt       = regexp.MustCompile(`\bwith\b`)
	pyRequestsSession = regexp.MustCompile(`\brequests\.Session\s*\(`)
)

func checkPythonResources(fileDiff git.FileDiff) []Issue {
	issues := make([]Issue, 0)

	for _, hunk := range fileDiff.Hunks {
		lines := hunk.Lines

		for i, line := range lines {
			if line.Type != "added" {
				continue
			}

			content := line.Content
			trimmed := strings.TrimSpace(content)

			if strings.HasPrefix(trimmed, "#") {
				continue
			}

			after := linesAfter(lines, i, 5)

			// open() without with statement
			if pyOpen.MatchString(content) && !pyWithOpen.MatchString(content) && !pyWithStmt.MatchString(content) {
				// Not inside a with block — check if it's a standalone assignment
				if !anyLineContains(after, ".close()") {
					issues = append(issues, Issue{
						ID:         "resource/python-no-context-manager",
						FixID:      "no-context-manager",
						Severity:   SeverityWarning,
						Category:   "resource-python",
						File:       fileDiff.Path,
						Line:       line.NewNum,
						Message:    "File opened without 'with' statement (potential file handle leak)",
						Suggestion: "Use 'with open(path) as f:' to ensure the file is automatically closed",
					})
				}
			}

			// DB connection without with or close
			if pyDBConnect.MatchString(content) && !pyWithStmt.MatchString(content) {
				if !anyLineContains(after, ".close()") {
					issues = append(issues, Issue{
						ID:         "resource/python-unclosed-connection",
						FixID:      "unclosed-connection",
						Severity:   SeverityWarning,
						Category:   "resource-python",
						File:       fileDiff.Path,
						Line:       line.NewNum,
						Message:    "Database connection opened without 'with' or .close()",
						Suggestion: "Use 'with' statement or ensure .close() is called in a finally block",
					})
				}
			}

			// requests.Session without with or close
			if pyRequestsSession.MatchString(content) && !pyWithStmt.MatchString(content) {
				if !anyLineContains(after, ".close()") {
					issues = append(issues, Issue{
						ID:         "resource/python-unclosed-session",
						FixID:      "unclosed-session",
						Severity:   SeverityWarning,
						Category:   "resource-python",
						File:       fileDiff.Path,
						Line:       line.NewNum,
						Message:    "requests.Session() created without 'with' or .close()",
						Suggestion: "Use 'with requests.Session() as s:' to ensure the session is closed",
					})
				}
			}
		}
	}

	return issues
}

// --- Rust resource leak detection ---

var (
	rustBoxLeak  = regexp.MustCompile(`\bBox::leak\s*\(`)
	rustMemForget = regexp.MustCompile(`\bstd::mem::forget\s*\(|\bmem::forget\s*\(`)
)

func checkRustResources(fileDiff git.FileDiff) []Issue {
	issues := make([]Issue, 0)

	for _, hunk := range fileDiff.Hunks {
		for _, line := range hunk.Lines {
			if line.Type != "added" {
				continue
			}

			content := line.Content
			trimmed := strings.TrimSpace(content)

			if strings.HasPrefix(trimmed, "//") || strings.HasPrefix(trimmed, "/*") || strings.HasPrefix(trimmed, "*") {
				continue
			}

			if rustBoxLeak.MatchString(content) {
				issues = append(issues, Issue{
					ID:         "resource/rust-leaked-box",
					FixID:      "leaked-box",
					Severity:   SeverityInfo,
					Category:   "resource-rust",
					File:       fileDiff.Path,
					Line:       line.NewNum,
					Message:    "Box::leak() intentionally leaks memory; ensure this is necessary",
					Suggestion: "Box::leak is rarely needed outside of static initialization. Consider using Arc or a scoped lifetime instead",
				})
			}

			if rustMemForget.MatchString(content) {
				issues = append(issues, Issue{
					ID:         "resource/rust-forget-drop",
					FixID:      "forget-drop",
					Severity:   SeverityInfo,
					Category:   "resource-rust",
					File:       fileDiff.Path,
					Line:       line.NewNum,
					Message:    "mem::forget() suppresses Drop, which can leak resources",
					Suggestion: "Ensure mem::forget is intentional. If you need to transfer ownership without dropping, consider ManuallyDrop",
				})
			}
		}
	}

	return issues
}

// --- Java resource leak detection ---

var (
	javaResourceNew   = regexp.MustCompile(`new\s+(FileInputStream|FileOutputStream|BufferedReader|BufferedWriter|FileReader|FileWriter|PrintWriter|Scanner|Socket|ServerSocket|DataInputStream|DataOutputStream)\s*\(`)
	javaDBConn        = regexp.MustCompile(`DriverManager\.getConnection\s*\(`)
	javaUsingKeyword  = regexp.MustCompile(`\btry\s*\(`)
	javaFinallyBlock  = regexp.MustCompile(`\bfinally\s*\{`)
)

func checkJavaResources(fileDiff git.FileDiff) []Issue {
	issues := make([]Issue, 0)

	for _, hunk := range fileDiff.Hunks {
		lines := hunk.Lines

		for i, line := range lines {
			if line.Type != "added" {
				continue
			}

			content := line.Content
			trimmed := strings.TrimSpace(content)

			if strings.HasPrefix(trimmed, "//") || strings.HasPrefix(trimmed, "/*") || strings.HasPrefix(trimmed, "*") {
				continue
			}

			// Check for resource creation without try-with-resources
			if javaResourceNew.MatchString(content) || javaDBConn.MatchString(content) {
				// Look backwards for try( (try-with-resources)
				hasTryWith := false
				for j := i; j >= max(0, i-3); j-- {
					if javaUsingKeyword.MatchString(lines[j].Content) {
						hasTryWith = true
						break
					}
				}
				// Also check current line for inline try-with-resources
				if javaUsingKeyword.MatchString(content) {
					hasTryWith = true
				}

				if !hasTryWith {
					after := linesAfter(lines, i, 10)
					hasFinally := anyLineMatches(after, javaFinallyBlock)
					hasClose := anyLineContains(after, ".close()")

					if !hasFinally && !hasClose {
						msg := "Resource created without try-with-resources or finally block"
						if javaDBConn.MatchString(content) {
							msg = "Database connection created without try-with-resources or finally block"
						}
						issues = append(issues, Issue{
							ID:         "resource/java-unclosed-resource",
							FixID:      "unclosed-resource",
							Severity:   SeverityWarning,
							Category:   "resource-java",
							File:       fileDiff.Path,
							Line:       line.NewNum,
							Message:    msg,
							Suggestion: "Use try-with-resources: try (var resource = new Resource()) { ... }",
						})
					}
				}
			}
		}
	}

	return issues
}

// --- C/C++ resource leak detection ---

var (
	cFopen       = regexp.MustCompile(`\bfopen\s*\(`)
	cFclose      = regexp.MustCompile(`\bfclose\s*\(`)
	cMalloc      = regexp.MustCompile(`\b(malloc|calloc|realloc)\s*\(`)
	cFree        = regexp.MustCompile(`\bfree\s*\(`)
	cSocket      = regexp.MustCompile(`\bsocket\s*\(`)
	cCloseSocket = regexp.MustCompile(`\b(close|closesocket)\s*\(`)
	cNewExpr     = regexp.MustCompile(`\bnew\s+\w+`)
	cDelete      = regexp.MustCompile(`\bdelete\b`)
	cSmartPtr    = regexp.MustCompile(`\b(unique_ptr|shared_ptr|make_unique|make_shared)\b`)
)

func checkCResources(fileDiff git.FileDiff) []Issue {
	issues := make([]Issue, 0)

	for _, hunk := range fileDiff.Hunks {
		lines := hunk.Lines

		// Collect all non-removed lines in this hunk for scope-level checks
		hunkContent := make([]string, 0, len(lines))
		for _, l := range lines {
			if l.Type != "removed" {
				hunkContent = append(hunkContent, l.Content)
			}
		}

		for i, line := range lines {
			if line.Type != "added" {
				continue
			}

			content := line.Content
			trimmed := strings.TrimSpace(content)

			if strings.HasPrefix(trimmed, "//") || strings.HasPrefix(trimmed, "/*") || strings.HasPrefix(trimmed, "*") || strings.HasPrefix(trimmed, "#") {
				continue
			}

			after := linesAfter(lines, i, 10)

			// fopen without fclose
			if cFopen.MatchString(content) {
				if !anyLineMatches(hunkContent, cFclose) {
					issues = append(issues, Issue{
						ID:         "resource/c-unclosed-file",
						FixID:      "unclosed-file",
						Severity:   SeverityWarning,
						Category:   "resource-c",
						File:       fileDiff.Path,
						Line:       line.NewNum,
						Message:    "fopen() without fclose() in same scope",
						Suggestion: "Ensure fclose() is called on all code paths, including error paths",
					})
				}
			}

			// malloc/calloc without free
			if cMalloc.MatchString(content) {
				if !anyLineMatches(after, cFree) && !anyLineMatches(hunkContent, cFree) {
					issues = append(issues, Issue{
						ID:         "resource/c-malloc-no-free",
						FixID:      "malloc-no-free",
						Severity:   SeverityWarning,
						Category:   "resource-c",
						File:       fileDiff.Path,
						Line:       line.NewNum,
						Message:    "Memory allocated with malloc/calloc without visible free() in nearby code",
						Suggestion: "Ensure free() is called on all code paths to avoid memory leaks",
					})
				}
			}

			// socket without close
			if cSocket.MatchString(content) {
				if !anyLineMatches(hunkContent, cCloseSocket) {
					issues = append(issues, Issue{
						ID:         "resource/c-unclosed-socket",
						FixID:      "unclosed-socket",
						Severity:   SeverityWarning,
						Category:   "resource-c",
						File:       fileDiff.Path,
						Line:       line.NewNum,
						Message:    "socket() opened without close()/closesocket() in same scope",
						Suggestion: "Ensure close() or closesocket() is called to release the socket descriptor",
					})
				}
			}

			// C++ new without delete or smart pointer
			if cNewExpr.MatchString(content) && !cSmartPtr.MatchString(content) {
				ext := strings.ToLower(filepath.Ext(fileDiff.Path))
				if ext == ".cpp" || ext == ".cc" || ext == ".hpp" {
					if !anyLineMatches(hunkContent, cDelete) && !anyLineMatches(hunkContent, cSmartPtr) {
						issues = append(issues, Issue{
							ID:         "resource/cpp-new-no-delete",
							FixID:      "malloc-no-free",
							Severity:   SeverityWarning,
							Category:   "resource-c",
							File:       fileDiff.Path,
							Line:       line.NewNum,
							Message:    "Raw 'new' without delete or smart pointer",
							Suggestion: "Use std::unique_ptr or std::shared_ptr instead of raw new/delete",
						})
					}
				}
			}
		}
	}

	return issues
}

// --- C# resource leak detection ---

var (
	csharpDisposable = regexp.MustCompile(`new\s+(FileStream|StreamReader|StreamWriter|SqlConnection|HttpClient|TcpClient|NetworkStream|BinaryReader|BinaryWriter|MemoryStream|CryptoStream)\s*\(`)
	csharpUsing      = regexp.MustCompile(`\busing\s*[\(\s]`)
	csharpDispose    = regexp.MustCompile(`\.Dispose\s*\(`)
	csharpAwaitUsing = regexp.MustCompile(`\bawait\s+using\b`)
)

func checkCSharpResources(fileDiff git.FileDiff) []Issue {
	issues := make([]Issue, 0)

	for _, hunk := range fileDiff.Hunks {
		lines := hunk.Lines

		for i, line := range lines {
			if line.Type != "added" {
				continue
			}

			content := line.Content
			trimmed := strings.TrimSpace(content)

			if strings.HasPrefix(trimmed, "//") || strings.HasPrefix(trimmed, "/*") || strings.HasPrefix(trimmed, "*") {
				continue
			}

			if csharpDisposable.MatchString(content) {
				// Check if wrapped in using statement
				if !csharpUsing.MatchString(content) && !csharpAwaitUsing.MatchString(content) {
					// Look backwards for using
					hasUsing := false
					for j := i - 1; j >= max(0, i-2); j-- {
						if csharpUsing.MatchString(lines[j].Content) || csharpAwaitUsing.MatchString(lines[j].Content) {
							hasUsing = true
							break
						}
					}
					after := linesAfter(lines, i, 8)
					if !hasUsing && !anyLineMatches(after, csharpDispose) {
						issues = append(issues, Issue{
							ID:         "resource/csharp-missing-using",
							FixID:      "missing-using",
							Severity:   SeverityWarning,
							Category:   "resource-csharp",
							File:       fileDiff.Path,
							Line:       line.NewNum,
							Message:    "Disposable resource created without 'using' statement or Dispose()",
							Suggestion: "Use 'using var x = new Resource()' or 'using (var x = new Resource()) { }' for automatic disposal",
						})
					}
				}
			}
		}
	}

	return issues
}

// --- Ruby resource leak detection ---

var (
	rubyFileOpen   = regexp.MustCompile(`\bFile\.open\s*\(`)
	rubyBlockForm  = regexp.MustCompile(`File\.open\s*\([^)]*\)\s*(do\s*\||\{)`)
)

func checkRubyResources(fileDiff git.FileDiff) []Issue {
	issues := make([]Issue, 0)

	for _, hunk := range fileDiff.Hunks {
		lines := hunk.Lines

		for i, line := range lines {
			if line.Type != "added" {
				continue
			}

			content := line.Content
			trimmed := strings.TrimSpace(content)

			if strings.HasPrefix(trimmed, "#") {
				continue
			}

			// File.open without block form
			if rubyFileOpen.MatchString(content) && !rubyBlockForm.MatchString(content) {
				// Check if next line starts a block
				after := linesAfter(lines, i, 2)
				hasBlock := false
				for _, a := range after {
					at := strings.TrimSpace(a)
					if strings.HasPrefix(at, "do") || strings.HasPrefix(at, "{") {
						hasBlock = true
						break
					}
				}
				if !hasBlock && !anyLineContains(after, ".close") {
					issues = append(issues, Issue{
						ID:         "resource/ruby-no-block-form",
						FixID:      "no-block-form",
						Severity:   SeverityWarning,
						Category:   "resource-ruby",
						File:       fileDiff.Path,
						Line:       line.NewNum,
						Message:    "File.open() without block form (file handle may leak)",
						Suggestion: "Use block form: File.open(path) { |f| ... } to ensure automatic close",
					})
				}
			}
		}
	}

	return issues
}

// --- PHP resource leak detection ---

var (
	phpFopen  = regexp.MustCompile(`\bfopen\s*\(`)
	phpFclose = regexp.MustCompile(`\bfclose\s*\(`)
	phpPDO    = regexp.MustCompile(`new\s+PDO\s*\(`)
)

func checkPHPResources(fileDiff git.FileDiff) []Issue {
	issues := make([]Issue, 0)

	for _, hunk := range fileDiff.Hunks {
		lines := hunk.Lines

		hunkContent := make([]string, 0, len(lines))
		for _, l := range lines {
			if l.Type != "removed" {
				hunkContent = append(hunkContent, l.Content)
			}
		}

		for _, line := range lines {
			if line.Type != "added" {
				continue
			}

			content := line.Content
			trimmed := strings.TrimSpace(content)

			if strings.HasPrefix(trimmed, "//") || strings.HasPrefix(trimmed, "/*") || strings.HasPrefix(trimmed, "*") || strings.HasPrefix(trimmed, "#") {
				continue
			}

			// fopen without fclose
			if phpFopen.MatchString(content) {
				if !anyLineMatches(hunkContent, phpFclose) {
					issues = append(issues, Issue{
						ID:         "resource/php-unclosed-handle",
						FixID:      "unclosed-handle",
						Severity:   SeverityWarning,
						Category:   "resource-php",
						File:       fileDiff.Path,
						Line:       line.NewNum,
						Message:    "fopen() without fclose() in nearby code",
						Suggestion: "Call fclose($handle) when done to release the file handle",
					})
				}
			}

			// PDO without null assignment (PHP closes on null or scope end, but explicit is better)
			if phpPDO.MatchString(content) {
				if !anyLineContains(hunkContent, "= null") && !anyLineContains(hunkContent, "= NULL") {
					issues = append(issues, Issue{
						ID:         "resource/php-unclosed-pdo",
						FixID:      "unclosed-handle",
						Severity:   SeverityInfo,
						Category:   "resource-php",
						File:       fileDiff.Path,
						Line:       line.NewNum,
						Message:    "PDO connection without explicit null assignment for cleanup",
						Suggestion: "Set $pdo = null when done, or ensure the connection goes out of scope promptly",
					})
				}
			}
		}
	}

	return issues
}

// --- Swift resource leak detection ---

var (
	swiftAutoreleasepool = regexp.MustCompile(`\bautoreleasepool\b`)
	swiftHeavyLoop       = regexp.MustCompile(`\bfor\b.*\bin\b`)
	swiftAllocation      = regexp.MustCompile(`\b(UIImage|NSImage|Data|NSData|CGImage|CIImage)\s*\(`)
)

func checkSwiftResources(fileDiff git.FileDiff) []Issue {
	issues := make([]Issue, 0)

	for _, hunk := range fileDiff.Hunks {
		lines := hunk.Lines

		for i, line := range lines {
			if line.Type != "added" {
				continue
			}

			content := line.Content
			trimmed := strings.TrimSpace(content)

			if strings.HasPrefix(trimmed, "//") || strings.HasPrefix(trimmed, "/*") || strings.HasPrefix(trimmed, "*") {
				continue
			}

			// Allocation inside a loop without autoreleasepool
			if swiftAllocation.MatchString(content) {
				// Look backwards for a for loop
				inLoop := false
				for j := i - 1; j >= max(0, i-10); j-- {
					if lines[j].Type == "removed" {
						continue
					}
					if swiftHeavyLoop.MatchString(lines[j].Content) {
						inLoop = true
						break
					}
				}
				if inLoop {
					// Check for autoreleasepool wrapper
					hasPool := false
					for j := i - 1; j >= max(0, i-5); j-- {
						if swiftAutoreleasepool.MatchString(lines[j].Content) {
							hasPool = true
							break
						}
					}
					if !hasPool {
						issues = append(issues, Issue{
							ID:         "resource/swift-missing-autoreleasepool",
							FixID:      "missing-autoreleasepool",
							Severity:   SeverityWarning,
							Category:   "resource-swift",
							File:       fileDiff.Path,
							Line:       line.NewNum,
							Message:    "Heavy allocation in loop without autoreleasepool",
							Suggestion: "Wrap loop body in autoreleasepool { } to prevent memory growth",
						})
					}
				}
			}
		}
	}

	return issues
}

// --- Kotlin resource leak detection ---

var (
	ktResourceNew = regexp.MustCompile(`\b(FileInputStream|FileOutputStream|BufferedReader|BufferedWriter|Socket|ServerSocket)\s*\(`)
	ktUse         = regexp.MustCompile(`\.use\s*\{`)
	ktCloseable   = regexp.MustCompile(`\.close\s*\(`)
)

func checkKotlinResources(fileDiff git.FileDiff) []Issue {
	issues := make([]Issue, 0)

	for _, hunk := range fileDiff.Hunks {
		lines := hunk.Lines

		for i, line := range lines {
			if line.Type != "added" {
				continue
			}

			content := line.Content
			trimmed := strings.TrimSpace(content)

			if strings.HasPrefix(trimmed, "//") || strings.HasPrefix(trimmed, "/*") || strings.HasPrefix(trimmed, "*") {
				continue
			}

			if ktResourceNew.MatchString(content) {
				// Check for .use { } on same line or nearby
				if ktUse.MatchString(content) {
					continue
				}
				after := linesAfter(lines, i, 5)
				if !anyLineMatches(after, ktUse) && !anyLineMatches(after, ktCloseable) {
					issues = append(issues, Issue{
						ID:         "resource/kotlin-unclosed-resource",
						FixID:      "unclosed-resource",
						Severity:   SeverityWarning,
						Category:   "resource-kotlin",
						File:       fileDiff.Path,
						Line:       line.NewNum,
						Message:    "Resource created without .use { } block or .close()",
						Suggestion: "Use Kotlin's .use { } extension function for automatic resource management",
					})
				}
			}
		}
	}

	return issues
}
