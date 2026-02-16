package analyzer

import (
	"path/filepath"
	"regexp"
	"strings"

	"github.com/joeabbey/fault/pkg/git"
)

// ConcurrencyAnalyzer detects concurrency safety issues in added code.
type ConcurrencyAnalyzer struct{}

// NewConcurrencyAnalyzer creates a new concurrency analyzer.
func NewConcurrencyAnalyzer() *ConcurrencyAnalyzer {
	return &ConcurrencyAnalyzer{}
}

// Name returns the analyzer name.
func (a *ConcurrencyAnalyzer) Name() string {
	return "concurrency"
}

// Analyze scans diff content for concurrency anti-patterns in added lines.
func (a *ConcurrencyAnalyzer) Analyze(ctx *AnalysisContext) ([]Issue, error) {
	issues := make([]Issue, 0)

	if ctx.Diff == nil || len(ctx.Diff.Files) == 0 {
		return issues, nil
	}

	for _, fileDiff := range ctx.Diff.Files {
		if fileDiff.Status == "deleted" || fileDiff.IsBinary {
			continue
		}

		if isConcurrencySkippedPath(fileDiff.Path) {
			continue
		}

		if isTestFile(fileDiff.Path) {
			continue
		}

		ext := strings.ToLower(filepath.Ext(fileDiff.Path))

		switch ext {
		case ".go":
			issues = append(issues, checkGoConcurrency(fileDiff)...)
		case ".ts", ".tsx", ".js", ".jsx", ".mjs":
			issues = append(issues, checkTSConcurrency(fileDiff)...)
		case ".py":
			issues = append(issues, checkPythonConcurrency(fileDiff)...)
		case ".rs":
			issues = append(issues, checkRustConcurrency(fileDiff)...)
		case ".java":
			issues = append(issues, checkJavaConcurrency(fileDiff)...)
		case ".c", ".cpp", ".cc", ".cxx", ".h", ".hpp", ".hxx":
			issues = append(issues, checkCConcurrency(fileDiff)...)
		case ".rb", ".rake":
			issues = append(issues, checkRubyConcurrency(fileDiff)...)
		case ".swift":
			issues = append(issues, checkSwiftConcurrency(fileDiff)...)
		case ".kt", ".kts":
			issues = append(issues, checkKotlinConcurrency(fileDiff)...)
		case ".cs":
			issues = append(issues, checkCSharpConcurrency(fileDiff)...)
		case ".scala", ".sc":
			issues = append(issues, checkScalaConcurrency(fileDiff)...)
		case ".ex", ".exs":
			issues = append(issues, checkElixirConcurrency(fileDiff)...)
		}
	}

	return issues, nil
}

// isConcurrencySkippedPath returns true for vendored/generated directories.
func isConcurrencySkippedPath(path string) bool {
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

// isComment returns true if the trimmed line is a single-line comment for C-style languages.
func isCStyleComment(trimmed string) bool {
	return strings.HasPrefix(trimmed, "//") || strings.HasPrefix(trimmed, "/*") || strings.HasPrefix(trimmed, "*")
}

// --- Go concurrency detection ---

var (
	goGoroutineLaunch  = regexp.MustCompile(`\bgo\s+(func\s*\(|[a-zA-Z_]\w*[\w.]*\()`)
	goGoroutineFunc    = regexp.MustCompile(`\bgo\s+func\s*\(`)
	goContextParam     = regexp.MustCompile(`\bctx\b|\bcontext\.`)
	goMakeChanUnbuf    = regexp.MustCompile(`\bmake\s*\(\s*chan\s+`)
	goMakeChanBuf      = regexp.MustCompile(`\bmake\s*\(\s*chan\s+\w+\s*,`)
	goMapWrite         = regexp.MustCompile(`\w+\s*\[[^\]]+\]\s*=`)
	goMutexLock        = regexp.MustCompile(`\.\s*(Lock|RLock|Unlock|RUnlock)\s*\(`)
	goSyncMutex        = regexp.MustCompile(`sync\.(Mutex|RWMutex)`)
)

func checkGoConcurrency(fileDiff git.FileDiff) []Issue {
	issues := make([]Issue, 0)

	for _, hunk := range fileDiff.Hunks {
		lines := hunk.Lines

		// Track whether we see goroutines and mutex usage in this hunk
		hasGoroutine := false
		hasLock := false
		goroutineLines := make([]int, 0)
		mapWriteLines := make([]int, 0)

		for _, line := range lines {
			if line.Type != "added" {
				continue
			}
			content := line.Content
			if goGoroutineLaunch.MatchString(content) {
				hasGoroutine = true
				goroutineLines = append(goroutineLines, line.NewNum)
			}
			if goMutexLock.MatchString(content) || goSyncMutex.MatchString(content) {
				hasLock = true
			}
			if goMapWrite.MatchString(content) {
				mapWriteLines = append(mapWriteLines, line.NewNum)
			}
		}

		for i, line := range lines {
			if line.Type != "added" {
				continue
			}

			content := line.Content
			trimmed := strings.TrimSpace(content)

			if isCStyleComment(trimmed) {
				continue
			}

			// goroutine-leak: go func() or go someFunc() without context.Context
			if goGoroutineLaunch.MatchString(content) {
				// Check the current line and the next few lines for context parameter
				hasContext := false
				for j := i; j < len(lines) && j <= i+3; j++ {
					if goContextParam.MatchString(lines[j].Content) {
						hasContext = true
						break
					}
				}
				if !hasContext {
					issues = append(issues, Issue{
						ID:         "concurrency/goroutine-leak",
						Severity:   SeverityWarning,
						Category:   "concurrency-go",
						File:       fileDiff.Path,
						Line:       line.NewNum,
						Message:    "Goroutine launched without context.Context — may run forever with no cancellation mechanism",
						Suggestion: "Pass a context.Context and select on ctx.Done() to allow the goroutine to be cancelled",
					})
				}
			}

			// channel-size-zero: make(chan X) without buffer size
			if goMakeChanUnbuf.MatchString(content) && !goMakeChanBuf.MatchString(content) {
				issues = append(issues, Issue{
					ID:         "concurrency/channel-size-zero",
					Severity:   SeverityInfo,
					Category:   "concurrency-go",
					File:       fileDiff.Path,
					Line:       line.NewNum,
					Message:    "Unbuffered channel — sender and receiver must be ready simultaneously or goroutine may block",
					Suggestion: "Consider using a buffered channel if the sender and receiver operate at different speeds, or document why unbuffered is intended",
				})
			}

			// shared-state / missing-lock: map write in scope that also has goroutine without lock
			if hasGoroutine && !hasLock && goMapWrite.MatchString(content) {
				// Only flag if there are goroutines and no locks visible in the hunk
				issues = append(issues, Issue{
					ID:         "concurrency/missing-lock",
					Severity:   SeverityWarning,
					Category:   "concurrency-go",
					File:       fileDiff.Path,
					Line:       line.NewNum,
					Message:    "Map/slice write in scope with goroutine but no visible mutex — potential data race",
					Suggestion: "Protect shared state with sync.Mutex or sync.RWMutex, or use sync.Map for concurrent map access",
				})
			}
		}

		// If we have goroutines and map writes but no locks, also flag goroutine launch
		// as shared-state risk (only if not already flagged above for missing-lock)
		_ = goroutineLines
		_ = mapWriteLines
	}

	return issues
}

// --- TypeScript/JavaScript concurrency detection ---

var (
	tsPromiseAll        = regexp.MustCompile(`Promise\.(all|allSettled)\s*\(\s*\[`)
	tsCatchHandler      = regexp.MustCompile(`\.catch\s*\(`)
	tsTryCatch          = regexp.MustCompile(`\btry\s*\{`)
	tsSetInterval       = regexp.MustCompile(`\bsetInterval\s*\(`)
	tsSetTimeout        = regexp.MustCompile(`\bsetTimeout\s*\(`)
	tsClearInterval     = regexp.MustCompile(`\bclearInterval\s*\(`)
	tsClearTimeout      = regexp.MustCompile(`\bclearTimeout\s*\(`)
	tsSetState          = regexp.MustCompile(`\bsetState\s*\(`)
	tsStoreSet          = regexp.MustCompile(`\.set\s*\(`)
	tsAsyncFunction     = regexp.MustCompile(`\basync\s+`)
)

func checkTSConcurrency(fileDiff git.FileDiff) []Issue {
	issues := make([]Issue, 0)

	for _, hunk := range fileDiff.Hunks {
		lines := hunk.Lines

		for i, line := range lines {
			if line.Type != "added" {
				continue
			}

			content := line.Content
			trimmed := strings.TrimSpace(content)

			if isCStyleComment(trimmed) {
				continue
			}

			// unhandled-concurrent: Promise.all/allSettled without catch or try-catch
			if tsPromiseAll.MatchString(content) {
				hasCatch := false
				hasTry := false

				// Look backwards for try {
				for j := i - 1; j >= 0 && j >= i-15; j-- {
					if tsTryCatch.MatchString(lines[j].Content) {
						hasTry = true
						break
					}
				}

				// Look ahead for .catch()
				for j := i; j < len(lines) && j <= i+5; j++ {
					if tsCatchHandler.MatchString(lines[j].Content) {
						hasCatch = true
						break
					}
				}

				if !hasCatch && !hasTry {
					issues = append(issues, Issue{
						ID:         "concurrency/unhandled-concurrent",
						Severity:   SeverityWarning,
						Category:   "concurrency-ts",
						File:       fileDiff.Path,
						Line:       line.NewNum,
						Message:    "Promise.all/allSettled without error handling — if any promise rejects, the error is unhandled",
						Suggestion: "Wrap in try-catch or chain a .catch() handler to handle concurrent promise rejections",
					})
				}
			}

			// missing-cleanup: setInterval/setTimeout without clear
			if tsSetInterval.MatchString(content) {
				hasClear := false
				for j := 0; j < len(lines); j++ {
					if tsClearInterval.MatchString(lines[j].Content) {
						hasClear = true
						break
					}
				}
				if !hasClear {
					issues = append(issues, Issue{
						ID:         "concurrency/missing-cleanup",
						Severity:   SeverityWarning,
						Category:   "concurrency-ts",
						File:       fileDiff.Path,
						Line:       line.NewNum,
						Message:    "setInterval() without clearInterval() — timer will run indefinitely and may cause memory leaks",
						Suggestion: "Store the interval ID and call clearInterval() in cleanup (e.g., componentWillUnmount, onDestroy, or useEffect return)",
					})
				}
			}

			if tsSetTimeout.MatchString(content) {
				hasClear := false
				for j := 0; j < len(lines); j++ {
					if tsClearTimeout.MatchString(lines[j].Content) {
						hasClear = true
						break
					}
				}
				if !hasClear {
					issues = append(issues, Issue{
						ID:         "concurrency/missing-cleanup",
						Severity:   SeverityInfo,
						Category:   "concurrency-ts",
						File:       fileDiff.Path,
						Line:       line.NewNum,
						Message:    "setTimeout() without clearTimeout() — may fire after component unmount causing state updates on unmounted components",
						Suggestion: "Store the timeout ID and call clearTimeout() in cleanup to prevent stale updates",
					})
				}
			}

			// race-condition: multiple setState calls in an async function
			if tsSetState.MatchString(content) {
				// Check if we're in an async context
				inAsync := false
				for j := i - 1; j >= 0 && j >= i-30; j-- {
					if tsAsyncFunction.MatchString(lines[j].Content) {
						inAsync = true
						break
					}
				}
				if inAsync {
					// Count setState calls in nearby lines
					stateSetCount := 0
					for j := i; j < len(lines) && j <= i+10; j++ {
						if lines[j].Type == "added" && tsSetState.MatchString(lines[j].Content) {
							stateSetCount++
						}
					}
					if stateSetCount >= 2 {
						issues = append(issues, Issue{
							ID:         "concurrency/race-condition",
							Severity:   SeverityWarning,
							Category:   "concurrency-ts",
							File:       fileDiff.Path,
							Line:       line.NewNum,
							Message:    "Multiple setState calls in async function — may cause race conditions or intermediate renders",
							Suggestion: "Batch state updates using a single setState call with an object, or use useReducer for complex state transitions",
						})
					}
				}
			}
		}
	}

	return issues
}

// --- Python concurrency detection ---

var (
	pyThreadStart       = regexp.MustCompile(`\.start\s*\(\s*\)`)
	pyThreadCreate      = regexp.MustCompile(`\bThread\s*\(`)
	pyThreadJoin        = regexp.MustCompile(`\.join\s*\(`)
	pyThreadingLock     = regexp.MustCompile(`\b(threading\.Lock|Lock)\s*\(`)
	pyAsyncDef          = regexp.MustCompile(`\basync\s+def\b`)
	pyTimeSleep         = regexp.MustCompile(`\btime\.sleep\s*\(`)
	pyRequestsCall      = regexp.MustCompile(`\brequests\.(get|post|put|delete|patch|head)\s*\(`)
	pyGlobalAssign      = regexp.MustCompile(`^[a-zA-Z_]\w*\s*=`)
	pyThreadTarget      = regexp.MustCompile(`target\s*=`)
)

func checkPythonConcurrency(fileDiff git.FileDiff) []Issue {
	issues := make([]Issue, 0)

	for _, hunk := range fileDiff.Hunks {
		lines := hunk.Lines

		// Track thread-related patterns in hunk
		hasThreadStart := false
		hasThreadJoin := false
		hasLock := false

		for _, line := range lines {
			if line.Type != "added" {
				continue
			}
			content := line.Content
			if pyThreadStart.MatchString(content) && pyThreadCreate.MatchString(content) {
				// Thread(...).start() on same line
				hasThreadStart = true
			} else if pyThreadStart.MatchString(content) {
				hasThreadStart = true
			}
			if pyThreadJoin.MatchString(content) {
				hasThreadJoin = true
			}
			if pyThreadingLock.MatchString(content) {
				hasLock = true
			}
		}

		inAsyncDef := false

		for _, line := range lines {
			if line.Type != "added" {
				continue
			}

			content := line.Content
			trimmed := strings.TrimSpace(content)

			if strings.HasPrefix(trimmed, "#") {
				continue
			}

			// Track async def context
			if pyAsyncDef.MatchString(content) {
				inAsyncDef = true
			}
			// Reset async context on new def
			if !pyAsyncDef.MatchString(content) && strings.HasPrefix(trimmed, "def ") {
				inAsyncDef = false
			}

			// async-blocking: blocking calls inside async def
			if inAsyncDef {
				if pyTimeSleep.MatchString(content) {
					issues = append(issues, Issue{
						ID:         "concurrency/async-blocking",
						Severity:   SeverityWarning,
						Category:   "concurrency-python",
						File:       fileDiff.Path,
						Line:       line.NewNum,
						Message:    "time.sleep() inside async function blocks the event loop",
						Suggestion: "Use 'await asyncio.sleep()' instead of 'time.sleep()' in async functions",
					})
				}
				if pyRequestsCall.MatchString(content) {
					issues = append(issues, Issue{
						ID:         "concurrency/async-blocking",
						Severity:   SeverityWarning,
						Category:   "concurrency-python",
						File:       fileDiff.Path,
						Line:       line.NewNum,
						Message:    "Blocking HTTP call (requests) inside async function blocks the event loop",
						Suggestion: "Use 'aiohttp' or 'httpx' with async support instead of 'requests' in async functions",
					})
				}
			}

			// missing-join: Thread().start() without .join()
			if pyThreadCreate.MatchString(content) && pyThreadStart.MatchString(content) {
				if !hasThreadJoin {
					issues = append(issues, Issue{
						ID:         "concurrency/missing-join",
						Severity:   SeverityWarning,
						Category:   "concurrency-python",
						File:       fileDiff.Path,
						Line:       line.NewNum,
						Message:    "Thread started without .join() — thread may outlive the calling scope",
						Suggestion: "Call thread.join() to wait for the thread to complete, or use a thread pool (concurrent.futures.ThreadPoolExecutor)",
					})
				}
			}

			// thread-unsafe-global: global variable assignment without lock in context with threads
			if hasThreadStart && !hasLock && pyGlobalAssign.MatchString(trimmed) && pyThreadTarget.MatchString(trimmed) {
				issues = append(issues, Issue{
					ID:         "concurrency/thread-unsafe-global",
					Severity:   SeverityWarning,
					Category:   "concurrency-python",
					File:       fileDiff.Path,
					Line:       line.NewNum,
					Message:    "Global variable modified in threaded context without threading.Lock",
					Suggestion: "Use threading.Lock to protect shared mutable state accessed by multiple threads",
				})
			}
		}
	}

	return issues
}

// --- Rust concurrency detection ---

var (
	rustArcNew        = regexp.MustCompile(`\bArc::new\s*\(`)
	rustMutexWrap     = regexp.MustCompile(`\bMutex::new\s*\(`)
	rustRwLockWrap    = regexp.MustCompile(`\bRwLock::new\s*\(`)
	rustUnsafeSend    = regexp.MustCompile(`\bunsafe\s+impl\s+Send\b`)
	rustUnsafeSync    = regexp.MustCompile(`\bunsafe\s+impl\s+Sync\b`)
)

func checkRustConcurrency(fileDiff git.FileDiff) []Issue {
	issues := make([]Issue, 0)

	for _, hunk := range fileDiff.Hunks {
		for i, line := range hunk.Lines {
			if line.Type != "added" {
				continue
			}

			content := line.Content
			trimmed := strings.TrimSpace(content)

			if isCStyleComment(trimmed) {
				continue
			}

			// unsafe-send: unsafe impl Send/Sync
			if rustUnsafeSend.MatchString(content) || rustUnsafeSync.MatchString(content) {
				issues = append(issues, Issue{
					ID:         "concurrency/unsafe-send",
					Severity:   SeverityError,
					Category:   "concurrency-rust",
					File:       fileDiff.Path,
					Line:       line.NewNum,
					Message:    "unsafe impl Send/Sync bypasses Rust's thread safety guarantees — extremely dangerous",
					Suggestion: "Remove the unsafe impl and restructure the type to be naturally Send/Sync, or add thorough documentation explaining why this is safe",
				})
			}

			// arc-without-mutex: Arc::new() not wrapping Mutex/RwLock
			if rustArcNew.MatchString(content) {
				hasMutex := false
				// Check if the Arc wraps a Mutex or RwLock on same or nearby lines
				for j := i; j < len(hunk.Lines) && j <= i+2; j++ {
					lineContent := hunk.Lines[j].Content
					if rustMutexWrap.MatchString(lineContent) || rustRwLockWrap.MatchString(lineContent) {
						hasMutex = true
						break
					}
				}
				// Also check current line for Arc::new(Mutex::new or Arc::new(RwLock::new
				if strings.Contains(content, "Mutex::new") || strings.Contains(content, "RwLock::new") {
					hasMutex = true
				}
				if !hasMutex {
					issues = append(issues, Issue{
						ID:         "concurrency/arc-without-mutex",
						Severity:   SeverityWarning,
						Category:   "concurrency-rust",
						File:       fileDiff.Path,
						Line:       line.NewNum,
						Message:    "Arc::new() without Mutex/RwLock — shared ownership without interior mutability protection",
						Suggestion: "Wrap the inner value in Mutex or RwLock: Arc::new(Mutex::new(value)) to safely share mutable state across threads",
					})
				}
			}
		}
	}

	return issues
}

// --- Java concurrency detection ---

var (
	javaHashMap             = regexp.MustCompile(`\bnew\s+HashMap\s*[<(]`)
	javaArrayList           = regexp.MustCompile(`\bnew\s+ArrayList\s*[<(]`)
	javaHashSet             = regexp.MustCompile(`\bnew\s+HashSet\s*[<(]`)
	javaLinkedList          = regexp.MustCompile(`\bnew\s+LinkedList\s*[<(]`)
	javaTreeMap             = regexp.MustCompile(`\bnew\s+TreeMap\s*[<(]`)
	javaConcurrentMap       = regexp.MustCompile(`\bnew\s+ConcurrentHashMap\s*[<(]`)
	javaSynchronized        = regexp.MustCompile(`\bsynchronized\b`)
	javaCollectionsSynced   = regexp.MustCompile(`\bCollections\.synchronized`)
	javaThreadStart         = regexp.MustCompile(`\.\s*start\s*\(\s*\)`)
	javaNewThread           = regexp.MustCompile(`\bnew\s+Thread\s*\(`)
	javaExecutorSubmit      = regexp.MustCompile(`\.(submit|execute)\s*\(`)
	javaDoubleCheckVolatile = regexp.MustCompile(`\bvolatile\b`)
	javaDoubleCheckSync     = regexp.MustCompile(`\bsynchronized\s*\(`)
	javaIfNull              = regexp.MustCompile(`if\s*\(\s*\w+\s*==\s*null\s*\)`)
)

func checkJavaConcurrency(fileDiff git.FileDiff) []Issue {
	issues := make([]Issue, 0)

	for _, hunk := range fileDiff.Hunks {
		lines := hunk.Lines

		// Track concurrent context indicators
		hasConcurrency := false
		hasVolatile := false

		for _, line := range lines {
			if line.Type != "added" {
				continue
			}
			content := line.Content
			if javaNewThread.MatchString(content) || javaExecutorSubmit.MatchString(content) ||
				javaThreadStart.MatchString(content) || javaSynchronized.MatchString(content) {
				hasConcurrency = true
			}
			if javaDoubleCheckVolatile.MatchString(content) {
				hasVolatile = true
			}
		}

		for i, line := range lines {
			if line.Type != "added" {
				continue
			}

			content := line.Content
			trimmed := strings.TrimSpace(content)

			if isCStyleComment(trimmed) {
				continue
			}

			// unsynchronized-collection: HashMap/ArrayList/etc in concurrent context
			if hasConcurrency {
				unsafeCollection := ""
				if javaHashMap.MatchString(content) {
					unsafeCollection = "HashMap"
				} else if javaArrayList.MatchString(content) {
					unsafeCollection = "ArrayList"
				} else if javaHashSet.MatchString(content) {
					unsafeCollection = "HashSet"
				} else if javaLinkedList.MatchString(content) {
					unsafeCollection = "LinkedList"
				} else if javaTreeMap.MatchString(content) {
					unsafeCollection = "TreeMap"
				}

				if unsafeCollection != "" && !javaConcurrentMap.MatchString(content) &&
					!javaCollectionsSynced.MatchString(content) {
					issues = append(issues, Issue{
						ID:         "concurrency/unsynchronized-collection",
						Severity:   SeverityWarning,
						Category:   "concurrency-java",
						File:       fileDiff.Path,
						Line:       line.NewNum,
						Message:    unsafeCollection + " is not thread-safe — concurrent access will cause data corruption",
						Suggestion: "Use ConcurrentHashMap, CopyOnWriteArrayList, or Collections.synchronized*() wrapper for thread-safe access",
					})
				}
			}

			// double-check-locking: if (x == null) { synchronized ... } without volatile
			if javaIfNull.MatchString(content) && !hasVolatile {
				// Look ahead for synchronized block
				hasSync := false
				for j := i + 1; j < len(lines) && j <= i+3; j++ {
					if javaDoubleCheckSync.MatchString(lines[j].Content) {
						hasSync = true
						break
					}
				}
				// Also look ahead for another null check (the inner one)
				if hasSync {
					hasInnerNull := false
					for j := i + 2; j < len(lines) && j <= i+6; j++ {
						if javaIfNull.MatchString(lines[j].Content) {
							hasInnerNull = true
							break
						}
					}
					if hasInnerNull {
						issues = append(issues, Issue{
							ID:         "concurrency/double-check-locking",
							Severity:   SeverityError,
							Category:   "concurrency-java",
							File:       fileDiff.Path,
							Line:       line.NewNum,
							Message:    "Double-checked locking without volatile — field may be seen in partially-constructed state by other threads",
							Suggestion: "Declare the field as 'volatile' or use an initialization-on-demand holder pattern",
						})
					}
				}
			}
		}
	}

	return issues
}

// --- C/C++ concurrency detection ---

var (
	cPthreadCreate   = regexp.MustCompile(`\bpthread_create\s*\(`)
	cPthreadJoin     = regexp.MustCompile(`\bpthread_join\s*\(`)
	cPthreadDetach   = regexp.MustCompile(`\bpthread_detach\s*\(`)
	cMutexLock       = regexp.MustCompile(`\bpthread_mutex_(lock|trylock)\s*\(`)
	cStdThread       = regexp.MustCompile(`\bstd::thread\b`)
	cStdAsync        = regexp.MustCompile(`\bstd::async\s*\(`)
	cGlobalVar       = regexp.MustCompile(`^(static\s+)?(int|char|float|double|long|unsigned|bool|void\s*\*|struct\s+\w+)\s+\w+`)
)

func checkCConcurrency(fileDiff git.FileDiff) []Issue {
	issues := make([]Issue, 0)

	for _, hunk := range fileDiff.Hunks {
		lines := hunk.Lines

		hasPthreadCreate := false
		hasPthreadJoin := false
		hasPthreadDetach := false
		hasMutex := false

		for _, line := range lines {
			if line.Type != "added" {
				continue
			}
			content := line.Content
			if cPthreadCreate.MatchString(content) || cStdThread.MatchString(content) || cStdAsync.MatchString(content) {
				hasPthreadCreate = true
			}
			if cPthreadJoin.MatchString(content) {
				hasPthreadJoin = true
			}
			if cPthreadDetach.MatchString(content) {
				hasPthreadDetach = true
			}
			if cMutexLock.MatchString(content) || strings.Contains(content, "std::mutex") || strings.Contains(content, "std::lock_guard") {
				hasMutex = true
			}
		}

		for _, line := range lines {
			if line.Type != "added" {
				continue
			}

			content := line.Content
			trimmed := strings.TrimSpace(content)

			if isCStyleComment(trimmed) {
				continue
			}

			// missing-join: pthread_create without pthread_join or pthread_detach
			if cPthreadCreate.MatchString(content) {
				if !hasPthreadJoin && !hasPthreadDetach {
					issues = append(issues, Issue{
						ID:         "concurrency/missing-join",
						Severity:   SeverityWarning,
						Category:   "concurrency-c",
						File:       fileDiff.Path,
						Line:       line.NewNum,
						Message:    "pthread_create() without pthread_join() or pthread_detach() — thread resources will leak",
						Suggestion: "Call pthread_join() to wait for the thread or pthread_detach() to let it clean up on exit",
					})
				}
			}

			// data-race: global/shared variable access without mutex in threaded context
			if hasPthreadCreate && !hasMutex && cGlobalVar.MatchString(trimmed) {
				// Only flag static or file-scope looking declarations
				if strings.HasPrefix(trimmed, "static ") {
					issues = append(issues, Issue{
						ID:         "concurrency/data-race",
						Severity:   SeverityWarning,
						Category:   "concurrency-c",
						File:       fileDiff.Path,
						Line:       line.NewNum,
						Message:    "Static variable in threaded context without mutex protection — potential data race",
						Suggestion: "Use pthread_mutex_lock/unlock to protect shared state, or make the variable thread-local with __thread",
					})
				}
			}
		}
	}

	return issues
}

// --- Ruby concurrency detection ---

var (
	rubyThreadNew    = regexp.MustCompile(`\bThread\.new\b`)
	rubyMutex        = regexp.MustCompile(`\bMutex\.new\b`)
	rubyMutexSync    = regexp.MustCompile(`\.synchronize\b`)
	rubyInstanceVar  = regexp.MustCompile(`@[a-zA-Z_]\w*\s*=`)
)

func checkRubyConcurrency(fileDiff git.FileDiff) []Issue {
	issues := make([]Issue, 0)

	for _, hunk := range fileDiff.Hunks {
		lines := hunk.Lines

		hasThread := false
		hasMutex := false

		for _, line := range lines {
			if line.Type != "added" {
				continue
			}
			content := line.Content
			if rubyThreadNew.MatchString(content) {
				hasThread = true
			}
			if rubyMutex.MatchString(content) || rubyMutexSync.MatchString(content) {
				hasMutex = true
			}
		}

		for _, line := range lines {
			if line.Type != "added" {
				continue
			}

			content := line.Content
			trimmed := strings.TrimSpace(content)

			if strings.HasPrefix(trimmed, "#") {
				continue
			}

			// thread-unsafe-instance: instance var modification in thread without mutex
			if hasThread && !hasMutex && rubyInstanceVar.MatchString(content) {
				issues = append(issues, Issue{
					ID:         "concurrency/thread-unsafe-instance",
					Severity:   SeverityWarning,
					Category:   "concurrency-ruby",
					File:       fileDiff.Path,
					Line:       line.NewNum,
					Message:    "Instance variable modification in threaded context without Mutex — potential race condition",
					Suggestion: "Use a Mutex#synchronize block to protect instance variable modifications in multi-threaded code",
				})
			}
		}
	}

	return issues
}

// --- Swift concurrency detection ---

var (
	swiftAsyncFunc     = regexp.MustCompile(`\bfunc\s+\w+\s*\([^)]*\)\s*async\b`)
	swiftAwait         = regexp.MustCompile(`\bawait\b`)
	swiftTask          = regexp.MustCompile(`\bTask\s*\{`)
	swiftTaskDetached  = regexp.MustCompile(`\bTask\.detached\s*\{`)
	swiftActor         = regexp.MustCompile(`\bactor\b`)
	swiftNonisolated   = regexp.MustCompile(`\bnonisolated\b`)
	swiftMainActor     = regexp.MustCompile(`@MainActor`)
	swiftVarDecl       = regexp.MustCompile(`\bvar\s+\w+`)
)

func checkSwiftConcurrency(fileDiff git.FileDiff) []Issue {
	issues := make([]Issue, 0)

	for _, hunk := range fileDiff.Hunks {
		lines := hunk.Lines

		hasAsyncContext := false

		for _, line := range lines {
			if line.Type != "added" {
				continue
			}
			content := line.Content
			if swiftTask.MatchString(content) || swiftTaskDetached.MatchString(content) ||
				swiftAsyncFunc.MatchString(content) {
				hasAsyncContext = true
			}
		}

		for _, line := range lines {
			if line.Type != "added" {
				continue
			}

			content := line.Content
			trimmed := strings.TrimSpace(content)

			if isCStyleComment(trimmed) {
				continue
			}

			// actor-violation: accessing non-isolated mutable state from async context
			if hasAsyncContext && swiftVarDecl.MatchString(content) &&
				!swiftActor.MatchString(content) && !swiftMainActor.MatchString(content) &&
				!swiftNonisolated.MatchString(content) {
				// Check if we're inside a Task {} block by looking back
				inTask := false
				for j := len(lines) - 1; j >= 0; j-- {
					if swiftTask.MatchString(lines[j].Content) || swiftTaskDetached.MatchString(lines[j].Content) {
						inTask = true
						break
					}
				}
				if inTask {
					issues = append(issues, Issue{
						ID:         "concurrency/actor-violation",
						Severity:   SeverityWarning,
						Category:   "concurrency-swift",
						File:       fileDiff.Path,
						Line:       line.NewNum,
						Message:    "Mutable state access from async Task context without actor isolation",
						Suggestion: "Use an actor to protect shared mutable state, or annotate with @MainActor if the state should be accessed from the main thread",
					})
				}
			}
		}
	}

	return issues
}

// --- Kotlin concurrency detection ---

var (
	kotlinCoroutineLaunch  = regexp.MustCompile(`\blaunch\s*\{`)
	kotlinCoroutineAsync   = regexp.MustCompile(`\basync\s*\{`)
	kotlinGlobalScope      = regexp.MustCompile(`\bGlobalScope\.(launch|async)\b`)
	kotlinMutex            = regexp.MustCompile(`\bMutex\s*\(`)
	kotlinMutableMap       = regexp.MustCompile(`\bmutableMapOf\b|\bHashMap\s*[<(]`)
	kotlinMutableList      = regexp.MustCompile(`\bmutableListOf\b|\bArrayList\s*[<(]`)
)

func checkKotlinConcurrency(fileDiff git.FileDiff) []Issue {
	issues := make([]Issue, 0)

	for _, hunk := range fileDiff.Hunks {
		for _, line := range hunk.Lines {
			if line.Type != "added" {
				continue
			}

			content := line.Content
			trimmed := strings.TrimSpace(content)

			if isCStyleComment(trimmed) {
				continue
			}

			// GlobalScope usage — leaks coroutines
			if kotlinGlobalScope.MatchString(content) {
				issues = append(issues, Issue{
					ID:         "concurrency/global-scope",
					Severity:   SeverityWarning,
					Category:   "concurrency-kotlin",
					File:       fileDiff.Path,
					Line:       line.NewNum,
					Message:    "GlobalScope.launch/async creates unstructured coroutines that may leak",
					Suggestion: "Use a structured CoroutineScope (viewModelScope, lifecycleScope) or create a scoped coroutine with coroutineScope {}",
				})
			}
		}
	}

	return issues
}

// --- C# concurrency detection ---

var (
	csharpAsyncVoid   = regexp.MustCompile(`\basync\s+void\b`)
	csharpTaskRun     = regexp.MustCompile(`\bTask\.Run\s*\(`)
	csharpLockStmt    = regexp.MustCompile(`\block\s*\(`)
)

func checkCSharpConcurrency(fileDiff git.FileDiff) []Issue {
	issues := make([]Issue, 0)

	for _, hunk := range fileDiff.Hunks {
		for _, line := range hunk.Lines {
			if line.Type != "added" {
				continue
			}

			content := line.Content
			trimmed := strings.TrimSpace(content)

			if isCStyleComment(trimmed) {
				continue
			}

			// async void is fire-and-forget, exceptions are unobserved
			if csharpAsyncVoid.MatchString(content) {
				issues = append(issues, Issue{
					ID:         "concurrency/async-void",
					Severity:   SeverityWarning,
					Category:   "concurrency-csharp",
					File:       fileDiff.Path,
					Line:       line.NewNum,
					Message:    "async void method — exceptions will crash the process and the method cannot be awaited",
					Suggestion: "Change to 'async Task' so callers can await and handle exceptions (async void is only appropriate for event handlers)",
				})
			}
		}
	}

	return issues
}

// --- Scala concurrency detection ---

func checkScalaConcurrency(fileDiff git.FileDiff) []Issue {
	issues := make([]Issue, 0)

	globalExecCtx := regexp.MustCompile(`\bExecutionContext\.global\b`)

	for _, hunk := range fileDiff.Hunks {
		for _, line := range hunk.Lines {
			if line.Type != "added" {
				continue
			}

			content := line.Content
			trimmed := strings.TrimSpace(content)

			if isCStyleComment(trimmed) {
				continue
			}

			if globalExecCtx.MatchString(content) {
				issues = append(issues, Issue{
					ID:         "concurrency/global-exec-context",
					Severity:   SeverityInfo,
					Category:   "concurrency-scala",
					File:       fileDiff.Path,
					Line:       line.NewNum,
					Message:    "Using ExecutionContext.global — shared thread pool may cause thread starvation under load",
					Suggestion: "Consider using a dedicated ExecutionContext for blocking operations to avoid starving the global pool",
				})
			}
		}
	}

	return issues
}

// --- Elixir concurrency detection ---

func checkElixirConcurrency(fileDiff git.FileDiff) []Issue {
	issues := make([]Issue, 0)

	spawnNoLink := regexp.MustCompile(`\bspawn\s*\(`)
	spawnLink := regexp.MustCompile(`\bspawn_link\s*\(`)
	taskAsync := regexp.MustCompile(`\bTask\.async\s*\(`)
	taskAwait := regexp.MustCompile(`\bTask\.await\b`)

	for _, hunk := range fileDiff.Hunks {
		hasTaskAwait := false
		for _, line := range hunk.Lines {
			if line.Type == "added" && taskAwait.MatchString(line.Content) {
				hasTaskAwait = true
			}
		}

		for _, line := range hunk.Lines {
			if line.Type != "added" {
				continue
			}

			content := line.Content
			trimmed := strings.TrimSpace(content)

			if strings.HasPrefix(trimmed, "#") {
				continue
			}

			// spawn without link — crash will go unnoticed
			if spawnNoLink.MatchString(content) && !spawnLink.MatchString(content) {
				issues = append(issues, Issue{
					ID:         "concurrency/unlinked-spawn",
					Severity:   SeverityInfo,
					Category:   "concurrency-elixir",
					File:       fileDiff.Path,
					Line:       line.NewNum,
					Message:    "spawn() without link — crashes in the spawned process will go unnoticed",
					Suggestion: "Use spawn_link/1 or Task.async/1 to link the process and propagate failures",
				})
			}

			// Task.async without Task.await
			if taskAsync.MatchString(content) && !hasTaskAwait {
				issues = append(issues, Issue{
					ID:         "concurrency/missing-task-await",
					Severity:   SeverityWarning,
					Category:   "concurrency-elixir",
					File:       fileDiff.Path,
					Line:       line.NewNum,
					Message:    "Task.async() without Task.await — result is never collected and errors may be lost",
					Suggestion: "Call Task.await/1 to collect the result, or use Task.start/1 if the result is not needed",
				})
			}
		}
	}

	return issues
}
