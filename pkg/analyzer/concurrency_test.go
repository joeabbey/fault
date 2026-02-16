package analyzer

import (
	"testing"

	"github.com/joeabbey/fault/pkg/config"
	"github.com/joeabbey/fault/pkg/git"
	"github.com/joeabbey/fault/pkg/parser"
)

func TestConcurrencyAnalyzerName(t *testing.T) {
	a := NewConcurrencyAnalyzer()
	if a.Name() != "concurrency" {
		t.Errorf("expected name 'concurrency', got %q", a.Name())
	}
}

func TestConcurrencyEmptyDiff(t *testing.T) {
	a := NewConcurrencyAnalyzer()
	ctx := &AnalysisContext{
		RepoPath:    "/nonexistent",
		Diff:        &git.Diff{Files: make([]git.FileDiff, 0)},
		ParsedFiles: make(map[string]*parser.ParsedFile),
		Config:      config.DefaultConfig(),
	}

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(issues) != 0 {
		t.Errorf("expected 0 issues for empty diff, got %d", len(issues))
	}
}

func TestConcurrencyNilDiff(t *testing.T) {
	a := NewConcurrencyAnalyzer()
	ctx := &AnalysisContext{
		RepoPath:    "/nonexistent",
		Diff:        nil,
		ParsedFiles: make(map[string]*parser.ParsedFile),
		Config:      config.DefaultConfig(),
	}

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(issues) != 0 {
		t.Errorf("expected 0 issues for nil diff, got %d", len(issues))
	}
}

// --- Go concurrency tests ---

func TestConcurrencyGoGoroutineLeak(t *testing.T) {
	a := NewConcurrencyAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "server.go", line: 20, content: `	go func() {`},
		{file: "server.go", line: 21, content: `		processItems(items)`},
		{file: "server.go", line: 22, content: `	}()`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "concurrency/goroutine-leak")
	if found == nil {
		t.Fatal("expected goroutine-leak issue for go func() without context")
	}
	if found.Severity != SeverityWarning {
		t.Errorf("expected warning severity, got %q", found.Severity)
	}
	if found.Category != "concurrency-go" {
		t.Errorf("expected category 'concurrency-go', got %q", found.Category)
	}
}

func TestConcurrencyGoGoroutineWithContext(t *testing.T) {
	a := NewConcurrencyAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "server.go", line: 20, content: `	go func(ctx context.Context) {`},
		{file: "server.go", line: 21, content: `		processItems(ctx, items)`},
		{file: "server.go", line: 22, content: `	}(ctx)`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "concurrency/goroutine-leak")
	if found != nil {
		t.Error("expected no goroutine-leak issue when context is present")
	}
}

func TestConcurrencyGoGoroutineNamedFunc(t *testing.T) {
	a := NewConcurrencyAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "worker.go", line: 10, content: `	go handleRequest(req)`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "concurrency/goroutine-leak")
	if found == nil {
		t.Fatal("expected goroutine-leak issue for go handleRequest() without context")
	}
}

func TestConcurrencyGoUnbufferedChannel(t *testing.T) {
	a := NewConcurrencyAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "worker.go", line: 15, content: `	ch := make(chan int)`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "concurrency/channel-size-zero")
	if found == nil {
		t.Fatal("expected channel-size-zero issue for unbuffered channel")
	}
	if found.Severity != SeverityInfo {
		t.Errorf("expected info severity, got %q", found.Severity)
	}
}

func TestConcurrencyGoBufferedChannel(t *testing.T) {
	a := NewConcurrencyAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "worker.go", line: 15, content: `	ch := make(chan int, 10)`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "concurrency/channel-size-zero")
	if found != nil {
		t.Error("expected no channel-size-zero issue for buffered channel")
	}
}

func TestConcurrencyGoMissingLock(t *testing.T) {
	a := NewConcurrencyAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "cache.go", line: 10, content: `	go func() {`},
		{file: "cache.go", line: 11, content: `		process()`},
		{file: "cache.go", line: 12, content: `	}()`},
		{file: "cache.go", line: 15, content: `	cache["key"] = value`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "concurrency/missing-lock")
	if found == nil {
		t.Fatal("expected missing-lock issue for map write with goroutine and no mutex")
	}
	if found.Severity != SeverityWarning {
		t.Errorf("expected warning severity, got %q", found.Severity)
	}
}

func TestConcurrencyGoMapWriteWithMutex(t *testing.T) {
	a := NewConcurrencyAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "cache.go", line: 10, content: `	go func() {`},
		{file: "cache.go", line: 11, content: `		process()`},
		{file: "cache.go", line: 12, content: `	}()`},
		{file: "cache.go", line: 14, content: `	mu.Lock()`},
		{file: "cache.go", line: 15, content: `	cache["key"] = value`},
		{file: "cache.go", line: 16, content: `	mu.Unlock()`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "concurrency/missing-lock")
	if found != nil {
		t.Error("expected no missing-lock issue when mutex is present")
	}
}

func TestConcurrencyGoTestFileSkipped(t *testing.T) {
	a := NewConcurrencyAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "server_test.go", line: 20, content: `	go func() {`},
		{file: "server_test.go", line: 21, content: `		processItems(items)`},
		{file: "server_test.go", line: 22, content: `	}()`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	if len(issues) != 0 {
		t.Error("expected no issues for test files, but got some")
	}
}

func TestConcurrencyGoVendorSkipped(t *testing.T) {
	a := NewConcurrencyAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "vendor/pkg/worker.go", line: 10, content: `	go handleRequest(req)`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	if len(issues) != 0 {
		t.Error("expected no issues for vendor/ files, but got some")
	}
}

func TestConcurrencyGoCommentSkipped(t *testing.T) {
	a := NewConcurrencyAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "server.go", line: 20, content: `	// go func() {`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	if len(issues) != 0 {
		t.Error("expected no issues for commented-out goroutine")
	}
}

// --- TypeScript concurrency tests ---

func TestConcurrencyTSPromiseAllUnhandled(t *testing.T) {
	a := NewConcurrencyAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "service.ts", line: 10, content: `  const results = await Promise.all([`},
		{file: "service.ts", line: 11, content: `    fetchUsers(),`},
		{file: "service.ts", line: 12, content: `    fetchPosts(),`},
		{file: "service.ts", line: 13, content: `  ]);`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "concurrency/unhandled-concurrent")
	if found == nil {
		t.Fatal("expected unhandled-concurrent issue for Promise.all without catch")
	}
	if found.Severity != SeverityWarning {
		t.Errorf("expected warning severity, got %q", found.Severity)
	}
}

func TestConcurrencyTSPromiseAllWithCatch(t *testing.T) {
	a := NewConcurrencyAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "service.ts", line: 10, content: `  const results = await Promise.all([`},
		{file: "service.ts", line: 11, content: `    fetchUsers(),`},
		{file: "service.ts", line: 12, content: `  ]).catch(err => console.error(err));`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "concurrency/unhandled-concurrent")
	if found != nil {
		t.Error("expected no unhandled-concurrent issue when .catch() is present")
	}
}

func TestConcurrencyTSPromiseAllInTryCatch(t *testing.T) {
	a := NewConcurrencyAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "service.ts", line: 8, content: `  try {`},
		{file: "service.ts", line: 10, content: `    const results = await Promise.all([`},
		{file: "service.ts", line: 11, content: `      fetchUsers(),`},
		{file: "service.ts", line: 12, content: `    ]);`},
		{file: "service.ts", line: 14, content: `  } catch (err) {`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "concurrency/unhandled-concurrent")
	if found != nil {
		t.Error("expected no unhandled-concurrent issue when in try-catch block")
	}
}

func TestConcurrencyTSSetIntervalNoCleanup(t *testing.T) {
	a := NewConcurrencyAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "component.tsx", line: 10, content: `  setInterval(() => { refresh(); }, 5000);`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "concurrency/missing-cleanup")
	if found == nil {
		t.Fatal("expected missing-cleanup issue for setInterval without clearInterval")
	}
	if found.Severity != SeverityWarning {
		t.Errorf("expected warning severity, got %q", found.Severity)
	}
}

func TestConcurrencyTSSetIntervalWithCleanup(t *testing.T) {
	a := NewConcurrencyAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "component.tsx", line: 10, content: `  const id = setInterval(() => { refresh(); }, 5000);`},
		{file: "component.tsx", line: 20, content: `  clearInterval(id);`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "concurrency/missing-cleanup")
	if found != nil {
		t.Error("expected no missing-cleanup issue when clearInterval is present")
	}
}

func TestConcurrencyTSSetTimeoutNoCleanup(t *testing.T) {
	a := NewConcurrencyAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "component.tsx", line: 10, content: `  setTimeout(() => { doSomething(); }, 1000);`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "concurrency/missing-cleanup")
	if found == nil {
		t.Fatal("expected missing-cleanup issue for setTimeout without clearTimeout")
	}
	if found.Severity != SeverityInfo {
		t.Errorf("expected info severity for setTimeout, got %q", found.Severity)
	}
}

func TestConcurrencyTSSetTimeoutWithCleanup(t *testing.T) {
	a := NewConcurrencyAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "component.tsx", line: 10, content: `  const id = setTimeout(() => { doSomething(); }, 1000);`},
		{file: "component.tsx", line: 20, content: `  clearTimeout(id);`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "concurrency/missing-cleanup")
	if found != nil {
		t.Error("expected no missing-cleanup issue when clearTimeout is present")
	}
}

// --- Python concurrency tests ---

func TestConcurrencyPythonAsyncBlocking(t *testing.T) {
	a := NewConcurrencyAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "handler.py", line: 10, content: `async def fetch_data():`},
		{file: "handler.py", line: 11, content: `    time.sleep(5)`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "concurrency/async-blocking")
	if found == nil {
		t.Fatal("expected async-blocking issue for time.sleep() in async function")
	}
	if found.Severity != SeverityWarning {
		t.Errorf("expected warning severity, got %q", found.Severity)
	}
	if found.Category != "concurrency-python" {
		t.Errorf("expected category 'concurrency-python', got %q", found.Category)
	}
}

func TestConcurrencyPythonAsyncRequestsBlocking(t *testing.T) {
	a := NewConcurrencyAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "handler.py", line: 10, content: `async def fetch_data():`},
		{file: "handler.py", line: 11, content: `    response = requests.get("https://api.example.com")`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "concurrency/async-blocking")
	if found == nil {
		t.Fatal("expected async-blocking issue for requests.get() in async function")
	}
}

func TestConcurrencyPythonSyncTimeSleepOk(t *testing.T) {
	a := NewConcurrencyAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "handler.py", line: 10, content: `def fetch_data():`},
		{file: "handler.py", line: 11, content: `    time.sleep(5)`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "concurrency/async-blocking")
	if found != nil {
		t.Error("expected no async-blocking issue for time.sleep() in sync function")
	}
}

func TestConcurrencyPythonMissingJoin(t *testing.T) {
	a := NewConcurrencyAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "worker.py", line: 10, content: `    Thread(target=process_item).start()`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "concurrency/missing-join")
	if found == nil {
		t.Fatal("expected missing-join issue for Thread.start() without .join()")
	}
	if found.Severity != SeverityWarning {
		t.Errorf("expected warning severity, got %q", found.Severity)
	}
}

func TestConcurrencyPythonThreadWithJoin(t *testing.T) {
	a := NewConcurrencyAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "worker.py", line: 10, content: `    t = Thread(target=process_item)`},
		{file: "worker.py", line: 11, content: `    t.start()`},
		{file: "worker.py", line: 15, content: `    t.join()`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	// This should not trigger missing-join since join is present
	found := findIssueByID(issues, "concurrency/missing-join")
	if found != nil {
		t.Error("expected no missing-join issue when .join() is present in scope")
	}
}

func TestConcurrencyPythonTestFileSkipped(t *testing.T) {
	a := NewConcurrencyAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "test_handler.py", line: 10, content: `async def fetch_data():`},
		{file: "test_handler.py", line: 11, content: `    time.sleep(5)`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	if len(issues) != 0 {
		t.Error("expected no issues for test files, but got some")
	}
}

// --- Rust concurrency tests ---

func TestConcurrencyRustUnsafeSend(t *testing.T) {
	a := NewConcurrencyAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "lib.rs", line: 10, content: `unsafe impl Send for MyStruct {}`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "concurrency/unsafe-send")
	if found == nil {
		t.Fatal("expected unsafe-send issue for 'unsafe impl Send'")
	}
	if found.Severity != SeverityError {
		t.Errorf("expected error severity, got %q", found.Severity)
	}
	if found.Category != "concurrency-rust" {
		t.Errorf("expected category 'concurrency-rust', got %q", found.Category)
	}
}

func TestConcurrencyRustUnsafeSync(t *testing.T) {
	a := NewConcurrencyAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "lib.rs", line: 10, content: `unsafe impl Sync for SharedState {}`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "concurrency/unsafe-send")
	if found == nil {
		t.Fatal("expected unsafe-send issue for 'unsafe impl Sync'")
	}
	if found.Severity != SeverityError {
		t.Errorf("expected error severity, got %q", found.Severity)
	}
}

func TestConcurrencyRustArcWithoutMutex(t *testing.T) {
	a := NewConcurrencyAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "lib.rs", line: 10, content: `    let shared = Arc::new(data);`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "concurrency/arc-without-mutex")
	if found == nil {
		t.Fatal("expected arc-without-mutex issue for Arc::new() without Mutex")
	}
	if found.Severity != SeverityWarning {
		t.Errorf("expected warning severity, got %q", found.Severity)
	}
}

func TestConcurrencyRustArcWithMutex(t *testing.T) {
	a := NewConcurrencyAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "lib.rs", line: 10, content: `    let shared = Arc::new(Mutex::new(data));`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "concurrency/arc-without-mutex")
	if found != nil {
		t.Error("expected no arc-without-mutex issue when Mutex is present")
	}
}

func TestConcurrencyRustArcWithRwLock(t *testing.T) {
	a := NewConcurrencyAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "lib.rs", line: 10, content: `    let shared = Arc::new(RwLock::new(data));`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "concurrency/arc-without-mutex")
	if found != nil {
		t.Error("expected no arc-without-mutex issue when RwLock is present")
	}
}

// --- Java concurrency tests ---

func TestConcurrencyJavaUnsynchronizedHashMap(t *testing.T) {
	a := NewConcurrencyAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "Service.java", line: 10, content: `    new Thread(() -> process()).start();`},
		{file: "Service.java", line: 15, content: `    Map<String, Object> cache = new HashMap<>();`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "concurrency/unsynchronized-collection")
	if found == nil {
		t.Fatal("expected unsynchronized-collection issue for HashMap in threaded context")
	}
	if found.Severity != SeverityWarning {
		t.Errorf("expected warning severity, got %q", found.Severity)
	}
}

func TestConcurrencyJavaConcurrentHashMap(t *testing.T) {
	a := NewConcurrencyAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "Service.java", line: 10, content: `    new Thread(() -> process()).start();`},
		{file: "Service.java", line: 15, content: `    Map<String, Object> cache = new ConcurrentHashMap<>();`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "concurrency/unsynchronized-collection")
	if found != nil {
		t.Error("expected no unsynchronized-collection issue for ConcurrentHashMap")
	}
}

func TestConcurrencyJavaUnsynchronizedArrayList(t *testing.T) {
	a := NewConcurrencyAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "Service.java", line: 10, content: `    executor.submit(() -> process());`},
		{file: "Service.java", line: 15, content: `    List<String> items = new ArrayList<>();`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "concurrency/unsynchronized-collection")
	if found == nil {
		t.Fatal("expected unsynchronized-collection issue for ArrayList in concurrent context")
	}
}

func TestConcurrencyJavaDoubleCheckLocking(t *testing.T) {
	a := NewConcurrencyAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "Singleton.java", line: 10, content: `    if (instance == null) {`},
		{file: "Singleton.java", line: 11, content: `        synchronized (Singleton.class) {`},
		{file: "Singleton.java", line: 12, content: `            if (instance == null) {`},
		{file: "Singleton.java", line: 13, content: `                instance = new Singleton();`},
		{file: "Singleton.java", line: 14, content: `            }`},
		{file: "Singleton.java", line: 15, content: `        }`},
		{file: "Singleton.java", line: 16, content: `    }`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "concurrency/double-check-locking")
	if found == nil {
		t.Fatal("expected double-check-locking issue for DCL pattern without volatile")
	}
	if found.Severity != SeverityError {
		t.Errorf("expected error severity, got %q", found.Severity)
	}
}

func TestConcurrencyJavaDoubleCheckLockingWithVolatile(t *testing.T) {
	a := NewConcurrencyAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "Singleton.java", line: 5, content: `    private volatile Singleton instance;`},
		{file: "Singleton.java", line: 10, content: `    if (instance == null) {`},
		{file: "Singleton.java", line: 11, content: `        synchronized (Singleton.class) {`},
		{file: "Singleton.java", line: 12, content: `            if (instance == null) {`},
		{file: "Singleton.java", line: 13, content: `                instance = new Singleton();`},
		{file: "Singleton.java", line: 14, content: `            }`},
		{file: "Singleton.java", line: 15, content: `        }`},
		{file: "Singleton.java", line: 16, content: `    }`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "concurrency/double-check-locking")
	if found != nil {
		t.Error("expected no double-check-locking issue when volatile is present")
	}
}

func TestConcurrencyJavaNoThreadsNoProblem(t *testing.T) {
	a := NewConcurrencyAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "Service.java", line: 15, content: `    Map<String, Object> cache = new HashMap<>();`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "concurrency/unsynchronized-collection")
	if found != nil {
		t.Error("expected no unsynchronized-collection issue without threading context")
	}
}

// --- C/C++ concurrency tests ---

func TestConcurrencyCPthreadMissingJoin(t *testing.T) {
	a := NewConcurrencyAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "main.c", line: 10, content: `    pthread_create(&thread, NULL, worker, NULL);`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "concurrency/missing-join")
	if found == nil {
		t.Fatal("expected missing-join issue for pthread_create without join/detach")
	}
	if found.Severity != SeverityWarning {
		t.Errorf("expected warning severity, got %q", found.Severity)
	}
	if found.Category != "concurrency-c" {
		t.Errorf("expected category 'concurrency-c', got %q", found.Category)
	}
}

func TestConcurrencyCPthreadWithJoin(t *testing.T) {
	a := NewConcurrencyAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "main.c", line: 10, content: `    pthread_create(&thread, NULL, worker, NULL);`},
		{file: "main.c", line: 20, content: `    pthread_join(thread, NULL);`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "concurrency/missing-join")
	if found != nil {
		t.Error("expected no missing-join issue when pthread_join is present")
	}
}

func TestConcurrencyCPthreadWithDetach(t *testing.T) {
	a := NewConcurrencyAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "main.c", line: 10, content: `    pthread_create(&thread, NULL, worker, NULL);`},
		{file: "main.c", line: 11, content: `    pthread_detach(thread);`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "concurrency/missing-join")
	if found != nil {
		t.Error("expected no missing-join issue when pthread_detach is present")
	}
}

func TestConcurrencyCDataRace(t *testing.T) {
	a := NewConcurrencyAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "main.c", line: 5, content: `static int counter = 0;`},
		{file: "main.c", line: 10, content: `    pthread_create(&thread, NULL, worker, NULL);`},
		{file: "main.c", line: 11, content: `    pthread_join(thread, NULL);`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "concurrency/data-race")
	if found == nil {
		t.Fatal("expected data-race issue for static variable in threaded context without mutex")
	}
}

// --- Ruby concurrency tests ---

func TestConcurrencyRubyThreadUnsafeInstance(t *testing.T) {
	a := NewConcurrencyAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "worker.rb", line: 10, content: `    Thread.new { process }`},
		{file: "worker.rb", line: 15, content: `    @counter = @counter + 1`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "concurrency/thread-unsafe-instance")
	if found == nil {
		t.Fatal("expected thread-unsafe-instance issue for instance var modification in threaded context")
	}
	if found.Severity != SeverityWarning {
		t.Errorf("expected warning severity, got %q", found.Severity)
	}
}

func TestConcurrencyRubyThreadWithMutex(t *testing.T) {
	a := NewConcurrencyAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "worker.rb", line: 10, content: `    Thread.new { process }`},
		{file: "worker.rb", line: 12, content: `    mutex = Mutex.new`},
		{file: "worker.rb", line: 15, content: `    @counter = @counter + 1`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "concurrency/thread-unsafe-instance")
	if found != nil {
		t.Error("expected no thread-unsafe-instance issue when Mutex is present")
	}
}

// --- Swift concurrency tests ---

func TestConcurrencySwiftActorViolation(t *testing.T) {
	a := NewConcurrencyAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "ViewModel.swift", line: 10, content: `    Task {`},
		{file: "ViewModel.swift", line: 11, content: `        var result = await fetchData()`},
		{file: "ViewModel.swift", line: 12, content: `    }`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "concurrency/actor-violation")
	if found == nil {
		t.Fatal("expected actor-violation issue for mutable state in Task context")
	}
	if found.Severity != SeverityWarning {
		t.Errorf("expected warning severity, got %q", found.Severity)
	}
}

// --- Kotlin concurrency tests ---

func TestConcurrencyKotlinGlobalScope(t *testing.T) {
	a := NewConcurrencyAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "Service.kt", line: 10, content: `    GlobalScope.launch {`},
		{file: "Service.kt", line: 11, content: `        processData()`},
		{file: "Service.kt", line: 12, content: `    }`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "concurrency/global-scope")
	if found == nil {
		t.Fatal("expected global-scope issue for GlobalScope.launch")
	}
	if found.Severity != SeverityWarning {
		t.Errorf("expected warning severity, got %q", found.Severity)
	}
}

// --- C# concurrency tests ---

func TestConcurrencyCSharpAsyncVoid(t *testing.T) {
	a := NewConcurrencyAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "Handler.cs", line: 10, content: `    public async void OnButtonClick(object sender, EventArgs e)`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "concurrency/async-void")
	if found == nil {
		t.Fatal("expected async-void issue for async void method")
	}
	if found.Severity != SeverityWarning {
		t.Errorf("expected warning severity, got %q", found.Severity)
	}
}

// --- Scala concurrency tests ---

func TestConcurrencyScalaGlobalExecContext(t *testing.T) {
	a := NewConcurrencyAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "Service.scala", line: 10, content: `  implicit val ec = ExecutionContext.global`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "concurrency/global-exec-context")
	if found == nil {
		t.Fatal("expected global-exec-context issue for ExecutionContext.global")
	}
	if found.Severity != SeverityInfo {
		t.Errorf("expected info severity, got %q", found.Severity)
	}
}

// --- Elixir concurrency tests ---

func TestConcurrencyElixirUnlinkedSpawn(t *testing.T) {
	a := NewConcurrencyAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "worker.ex", line: 10, content: `    spawn(fn -> do_work() end)`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "concurrency/unlinked-spawn")
	if found == nil {
		t.Fatal("expected unlinked-spawn issue for spawn() without link")
	}
	if found.Severity != SeverityInfo {
		t.Errorf("expected info severity, got %q", found.Severity)
	}
}

func TestConcurrencyElixirTaskAsyncWithoutAwait(t *testing.T) {
	a := NewConcurrencyAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "worker.ex", line: 10, content: `    Task.async(fn -> expensive_work() end)`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "concurrency/missing-task-await")
	if found == nil {
		t.Fatal("expected missing-task-await issue for Task.async without Task.await")
	}
	if found.Severity != SeverityWarning {
		t.Errorf("expected warning severity, got %q", found.Severity)
	}
}

// --- Deleted/binary file tests ---

func TestConcurrencyDeletedFileSkipped(t *testing.T) {
	a := NewConcurrencyAnalyzer()
	ctx := &AnalysisContext{
		RepoPath: "/nonexistent",
		Diff: &git.Diff{
			Files: []git.FileDiff{
				{
					Path:   "deleted.go",
					Status: "deleted",
					Hunks: []git.Hunk{{
						Lines: []git.Line{
							{Type: "removed", Content: `go func() { process() }()`, OldNum: 10},
						},
					}},
				},
			},
		},
		ParsedFiles: make(map[string]*parser.ParsedFile),
		Config:      config.DefaultConfig(),
	}

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(issues) != 0 {
		t.Errorf("expected 0 issues for deleted file, got %d", len(issues))
	}
}

func TestConcurrencyBinaryFileSkipped(t *testing.T) {
	a := NewConcurrencyAnalyzer()
	ctx := &AnalysisContext{
		RepoPath: "/nonexistent",
		Diff: &git.Diff{
			Files: []git.FileDiff{
				{
					Path:     "image.png",
					Status:   "modified",
					IsBinary: true,
				},
			},
		},
		ParsedFiles: make(map[string]*parser.ParsedFile),
		Config:      config.DefaultConfig(),
	}

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(issues) != 0 {
		t.Errorf("expected 0 issues for binary file, got %d", len(issues))
	}
}
