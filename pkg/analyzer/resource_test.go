package analyzer

import (
	"testing"

	"github.com/joeabbey/fault/pkg/config"
	"github.com/joeabbey/fault/pkg/git"
	"github.com/joeabbey/fault/pkg/parser"
)

func TestResourceAnalyzerName(t *testing.T) {
	a := NewResourceAnalyzer()
	if a.Name() != "resource" {
		t.Errorf("expected name 'resource', got %q", a.Name())
	}
}

func TestResourceEmptyDiff(t *testing.T) {
	a := NewResourceAnalyzer()
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

func TestResourceNilDiff(t *testing.T) {
	a := NewResourceAnalyzer()
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

// makeResourceContext builds an AnalysisContext with sequential added lines in a single hunk.
// This is needed because resource checks use lookahead across multiple lines.
func makeResourceContext(file string, lines []string) *AnalysisContext {
	gitLines := make([]git.Line, 0, len(lines))
	for i, content := range lines {
		gitLines = append(gitLines, git.Line{
			Type:    "added",
			Content: content,
			NewNum:  i + 1,
		})
	}

	return &AnalysisContext{
		RepoPath: "/nonexistent",
		Diff: &git.Diff{
			Files: []git.FileDiff{
				{
					Path:   file,
					Status: "modified",
					Hunks:  []git.Hunk{{Lines: gitLines}},
				},
			},
		},
		ParsedFiles: make(map[string]*parser.ParsedFile),
		Config:      config.DefaultConfig(),
	}
}

// --- Go resource leak tests ---

func TestResourceGoUnclosedFile(t *testing.T) {
	a := NewResourceAnalyzer()
	ctx := makeResourceContext("main.go", []string{
		`	f, err := os.Open("data.txt")`,
		`	if err != nil {`,
		`		return err`,
		`	}`,
		`	// no defer f.Close() here`,
		`	data := readAll(f)`,
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	found := findIssueByID(issues, "resource/go-unclosed-file")
	if found == nil {
		t.Fatal("expected go-unclosed-file issue")
	}
	if found.Severity != SeverityWarning {
		t.Errorf("expected warning severity, got %q", found.Severity)
	}
}

func TestResourceGoClosedFile(t *testing.T) {
	a := NewResourceAnalyzer()
	ctx := makeResourceContext("main.go", []string{
		`	f, err := os.Open("data.txt")`,
		`	if err != nil {`,
		`		return err`,
		`	}`,
		`	defer f.Close()`,
		`	data := readAll(f)`,
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	found := findIssueByID(issues, "resource/go-unclosed-file")
	if found != nil {
		t.Fatal("should not flag os.Open when defer Close() is present")
	}
}

func TestResourceGoUnclosedBody(t *testing.T) {
	a := NewResourceAnalyzer()
	ctx := makeResourceContext("client.go", []string{
		`	resp, err := http.Get("https://example.com")`,
		`	if err != nil {`,
		`		return err`,
		`	}`,
		`	body, _ := io.ReadAll(resp.Body)`,
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	found := findIssueByID(issues, "resource/go-unclosed-body")
	if found == nil {
		t.Fatal("expected go-unclosed-body issue")
	}
}

func TestResourceGoClosedBody(t *testing.T) {
	a := NewResourceAnalyzer()
	ctx := makeResourceContext("client.go", []string{
		`	resp, err := http.Get("https://example.com")`,
		`	if err != nil {`,
		`		return err`,
		`	}`,
		`	defer resp.Body.Close()`,
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	found := findIssueByID(issues, "resource/go-unclosed-body")
	if found != nil {
		t.Fatal("should not flag http.Get when defer resp.Body.Close() is present")
	}
}

func TestResourceGoUnclosedRows(t *testing.T) {
	a := NewResourceAnalyzer()
	ctx := makeResourceContext("db.go", []string{
		`	rows, err := db.Query("SELECT * FROM users")`,
		`	if err != nil {`,
		`		return err`,
		`	}`,
		`	for rows.Next() {`,
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	found := findIssueByID(issues, "resource/go-unclosed-rows")
	if found == nil {
		t.Fatal("expected go-unclosed-rows issue")
	}
}

func TestResourceGoClosedRows(t *testing.T) {
	a := NewResourceAnalyzer()
	ctx := makeResourceContext("db.go", []string{
		`	rows, err := db.Query("SELECT * FROM users")`,
		`	if err != nil {`,
		`		return err`,
		`	}`,
		`	defer rows.Close()`,
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	found := findIssueByID(issues, "resource/go-unclosed-rows")
	if found != nil {
		t.Fatal("should not flag db.Query when defer rows.Close() is present")
	}
}

func TestResourceGoQueryRowNoIssue(t *testing.T) {
	a := NewResourceAnalyzer()
	ctx := makeResourceContext("db.go", []string{
		`	row := db.QueryRow("SELECT name FROM users WHERE id = ?", id)`,
		`	var name string`,
		`	err := row.Scan(&name)`,
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	found := findIssueByID(issues, "resource/go-unclosed-rows")
	if found != nil {
		t.Fatal("QueryRow should not be flagged (it auto-closes)")
	}
}

func TestResourceGoUnclosedListener(t *testing.T) {
	a := NewResourceAnalyzer()
	ctx := makeResourceContext("server.go", []string{
		`	ln, err := net.Listen("tcp", ":8080")`,
		`	if err != nil {`,
		`		log.Fatal(err)`,
		`	}`,
		`	http.Serve(ln, nil)`,
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	found := findIssueByID(issues, "resource/go-unclosed-listener")
	if found == nil {
		t.Fatal("expected go-unclosed-listener issue")
	}
}

func TestResourceGoTestFileSkipped(t *testing.T) {
	a := NewResourceAnalyzer()
	ctx := makeResourceContext("main_test.go", []string{
		`	f, _ := os.Open("testdata.txt")`,
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(issues) != 0 {
		t.Errorf("expected 0 issues for test file, got %d", len(issues))
	}
}

func TestResourceGoVendorSkipped(t *testing.T) {
	a := NewResourceAnalyzer()
	ctx := makeResourceContext("vendor/lib/file.go", []string{
		`	f, _ := os.Open("data.txt")`,
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(issues) != 0 {
		t.Errorf("expected 0 issues for vendor path, got %d", len(issues))
	}
}

func TestResourceGoCommentSkipped(t *testing.T) {
	a := NewResourceAnalyzer()
	ctx := makeResourceContext("main.go", []string{
		`	// f, _ := os.Open("data.txt")`,
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(issues) != 0 {
		t.Errorf("expected 0 issues for comment, got %d", len(issues))
	}
}

func TestResourceGoSQLOpen(t *testing.T) {
	a := NewResourceAnalyzer()
	ctx := makeResourceContext("db.go", []string{
		`	db, err := sql.Open("postgres", dsn)`,
		`	if err != nil {`,
		`		return err`,
		`	}`,
		`	return db.Ping()`,
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	found := findIssueByID(issues, "resource/go-unclosed-db")
	if found == nil {
		t.Fatal("expected go-unclosed-db issue")
	}
}

func TestResourceGoCreateFile(t *testing.T) {
	a := NewResourceAnalyzer()
	ctx := makeResourceContext("writer.go", []string{
		`	f, err := os.Create("output.txt")`,
		`	if err != nil {`,
		`		return err`,
		`	}`,
		`	f.WriteString("hello")`,
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	found := findIssueByID(issues, "resource/go-unclosed-file")
	if found == nil {
		t.Fatal("expected go-unclosed-file issue for os.Create")
	}
}

// --- TypeScript/JavaScript resource leak tests ---

func TestResourceTSWebSocket(t *testing.T) {
	a := NewResourceAnalyzer()
	ctx := makeResourceContext("chat.ts", []string{
		`  const ws = new WebSocket("ws://localhost:8080");`,
		`  ws.onmessage = (e) => { console.log(e.data); };`,
		`  ws.onopen = () => { ws.send("hello"); };`,
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	found := findIssueByID(issues, "resource/ts-unclosed-connection")
	if found == nil {
		t.Fatal("expected ts-unclosed-connection issue")
	}
}

func TestResourceTSWebSocketClosed(t *testing.T) {
	a := NewResourceAnalyzer()
	ctx := makeResourceContext("chat.ts", []string{
		`  const ws = new WebSocket("ws://localhost:8080");`,
		`  ws.onmessage = (e) => { console.log(e.data); };`,
		`  ws.close();`,
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	found := findIssueByID(issues, "resource/ts-unclosed-connection")
	if found != nil {
		t.Fatal("should not flag WebSocket when .close() is present")
	}
}

func TestResourceTSAddEventListener(t *testing.T) {
	a := NewResourceAnalyzer()
	ctx := makeResourceContext("component.tsx", []string{
		`  useEffect(() => {`,
		`    window.addEventListener("resize", handleResize);`,
		`  }, []);`,
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	found := findIssueByID(issues, "resource/ts-missing-cleanup")
	if found == nil {
		t.Fatal("expected ts-missing-cleanup issue for addEventListener without removeEventListener")
	}
}

func TestResourceTSAddEventListenerWithCleanup(t *testing.T) {
	a := NewResourceAnalyzer()
	ctx := makeResourceContext("component.tsx", []string{
		`  useEffect(() => {`,
		`    window.addEventListener("resize", handleResize);`,
		`    return () => {`,
		`      window.removeEventListener("resize", handleResize);`,
		`    };`,
		`  }, []);`,
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	found := findIssueByID(issues, "resource/ts-missing-cleanup")
	if found != nil {
		t.Fatal("should not flag addEventListener when removeEventListener is in same hunk")
	}
}

func TestResourceTSStream(t *testing.T) {
	a := NewResourceAnalyzer()
	ctx := makeResourceContext("upload.js", []string{
		`  const stream = fs.createReadStream("big-file.dat");`,
		`  let data = "";`,
		`  stream.on("data", (chunk) => { data += chunk; });`,
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	found := findIssueByID(issues, "resource/ts-unclosed-stream")
	if found == nil {
		t.Fatal("expected ts-unclosed-stream issue")
	}
}

func TestResourceTSStreamPiped(t *testing.T) {
	a := NewResourceAnalyzer()
	ctx := makeResourceContext("upload.js", []string{
		`  const stream = fs.createReadStream("big-file.dat");`,
		`  stream.pipe(res);`,
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	found := findIssueByID(issues, "resource/ts-unclosed-stream")
	if found != nil {
		t.Fatal("should not flag stream when .pipe() is used")
	}
}

func TestResourceTSFetchInEffect(t *testing.T) {
	a := NewResourceAnalyzer()
	ctx := makeResourceContext("component.tsx", []string{
		`  useEffect(() => {`,
		`    fetch("/api/data")`,
		`      .then(res => res.json())`,
		`      .then(data => setData(data));`,
		`  }, []);`,
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	found := findIssueByID(issues, "resource/ts-missing-abort-controller")
	if found == nil {
		t.Fatal("expected ts-missing-abort-controller issue")
	}
}

func TestResourceTSFetchWithAbort(t *testing.T) {
	a := NewResourceAnalyzer()
	ctx := makeResourceContext("component.tsx", []string{
		`  useEffect(() => {`,
		`    const controller = new AbortController();`,
		`    fetch("/api/data", { signal: controller.signal })`,
		`      .then(res => res.json());`,
		`    return () => controller.abort();`,
		`  }, []);`,
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	found := findIssueByID(issues, "resource/ts-missing-abort-controller")
	if found != nil {
		t.Fatal("should not flag fetch when AbortController is present")
	}
}

func TestResourceTSSetInterval(t *testing.T) {
	a := NewResourceAnalyzer()
	ctx := makeResourceContext("poller.ts", []string{
		`  const id = setInterval(() => {`,
		`    poll();`,
		`  }, 5000);`,
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	found := findIssueByID(issues, "resource/ts-interval-leak")
	if found == nil {
		t.Fatal("expected ts-interval-leak issue")
	}
}

func TestResourceTSSetIntervalCleared(t *testing.T) {
	a := NewResourceAnalyzer()
	ctx := makeResourceContext("poller.ts", []string{
		`  const id = setInterval(() => { poll(); }, 5000);`,
		`  return () => clearInterval(id);`,
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	found := findIssueByID(issues, "resource/ts-interval-leak")
	if found != nil {
		t.Fatal("should not flag setInterval when clearInterval is present")
	}
}

// --- Python resource leak tests ---

func TestResourcePythonNoContextManager(t *testing.T) {
	a := NewResourceAnalyzer()
	ctx := makeResourceContext("script.py", []string{
		`f = open("data.txt", "r")`,
		`data = f.read()`,
		`process(data)`,
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	found := findIssueByID(issues, "resource/python-no-context-manager")
	if found == nil {
		t.Fatal("expected python-no-context-manager issue")
	}
}

func TestResourcePythonWithStatement(t *testing.T) {
	a := NewResourceAnalyzer()
	ctx := makeResourceContext("script.py", []string{
		`with open("data.txt", "r") as f:`,
		`    data = f.read()`,
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	found := findIssueByID(issues, "resource/python-no-context-manager")
	if found != nil {
		t.Fatal("should not flag open() when used with 'with' statement")
	}
}

func TestResourcePythonDBConnect(t *testing.T) {
	a := NewResourceAnalyzer()
	ctx := makeResourceContext("db.py", []string{
		`conn = sqlite3.connect("app.db")`,
		`cursor = conn.cursor()`,
		`cursor.execute("SELECT * FROM users")`,
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	found := findIssueByID(issues, "resource/python-unclosed-connection")
	if found == nil {
		t.Fatal("expected python-unclosed-connection issue")
	}
}

func TestResourcePythonDBConnectWithClose(t *testing.T) {
	a := NewResourceAnalyzer()
	ctx := makeResourceContext("db.py", []string{
		`conn = sqlite3.connect("app.db")`,
		`cursor = conn.cursor()`,
		`cursor.execute("SELECT * FROM users")`,
		`conn.close()`,
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	found := findIssueByID(issues, "resource/python-unclosed-connection")
	if found != nil {
		t.Fatal("should not flag connect() when .close() is present")
	}
}

func TestResourcePythonSession(t *testing.T) {
	a := NewResourceAnalyzer()
	ctx := makeResourceContext("api.py", []string{
		`s = requests.Session()`,
		`s.headers.update({"Authorization": "Bearer token"})`,
		`resp = s.get("https://api.example.com/data")`,
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	found := findIssueByID(issues, "resource/python-unclosed-session")
	if found == nil {
		t.Fatal("expected python-unclosed-session issue")
	}
}

func TestResourcePythonCommentSkipped(t *testing.T) {
	a := NewResourceAnalyzer()
	ctx := makeResourceContext("script.py", []string{
		`# f = open("data.txt")`,
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(issues) != 0 {
		t.Errorf("expected 0 issues for comment, got %d", len(issues))
	}
}

// --- Rust resource leak tests ---

func TestResourceRustBoxLeak(t *testing.T) {
	a := NewResourceAnalyzer()
	ctx := makeResourceContext("main.rs", []string{
		`    let leaked = Box::leak(Box::new(Config::default()));`,
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	found := findIssueByID(issues, "resource/rust-leaked-box")
	if found == nil {
		t.Fatal("expected rust-leaked-box issue")
	}
	if found.Severity != SeverityInfo {
		t.Errorf("expected info severity, got %q", found.Severity)
	}
}

func TestResourceRustMemForget(t *testing.T) {
	a := NewResourceAnalyzer()
	ctx := makeResourceContext("ffi.rs", []string{
		`    std::mem::forget(handle);`,
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	found := findIssueByID(issues, "resource/rust-forget-drop")
	if found == nil {
		t.Fatal("expected rust-forget-drop issue")
	}
	if found.Severity != SeverityInfo {
		t.Errorf("expected info severity, got %q", found.Severity)
	}
}

func TestResourceRustMemForgetShort(t *testing.T) {
	a := NewResourceAnalyzer()
	ctx := makeResourceContext("ffi.rs", []string{
		`    mem::forget(handle);`,
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	found := findIssueByID(issues, "resource/rust-forget-drop")
	if found == nil {
		t.Fatal("expected rust-forget-drop issue for mem::forget")
	}
}

// --- Java resource leak tests ---

func TestResourceJavaUnclosedFileStream(t *testing.T) {
	a := NewResourceAnalyzer()
	ctx := makeResourceContext("Reader.java", []string{
		`        FileInputStream fis = new FileInputStream("data.bin");`,
		`        int data = fis.read();`,
		`        System.out.println(data);`,
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	found := findIssueByID(issues, "resource/java-unclosed-resource")
	if found == nil {
		t.Fatal("expected java-unclosed-resource issue")
	}
}

func TestResourceJavaTryWithResources(t *testing.T) {
	a := NewResourceAnalyzer()
	ctx := makeResourceContext("Reader.java", []string{
		`        try (FileInputStream fis = new FileInputStream("data.bin")) {`,
		`            int data = fis.read();`,
		`        }`,
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	found := findIssueByID(issues, "resource/java-unclosed-resource")
	if found != nil {
		t.Fatal("should not flag when using try-with-resources")
	}
}

func TestResourceJavaDBConnection(t *testing.T) {
	a := NewResourceAnalyzer()
	ctx := makeResourceContext("Database.java", []string{
		`        Connection conn = DriverManager.getConnection(url, user, pass);`,
		`        Statement stmt = conn.createStatement();`,
		`        ResultSet rs = stmt.executeQuery("SELECT 1");`,
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	found := findIssueByID(issues, "resource/java-unclosed-resource")
	if found == nil {
		t.Fatal("expected java-unclosed-resource issue for DriverManager.getConnection")
	}
}

func TestResourceJavaWithFinally(t *testing.T) {
	a := NewResourceAnalyzer()
	ctx := makeResourceContext("Reader.java", []string{
		`        FileInputStream fis = new FileInputStream("data.bin");`,
		`        try {`,
		`            int data = fis.read();`,
		`        } finally {`,
		`            fis.close();`,
		`        }`,
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	found := findIssueByID(issues, "resource/java-unclosed-resource")
	if found != nil {
		t.Fatal("should not flag when finally block is present")
	}
}

// --- C/C++ resource leak tests ---

func TestResourceCUnclosedFile(t *testing.T) {
	a := NewResourceAnalyzer()
	ctx := makeResourceContext("main.c", []string{
		`    FILE *fp = fopen("data.txt", "r");`,
		`    char buf[256];`,
		`    fgets(buf, sizeof(buf), fp);`,
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	found := findIssueByID(issues, "resource/c-unclosed-file")
	if found == nil {
		t.Fatal("expected c-unclosed-file issue")
	}
}

func TestResourceCClosedFile(t *testing.T) {
	a := NewResourceAnalyzer()
	ctx := makeResourceContext("main.c", []string{
		`    FILE *fp = fopen("data.txt", "r");`,
		`    char buf[256];`,
		`    fgets(buf, sizeof(buf), fp);`,
		`    fclose(fp);`,
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	found := findIssueByID(issues, "resource/c-unclosed-file")
	if found != nil {
		t.Fatal("should not flag fopen when fclose is present")
	}
}

func TestResourceCMallocNoFree(t *testing.T) {
	a := NewResourceAnalyzer()
	ctx := makeResourceContext("alloc.c", []string{
		`    int *arr = malloc(100 * sizeof(int));`,
		`    arr[0] = 42;`,
		`    return arr[0];`,
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	found := findIssueByID(issues, "resource/c-malloc-no-free")
	if found == nil {
		t.Fatal("expected c-malloc-no-free issue")
	}
}

func TestResourceCMallocWithFree(t *testing.T) {
	a := NewResourceAnalyzer()
	ctx := makeResourceContext("alloc.c", []string{
		`    int *arr = malloc(100 * sizeof(int));`,
		`    arr[0] = 42;`,
		`    int val = arr[0];`,
		`    free(arr);`,
		`    return val;`,
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	found := findIssueByID(issues, "resource/c-malloc-no-free")
	if found != nil {
		t.Fatal("should not flag malloc when free is present")
	}
}

func TestResourceCSocket(t *testing.T) {
	a := NewResourceAnalyzer()
	ctx := makeResourceContext("net.c", []string{
		`    int fd = socket(AF_INET, SOCK_STREAM, 0);`,
		`    connect(fd, (struct sockaddr*)&addr, sizeof(addr));`,
		`    send(fd, data, len, 0);`,
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	found := findIssueByID(issues, "resource/c-unclosed-socket")
	if found == nil {
		t.Fatal("expected c-unclosed-socket issue")
	}
}

func TestResourceCSocketClosed(t *testing.T) {
	a := NewResourceAnalyzer()
	ctx := makeResourceContext("net.c", []string{
		`    int fd = socket(AF_INET, SOCK_STREAM, 0);`,
		`    connect(fd, (struct sockaddr*)&addr, sizeof(addr));`,
		`    close(fd);`,
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	found := findIssueByID(issues, "resource/c-unclosed-socket")
	if found != nil {
		t.Fatal("should not flag socket when close is present")
	}
}

func TestResourceCppNewNoDelete(t *testing.T) {
	a := NewResourceAnalyzer()
	ctx := makeResourceContext("main.cpp", []string{
		`    Widget* w = new Widget();`,
		`    w->render();`,
		`    return 0;`,
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	found := findIssueByID(issues, "resource/cpp-new-no-delete")
	if found == nil {
		t.Fatal("expected cpp-new-no-delete issue")
	}
}

func TestResourceCppSmartPointer(t *testing.T) {
	a := NewResourceAnalyzer()
	ctx := makeResourceContext("main.cpp", []string{
		`    auto w = std::make_unique<Widget>();`,
		`    w->render();`,
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	found := findIssueByID(issues, "resource/cpp-new-no-delete")
	if found != nil {
		t.Fatal("should not flag when smart pointer is used")
	}
}

// --- C# resource leak tests ---

func TestResourceCSharpMissingUsing(t *testing.T) {
	a := NewResourceAnalyzer()
	ctx := makeResourceContext("Service.cs", []string{
		`        var client = new HttpClient();`,
		`        var response = client.GetAsync(url);`,
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	found := findIssueByID(issues, "resource/csharp-missing-using")
	if found == nil {
		t.Fatal("expected csharp-missing-using issue")
	}
}

func TestResourceCSharpWithUsing(t *testing.T) {
	a := NewResourceAnalyzer()
	ctx := makeResourceContext("Service.cs", []string{
		`        using var client = new HttpClient();`,
		`        var response = client.GetAsync(url);`,
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	found := findIssueByID(issues, "resource/csharp-missing-using")
	if found != nil {
		t.Fatal("should not flag when using statement is present")
	}
}

func TestResourceCSharpSqlConnection(t *testing.T) {
	a := NewResourceAnalyzer()
	ctx := makeResourceContext("Db.cs", []string{
		`        var conn = new SqlConnection(connString);`,
		`        conn.Open();`,
		`        var cmd = conn.CreateCommand();`,
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	found := findIssueByID(issues, "resource/csharp-missing-using")
	if found == nil {
		t.Fatal("expected csharp-missing-using issue for SqlConnection")
	}
}

// --- Ruby resource leak tests ---

func TestResourceRubyNoBlockForm(t *testing.T) {
	a := NewResourceAnalyzer()
	ctx := makeResourceContext("script.rb", []string{
		`  f = File.open("data.txt", "r")`,
		`  content = f.read`,
		`  puts content`,
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	found := findIssueByID(issues, "resource/ruby-no-block-form")
	if found == nil {
		t.Fatal("expected ruby-no-block-form issue")
	}
}

func TestResourceRubyBlockForm(t *testing.T) {
	a := NewResourceAnalyzer()
	ctx := makeResourceContext("script.rb", []string{
		`  File.open("data.txt", "r") do |f|`,
		`    content = f.read`,
		`  end`,
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	found := findIssueByID(issues, "resource/ruby-no-block-form")
	if found != nil {
		t.Fatal("should not flag File.open when block form is used")
	}
}

func TestResourceRubyBlockFormBrace(t *testing.T) {
	a := NewResourceAnalyzer()
	ctx := makeResourceContext("script.rb", []string{
		`  File.open("data.txt") { |f| f.read }`,
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	found := findIssueByID(issues, "resource/ruby-no-block-form")
	if found != nil {
		t.Fatal("should not flag File.open when brace block form is used")
	}
}

// --- PHP resource leak tests ---

func TestResourcePHPFopenNoClose(t *testing.T) {
	a := NewResourceAnalyzer()
	ctx := makeResourceContext("handler.php", []string{
		`  $fp = fopen("log.txt", "a");`,
		`  fwrite($fp, $message);`,
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	found := findIssueByID(issues, "resource/php-unclosed-handle")
	if found == nil {
		t.Fatal("expected php-unclosed-handle issue")
	}
}

func TestResourcePHPFopenWithClose(t *testing.T) {
	a := NewResourceAnalyzer()
	ctx := makeResourceContext("handler.php", []string{
		`  $fp = fopen("log.txt", "a");`,
		`  fwrite($fp, $message);`,
		`  fclose($fp);`,
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	found := findIssueByID(issues, "resource/php-unclosed-handle")
	if found != nil {
		t.Fatal("should not flag fopen when fclose is present")
	}
}

// --- Swift resource leak tests ---

func TestResourceSwiftMissingAutoreleasepool(t *testing.T) {
	a := NewResourceAnalyzer()
	ctx := makeResourceContext("ImageLoader.swift", []string{
		`    for path in imagePaths {`,
		`        let image = UIImage(contentsOfFile: path)`,
		`        process(image)`,
		`    }`,
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	found := findIssueByID(issues, "resource/swift-missing-autoreleasepool")
	if found == nil {
		t.Fatal("expected swift-missing-autoreleasepool issue")
	}
}

func TestResourceSwiftWithAutoreleasepool(t *testing.T) {
	a := NewResourceAnalyzer()
	ctx := makeResourceContext("ImageLoader.swift", []string{
		`    for path in imagePaths {`,
		`        autoreleasepool {`,
		`            let image = UIImage(contentsOfFile: path)`,
		`            process(image)`,
		`        }`,
		`    }`,
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	found := findIssueByID(issues, "resource/swift-missing-autoreleasepool")
	if found != nil {
		t.Fatal("should not flag when autoreleasepool is used")
	}
}

// --- Kotlin resource leak tests ---

func TestResourceKotlinUnclosed(t *testing.T) {
	a := NewResourceAnalyzer()
	ctx := makeResourceContext("Reader.kt", []string{
		`    val reader = BufferedReader(FileReader("data.txt"))`,
		`    val line = reader.readLine()`,
		`    println(line)`,
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	found := findIssueByID(issues, "resource/kotlin-unclosed-resource")
	if found == nil {
		t.Fatal("expected kotlin-unclosed-resource issue")
	}
}

func TestResourceKotlinUse(t *testing.T) {
	a := NewResourceAnalyzer()
	ctx := makeResourceContext("Reader.kt", []string{
		`    BufferedReader(FileReader("data.txt")).use { reader ->`,
		`        val line = reader.readLine()`,
		`        println(line)`,
		`    }`,
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	found := findIssueByID(issues, "resource/kotlin-unclosed-resource")
	if found != nil {
		t.Fatal("should not flag when .use { } is present")
	}
}

// --- Deleted file test ---

func TestResourceDeletedFileSkipped(t *testing.T) {
	a := NewResourceAnalyzer()
	ctx := &AnalysisContext{
		RepoPath: "/nonexistent",
		Diff: &git.Diff{
			Files: []git.FileDiff{
				{
					Path:   "main.go",
					Status: "deleted",
					Hunks: []git.Hunk{{
						Lines: []git.Line{
							{Type: "removed", Content: `	f, _ := os.Open("data.txt")`, OldNum: 1},
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
