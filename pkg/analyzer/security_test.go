package analyzer

import (
	"testing"

	"github.com/joeabbey/fault/pkg/config"
	"github.com/joeabbey/fault/pkg/git"
	"github.com/joeabbey/fault/pkg/parser"
)

func TestSecurityAnalyzerName(t *testing.T) {
	a := NewSecurityAnalyzer()
	if a.Name() != "security" {
		t.Errorf("expected name 'security', got %q", a.Name())
	}
}

func TestSecurityEmptyDiff(t *testing.T) {
	a := NewSecurityAnalyzer()
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

func TestSecurityNilDiff(t *testing.T) {
	a := NewSecurityAnalyzer()
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

// --- SQL Injection tests ---

func TestSecuritySQLInjectionGoConcat(t *testing.T) {
	a := NewSecurityAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "db.go", line: 10, content: `	rows, err := db.Query("SELECT * FROM users WHERE id=" + userID)`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "security/sql-injection")
	if found == nil {
		t.Fatal("expected sql-injection issue for Go string concat query")
	}
	if found.Severity != SeverityError {
		t.Errorf("expected error severity, got %q", found.Severity)
	}
}

func TestSecuritySQLInjectionGoSprintf(t *testing.T) {
	a := NewSecurityAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "db.go", line: 15, content: `	query := fmt.Sprintf("SELECT * FROM users WHERE name='%s'", name)`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "security/sql-injection")
	if found == nil {
		t.Fatal("expected sql-injection issue for Go fmt.Sprintf query")
	}
}

func TestSecuritySQLInjectionPythonFString(t *testing.T) {
	a := NewSecurityAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "db.py", line: 5, content: `    cursor.execute(f"SELECT * FROM users WHERE id={user_id}")`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "security/sql-injection")
	if found == nil {
		t.Fatal("expected sql-injection issue for Python f-string query")
	}
}

func TestSecuritySQLInjectionPythonConcat(t *testing.T) {
	a := NewSecurityAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "db.py", line: 8, content: `    cursor.execute("SELECT * FROM users WHERE id=" + user_id)`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "security/sql-injection")
	if found == nil {
		t.Fatal("expected sql-injection issue for Python string concat query")
	}
}

func TestSecuritySQLInjectionPythonPercent(t *testing.T) {
	a := NewSecurityAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "db.py", line: 8, content: `    cursor.execute("SELECT * FROM users WHERE name='%s'" % name)`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "security/sql-injection")
	if found == nil {
		t.Fatal("expected sql-injection issue for Python percent-format query")
	}
}

func TestSecuritySQLInjectionJSTemplateLiteral(t *testing.T) {
	a := NewSecurityAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "db.ts", line: 10, content: "	const result = await db.query(`SELECT * FROM users WHERE id=${userId}`)"},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "security/sql-injection")
	if found == nil {
		t.Fatal("expected sql-injection issue for JS template literal query")
	}
}

func TestSecuritySQLInjectionJSConcat(t *testing.T) {
	a := NewSecurityAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "db.ts", line: 10, content: `	const result = await db.query("SELECT * FROM users WHERE id=" + userId)`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "security/sql-injection")
	if found == nil {
		t.Fatal("expected sql-injection issue for JS string concat query")
	}
}

// True negatives for SQL injection

func TestSecuritySQLInjectionParameterizedIgnored(t *testing.T) {
	a := NewSecurityAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "db.go", line: 10, content: `	rows, err := db.Query("SELECT * FROM users WHERE id=$1", userID)`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "security/sql-injection")
	if found != nil {
		t.Errorf("did not expect sql-injection for parameterized query with $1")
	}
}

func TestSecuritySQLInjectionCommentIgnored(t *testing.T) {
	a := NewSecurityAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "db.go", line: 10, content: `	// db.Query("SELECT * FROM users WHERE id=" + userID)`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "security/sql-injection")
	if found != nil {
		t.Errorf("did not expect sql-injection for commented-out code")
	}
}

func TestSecuritySQLInjectionTestFileIgnored(t *testing.T) {
	a := NewSecurityAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "db_test.go", line: 10, content: `	rows, err := db.Query("SELECT * FROM users WHERE id=" + userID)`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "security/sql-injection")
	if found != nil {
		t.Errorf("did not expect sql-injection for test file")
	}
}

// --- XSS tests ---

func TestSecurityXSSDangerouslySetInnerHTML(t *testing.T) {
	a := NewSecurityAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "component.tsx", line: 15, content: `      <div dangerouslySetInnerHTML={{__html: userInput}} />`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "security/xss")
	if found == nil {
		t.Fatal("expected xss issue for dangerouslySetInnerHTML")
	}
	if found.Severity != SeverityError {
		t.Errorf("expected error severity, got %q", found.Severity)
	}
}

func TestSecurityXSSInnerHTML(t *testing.T) {
	a := NewSecurityAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "app.js", line: 20, content: `    element.innerHTML = data.content;`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "security/xss")
	if found == nil {
		t.Fatal("expected xss issue for innerHTML assignment")
	}
}

func TestSecurityXSSDocumentWrite(t *testing.T) {
	a := NewSecurityAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "app.js", line: 25, content: `    document.write(userContent);`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "security/xss")
	if found == nil {
		t.Fatal("expected xss issue for document.write")
	}
}

// True negatives for XSS

func TestSecurityXSSSanitizedIgnored(t *testing.T) {
	a := NewSecurityAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "component.tsx", line: 15, content: `      <div dangerouslySetInnerHTML={{__html: DOMPurify.sanitize(content)}} />`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "security/xss")
	if found != nil {
		t.Errorf("did not expect xss issue when DOMPurify is used")
	}
}

func TestSecurityXSSTestFileIgnored(t *testing.T) {
	a := NewSecurityAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "component.test.tsx", line: 15, content: `      <div dangerouslySetInnerHTML={{__html: userInput}} />`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "security/xss")
	if found != nil {
		t.Errorf("did not expect xss issue in test file")
	}
}

// --- Path Traversal tests ---

func TestSecurityPathTraversalGoOsOpen(t *testing.T) {
	a := NewSecurityAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "handler.go", line: 30, content: `	f, err := os.Open(req.URL.Query().Get("file"))`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "security/path-traversal")
	if found == nil {
		t.Fatal("expected path-traversal issue for os.Open with user input")
	}
	if found.Severity != SeverityWarning {
		t.Errorf("expected warning severity, got %q", found.Severity)
	}
}

func TestSecurityPathTraversalNodeFS(t *testing.T) {
	a := NewSecurityAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "handler.ts", line: 10, content: `    const data = fs.readFileSync(req.params.path);`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "security/path-traversal")
	if found == nil {
		t.Fatal("expected path-traversal issue for fs.readFileSync with req.params")
	}
}

func TestSecurityPathTraversalPython(t *testing.T) {
	a := NewSecurityAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "views.py", line: 20, content: `    f = open(request.args.get("filename"))`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "security/path-traversal")
	if found == nil {
		t.Fatal("expected path-traversal issue for Python open with request input")
	}
}

// True negatives for path traversal

func TestSecurityPathTraversalHardcodedPathIgnored(t *testing.T) {
	a := NewSecurityAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "handler.go", line: 30, content: `	f, err := os.Open("/etc/config.json")`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "security/path-traversal")
	if found != nil {
		t.Errorf("did not expect path-traversal for hardcoded string path")
	}
}

func TestSecurityPathTraversalSanitizedIgnored(t *testing.T) {
	a := NewSecurityAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "handler.go", line: 30, content: `	f, err := os.Open(filepath.Clean(userPath))`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "security/path-traversal")
	if found != nil {
		t.Errorf("did not expect path-traversal when filepath.Clean is used")
	}
}

// --- Hardcoded Secrets tests ---

func TestSecuritySecretAWSKey(t *testing.T) {
	a := NewSecurityAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "config.go", line: 5, content: "	awsKey := \"AKIA" + "IOSFODNN7ABCDEFG\""},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "security/hardcoded-secret")
	if found == nil {
		t.Fatal("expected hardcoded-secret issue for AWS key")
	}
	if found.Severity != SeverityError {
		t.Errorf("expected error severity, got %q", found.Severity)
	}
}

func TestSecuritySecretStripeKey(t *testing.T) {
	a := NewSecurityAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "billing.go", line: 10, content: "	stripe.Key = \"sk_live_" + "abcdefghijklmnopqrstuvwx\""},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "security/hardcoded-secret")
	if found == nil {
		t.Fatal("expected hardcoded-secret issue for Stripe key")
	}
}

func TestSecuritySecretPrivateKey(t *testing.T) {
	a := NewSecurityAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "auth.go", line: 3, content: `	key := "-----BEGIN RSA PRIVATE KEY-----"`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "security/hardcoded-secret")
	if found == nil {
		t.Fatal("expected hardcoded-secret issue for private key")
	}
}

func TestSecuritySecretJWT(t *testing.T) {
	a := NewSecurityAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "auth.go", line: 5, content: `	token := "eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiIxMjM0NTY3ODkwIn0"`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "security/hardcoded-secret")
	if found == nil {
		t.Fatal("expected hardcoded-secret issue for JWT token")
	}
}

func TestSecuritySecretGitHubToken(t *testing.T) {
	a := NewSecurityAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "ci.go", line: 12, content: "	token := \"ghp_" + "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijkl\""},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "security/hardcoded-secret")
	if found == nil {
		t.Fatal("expected hardcoded-secret issue for GitHub token")
	}
}

func TestSecuritySecretGenericAPIKey(t *testing.T) {
	a := NewSecurityAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "config.go", line: 7, content: `	api_key = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnop"`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "security/hardcoded-secret")
	if found == nil {
		t.Fatal("expected hardcoded-secret issue for generic API key")
	}
}

func TestSecuritySecretGCPServiceAccount(t *testing.T) {
	a := NewSecurityAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "gcp.go", line: 3, content: `	data := '{"type": "service_account", "project_id": "my-project"}'`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "security/hardcoded-secret")
	if found == nil {
		t.Fatal("expected hardcoded-secret issue for GCP service account")
	}
}

// True negatives for secrets

func TestSecuritySecretEnvVarIgnored(t *testing.T) {
	a := NewSecurityAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "config.go", line: 5, content: `	apiKey := os.Getenv("API_KEY")`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "security/hardcoded-secret")
	if found != nil {
		t.Errorf("did not expect secret issue for os.Getenv")
	}
}

func TestSecuritySecretProcessEnvIgnored(t *testing.T) {
	a := NewSecurityAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "config.ts", line: 5, content: `  const apiKey = process.env.API_KEY;`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "security/hardcoded-secret")
	if found != nil {
		t.Errorf("did not expect secret issue for process.env")
	}
}

func TestSecuritySecretPlaceholderIgnored(t *testing.T) {
	a := NewSecurityAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "config.go", line: 5, content: `	apiKey := "your-key-here-CHANGEME-placeholder"`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "security/hardcoded-secret")
	if found != nil {
		t.Errorf("did not expect secret issue for placeholder value")
	}
}

func TestSecuritySecretTestFileIgnored(t *testing.T) {
	a := NewSecurityAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "auth_test.go", line: 5, content: "	awsKey := \"AKIA" + "IOSFODNN7ABCDEFG\""},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "security/hardcoded-secret")
	if found != nil {
		t.Errorf("did not expect secret issue in test file")
	}
}

func TestSecuritySecretTestExampleIgnored(t *testing.T) {
	a := NewSecurityAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "config.go", line: 5, content: `	token = "exampleABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghij"`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "security/hardcoded-secret")
	if found != nil {
		t.Errorf("did not expect secret issue for example/test value")
	}
}

// --- Insecure Crypto tests ---

func TestSecurityInsecureCryptoMD5Go(t *testing.T) {
	a := NewSecurityAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "auth.go", line: 10, content: `	h := md5.New()`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "security/insecure-crypto")
	if found == nil {
		t.Fatal("expected insecure-crypto issue for md5.New()")
	}
	if found.Severity != SeverityWarning {
		t.Errorf("expected warning severity, got %q", found.Severity)
	}
}

func TestSecurityInsecureCryptoMD5Python(t *testing.T) {
	a := NewSecurityAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "auth.py", line: 5, content: `    h = hashlib.md5(password.encode())`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "security/insecure-crypto")
	if found == nil {
		t.Fatal("expected insecure-crypto issue for hashlib.md5")
	}
}

func TestSecurityInsecureCryptoMD5JS(t *testing.T) {
	a := NewSecurityAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "auth.ts", line: 5, content: `    const hash = crypto.createHash('md5').update(password).digest('hex');`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "security/insecure-crypto")
	if found == nil {
		t.Fatal("expected insecure-crypto issue for crypto.createHash('md5')")
	}
}

func TestSecurityInsecureCryptoSHA1Go(t *testing.T) {
	a := NewSecurityAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "auth.go", line: 10, content: `	h := sha1.New()`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "security/insecure-crypto")
	if found == nil {
		t.Fatal("expected insecure-crypto issue for sha1.New()")
	}
}

func TestSecurityInsecureCryptoMathRandom(t *testing.T) {
	a := NewSecurityAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "token.ts", line: 8, content: `    const token = Math.random().toString(36).substring(2);`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "security/insecure-crypto")
	if found == nil {
		t.Fatal("expected insecure-crypto issue for Math.random()")
	}
}

// True negatives for insecure crypto

func TestSecurityCryptoMD5ChecksumIgnored(t *testing.T) {
	a := NewSecurityAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "util.go", line: 10, content: `	checksum := md5.Sum(fileData)`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "security/insecure-crypto")
	if found != nil {
		t.Errorf("did not expect insecure-crypto issue for MD5 checksum")
	}
}

func TestSecurityCryptoMathRandomUIIgnored(t *testing.T) {
	a := NewSecurityAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "animation.ts", line: 8, content: `    const delay = Math.random() * 1000; // animation jitter`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "security/insecure-crypto")
	if found != nil {
		t.Errorf("did not expect insecure-crypto issue for Math.random used in animation")
	}
}

func TestSecurityCryptoMathRandomNonJSIgnored(t *testing.T) {
	a := NewSecurityAnalyzer()
	ctx := makePatternContext([]addedLine{
		// Math.random() in a Go file shouldn't match (not JS/TS)
		{file: "util.go", line: 8, content: `	val := Math.random()`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "security/insecure-crypto")
	if found != nil {
		t.Errorf("did not expect insecure-crypto issue for Math.random in a Go file")
	}
}

// --- File/directory exclusion tests ---

func TestSecurityDeletedFileSkipped(t *testing.T) {
	a := NewSecurityAnalyzer()
	ctx := &AnalysisContext{
		RepoPath: "/nonexistent",
		Diff: &git.Diff{Files: []git.FileDiff{
			{
				Path:   "old.go",
				Status: "deleted",
				Hunks: []git.Hunk{{
					Lines: []git.Line{
						{Type: "added", Content: `	db.Query("SELECT * FROM users WHERE id=" + id)`, NewNum: 1},
					},
				}},
			},
		}},
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

func TestSecurityVendorDirSkipped(t *testing.T) {
	a := NewSecurityAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "vendor/github.com/lib/pq/db.go", line: 10, content: `	db.Query("SELECT * FROM users WHERE id=" + id)`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(issues) != 0 {
		t.Errorf("expected 0 issues for vendor dir, got %d", len(issues))
	}
}

func TestSecurityNodeModulesSkipped(t *testing.T) {
	a := NewSecurityAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "node_modules/express/lib/router.js", line: 10, content: `    document.write(data);`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(issues) != 0 {
		t.Errorf("expected 0 issues for node_modules, got %d", len(issues))
	}
}

func TestSecurityTestdataDirSkipped(t *testing.T) {
	a := NewSecurityAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "testdata/fixtures/vuln.go", line: 5, content: `	db.Query("SELECT * FROM users WHERE id=" + id)`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(issues) != 0 {
		t.Errorf("expected 0 issues for testdata dir, got %d", len(issues))
	}
}

func TestSecurityRemovedLinesIgnored(t *testing.T) {
	a := NewSecurityAnalyzer()
	ctx := &AnalysisContext{
		RepoPath: "/nonexistent",
		Diff: &git.Diff{Files: []git.FileDiff{
			{
				Path:   "db.go",
				Status: "modified",
				Hunks: []git.Hunk{{
					Lines: []git.Line{
						{Type: "removed", Content: `	db.Query("SELECT * FROM users WHERE id=" + id)`, OldNum: 10},
					},
				}},
			},
		}},
		ParsedFiles: make(map[string]*parser.ParsedFile),
		Config:      config.DefaultConfig(),
	}

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(issues) != 0 {
		t.Errorf("expected 0 issues for removed lines, got %d", len(issues))
	}
}

func TestSecurityMarkdownSkipped(t *testing.T) {
	a := NewSecurityAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "docs/security.md", line: 5, content: `db.Query("SELECT * FROM users WHERE id=" + id)`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(issues) != 0 {
		t.Errorf("expected 0 issues for markdown file, got %d", len(issues))
	}
}

// --- Edge cases ---

func TestSecurityMultipleIssuesSameLine(t *testing.T) {
	// A line that triggers both secret and sql-injection should only report one per category
	a := NewSecurityAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "bad.go", line: 5, content: `	db.Query("SELECT * FROM users WHERE id=" + id)`},
		{file: "bad.go", line: 6, content: "	key := \"AKIA" + "IOSFODNN7ABCDEFG\""},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	sqlCount := 0
	secretCount := 0
	for _, issue := range issues {
		switch issue.ID {
		case "security/sql-injection":
			sqlCount++
		case "security/hardcoded-secret":
			secretCount++
		}
	}

	if sqlCount != 1 {
		t.Errorf("expected 1 sql-injection issue, got %d", sqlCount)
	}
	if secretCount != 1 {
		t.Errorf("expected 1 hardcoded-secret issue, got %d", secretCount)
	}
}

func TestSecuritySecretCommentIgnored(t *testing.T) {
	a := NewSecurityAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "config.go", line: 5, content: `	// awsKey := "AKIAIOSFODNN7EXAMPLE"`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "security/hardcoded-secret")
	if found != nil {
		t.Errorf("did not expect secret issue for commented-out code")
	}
}

// --- Go security tests ---

func TestSecurityGoListenAndServe(t *testing.T) {
	a := NewSecurityAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "server.go", line: 10, content: `	http.ListenAndServe(":8080", mux)`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "security/go-no-tls")
	if found == nil {
		t.Fatal("expected go-no-tls issue for http.ListenAndServe")
	}
	if found.Severity != SeverityWarning {
		t.Errorf("expected warning severity, got %q", found.Severity)
	}
}

func TestSecurityGoListenAndServeNonGoIgnored(t *testing.T) {
	a := NewSecurityAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "notes.md", line: 10, content: `http.ListenAndServe(":8080", mux)`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "security/go-no-tls")
	if found != nil {
		t.Errorf("did not expect go-no-tls for non-Go file")
	}
}

func TestSecurityGoExecCommandConcat(t *testing.T) {
	a := NewSecurityAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "runner.go", line: 15, content: `	cmd := exec.Command("sh", "-c", "ls " + userInput)`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "security/go-command-injection")
	if found == nil {
		t.Fatal("expected go-command-injection issue for exec.Command with concat")
	}
	if found.Severity != SeverityWarning {
		t.Errorf("expected warning severity, got %q", found.Severity)
	}
}

func TestSecurityGoExecCommandVariable(t *testing.T) {
	a := NewSecurityAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "runner.go", line: 15, content: `	cmd := exec.Command(userCmd, args...)`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "security/go-command-injection")
	if found == nil {
		t.Fatal("expected go-command-injection issue for exec.Command with variable")
	}
}

func TestSecurityGoExecCommandLiteralSafe(t *testing.T) {
	a := NewSecurityAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "runner.go", line: 15, content: `	cmd := exec.Command("git", "status")`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "security/go-command-injection")
	if found != nil {
		t.Errorf("did not expect go-command-injection for literal exec.Command")
	}
}

func TestSecurityGoServerNoTimeout(t *testing.T) {
	a := NewSecurityAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "server.go", line: 20, content: `	srv := &http.Server{Addr: ":8080", Handler: mux}`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "security/go-no-timeout")
	if found == nil {
		t.Fatal("expected go-no-timeout issue for http.Server without timeouts")
	}
	if found.Severity != SeverityInfo {
		t.Errorf("expected info severity, got %q", found.Severity)
	}
}

func TestSecurityGoServerWithTimeoutSafe(t *testing.T) {
	a := NewSecurityAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "server.go", line: 20, content: `	srv := &http.Server{Addr: ":8080", Handler: mux, ReadTimeout: 5 * time.Second}`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "security/go-no-timeout")
	if found != nil {
		t.Errorf("did not expect go-no-timeout when ReadTimeout is set")
	}
}

// --- Python security tests ---

func TestSecurityPythonPickleLoad(t *testing.T) {
	a := NewSecurityAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "loader.py", line: 10, content: `    data = pickle.load(open("data.pkl", "rb"))`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "security/python-pickle")
	if found == nil {
		t.Fatal("expected python-pickle issue for pickle.load")
	}
	if found.Severity != SeverityWarning {
		t.Errorf("expected warning severity, got %q", found.Severity)
	}
}

func TestSecurityPythonPickleLoads(t *testing.T) {
	a := NewSecurityAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "loader.py", line: 10, content: `    data = pickle.loads(raw_bytes)`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "security/python-pickle")
	if found == nil {
		t.Fatal("expected python-pickle issue for pickle.loads")
	}
}

func TestSecurityPythonSubprocessShellTrue(t *testing.T) {
	a := NewSecurityAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "runner.py", line: 5, content: `    subprocess.call("ls -la " + user_dir, shell=True)`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "security/python-shell-injection")
	if found == nil {
		t.Fatal("expected python-shell-injection for subprocess.call(shell=True)")
	}
	if found.Severity != SeverityWarning {
		t.Errorf("expected warning severity, got %q", found.Severity)
	}
}

func TestSecurityPythonSubprocessPopenShellTrue(t *testing.T) {
	a := NewSecurityAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "runner.py", line: 5, content: `    proc = subprocess.Popen(cmd, shell=True)`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "security/python-shell-injection")
	if found == nil {
		t.Fatal("expected python-shell-injection for subprocess.Popen(shell=True)")
	}
}

func TestSecurityPythonSubprocessShellFalseSafe(t *testing.T) {
	a := NewSecurityAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "runner.py", line: 5, content: `    subprocess.call(["ls", "-la"], shell=False)`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "security/python-shell-injection")
	if found != nil {
		t.Errorf("did not expect python-shell-injection for shell=False")
	}
}

func TestSecurityPythonUnsafeYamlLoad(t *testing.T) {
	a := NewSecurityAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "config.py", line: 8, content: `    data = yaml.load(open("config.yml"))`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "security/python-unsafe-yaml")
	if found == nil {
		t.Fatal("expected python-unsafe-yaml for yaml.load without SafeLoader")
	}
	if found.Severity != SeverityWarning {
		t.Errorf("expected warning severity, got %q", found.Severity)
	}
}

func TestSecurityPythonYamlSafeLoaderSafe(t *testing.T) {
	a := NewSecurityAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "config.py", line: 8, content: `    data = yaml.load(open("config.yml"), Loader=yaml.SafeLoader)`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "security/python-unsafe-yaml")
	if found != nil {
		t.Errorf("did not expect python-unsafe-yaml when SafeLoader is used")
	}
}

func TestSecurityPythonYamlSafeLoadSafe(t *testing.T) {
	a := NewSecurityAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "config.py", line: 8, content: `    data = yaml.safe_load(open("config.yml"))`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "security/python-unsafe-yaml")
	if found != nil {
		t.Errorf("did not expect python-unsafe-yaml for yaml.safe_load")
	}
}

func TestSecurityPythonNonPyIgnored(t *testing.T) {
	a := NewSecurityAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "config.go", line: 8, content: `    data = pickle.load(open("data.pkl"))`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "security/python-pickle")
	if found != nil {
		t.Errorf("did not expect python-pickle for non-Python file")
	}
}

// --- TypeScript/JS security tests ---

func TestSecurityTSChildProcessExecTemplate(t *testing.T) {
	a := NewSecurityAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "runner.ts", line: 10, content: "    child_process.exec(`ls ${userDir}`)"},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "security/ts-command-injection")
	if found == nil {
		t.Fatal("expected ts-command-injection for child_process.exec with template literal")
	}
	if found.Severity != SeverityWarning {
		t.Errorf("expected warning severity, got %q", found.Severity)
	}
}

func TestSecurityTSChildProcessExecConcat(t *testing.T) {
	a := NewSecurityAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "runner.js", line: 10, content: `    child_process.exec("ls " + userDir)`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "security/ts-command-injection")
	if found == nil {
		t.Fatal("expected ts-command-injection for child_process.exec with concat")
	}
}

func TestSecurityTSDynamicRequire(t *testing.T) {
	a := NewSecurityAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "loader.js", line: 5, content: `    const mod = require(modulePath);`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "security/ts-dynamic-require")
	if found == nil {
		t.Fatal("expected ts-dynamic-require for require with variable argument")
	}
	if found.Severity != SeverityWarning {
		t.Errorf("expected warning severity, got %q", found.Severity)
	}
}

func TestSecurityTSRequireLiteralSafe(t *testing.T) {
	a := NewSecurityAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "app.js", line: 5, content: `    const fs = require("fs");`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "security/ts-dynamic-require")
	if found != nil {
		t.Errorf("did not expect ts-dynamic-require for string literal require")
	}
}

func TestSecurityTSProtoPollutionDunderProto(t *testing.T) {
	a := NewSecurityAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "merge.js", line: 12, content: `    obj.__proto__ = malicious;`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "security/ts-prototype-pollution")
	if found == nil {
		t.Fatal("expected ts-prototype-pollution for __proto__ assignment")
	}
	if found.Severity != SeverityWarning {
		t.Errorf("expected warning severity, got %q", found.Severity)
	}
}

func TestSecurityTSProtoPollutionConstructor(t *testing.T) {
	a := NewSecurityAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "util.ts", line: 8, content: `    constructor.prototype = polluted;`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "security/ts-prototype-pollution")
	if found == nil {
		t.Fatal("expected ts-prototype-pollution for constructor.prototype assignment")
	}
}

func TestSecurityTSNonTSFileIgnored(t *testing.T) {
	a := NewSecurityAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "merge.go", line: 12, content: `    obj.__proto__ = malicious;`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "security/ts-prototype-pollution")
	if found != nil {
		t.Errorf("did not expect ts-prototype-pollution for non-TS/JS file")
	}
}

// --- Hardcoded IP tests ---

func TestSecurityHardcodedIP(t *testing.T) {
	a := NewSecurityAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "config.go", line: 5, content: `	host := "192.168.1.100"`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "security/hardcoded-ip")
	if found == nil {
		t.Fatal("expected hardcoded-ip issue for private IP address")
	}
	if found.Severity != SeverityInfo {
		t.Errorf("expected info severity, got %q", found.Severity)
	}
}

func TestSecurityHardcodedIPPublic(t *testing.T) {
	a := NewSecurityAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "deploy.py", line: 10, content: `    server = "10.0.0.50"`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "security/hardcoded-ip")
	if found == nil {
		t.Fatal("expected hardcoded-ip issue for IP address in string")
	}
}

func TestSecurityHardcodedIPLocalhostIgnored(t *testing.T) {
	a := NewSecurityAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "server.go", line: 5, content: `	addr := "127.0.0.1:8080"`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "security/hardcoded-ip")
	if found != nil {
		t.Errorf("did not expect hardcoded-ip for localhost 127.0.0.1")
	}
}

func TestSecurityHardcodedIPBindAllIgnored(t *testing.T) {
	a := NewSecurityAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "server.go", line: 5, content: `	addr := "0.0.0.0:8080"`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "security/hardcoded-ip")
	if found != nil {
		t.Errorf("did not expect hardcoded-ip for 0.0.0.0")
	}
}

func TestSecurityHardcodedIPVersionStringIgnored(t *testing.T) {
	a := NewSecurityAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "version.go", line: 5, content: `	version := "1.2.3.4" // semver`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "security/hardcoded-ip")
	if found != nil {
		t.Errorf("did not expect hardcoded-ip for version string")
	}
}

func TestSecurityHardcodedIPTestFileIgnored(t *testing.T) {
	a := NewSecurityAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "config_test.go", line: 5, content: `	host := "192.168.1.100"`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "security/hardcoded-ip")
	if found != nil {
		t.Errorf("did not expect hardcoded-ip in test file")
	}
}

func TestSecurityHardcodedIPCommentIgnored(t *testing.T) {
	a := NewSecurityAnalyzer()
	ctx := makePatternContext([]addedLine{
		{file: "config.go", line: 5, content: `	// The server at "192.168.1.100" is the gateway`},
	})

	issues, err := a.Analyze(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	found := findIssueByID(issues, "security/hardcoded-ip")
	if found != nil {
		t.Errorf("did not expect hardcoded-ip in comment")
	}
}

// --- isValidIP unit tests ---

func TestIsValidIP(t *testing.T) {
	tests := []struct {
		ip   string
		want bool
	}{
		{"192.168.1.1", true},
		{"10.0.0.1", true},
		{"255.255.255.255", true},
		{"0.0.0.0", true},
		{"256.1.1.1", false},
		{"1.1.1.999", false},
		{"1.2.3", false},
		{"1.2.3.4.5", false},
		{"01.2.3.4", false},   // leading zero
		{"1.02.3.4", false},   // leading zero
		{"abc.1.2.3", false},
	}

	for _, tt := range tests {
		got := isValidIP(tt.ip)
		if got != tt.want {
			t.Errorf("isValidIP(%q) = %v, want %v", tt.ip, got, tt.want)
		}
	}
}
