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
