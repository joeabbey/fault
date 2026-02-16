package analyzer

import (
	"path/filepath"
	"regexp"
	"strings"

	"github.com/joeabbey/fault/pkg/git"
)

// SecurityAnalyzer detects OWASP-style vulnerabilities in added code.
type SecurityAnalyzer struct{}

// NewSecurityAnalyzer creates a new security analyzer.
func NewSecurityAnalyzer() *SecurityAnalyzer {
	return &SecurityAnalyzer{}
}

// Name returns the analyzer name.
func (a *SecurityAnalyzer) Name() string {
	return "security"
}

// Analyze scans diff content for security vulnerabilities in added lines.
func (a *SecurityAnalyzer) Analyze(ctx *AnalysisContext) ([]Issue, error) {
	issues := make([]Issue, 0)

	if ctx.Diff == nil || len(ctx.Diff.Files) == 0 {
		return issues, nil
	}

	for _, fileDiff := range ctx.Diff.Files {
		if fileDiff.Status == "deleted" || fileDiff.IsBinary {
			continue
		}

		if !isAnalyzableFile(fileDiff.Path) {
			continue
		}

		if isSecuritySkippedPath(fileDiff.Path) {
			continue
		}

		testFile := isTestFile(fileDiff.Path)

		// Hardcoded secrets are checked everywhere except test files.
		if !testFile {
			issues = append(issues, checkSecuritySecrets(fileDiff)...)
		}

		// All other security checks skip test files entirely.
		if testFile {
			continue
		}

		issues = append(issues, checkSQLInjection(fileDiff)...)
		issues = append(issues, checkXSS(fileDiff)...)
		issues = append(issues, checkPathTraversal(fileDiff)...)
		issues = append(issues, checkInsecureCrypto(fileDiff)...)
		issues = append(issues, checkRustSecurity(fileDiff)...)
		issues = append(issues, checkTSCodeExecution(fileDiff)...)
		issues = append(issues, checkGoSecurity(fileDiff)...)
		issues = append(issues, checkPythonSecurity(fileDiff)...)
		issues = append(issues, checkTSSecurity(fileDiff)...)
		issues = append(issues, checkHardcodedIPs(fileDiff)...)

		// Java-specific security checks
		ext := strings.ToLower(filepath.Ext(fileDiff.Path))
		if ext == ".java" {
			issues = append(issues, checkJavaSecurity(fileDiff)...)
		}

		// Ruby-specific security checks
		if ext == ".rb" || ext == ".rake" {
			issues = append(issues, checkRubySecurity(fileDiff)...)
		}

		// Kotlin inherits Java security rules plus Kotlin-specific checks
		if ext == ".kt" || ext == ".kts" {
			issues = append(issues, checkKotlinSecurity(fileDiff)...)
		}

		// C#-specific security checks
		if ext == ".cs" {
			issues = append(issues, checkCSharpSecurity(fileDiff)...)
		}

		// PHP-specific security checks
		if ext == ".php" {
			issues = append(issues, checkPHPSecurity(fileDiff)...)
		}

		// Swift-specific security checks
		if ext == ".swift" {
			issues = append(issues, checkSwiftSecurity(fileDiff)...)
		}

		// C-specific security checks
		if ext == ".c" || ext == ".h" {
			issues = append(issues, checkCSecurity(fileDiff)...)
		}

		// C++-specific security checks (includes C rules)
		if ext == ".cpp" || ext == ".cc" || ext == ".cxx" || ext == ".hpp" || ext == ".hxx" {
			issues = append(issues, checkCppSecurity(fileDiff)...)
		}

		// Objective-C-specific security checks
		if ext == ".m" || ext == ".mm" {
			issues = append(issues, checkObjCSecurity(fileDiff)...)
		}

		// Bash-specific security checks
		if ext == ".sh" || ext == ".bash" {
			issues = append(issues, checkBashSecurity(fileDiff)...)
		}

		// SQL-specific security checks
		if ext == ".sql" {
			issues = append(issues, checkSQLFileSecurity(fileDiff)...)
		}

		// Dart-specific security checks
		if ext == ".dart" {
			issues = append(issues, checkDartSecurity(fileDiff)...)
		}

		// Scala-specific security checks
		if ext == ".scala" {
			issues = append(issues, checkScalaSecurity(fileDiff)...)
		}

		// R-specific security checks
		if ext == ".r" || ext == ".rmd" {
			issues = append(issues, checkRSecurity(fileDiff)...)
		}

		// Elixir-specific security checks
		if ext == ".ex" || ext == ".exs" {
			issues = append(issues, checkElixirSecurity(fileDiff)...)
		}

		// Lua-specific security checks
		if ext == ".lua" {
			issues = append(issues, checkLuaSecurity(fileDiff)...)
		}
	}

	return issues, nil
}

// isSecuritySkippedPath returns true for vendored/test-data directories.
func isSecuritySkippedPath(path string) bool {
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

// --- SQL Injection detection ---

type sqlInjectionRule struct {
	pattern    *regexp.Regexp
	exclusions []*regexp.Regexp
}

var sqlInjectionRules = []sqlInjectionRule{
	// Go: db.Query/db.Exec with string concatenation
	{
		pattern: regexp.MustCompile(`\bdb\.(Query|Exec|QueryRow)\s*\(\s*"[^"]*"\s*\+`),
		exclusions: []*regexp.Regexp{
			regexp.MustCompile(`\$\d`),  // parameterized: $1, $2
			regexp.MustCompile(`\?\s*`), // parameterized: ?
		},
	},
	// Go: fmt.Sprintf with SELECT/INSERT/UPDATE/DELETE in query context
	{
		pattern: regexp.MustCompile(`fmt\.Sprintf\(\s*"(?i)(SELECT|INSERT|UPDATE|DELETE|DROP|ALTER)\b[^"]*%[sv]`),
		exclusions: []*regexp.Regexp{
			regexp.MustCompile(`(?i)//`), // commented
		},
	},
	// Python: f-string SQL
	{
		pattern: regexp.MustCompile(`f["'](?i)(SELECT|INSERT|UPDATE|DELETE|DROP|ALTER)\b[^"']*\{`),
		exclusions: []*regexp.Regexp{},
	},
	// Python: cursor.execute with concatenation
	{
		pattern: regexp.MustCompile(`cursor\.execute\s*\(\s*["'][^"']*["']\s*\+`),
		exclusions: []*regexp.Regexp{},
	},
	// Python: %-formatting SQL
	{
		pattern: regexp.MustCompile(`["'](?i)(SELECT|INSERT|UPDATE|DELETE|DROP|ALTER)\b.*%s.*["']\s*%`),
		exclusions: []*regexp.Regexp{},
	},
	// TypeScript/JS: template literal SQL
	{
		pattern: regexp.MustCompile("`(?i)(SELECT|INSERT|UPDATE|DELETE|DROP|ALTER)\\b[^`]*\\$\\{"),
		exclusions: []*regexp.Regexp{},
	},
	// TypeScript/JS: query with concatenation
	{
		pattern: regexp.MustCompile(`\.query\s*\(\s*["'][^"']*["']\s*\+`),
		exclusions: []*regexp.Regexp{
			regexp.MustCompile(`\$\d`), // parameterized
			regexp.MustCompile(`\?`),   // parameterized
		},
	},
	// Java: Statement.execute/executeQuery with string concatenation
	{
		pattern: regexp.MustCompile(`\.(execute|executeQuery|executeUpdate)\s*\(\s*"[^"]*"\s*\+`),
		exclusions: []*regexp.Regexp{
			regexp.MustCompile(`\?`), // parameterized
		},
	},
	// Java: String.format with SQL keywords
	{
		pattern: regexp.MustCompile(`String\.format\(\s*"(?i)(SELECT|INSERT|UPDATE|DELETE|DROP|ALTER)\b[^"]*%s`),
		exclusions: []*regexp.Regexp{},
	},
}

func checkSQLInjection(fileDiff git.FileDiff) []Issue {
	issues := make([]Issue, 0)

	for _, hunk := range fileDiff.Hunks {
		for _, line := range hunk.Lines {
			if line.Type != "added" {
				continue
			}

			content := line.Content
			trimmed := strings.TrimSpace(content)

			// Skip comments.
			if strings.HasPrefix(trimmed, "//") || strings.HasPrefix(trimmed, "#") || strings.HasPrefix(trimmed, "/*") || strings.HasPrefix(trimmed, "*") {
				continue
			}

			for _, rule := range sqlInjectionRules {
				if !rule.pattern.MatchString(content) {
					continue
				}

				excluded := false
				for _, excl := range rule.exclusions {
					if excl.MatchString(content) {
						excluded = true
						break
					}
				}
				if excluded {
					continue
				}

				issues = append(issues, Issue{
					ID:         "security/sql-injection",
					Severity:   SeverityError,
					Category:   "security",
					File:       fileDiff.Path,
					Line:       line.NewNum,
					Message:    "Potential SQL injection: user input concatenated into query string",
					Suggestion: "Use parameterized queries ($1, ?, :name) instead of string concatenation",
				})
				break // one issue per line
			}
		}
	}

	return issues
}

// --- XSS detection ---

var xssPatterns = []*regexp.Regexp{
	regexp.MustCompile(`dangerouslySetInnerHTML`),
	regexp.MustCompile(`\.innerHTML\s*=`),
	regexp.MustCompile(`document\.write\s*\(`),
}

var xssExclusions = []*regexp.Regexp{
	regexp.MustCompile(`(?i)DOMPurify`),
	regexp.MustCompile(`(?i)sanitize[_-]?html`),
	regexp.MustCompile(`(?i)sanitize\(`),
	regexp.MustCompile(`(?i)xss\(`),
}

func checkXSS(fileDiff git.FileDiff) []Issue {
	issues := make([]Issue, 0)

	for _, hunk := range fileDiff.Hunks {
		for _, line := range hunk.Lines {
			if line.Type != "added" {
				continue
			}

			content := line.Content
			trimmed := strings.TrimSpace(content)

			if strings.HasPrefix(trimmed, "//") || strings.HasPrefix(trimmed, "#") || strings.HasPrefix(trimmed, "/*") || strings.HasPrefix(trimmed, "*") {
				continue
			}

			for _, pat := range xssPatterns {
				if !pat.MatchString(content) {
					continue
				}

				excluded := false
				for _, excl := range xssExclusions {
					if excl.MatchString(content) {
						excluded = true
						break
					}
				}
				if excluded {
					continue
				}

				issues = append(issues, Issue{
					ID:         "security/xss",
					Severity:   SeverityError,
					Category:   "security",
					File:       fileDiff.Path,
					Line:       line.NewNum,
					Message:    "Potential XSS: unescaped content injected into DOM",
					Suggestion: "Sanitize input with DOMPurify or use safe framework bindings instead of raw HTML injection",
				})
				break
			}
		}
	}

	return issues
}

// --- Path Traversal detection ---

type pathTraversalRule struct {
	pattern    *regexp.Regexp
	exclusions []*regexp.Regexp
}

var pathTraversalRules = []pathTraversalRule{
	// Go: os.Open/os.ReadFile with variable (not string literal)
	{
		pattern: regexp.MustCompile(`\b(os\.Open|os\.ReadFile|os\.Create|os\.OpenFile|ioutil\.ReadFile)\s*\(\s*[a-zA-Z]`),
		exclusions: []*regexp.Regexp{
			regexp.MustCompile(`\b(os\.Open|os\.ReadFile|os\.Create|os\.OpenFile|ioutil\.ReadFile)\s*\(\s*"[^"]*"`), // string literal
			regexp.MustCompile(`filepath\.(Clean|Abs)\(`),                                                           // sanitized
		},
	},
	// Node.js: fs operations with request parameters
	{
		pattern: regexp.MustCompile(`\bfs\.(readFile|readFileSync|writeFile|writeFileSync|createReadStream)\s*\(\s*(req\.|request\.)`),
		exclusions: []*regexp.Regexp{
			regexp.MustCompile(`path\.(resolve|normalize)\(`),
		},
	},
	// Python: open with request input
	{
		pattern: regexp.MustCompile(`\bopen\s*\(\s*request\.(args|form|params)`),
		exclusions: []*regexp.Regexp{},
	},
	// Path concatenation with ../
	{
		pattern: regexp.MustCompile(`["']\.\./["']\s*\+\s*[a-zA-Z]`),
		exclusions: []*regexp.Regexp{},
	},
}

func checkPathTraversal(fileDiff git.FileDiff) []Issue {
	issues := make([]Issue, 0)

	for _, hunk := range fileDiff.Hunks {
		for _, line := range hunk.Lines {
			if line.Type != "added" {
				continue
			}

			content := line.Content
			trimmed := strings.TrimSpace(content)

			if strings.HasPrefix(trimmed, "//") || strings.HasPrefix(trimmed, "#") || strings.HasPrefix(trimmed, "/*") || strings.HasPrefix(trimmed, "*") {
				continue
			}

			for _, rule := range pathTraversalRules {
				if !rule.pattern.MatchString(content) {
					continue
				}

				excluded := false
				for _, excl := range rule.exclusions {
					if excl.MatchString(content) {
						excluded = true
						break
					}
				}
				if excluded {
					continue
				}

				issues = append(issues, Issue{
					ID:         "security/path-traversal",
					Severity:   SeverityWarning,
					Category:   "security",
					File:       fileDiff.Path,
					Line:       line.NewNum,
					Message:    "Potential path traversal: user-controlled input in file path",
					Suggestion: "Validate and sanitize file paths; use filepath.Clean/filepath.Abs or path.resolve, and verify the result is within the expected directory",
				})
				break
			}
		}
	}

	return issues
}

// --- Hardcoded Secrets detection (expanded) ---

type secretRule struct {
	pattern *regexp.Regexp
	label   string
}

var secretRules = []secretRule{
	{regexp.MustCompile(`AKIA[0-9A-Z]{16}`), "AWS access key"},
	{regexp.MustCompile(`"type"\s*:\s*"service_account"`), "GCP service account key"},
	{regexp.MustCompile(`sk_live_[0-9a-zA-Z]{24,}`), "Stripe secret key"},
	{regexp.MustCompile(`rk_live_[0-9a-zA-Z]{24,}`), "Stripe restricted key"},
	{regexp.MustCompile(`-----BEGIN\s+(RSA\s+|EC\s+|DSA\s+|OPENSSH\s+)?PRIVATE KEY-----`), "Private key"},
	{regexp.MustCompile(`eyJ[A-Za-z0-9\-_]{10,}\.eyJ[A-Za-z0-9\-_]{10,}`), "JWT token"},
	{regexp.MustCompile(`gh[ps]_[A-Za-z0-9_]{36,}`), "GitHub token"},
	{regexp.MustCompile(`(?i)(api[_\-]?key|secret|token|password)\s*[:=]\s*["'][A-Za-z0-9+/=]{20,}["']`), "Hardcoded secret"},
}

var secretExclusions = []*regexp.Regexp{
	regexp.MustCompile(`["']\s*["']`),                                            // empty string
	regexp.MustCompile(`(?i)(CHANGEME|xxx|your[_\-]?key[_\-]?here|placeholder)`), // placeholder values
	regexp.MustCompile(`(?i)os\.Getenv\(`),                                       // Go env lookup
	regexp.MustCompile(`(?i)process\.env\b`),                                     // Node env lookup
	regexp.MustCompile(`(?i)os\.environ`),                                        // Python env lookup
	regexp.MustCompile(`(?i)viper\.(Get|GetString)\(`),                           // Go config library
	regexp.MustCompile(`(?i)config\.(get|Get)\(`),                                // generic config lookup
	regexp.MustCompile(`\$\{`),                                                   // template variable
	regexp.MustCompile(`<[^>]+>`),                                                // placeholder like <token>
	regexp.MustCompile(`(?i)[:=]\s*["'](example|dummy|fake|test|mock|sample)`),     // value starts with placeholder word
	regexp.MustCompile(`(?i)[:=]\s*["'][^"']*_(example|dummy|fake|test|mock|sample)["']`), // value ends with placeholder word
}

func checkSecuritySecrets(fileDiff git.FileDiff) []Issue {
	issues := make([]Issue, 0)

	for _, hunk := range fileDiff.Hunks {
		for _, line := range hunk.Lines {
			if line.Type != "added" {
				continue
			}

			content := line.Content
			trimmed := strings.TrimSpace(content)

			// Skip comments.
			if strings.HasPrefix(trimmed, "//") || strings.HasPrefix(trimmed, "#") || strings.HasPrefix(trimmed, "/*") || strings.HasPrefix(trimmed, "*") {
				continue
			}

			for _, rule := range secretRules {
				if !rule.pattern.MatchString(content) {
					continue
				}

				excluded := false
				for _, excl := range secretExclusions {
					if excl.MatchString(content) {
						excluded = true
						break
					}
				}
				if excluded {
					continue
				}

				issues = append(issues, Issue{
					ID:         "security/hardcoded-secret",
					FixID:      "sec-hardcoded-secret",
					Severity:   SeverityError,
					Category:   "security",
					File:       fileDiff.Path,
					Line:       line.NewNum,
					Message:    "Hardcoded secret detected: " + rule.label,
					Suggestion: "Use environment variables or a secrets manager; never commit secrets to source control",
				})
				break // one issue per line
			}
		}
	}

	return issues
}

// --- Insecure Crypto detection ---

type cryptoRule struct {
	pattern    *regexp.Regexp
	exclusions []*regexp.Regexp
	label      string
	jsOnly     bool // only applies to JS/TS files
}

var insecureCryptoRules = []cryptoRule{
	// MD5 usage
	{
		pattern: regexp.MustCompile(`\bmd5\.(New|Sum)\b`),
		exclusions: []*regexp.Regexp{
			regexp.MustCompile(`(?i)(checksum|hash.*file|content.?hash|etag|fingerprint)`),
		},
		label: "MD5 is cryptographically broken",
	},
	{
		pattern: regexp.MustCompile(`(?i)\bhashlib\.md5\b`),
		exclusions: []*regexp.Regexp{
			regexp.MustCompile(`(?i)(checksum|file.?hash|content.?hash|fingerprint)`),
		},
		label: "MD5 is cryptographically broken",
	},
	{
		pattern: regexp.MustCompile(`crypto\.createHash\s*\(\s*['"]md5['"]\s*\)`),
		exclusions: []*regexp.Regexp{
			regexp.MustCompile(`(?i)(checksum|etag|fingerprint)`),
		},
		label: "MD5 is cryptographically broken",
	},
	// SHA1 usage
	{
		pattern: regexp.MustCompile(`\bsha1\.(New|Sum)\b`),
		exclusions: []*regexp.Regexp{
			regexp.MustCompile(`(?i)(checksum|hash.*file|content.?hash|git|fingerprint)`),
		},
		label: "SHA1 is cryptographically weak",
	},
	{
		pattern: regexp.MustCompile(`(?i)\bhashlib\.sha1\b`),
		exclusions: []*regexp.Regexp{
			regexp.MustCompile(`(?i)(checksum|file.?hash|content.?hash|fingerprint)`),
		},
		label: "SHA1 is cryptographically weak",
	},
	{
		pattern: regexp.MustCompile(`crypto\.createHash\s*\(\s*['"]sha1['"]\s*\)`),
		exclusions: []*regexp.Regexp{
			regexp.MustCompile(`(?i)(checksum|fingerprint)`),
		},
		label: "SHA1 is cryptographically weak",
	},
	// Math.random for security (JS/TS only)
	{
		pattern: regexp.MustCompile(`Math\.random\s*\(\s*\)`),
		exclusions: []*regexp.Regexp{
			regexp.MustCompile(`(?i)(color|animation|delay|jitter|ui|display|render|shuffle|sample)`),
		},
		label:  "Math.random() is not cryptographically secure",
		jsOnly: true,
	},
}

func checkInsecureCrypto(fileDiff git.FileDiff) []Issue {
	issues := make([]Issue, 0)

	ext := strings.ToLower(filepath.Ext(fileDiff.Path))

	for _, hunk := range fileDiff.Hunks {
		for _, line := range hunk.Lines {
			if line.Type != "added" {
				continue
			}

			content := line.Content
			trimmed := strings.TrimSpace(content)

			if strings.HasPrefix(trimmed, "//") || strings.HasPrefix(trimmed, "#") || strings.HasPrefix(trimmed, "/*") || strings.HasPrefix(trimmed, "*") {
				continue
			}

			for _, rule := range insecureCryptoRules {
				if !rule.pattern.MatchString(content) {
					continue
				}

				// Language filtering: some rules only apply to JS/TS files.
				if rule.jsOnly && ext != ".js" && ext != ".ts" && ext != ".tsx" && ext != ".jsx" {
					continue
				}

				excluded := false
				for _, excl := range rule.exclusions {
					if excl.MatchString(content) {
						excluded = true
						break
					}
				}
				if excluded {
					continue
				}

				issues = append(issues, Issue{
					ID:         "security/insecure-crypto",
					FixID:      "sec-insecure-random",
					Severity:   SeverityWarning,
					Category:   "security",
					File:       fileDiff.Path,
					Line:       line.NewNum,
					Message:    "Insecure cryptography: " + rule.label,
					Suggestion: "Use SHA-256+ for hashing, crypto.getRandomValues() or crypto/rand for random values",
				})
				break
			}
		}
	}

	return issues
}

// --- Java-specific security detection ---

type javaSecurityRule struct {
	pattern    *regexp.Regexp
	exclusions []*regexp.Regexp
	id         string
	message    string
	suggestion string
}

var javaSecurityRules = []javaSecurityRule{
	{
		pattern:    regexp.MustCompile(`DocumentBuilderFactory\.newInstance\(\)`),
		exclusions: []*regexp.Regexp{regexp.MustCompile(`setFeature`)},
		id:         "security/xxe",
		message:    "Potential XXE vulnerability: DocumentBuilderFactory without disabling external entities",
		suggestion: "Call factory.setFeature(\"http://apache.org/xml/features/disallow-doctype-decl\", true) to prevent XXE attacks",
	},
	{
		pattern:    regexp.MustCompile(`ObjectInputStream\s*\(`),
		exclusions: []*regexp.Regexp{},
		id:         "security/deserialization",
		message:    "Potential insecure deserialization: ObjectInputStream on untrusted data",
		suggestion: "Avoid deserializing untrusted data. Use allowlists (ObjectInputFilter) or safer formats like JSON",
	},
	{
		pattern:    regexp.MustCompile(`\.lookup\s*\(\s*[a-zA-Z]`),
		exclusions: []*regexp.Regexp{regexp.MustCompile(`\.lookup\s*\(\s*"[^"]*"\s*\)`)},
		id:         "security/jndi-injection",
		message:    "Potential JNDI injection: lookup() with variable argument",
		suggestion: "Validate and sanitize JNDI lookup names; restrict allowed protocols and hosts",
	},
	{
		pattern: regexp.MustCompile(`(?i)(password|passwd|secret|apiKey|api_key)\s*=\s*"[^"]{8,}"`),
		exclusions: []*regexp.Regexp{
			regexp.MustCompile(`(?i)=\s*""\s*;`),
			regexp.MustCompile(`(?i)System\.getenv\(`),
			regexp.MustCompile(`(?i)System\.getProperty\(`),
			regexp.MustCompile(`(?i)(test|example|dummy|fake|mock|sample|placeholder|changeme)`),
		},
		id:         "security/hardcoded-secret",
		message:    "Hardcoded credential detected in Java code",
		suggestion: "Use environment variables or a secrets manager instead of hardcoding credentials",
	},
}

func checkJavaSecurity(fileDiff git.FileDiff) []Issue {
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

			for _, rule := range javaSecurityRules {
				if !rule.pattern.MatchString(content) {
					continue
				}

				excluded := false
				for _, excl := range rule.exclusions {
					if excl.MatchString(content) {
						excluded = true
						break
					}
				}
				if excluded {
					continue
				}

				issues = append(issues, Issue{
					ID:         rule.id,
					Severity:   SeverityError,
					Category:   "security",
					File:       fileDiff.Path,
					Line:       line.NewNum,
					Message:    rule.message,
					Suggestion: rule.suggestion,
				})
				break
			}
		}
	}

	return issues
}

// --- Rust-specific security checks ---

type rustSecurityRule struct {
	pattern    *regexp.Regexp
	exclusions []*regexp.Regexp
	id         string
	severity   Severity
	message    string
	suggestion string
}

var rustSecurityRules = []rustSecurityRule{
	{
		pattern:    regexp.MustCompile(`\bunsafe\s*\{`),
		exclusions: []*regexp.Regexp{},
		id:         "security/rust-unsafe",
		severity:   SeverityWarning,
		message:    "Unsafe block detected — requires manual review",
		suggestion: "Minimize unsafe code; document safety invariants and consider safe alternatives",
	},
	{
		pattern:    regexp.MustCompile(`\*\w+\s*\.`),
		exclusions: []*regexp.Regexp{},
		id:         "security/rust-raw-pointer",
		severity:   SeverityWarning,
		message:    "Possible raw pointer dereference",
		suggestion: "Ensure raw pointer access is within a well-documented unsafe block with verified invariants",
	},
	{
		pattern:    regexp.MustCompile(`std::process::Command::new\(`),
		exclusions: []*regexp.Regexp{},
		id:         "security/rust-command-injection",
		severity:   SeverityWarning,
		message:    "Process command execution — verify input is not user-controlled",
		suggestion: "Validate and sanitize all inputs to Command::new and .arg() to prevent command injection",
	},
	{
		pattern:    regexp.MustCompile(`(?i)(?:let|const)\s+(?:mut\s+)?(?:secret|api_key|password|token)\s*(?::\s*&?str)?\s*=\s*"[^"]{8,}"`),
		exclusions: []*regexp.Regexp{
			regexp.MustCompile(`(?i)(CHANGEME|xxx|placeholder|example|dummy|test|mock)`),
			regexp.MustCompile(`std::env::var\(`),
		},
		id:         "security/rust-hardcoded-secret",
		severity:   SeverityError,
		message:    "Possible hardcoded secret in Rust code",
		suggestion: "Use environment variables (std::env::var) or a config file instead of hardcoding secrets",
	},
}

func checkRustSecurity(fileDiff git.FileDiff) []Issue {
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

			if strings.HasPrefix(trimmed, "//") || strings.HasPrefix(trimmed, "/*") || strings.HasPrefix(trimmed, "*") {
				continue
			}

			for _, rule := range rustSecurityRules {
				if !rule.pattern.MatchString(content) {
					continue
				}

				excluded := false
				for _, excl := range rule.exclusions {
					if excl.MatchString(content) {
						excluded = true
						break
					}
				}
				if excluded {
					continue
				}

				issues = append(issues, Issue{
					ID:         rule.id,
					Severity:   rule.severity,
					Category:   "security",
					File:       fileDiff.Path,
					Line:       line.NewNum,
					Message:    rule.message,
					Suggestion: rule.suggestion,
				})
				break
			}
		}
	}

	return issues
}

// --- TypeScript/JS code execution detection ---

type codeExecRule struct {
	pattern    *regexp.Regexp
	exclusions []*regexp.Regexp
	label      string
}

var codeExecRules = []codeExecRule{
	// eval() usage
	{
		pattern: regexp.MustCompile(`\beval\s*\(`),
		exclusions: []*regexp.Regexp{
			regexp.MustCompile(`(?i)(json\.parse|JSON\.parse)`),
		},
		label: "eval() allows arbitrary code execution",
	},
	// new Function() with string argument
	{
		pattern:    regexp.MustCompile(`new\s+Function\s*\(`),
		exclusions: []*regexp.Regexp{},
		label:      "new Function() allows arbitrary code execution",
	},
	// setTimeout/setInterval with string argument (not a function reference)
	{
		pattern:    regexp.MustCompile(`\b(setTimeout|setInterval)\s*\(\s*["'\x60]`),
		exclusions: []*regexp.Regexp{},
		label:      "setTimeout/setInterval with string argument allows code injection",
	},
	// document.writeln()
	{
		pattern:    regexp.MustCompile(`document\.writeln\s*\(`),
		exclusions: []*regexp.Regexp{},
		label:      "document.writeln() can introduce XSS vulnerabilities",
	},
}

func checkTSCodeExecution(fileDiff git.FileDiff) []Issue {
	issues := make([]Issue, 0)

	ext := strings.ToLower(filepath.Ext(fileDiff.Path))
	if ext != ".ts" && ext != ".tsx" && ext != ".js" && ext != ".jsx" && ext != ".mjs" {
		return issues
	}

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

			for _, rule := range codeExecRules {
				if !rule.pattern.MatchString(content) {
					continue
				}

				excluded := false
				for _, excl := range rule.exclusions {
					if excl.MatchString(content) {
						excluded = true
						break
					}
				}
				if excluded {
					continue
				}

				issues = append(issues, Issue{
					ID:         "security/code-execution",
					Severity:   SeverityError,
					Category:   "security",
					File:       fileDiff.Path,
					Line:       line.NewNum,
					Message:    "Unsafe code execution: " + rule.label,
					Suggestion: "Avoid eval(), new Function(), and string arguments to setTimeout/setInterval; use safer alternatives",
				})
				break
			}
		}
	}

	return issues
}

// --- Ruby-specific security checks ---

type rubySecurityRule struct {
	pattern    *regexp.Regexp
	exclusions []*regexp.Regexp
	id         string
	severity   Severity
	message    string
	suggestion string
}

var rubySecurityRules = []rubySecurityRule{
	{
		pattern:    regexp.MustCompile(`\beval\s*\(`),
		exclusions: []*regexp.Regexp{},
		id:         "security/ruby-eval",
		severity:   SeverityError,
		message:    "Use of eval() allows arbitrary code execution",
		suggestion: "Avoid eval(); use safer alternatives like send() with whitelisted methods or JSON.parse for data",
	},
	{
		pattern:    regexp.MustCompile(`\b(system|exec)\s*\(`),
		exclusions: []*regexp.Regexp{},
		id:         "security/ruby-command-injection",
		severity:   SeverityWarning,
		message:    "Shell command execution — verify input is not user-controlled",
		suggestion: "Use array form of system() to avoid shell interpolation, e.g., system('cmd', arg1, arg2)",
	},
	{
		pattern: regexp.MustCompile(`\.send\s*\(\s*(params|request|input)`),
		exclusions: []*regexp.Regexp{
			regexp.MustCompile(`(?i)#`),
		},
		id:         "security/ruby-unsafe-send",
		severity:   SeverityError,
		message:    "Dynamic method dispatch with user input via send()",
		suggestion: "Validate the method name against an allowlist before calling send()",
	},
	{
		pattern: regexp.MustCompile(`\bKernel\.open\s*\(`),
		exclusions: []*regexp.Regexp{
			regexp.MustCompile(`Kernel\.open\s*\(\s*["']`),
		},
		id:         "security/ruby-kernel-open",
		severity:   SeverityWarning,
		message:    "Kernel.open() can execute shell commands via pipe character",
		suggestion: "Use File.open() or URI.open() instead of Kernel.open() to avoid command injection",
	},
	{
		pattern:    regexp.MustCompile(`\bYAML\.load\b`),
		exclusions: []*regexp.Regexp{regexp.MustCompile(`YAML\.load_file`)},
		id:         "security/ruby-yaml-load",
		severity:   SeverityWarning,
		message:    "YAML.load() can deserialize arbitrary Ruby objects",
		suggestion: "Use YAML.safe_load() instead to prevent deserialization attacks",
	},
	{
		pattern:    regexp.MustCompile(`\bMarshal\.load\s*\(`),
		exclusions: []*regexp.Regexp{},
		id:         "security/ruby-marshal-load",
		severity:   SeverityWarning,
		message:    "Marshal.load() deserializes arbitrary Ruby objects from untrusted data",
		suggestion: "Avoid Marshal.load() on untrusted input; use JSON or YAML.safe_load instead",
	},
}

func checkRubySecurity(fileDiff git.FileDiff) []Issue {
	issues := make([]Issue, 0)

	for _, hunk := range fileDiff.Hunks {
		for _, line := range hunk.Lines {
			if line.Type != "added" {
				continue
			}

			content := line.Content
			trimmed := strings.TrimSpace(content)

			if strings.HasPrefix(trimmed, "#") {
				continue
			}

			for _, rule := range rubySecurityRules {
				if !rule.pattern.MatchString(content) {
					continue
				}

				excluded := false
				for _, excl := range rule.exclusions {
					if excl.MatchString(content) {
						excluded = true
						break
					}
				}
				if excluded {
					continue
				}

				issues = append(issues, Issue{
					ID:         rule.id,
					Severity:   rule.severity,
					Category:   "security",
					File:       fileDiff.Path,
					Line:       line.NewNum,
					Message:    rule.message,
					Suggestion: rule.suggestion,
				})
				break
			}
		}
	}

	return issues
}

// --- Go-specific security checks ---

type goSecurityRule struct {
	pattern    *regexp.Regexp
	exclusions []*regexp.Regexp
	id         string
	severity   Severity
	message    string
	suggestion string
}

var goSecurityRules = []goSecurityRule{
	// http.ListenAndServe without TLS
	{
		pattern:    regexp.MustCompile(`\bhttp\.ListenAndServe\s*\(`),
		exclusions: []*regexp.Regexp{},
		id:         "security/go-no-tls",
		severity:   SeverityWarning,
		message:    "http.ListenAndServe serves traffic without TLS encryption",
		suggestion: "Use http.ListenAndServeTLS or terminate TLS at a reverse proxy",
	},
	// os/exec.Command with string concatenation
	{
		pattern: regexp.MustCompile(`exec\.Command\s*\([^)]*\+`),
		exclusions: []*regexp.Regexp{
			regexp.MustCompile(`exec\.Command\s*\(\s*"[^"]*"\s*\)`), // literal-only
		},
		id:         "security/go-command-injection",
		severity:   SeverityWarning,
		message:    "Potential command injection: exec.Command with string concatenation",
		suggestion: "Pass command arguments as separate parameters to exec.Command instead of concatenating strings",
	},
	// exec.Command with a variable as first argument (not a string literal)
	{
		pattern: regexp.MustCompile(`exec\.Command\s*\(\s*[a-zA-Z_]`),
		exclusions: []*regexp.Regexp{
			regexp.MustCompile(`exec\.Command\s*\(\s*"[^"]*"`), // starts with literal
		},
		id:         "security/go-command-injection",
		severity:   SeverityWarning,
		message:    "Potential command injection: exec.Command with variable argument",
		suggestion: "Validate and sanitize the command name; prefer hardcoded command paths",
	},
	// net/http server without timeout
	{
		pattern: regexp.MustCompile(`&http\.Server\s*\{`),
		exclusions: []*regexp.Regexp{
			regexp.MustCompile(`(?i)(ReadTimeout|WriteTimeout|IdleTimeout)`),
		},
		id:         "security/go-no-timeout",
		severity:   SeverityInfo,
		message:    "HTTP server created without explicit timeout configuration",
		suggestion: "Set ReadTimeout, WriteTimeout, and IdleTimeout on http.Server to prevent slowloris attacks",
	},
}

func checkGoSecurity(fileDiff git.FileDiff) []Issue {
	issues := make([]Issue, 0)

	ext := strings.ToLower(filepath.Ext(fileDiff.Path))
	if ext != ".go" {
		return issues
	}

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

			for _, rule := range goSecurityRules {
				if !rule.pattern.MatchString(content) {
					continue
				}

				excluded := false
				for _, excl := range rule.exclusions {
					if excl.MatchString(content) {
						excluded = true
						break
					}
				}
				if excluded {
					continue
				}

				issues = append(issues, Issue{
					ID:         rule.id,
					Severity:   rule.severity,
					Category:   "security",
					File:       fileDiff.Path,
					Line:       line.NewNum,
					Message:    rule.message,
					Suggestion: rule.suggestion,
				})
				break
			}
		}
	}

	return issues
}

// --- Python-specific security checks ---

type pythonSecurityRule struct {
	pattern    *regexp.Regexp
	exclusions []*regexp.Regexp
	id         string
	severity   Severity
	message    string
	suggestion string
}

var pythonSecurityRules = []pythonSecurityRule{
	// pickle.load / pickle.loads
	{
		pattern:    regexp.MustCompile(`\bpickle\.(load|loads)\s*\(`),
		exclusions: []*regexp.Regexp{},
		id:         "security/python-pickle",
		severity:   SeverityWarning,
		message:    "Insecure deserialization: pickle.load() can execute arbitrary code",
		suggestion: "Use JSON or a safer serialization format; avoid unpickling untrusted data",
	},
	// subprocess with shell=True
	{
		pattern:    regexp.MustCompile(`\bsubprocess\.(call|Popen|run|check_call|check_output)\s*\([^)]*shell\s*=\s*True`),
		exclusions: []*regexp.Regexp{},
		id:         "security/python-shell-injection",
		severity:   SeverityWarning,
		message:    "Potential command injection: subprocess with shell=True",
		suggestion: "Use shell=False (the default) and pass arguments as a list instead of a string",
	},
	// yaml.load without SafeLoader
	{
		pattern: regexp.MustCompile(`\byaml\.load\s*\(`),
		exclusions: []*regexp.Regexp{
			regexp.MustCompile(`Loader\s*=\s*(yaml\.)?SafeLoader`),
			regexp.MustCompile(`yaml\.safe_load`),
		},
		id:         "security/python-unsafe-yaml",
		severity:   SeverityWarning,
		message:    "Insecure YAML loading: yaml.load() without SafeLoader can execute arbitrary code",
		suggestion: "Use yaml.safe_load() or pass Loader=yaml.SafeLoader to yaml.load()",
	},
}

func checkPythonSecurity(fileDiff git.FileDiff) []Issue {
	issues := make([]Issue, 0)

	if !isPythonFile(fileDiff.Path) {
		return issues
	}

	for _, hunk := range fileDiff.Hunks {
		for _, line := range hunk.Lines {
			if line.Type != "added" {
				continue
			}

			content := line.Content
			trimmed := strings.TrimSpace(content)

			if strings.HasPrefix(trimmed, "#") {
				continue
			}

			for _, rule := range pythonSecurityRules {
				if !rule.pattern.MatchString(content) {
					continue
				}

				excluded := false
				for _, excl := range rule.exclusions {
					if excl.MatchString(content) {
						excluded = true
						break
					}
				}
				if excluded {
					continue
				}

				issues = append(issues, Issue{
					ID:         rule.id,
					Severity:   rule.severity,
					Category:   "security",
					File:       fileDiff.Path,
					Line:       line.NewNum,
					Message:    rule.message,
					Suggestion: rule.suggestion,
				})
				break
			}
		}
	}

	return issues
}

// --- TypeScript/JS-specific security checks ---

type tsSecurityRule struct {
	pattern    *regexp.Regexp
	exclusions []*regexp.Regexp
	id         string
	severity   Severity
	message    string
	suggestion string
}

var tsSecurityRules = []tsSecurityRule{
	// child_process.exec with template literal
	{
		pattern:    regexp.MustCompile("child_process\\.exec\\s*\\(\\s*`"),
		exclusions: []*regexp.Regexp{},
		id:         "security/ts-command-injection",
		severity:   SeverityWarning,
		message:    "Potential command injection: child_process.exec with template literal",
		suggestion: "Use child_process.execFile with argument array instead of exec with interpolated strings",
	},
	// child_process.exec with string concatenation
	{
		pattern:    regexp.MustCompile(`child_process\.exec\s*\([^)]*\+`),
		exclusions: []*regexp.Regexp{},
		id:         "security/ts-command-injection",
		severity:   SeverityWarning,
		message:    "Potential command injection: child_process.exec with string concatenation",
		suggestion: "Use child_process.execFile with argument array instead of exec with concatenated strings",
	},
	// require() with variable argument (not a string literal)
	{
		pattern: regexp.MustCompile(`\brequire\s*\(\s*[a-zA-Z_$]`),
		exclusions: []*regexp.Regexp{
			regexp.MustCompile(`\brequire\s*\(\s*["']`), // string literal
		},
		id:         "security/ts-dynamic-require",
		severity:   SeverityWarning,
		message:    "Dynamic require() with variable argument may load untrusted modules",
		suggestion: "Use a string literal in require() calls; validate dynamic paths if unavoidable",
	},
	// Prototype pollution: __proto__
	{
		pattern:    regexp.MustCompile(`__proto__\s*[=\[]`),
		exclusions: []*regexp.Regexp{},
		id:         "security/ts-prototype-pollution",
		severity:   SeverityWarning,
		message:    "Potential prototype pollution: __proto__ assignment",
		suggestion: "Use Object.create(null) for lookup objects and validate property names from user input",
	},
	// Prototype pollution: constructor.prototype
	{
		pattern:    regexp.MustCompile(`constructor\.prototype\s*[=\[]`),
		exclusions: []*regexp.Regexp{},
		id:         "security/ts-prototype-pollution",
		severity:   SeverityWarning,
		message:    "Potential prototype pollution: constructor.prototype assignment",
		suggestion: "Avoid modifying constructor.prototype directly; validate property names from user input",
	},
}

func checkTSSecurity(fileDiff git.FileDiff) []Issue {
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

			if strings.HasPrefix(trimmed, "//") || strings.HasPrefix(trimmed, "/*") || strings.HasPrefix(trimmed, "*") {
				continue
			}

			for _, rule := range tsSecurityRules {
				if !rule.pattern.MatchString(content) {
					continue
				}

				excluded := false
				for _, excl := range rule.exclusions {
					if excl.MatchString(content) {
						excluded = true
						break
					}
				}
				if excluded {
					continue
				}

				issues = append(issues, Issue{
					ID:         rule.id,
					Severity:   rule.severity,
					Category:   "security",
					File:       fileDiff.Path,
					Line:       line.NewNum,
					Message:    rule.message,
					Suggestion: rule.suggestion,
				})
				break
			}
		}
	}

	return issues
}

// --- Hardcoded IP address detection (cross-language) ---

// ipAddressPattern matches IPv4 addresses in string literals.
var ipAddressPattern = regexp.MustCompile(`["'](\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3})["']`)

// ipExclusions skip common non-production IPs and false positives.
var ipExclusions = []*regexp.Regexp{
	regexp.MustCompile(`["']0\.0\.0\.0["']`),     // bind-all
	regexp.MustCompile(`["']127\.0\.0\.1["']`),    // localhost
	regexp.MustCompile(`["']255\.255\.\d`),        // subnet mask
	regexp.MustCompile(`(?i)(version|semver)`),     // version strings like "1.2.3.4"
	regexp.MustCompile(`(?i)(test|mock|fake|example|placeholder)`),
}

func checkHardcodedIPs(fileDiff git.FileDiff) []Issue {
	issues := make([]Issue, 0)

	for _, hunk := range fileDiff.Hunks {
		for _, line := range hunk.Lines {
			if line.Type != "added" {
				continue
			}

			content := line.Content
			trimmed := strings.TrimSpace(content)

			if strings.HasPrefix(trimmed, "//") || strings.HasPrefix(trimmed, "#") || strings.HasPrefix(trimmed, "/*") || strings.HasPrefix(trimmed, "*") {
				continue
			}

			if !ipAddressPattern.MatchString(content) {
				continue
			}

			excluded := false
			for _, excl := range ipExclusions {
				if excl.MatchString(content) {
					excluded = true
					break
				}
			}
			if excluded {
				continue
			}

			// Validate it looks like a real IP (each octet 0-255)
			matches := ipAddressPattern.FindStringSubmatch(content)
			if len(matches) < 2 || !isValidIP(matches[1]) {
				continue
			}

			issues = append(issues, Issue{
				ID:         "security/hardcoded-ip",
				Severity:   SeverityInfo,
				Category:   "security",
				File:       fileDiff.Path,
				Line:       line.NewNum,
				Message:    "Hardcoded IP address detected: " + matches[1],
				Suggestion: "Use configuration, environment variables, or DNS names instead of hardcoded IP addresses",
			})
		}
	}

	return issues
}

// --- Kotlin-specific security checks ---

type kotlinSecurityRule struct {
	pattern    *regexp.Regexp
	exclusions []*regexp.Regexp
	id         string
	message    string
	suggestion string
}

var kotlinSecurityRules = []kotlinSecurityRule{
	// Java XXE vulnerability also applies to Kotlin
	{
		pattern:    regexp.MustCompile(`DocumentBuilderFactory\.newInstance\(\)`),
		exclusions: []*regexp.Regexp{regexp.MustCompile(`setFeature`)},
		id:         "security/xxe",
		message:    "Potential XXE vulnerability: DocumentBuilderFactory without disabling external entities",
		suggestion: "Call factory.setFeature(\"http://apache.org/xml/features/disallow-doctype-decl\", true) to prevent XXE attacks",
	},
	// Java deserialization also applies to Kotlin
	{
		pattern:    regexp.MustCompile(`ObjectInputStream\s*\(`),
		exclusions: []*regexp.Regexp{},
		id:         "security/deserialization",
		message:    "Potential insecure deserialization: ObjectInputStream on untrusted data",
		suggestion: "Avoid deserializing untrusted data. Use allowlists (ObjectInputFilter) or safer formats like JSON",
	},
	// Runtime.exec() command injection
	{
		pattern:    regexp.MustCompile(`Runtime\.getRuntime\(\)\.exec\s*\(`),
		exclusions: []*regexp.Regexp{},
		id:         "security/command-injection",
		message:    "Potential command injection via Runtime.exec()",
		suggestion: "Use ProcessBuilder with explicit argument lists instead of Runtime.exec() with string concatenation",
	},
	// ProcessBuilder with variable argument
	{
		pattern:    regexp.MustCompile(`ProcessBuilder\s*\(\s*[a-zA-Z]`),
		exclusions: []*regexp.Regexp{regexp.MustCompile(`ProcessBuilder\s*\(\s*"[^"]*"\s*\)`)},
		id:         "security/command-injection",
		message:    "Potential command injection via ProcessBuilder with variable argument",
		suggestion: "Validate and sanitize all inputs to ProcessBuilder to prevent command injection",
	},
	// Hardcoded credentials in Kotlin
	{
		pattern: regexp.MustCompile(`(?i)(password|passwd|secret|apiKey|api_key)\s*=\s*"[^"]{8,}"`),
		exclusions: []*regexp.Regexp{
			regexp.MustCompile(`(?i)=\s*""\s*$`),
			regexp.MustCompile(`(?i)System\.getenv\(`),
			regexp.MustCompile(`(?i)(test|example|dummy|fake|mock|sample|placeholder|changeme)`),
		},
		id:         "security/hardcoded-secret",
		message:    "Hardcoded credential detected in Kotlin code",
		suggestion: "Use environment variables or a secrets manager instead of hardcoding credentials",
	},
}

func checkKotlinSecurity(fileDiff git.FileDiff) []Issue {
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

			for _, rule := range kotlinSecurityRules {
				if !rule.pattern.MatchString(content) {
					continue
				}

				excluded := false
				for _, excl := range rule.exclusions {
					if excl.MatchString(content) {
						excluded = true
						break
					}
				}
				if excluded {
					continue
				}

				issues = append(issues, Issue{
					ID:         rule.id,
					Severity:   SeverityError,
					Category:   "security",
					File:       fileDiff.Path,
					Line:       line.NewNum,
					Message:    rule.message,
					Suggestion: rule.suggestion,
				})
				break
			}
		}
	}

	return issues
}

// --- C#-specific security detection ---

type csharpSecurityRule struct {
	pattern    *regexp.Regexp
	exclusions []*regexp.Regexp
	id         string
	message    string
	suggestion string
}

var csharpSecurityRules = []csharpSecurityRule{
	{
		pattern:    regexp.MustCompile(`BinaryFormatter`),
		exclusions: []*regexp.Regexp{},
		id:         "security/deserialization",
		message:    "BinaryFormatter is insecure and can lead to remote code execution via deserialization",
		suggestion: "Use System.Text.Json or a safe serializer instead of BinaryFormatter",
	},
	{
		pattern:    regexp.MustCompile(`Process\.Start\s*\([^)]*\+`),
		exclusions: []*regexp.Regexp{},
		id:         "security/command-injection",
		message:    "Potential command injection: Process.Start with string concatenation",
		suggestion: "Validate and sanitize all inputs to Process.Start; avoid string concatenation for command arguments",
	},
	{
		pattern:    regexp.MustCompile(`\$"(?i)(SELECT|INSERT|UPDATE|DELETE|DROP|ALTER)\b[^"]*\{`),
		exclusions: []*regexp.Regexp{},
		id:         "security/sql-injection",
		message:    "Potential SQL injection: string interpolation in SQL query",
		suggestion: "Use parameterized queries (SqlParameter) instead of string interpolation in SQL",
	},
	{
		pattern:    regexp.MustCompile(`\[AllowAnonymous\]`),
		exclusions: []*regexp.Regexp{},
		id:         "security/allow-anonymous",
		message:    "[AllowAnonymous] attribute detected — verify this endpoint should be publicly accessible",
		suggestion: "Review whether this endpoint handles sensitive data or actions that require authentication",
	},
}

func checkCSharpSecurity(fileDiff git.FileDiff) []Issue {
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

			for _, rule := range csharpSecurityRules {
				if !rule.pattern.MatchString(content) {
					continue
				}

				excluded := false
				for _, excl := range rule.exclusions {
					if excl.MatchString(content) {
						excluded = true
						break
					}
				}
				if excluded {
					continue
				}

				severity := SeverityWarning
				if rule.id == "security/deserialization" || rule.id == "security/sql-injection" {
					severity = SeverityError
				}

				issues = append(issues, Issue{
					ID:         rule.id,
					Severity:   severity,
					Category:   "security",
					File:       fileDiff.Path,
					Line:       line.NewNum,
					Message:    rule.message,
					Suggestion: rule.suggestion,
				})
				break
			}
		}
	}

	return issues
}

// --- PHP-specific security detection ---

type phpSecurityRule struct {
	pattern    *regexp.Regexp
	exclusions []*regexp.Regexp
	id         string
	severity   Severity
	message    string
	suggestion string
}

var phpSecurityRules = []phpSecurityRule{
	{
		pattern:    regexp.MustCompile(`\beval\s*\(\s*\$`),
		exclusions: []*regexp.Regexp{},
		id:         "security/code-execution",
		severity:   SeverityError,
		message:    "eval() with variable input allows arbitrary code execution",
		suggestion: "Avoid eval() entirely; use safer alternatives like call_user_func() or match expressions",
	},
	{
		pattern:    regexp.MustCompile(`\b(exec|system|shell_exec|passthru|popen|proc_open)\s*\(`),
		exclusions: []*regexp.Regexp{},
		id:         "security/command-injection",
		severity:   SeverityError,
		message:    "Command execution function detected — verify input is not user-controlled",
		suggestion: "Use escapeshellarg()/escapeshellcmd() to sanitize arguments, or avoid shell commands entirely",
	},
	{
		pattern:    regexp.MustCompile(`preg_replace\s*\(\s*['"][^'"]*\/e\b`),
		exclusions: []*regexp.Regexp{},
		id:         "security/code-execution",
		severity:   SeverityError,
		message:    "preg_replace with /e modifier allows code execution (deprecated since PHP 5.5)",
		suggestion: "Use preg_replace_callback() instead of the /e modifier",
	},
	{
		pattern:    regexp.MustCompile(`\bunserialize\s*\(\s*\$_(GET|POST|REQUEST|COOKIE|SERVER)`),
		exclusions: []*regexp.Regexp{},
		id:         "security/deserialization",
		severity:   SeverityError,
		message:    "Insecure deserialization: unserialize() on user input",
		suggestion: "Never unserialize user input. Use json_decode() or specify allowed_classes option",
	},
	{
		pattern:    regexp.MustCompile(`\b(include|require|include_once|require_once)\s*\(\s*\$`),
		exclusions: []*regexp.Regexp{},
		id:         "security/file-inclusion",
		severity:   SeverityError,
		message:    "File inclusion with variable path — potential local/remote file inclusion",
		suggestion: "Validate and whitelist file paths; never include files based on user input",
	},
	{
		pattern:    regexp.MustCompile(`mysqli_query\s*\(\s*\$\w+\s*,\s*["'][^"']*["']\s*\.`),
		exclusions: []*regexp.Regexp{
			regexp.MustCompile(`\?`), // parameterized
		},
		id:         "security/sql-injection",
		severity:   SeverityError,
		message:    "Potential SQL injection: string concatenation in mysqli_query()",
		suggestion: "Use prepared statements with mysqli_prepare() and bind_param() instead of string concatenation",
	},
	{
		pattern:    regexp.MustCompile(`\$_(GET|POST|REQUEST)\s*\[`),
		exclusions: []*regexp.Regexp{
			regexp.MustCompile(`htmlspecialchars`),
			regexp.MustCompile(`htmlentities`),
			regexp.MustCompile(`filter_input`),
			regexp.MustCompile(`filter_var`),
			regexp.MustCompile(`intval`),
			regexp.MustCompile(`\(int\)`),
		},
		id:         "security/input-validation",
		severity:   SeverityWarning,
		message:    "Direct use of superglobal — validate and sanitize before use",
		suggestion: "Use filter_input() or filter_var() to validate superglobal data before using it",
	},
}

func checkPHPSecurity(fileDiff git.FileDiff) []Issue {
	issues := make([]Issue, 0)

	for _, hunk := range fileDiff.Hunks {
		for _, line := range hunk.Lines {
			if line.Type != "added" {
				continue
			}

			content := line.Content
			trimmed := strings.TrimSpace(content)

			if strings.HasPrefix(trimmed, "//") || strings.HasPrefix(trimmed, "/*") ||
				strings.HasPrefix(trimmed, "*") || strings.HasPrefix(trimmed, "#") {
				continue
			}

			for _, rule := range phpSecurityRules {
				if !rule.pattern.MatchString(content) {
					continue
				}

				excluded := false
				for _, excl := range rule.exclusions {
					if excl.MatchString(content) {
						excluded = true
						break
					}
				}
				if excluded {
					continue
				}

				issues = append(issues, Issue{
					ID:         rule.id,
					Severity:   rule.severity,
					Category:   "security",
					File:       fileDiff.Path,
					Line:       line.NewNum,
					Message:    rule.message,
					Suggestion: rule.suggestion,
				})
				break
			}
		}
	}

	return issues
}

// --- Swift-specific security detection ---

type swiftSecurityRule struct {
	pattern    *regexp.Regexp
	exclusions []*regexp.Regexp
	id         string
	severity   Severity
	message    string
	suggestion string
}

var swiftSecurityRules = []swiftSecurityRule{
	{
		pattern:    regexp.MustCompile(`NSKeyedUnarchiver\s*\(`),
		exclusions: []*regexp.Regexp{regexp.MustCompile(`requiresSecureCoding`)},
		id:         "security/swift-insecure-deserialization",
		severity:   SeverityError,
		message:    "NSKeyedUnarchiver without requiresSecureCoding may allow insecure deserialization",
		suggestion: "Use NSSecureCoding and set requiresSecureCoding = true, or use NSKeyedUnarchiver.unarchivedObject(ofClass:from:)",
	},
	{
		pattern:    regexp.MustCompile(`\w+!\.[a-zA-Z]`),
		exclusions: []*regexp.Regexp{},
		id:         "security/swift-force-unwrap",
		severity:   SeverityWarning,
		message:    "Force unwrap (!) on optional may crash at runtime",
		suggestion: "Use optional binding (if let/guard let) or nil-coalescing (??) instead of force unwrapping",
	},
	{
		pattern:    regexp.MustCompile(`\btry!\s`),
		exclusions: []*regexp.Regexp{},
		id:         "security/swift-force-try",
		severity:   SeverityWarning,
		message:    "Force try (try!) will crash on error instead of handling it",
		suggestion: "Use do/catch for proper error handling, or try? to convert errors to nil",
	},
	{
		pattern:    regexp.MustCompile(`\bUnsafe(?:Mutable)?(?:Raw)?Pointer\b`),
		exclusions: []*regexp.Regexp{},
		id:         "security/swift-unsafe-pointer",
		severity:   SeverityWarning,
		message:    "Unsafe pointer usage bypasses Swift's memory safety guarantees",
		suggestion: "Use safe alternatives like Array, Data, or withUnsafeBufferPointer when possible",
	},
}

func checkSwiftSecurity(fileDiff git.FileDiff) []Issue {
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

			for _, rule := range swiftSecurityRules {
				if !rule.pattern.MatchString(content) {
					continue
				}

				excluded := false
				for _, excl := range rule.exclusions {
					if excl.MatchString(content) {
						excluded = true
						break
					}
				}
				if excluded {
					continue
				}

				issues = append(issues, Issue{
					ID:         rule.id,
					Severity:   rule.severity,
					Category:   "security",
					File:       fileDiff.Path,
					Line:       line.NewNum,
					Message:    rule.message,
					Suggestion: rule.suggestion,
				})
				break
			}
		}
	}

	return issues
}

// isValidIP validates that each octet in a dotted-quad is 0-255.
func isValidIP(ip string) bool {
	parts := strings.Split(ip, ".")
	if len(parts) != 4 {
		return false
	}
	for _, part := range parts {
		if len(part) == 0 || len(part) > 3 {
			return false
		}
		n := 0
		for _, ch := range part {
			if ch < '0' || ch > '9' {
				return false
			}
			n = n*10 + int(ch-'0')
		}
		if n > 255 {
			return false
		}
		// Reject leading zeros (e.g., "01")
		if len(part) > 1 && part[0] == '0' {
			return false
		}
	}
	return true
}
