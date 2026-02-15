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
		issues = append(issues, checkTSCodeExecution(fileDiff)...)
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
