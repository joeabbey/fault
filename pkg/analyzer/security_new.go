package analyzer

import (
	"regexp"
	"strings"

	"github.com/joeabbey/fault/pkg/git"
)

// --- C-specific security detection ---

type cSecurityRule struct {
	pattern    *regexp.Regexp
	exclusions []*regexp.Regexp
	id         string
	severity   Severity
	message    string
	suggestion string
}

var cSecurityRules = []cSecurityRule{
	{
		pattern:    regexp.MustCompile(`\bgets\s*\(`),
		exclusions: []*regexp.Regexp{},
		id:         "security/c-banned-function",
		severity:   SeverityError,
		message:    "Banned function gets() is vulnerable to buffer overflow",
		suggestion: "Use fgets() with a specified buffer size instead of gets()",
	},
	{
		pattern:    regexp.MustCompile(`\bstrcpy\s*\(`),
		exclusions: []*regexp.Regexp{regexp.MustCompile(`\bstrncpy\s*\(`)},
		id:         "security/c-buffer-overflow",
		severity:   SeverityError,
		message:    "strcpy() is vulnerable to buffer overflow",
		suggestion: "Use strncpy() with a specified buffer size instead of strcpy()",
	},
	{
		pattern:    regexp.MustCompile(`\bstrcat\s*\(`),
		exclusions: []*regexp.Regexp{regexp.MustCompile(`\bstrncat\s*\(`)},
		id:         "security/c-buffer-overflow",
		severity:   SeverityError,
		message:    "strcat() is vulnerable to buffer overflow",
		suggestion: "Use strncat() with a specified buffer size instead of strcat()",
	},
	{
		pattern:    regexp.MustCompile(`\bsprintf\s*\(`),
		exclusions: []*regexp.Regexp{regexp.MustCompile(`\bsnprintf\s*\(`)},
		id:         "security/c-buffer-overflow",
		severity:   SeverityError,
		message:    "sprintf() is vulnerable to buffer overflow",
		suggestion: "Use snprintf() with a specified buffer size instead of sprintf()",
	},
	{
		pattern:    regexp.MustCompile(`\bsystem\s*\(`),
		exclusions: []*regexp.Regexp{},
		id:         "security/c-command-injection",
		severity:   SeverityWarning,
		message:    "system() executes shell commands — verify input is not user-controlled",
		suggestion: "Avoid system(); use exec-family functions with explicit argument lists to prevent command injection",
	},
	{
		pattern:    regexp.MustCompile(`\bpopen\s*\(`),
		exclusions: []*regexp.Regexp{},
		id:         "security/c-command-injection",
		severity:   SeverityWarning,
		message:    "popen() executes shell commands — verify input is not user-controlled",
		suggestion: "Avoid popen(); use pipe()/fork()/exec() for safer process creation",
	},
}

func checkCSecurity(fileDiff git.FileDiff) []Issue {
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

			for _, rule := range cSecurityRules {
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

// --- C++-specific security detection ---

type cppSecurityRule struct {
	pattern    *regexp.Regexp
	exclusions []*regexp.Regexp
	id         string
	severity   Severity
	message    string
	suggestion string
}

var cppSecurityRules = []cppSecurityRule{
	{
		pattern:    regexp.MustCompile(`\bnew\s+`),
		exclusions: []*regexp.Regexp{
			regexp.MustCompile(`(make_unique|make_shared|unique_ptr|shared_ptr)`),
		},
		id:         "security/cpp-raw-new",
		severity:   SeverityWarning,
		message:    "Raw new without smart pointer context — potential memory leak",
		suggestion: "Prefer std::make_unique or std::make_shared over raw new",
	},
	{
		pattern:    regexp.MustCompile(`\breinterpret_cast\b`),
		exclusions: []*regexp.Regexp{},
		id:         "security/cpp-unsafe-cast",
		severity:   SeverityWarning,
		message:    "reinterpret_cast is an unsafe cast that bypasses type safety",
		suggestion: "Use static_cast or dynamic_cast where possible; document the safety invariants if reinterpret_cast is necessary",
	},
	{
		pattern:    regexp.MustCompile(`\bconst_cast\b`),
		exclusions: []*regexp.Regexp{},
		id:         "security/cpp-const-cast",
		severity:   SeverityWarning,
		message:    "const_cast removes const safety guarantees",
		suggestion: "Redesign the API to avoid casting away const; modifying a const object leads to undefined behavior",
	},
}

func checkCppSecurity(fileDiff git.FileDiff) []Issue {
	issues := make([]Issue, 0)

	// C++ inherits all C rules.
	issues = append(issues, checkCSecurity(fileDiff)...)

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

			for _, rule := range cppSecurityRules {
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

// --- Objective-C-specific security detection ---

type objcSecurityRule struct {
	pattern    *regexp.Regexp
	exclusions []*regexp.Regexp
	id         string
	severity   Severity
	message    string
	suggestion string
}

var objcSecurityRules = []objcSecurityRule{
	{
		pattern:    regexp.MustCompile(`NSKeyedUnarchiver`),
		exclusions: []*regexp.Regexp{regexp.MustCompile(`requiresSecureCoding`)},
		id:         "security/objc-insecure-deserialization",
		severity:   SeverityError,
		message:    "NSKeyedUnarchiver without requiresSecureCoding may allow insecure deserialization",
		suggestion: "Use NSSecureCoding and set requiresSecureCoding = YES",
	},
	{
		pattern:    regexp.MustCompile(`stringWithFormat:\s*[a-zA-Z_]`),
		exclusions: []*regexp.Regexp{regexp.MustCompile(`stringWithFormat:\s*@"`)},
		id:         "security/objc-format-string",
		severity:   SeverityWarning,
		message:    "Potential format string vulnerability: stringWithFormat with variable (not literal) format",
		suggestion: "Use a string literal as the format argument to stringWithFormat:",
	},
	{
		pattern:    regexp.MustCompile(`\bNSLog\s*\(`),
		exclusions: []*regexp.Regexp{},
		id:         "security/objc-information-disclosure",
		severity:   SeverityInfo,
		message:    "NSLog() may disclose sensitive information in production logs",
		suggestion: "Remove NSLog() calls with sensitive data or use a logging framework with log levels",
	},
}

func checkObjCSecurity(fileDiff git.FileDiff) []Issue {
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

			for _, rule := range objcSecurityRules {
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

// --- Bash-specific security detection ---

type bashSecurityRule struct {
	pattern    *regexp.Regexp
	exclusions []*regexp.Regexp
	id         string
	severity   Severity
	message    string
	suggestion string
}

var bashSecurityRules = []bashSecurityRule{
	{
		pattern:    regexp.MustCompile(`\beval\s+`),
		exclusions: []*regexp.Regexp{},
		id:         "security/bash-code-injection",
		severity:   SeverityError,
		message:    "eval allows arbitrary code injection",
		suggestion: "Avoid eval; use arrays and proper quoting for dynamic command construction",
	},
	{
		pattern:    regexp.MustCompile(`curl.*\|\s*(sh|bash)`),
		exclusions: []*regexp.Regexp{},
		id:         "security/bash-pipe-to-shell",
		severity:   SeverityError,
		message:    "Piping curl output to shell is dangerous — executes arbitrary remote code",
		suggestion: "Download the script first, review it, then execute it",
	},
	{
		pattern:    regexp.MustCompile(`chmod\s+777\b`),
		exclusions: []*regexp.Regexp{},
		id:         "security/bash-overly-permissive",
		severity:   SeverityWarning,
		message:    "chmod 777 sets overly permissive file permissions",
		suggestion: "Use more restrictive permissions (e.g., 755 for directories, 644 for files)",
	},
	{
		pattern:    regexp.MustCompile(`chmod.*o\+w`),
		exclusions: []*regexp.Regexp{},
		id:         "security/bash-world-writable",
		severity:   SeverityWarning,
		message:    "Setting world-writable permissions is a security risk",
		suggestion: "Avoid world-writable permissions; use group permissions or ACLs instead",
	},
}

func checkBashSecurity(fileDiff git.FileDiff) []Issue {
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

			for _, rule := range bashSecurityRules {
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

// --- SQL-specific security detection ---

type sqlSecurityRule struct {
	pattern    *regexp.Regexp
	exclusions []*regexp.Regexp
	id         string
	severity   Severity
	message    string
	suggestion string
}

var sqlFileSecurityRules = []sqlSecurityRule{
	{
		pattern:    regexp.MustCompile(`(?i)\bGRANT\s+ALL\b`),
		exclusions: []*regexp.Regexp{},
		id:         "security/sql-overly-permissive",
		severity:   SeverityWarning,
		message:    "GRANT ALL is overly permissive — grants all privileges",
		suggestion: "Grant only the specific privileges needed (SELECT, INSERT, UPDATE, etc.)",
	},
	{
		pattern:    regexp.MustCompile(`(?i)\bDROP\s+TABLE\b`),
		exclusions: []*regexp.Regexp{regexp.MustCompile(`(?i)IF\s+EXISTS`)},
		id:         "security/sql-drop-without-guard",
		severity:   SeverityWarning,
		message:    "DROP TABLE without IF EXISTS may cause errors if table does not exist",
		suggestion: "Use DROP TABLE IF EXISTS to prevent errors in idempotent scripts",
	},
	{
		pattern:    regexp.MustCompile(`(?i)\bEXECUTE\s+IMMEDIATE\b`),
		exclusions: []*regexp.Regexp{},
		id:         "security/sql-dynamic-execution",
		severity:   SeverityWarning,
		message:    "EXECUTE IMMEDIATE with dynamic SQL is vulnerable to injection",
		suggestion: "Use parameterized queries or bind variables instead of dynamic SQL",
	},
	{
		pattern:    regexp.MustCompile(`(?i)\bxp_cmdshell\b`),
		exclusions: []*regexp.Regexp{},
		id:         "security/sql-command-execution",
		severity:   SeverityError,
		message:    "xp_cmdshell allows operating system command execution from SQL Server",
		suggestion: "Avoid xp_cmdshell; use application-level logic for system operations",
	},
}

func checkSQLFileSecurity(fileDiff git.FileDiff) []Issue {
	issues := make([]Issue, 0)

	for _, hunk := range fileDiff.Hunks {
		for _, line := range hunk.Lines {
			if line.Type != "added" {
				continue
			}

			content := line.Content
			trimmed := strings.TrimSpace(content)

			if strings.HasPrefix(trimmed, "--") || strings.HasPrefix(trimmed, "/*") || strings.HasPrefix(trimmed, "*") {
				continue
			}

			for _, rule := range sqlFileSecurityRules {
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

// --- Dart-specific security detection ---

type dartSecurityRule struct {
	pattern    *regexp.Regexp
	exclusions []*regexp.Regexp
	id         string
	severity   Severity
	message    string
	suggestion string
}

var dartSecurityRules = []dartSecurityRule{
	{
		pattern:    regexp.MustCompile(`Process\.(run|start)\s*\(`),
		exclusions: []*regexp.Regexp{},
		id:         "security/dart-command-execution",
		severity:   SeverityWarning,
		message:    "Process.run/Process.start executes system commands — verify input is not user-controlled",
		suggestion: "Validate and sanitize all inputs to Process.run/Process.start to prevent command injection",
	},
	{
		pattern:    regexp.MustCompile(`import\s+['"]dart:mirrors['"]`),
		exclusions: []*regexp.Regexp{},
		id:         "security/dart-reflection",
		severity:   SeverityWarning,
		message:    "dart:mirrors import enables reflection which can be abused for code injection",
		suggestion: "Avoid dart:mirrors in production code; use code generation or explicit type checks instead",
	},
	{
		pattern:    regexp.MustCompile(`\bHttpClient\s*\(`),
		exclusions: []*regexp.Regexp{regexp.MustCompile(`(?i)(certificate|badCertificateCallback|SecurityContext)`)},
		id:         "security/dart-insecure-http",
		severity:   SeverityInfo,
		message:    "HttpClient created without explicit certificate validation",
		suggestion: "Configure badCertificateCallback or use a SecurityContext for proper TLS certificate validation",
	},
}

func checkDartSecurity(fileDiff git.FileDiff) []Issue {
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

			for _, rule := range dartSecurityRules {
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

// --- Scala-specific security detection ---

type scalaSecurityRule struct {
	pattern    *regexp.Regexp
	exclusions []*regexp.Regexp
	id         string
	severity   Severity
	message    string
	suggestion string
}

var scalaSecurityRules = []scalaSecurityRule{
	{
		pattern:    regexp.MustCompile(`Runtime\.getRuntime\(\)\.exec\s*\(`),
		exclusions: []*regexp.Regexp{},
		id:         "security/scala-command-injection",
		severity:   SeverityError,
		message:    "Potential command injection via Runtime.exec()",
		suggestion: "Use ProcessBuilder with explicit argument lists instead of Runtime.exec()",
	},
	{
		pattern:    regexp.MustCompile(`scala\.sys\.process`),
		exclusions: []*regexp.Regexp{},
		id:         "security/scala-process-injection",
		severity:   SeverityWarning,
		message:    "scala.sys.process with string interpolation may be vulnerable to command injection",
		suggestion: "Validate and sanitize all inputs; use Seq-based process construction instead of string interpolation",
	},
	{
		pattern:    regexp.MustCompile(`XML\.load\s*\(`),
		exclusions: []*regexp.Regexp{},
		id:         "security/scala-xxe",
		severity:   SeverityWarning,
		message:    "XML.load() may be vulnerable to XXE (XML External Entity) attacks",
		suggestion: "Use a SAX parser with external entities disabled, or use a JSON format instead",
	},
	{
		pattern:    regexp.MustCompile(`ObjectInputStream\s*\(`),
		exclusions: []*regexp.Regexp{},
		id:         "security/scala-deserialization",
		severity:   SeverityError,
		message:    "Potential insecure deserialization: ObjectInputStream on untrusted data",
		suggestion: "Avoid deserializing untrusted data. Use allowlists (ObjectInputFilter) or safer formats like JSON",
	},
}

func checkScalaSecurity(fileDiff git.FileDiff) []Issue {
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

			for _, rule := range scalaSecurityRules {
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

// --- R-specific security detection ---

type rSecurityRule struct {
	pattern    *regexp.Regexp
	exclusions []*regexp.Regexp
	id         string
	severity   Severity
	message    string
	suggestion string
}

var rSecurityRules = []rSecurityRule{
	{
		pattern:    regexp.MustCompile(`\bsystem\s*\(`),
		exclusions: []*regexp.Regexp{},
		id:         "security/r-command-execution",
		severity:   SeverityWarning,
		message:    "system() executes shell commands — verify input is not user-controlled",
		suggestion: "Validate and sanitize all inputs to system(); use system2() with explicit argument vectors when possible",
	},
	{
		pattern:    regexp.MustCompile(`\bsystem2\s*\(`),
		exclusions: []*regexp.Regexp{},
		id:         "security/r-command-execution",
		severity:   SeverityWarning,
		message:    "system2() executes shell commands — verify input is not user-controlled",
		suggestion: "Validate and sanitize all inputs to system2() to prevent command injection",
	},
	{
		pattern:    regexp.MustCompile(`\beval\s*\(\s*parse\s*\(\s*text\s*=`),
		exclusions: []*regexp.Regexp{},
		id:         "security/r-code-injection",
		severity:   SeverityError,
		message:    "eval(parse(text=...)) allows arbitrary code injection",
		suggestion: "Avoid eval(parse(text=...)); use safer alternatives like switch() or match.arg()",
	},
	{
		pattern:    regexp.MustCompile(`\breadRDS\s*\(\s*url\s*\(`),
		exclusions: []*regexp.Regexp{},
		id:         "security/r-unsafe-deserialization",
		severity:   SeverityWarning,
		message:    "Loading serialized R objects from URLs may execute arbitrary code",
		suggestion: "Only load RDS files from trusted sources; validate the URL and verify data integrity",
	},
}

func checkRSecurity(fileDiff git.FileDiff) []Issue {
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

			for _, rule := range rSecurityRules {
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

// --- Elixir-specific security detection ---

type elixirSecurityRule struct {
	pattern    *regexp.Regexp
	exclusions []*regexp.Regexp
	id         string
	severity   Severity
	message    string
	suggestion string
}

var elixirSecurityRules = []elixirSecurityRule{
	{
		pattern:    regexp.MustCompile(`Code\.eval_string\s*\(`),
		exclusions: []*regexp.Regexp{},
		id:         "security/elixir-code-injection",
		severity:   SeverityError,
		message:    "Code.eval_string() allows arbitrary code injection",
		suggestion: "Avoid Code.eval_string() with user input; use pattern matching or safe alternatives",
	},
	{
		pattern:    regexp.MustCompile(`System\.cmd\s*\(`),
		exclusions: []*regexp.Regexp{},
		id:         "security/elixir-command-injection",
		severity:   SeverityWarning,
		message:    "System.cmd() executes system commands — verify input is not user-controlled",
		suggestion: "Validate and sanitize all inputs to System.cmd() to prevent command injection",
	},
	{
		pattern:    regexp.MustCompile(`:os\.cmd\s*\(`),
		exclusions: []*regexp.Regexp{},
		id:         "security/elixir-command-execution",
		severity:   SeverityWarning,
		message:    ":os.cmd() executes operating system commands",
		suggestion: "Use System.cmd() instead of :os.cmd() for better argument handling; validate all inputs",
	},
}

func checkElixirSecurity(fileDiff git.FileDiff) []Issue {
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

			for _, rule := range elixirSecurityRules {
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

// --- Lua-specific security detection ---

type luaSecurityRule struct {
	pattern    *regexp.Regexp
	exclusions []*regexp.Regexp
	id         string
	severity   Severity
	message    string
	suggestion string
}

var luaSecurityRules = []luaSecurityRule{
	{
		pattern:    regexp.MustCompile(`\bloadstring\s*\(`),
		exclusions: []*regexp.Regexp{},
		id:         "security/lua-code-execution",
		severity:   SeverityError,
		message:    "loadstring() compiles and can execute arbitrary Lua code",
		suggestion: "Avoid loadstring() with user input; use safe data formats like JSON for dynamic data",
	},
	{
		pattern:    regexp.MustCompile(`\bload\s*\(`),
		exclusions: []*regexp.Regexp{regexp.MustCompile(`\bloadstring\s*\(`)},
		id:         "security/lua-code-execution",
		severity:   SeverityError,
		message:    "load() compiles and can execute arbitrary Lua code",
		suggestion: "Avoid load() with user input; use safe data formats like JSON for dynamic data",
	},
	{
		pattern:    regexp.MustCompile(`\bos\.execute\s*\(`),
		exclusions: []*regexp.Regexp{},
		id:         "security/lua-command-execution",
		severity:   SeverityWarning,
		message:    "os.execute() runs operating system commands — verify input is not user-controlled",
		suggestion: "Validate and sanitize all inputs to os.execute() to prevent command injection",
	},
	{
		pattern:    regexp.MustCompile(`\bio\.popen\s*\(`),
		exclusions: []*regexp.Regexp{},
		id:         "security/lua-command-execution",
		severity:   SeverityWarning,
		message:    "io.popen() runs operating system commands — verify input is not user-controlled",
		suggestion: "Validate and sanitize all inputs to io.popen() to prevent command injection",
	},
	{
		pattern:    regexp.MustCompile(`\bdebug\.getinfo\s*\(`),
		exclusions: []*regexp.Regexp{},
		id:         "security/lua-introspection",
		severity:   SeverityInfo,
		message:    "debug.getinfo() enables runtime introspection which can be abused",
		suggestion: "Avoid exposing debug library functions in production; restrict access to the debug module",
	},
}

func checkLuaSecurity(fileDiff git.FileDiff) []Issue {
	issues := make([]Issue, 0)

	for _, hunk := range fileDiff.Hunks {
		for _, line := range hunk.Lines {
			if line.Type != "added" {
				continue
			}

			content := line.Content
			trimmed := strings.TrimSpace(content)

			if strings.HasPrefix(trimmed, "--") {
				continue
			}

			for _, rule := range luaSecurityRules {
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
