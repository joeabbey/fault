package analyzer

import (
	"regexp"
	"strings"

	"github.com/joeabbey/fault/pkg/git"
)

// --- Zig-specific security detection ---

type zigSecurityRule struct {
	pattern    *regexp.Regexp
	exclusions []*regexp.Regexp
	id         string
	severity   Severity
	message    string
	suggestion string
}

var zigSecurityRules = []zigSecurityRule{
	{
		pattern:    regexp.MustCompile(`@ptrCast\b`),
		exclusions: []*regexp.Regexp{},
		id:         "security/zig-unsafe-cast",
		severity:   SeverityWarning,
		message:    "@ptrCast is an unsafe pointer cast that bypasses type safety",
		suggestion: "Verify the cast is correct and document the safety invariants",
	},
	{
		pattern:    regexp.MustCompile(`@alignCast\b`),
		exclusions: []*regexp.Regexp{},
		id:         "security/zig-unsafe-cast",
		severity:   SeverityWarning,
		message:    "@alignCast is an unsafe alignment cast",
		suggestion: "Ensure the alignment is correct to avoid undefined behavior",
	},
	{
		pattern:    regexp.MustCompile(`@intToPtr\b`),
		exclusions: []*regexp.Regexp{},
		id:         "security/zig-unsafe-cast",
		severity:   SeverityWarning,
		message:    "@intToPtr converts an integer to a pointer — potential for memory corruption",
		suggestion: "Avoid @intToPtr unless interfacing with C; use typed pointers instead",
	},
	{
		pattern:    regexp.MustCompile(`std\.c\.system\b`),
		exclusions: []*regexp.Regexp{},
		id:         "security/zig-command-execution",
		severity:   SeverityError,
		message:    "std.c.system() executes shell commands — vulnerable to command injection",
		suggestion: "Use std.ChildProcess with explicit argument lists instead of std.c.system()",
	},
	{
		pattern:    regexp.MustCompile(`allowzero\b`),
		exclusions: []*regexp.Regexp{},
		id:         "security/zig-allowzero",
		severity:   SeverityWarning,
		message:    "allowzero pointer permits null dereference",
		suggestion: "Avoid allowzero pointers; use optional pointers (?*T) for nullable references",
	},
}

func checkZigSecurity(fileDiff git.FileDiff) []Issue {
	issues := make([]Issue, 0)

	for _, hunk := range fileDiff.Hunks {
		for _, line := range hunk.Lines {
			if line.Type != "added" {
				continue
			}
			content := line.Content
			trimmed := strings.TrimSpace(content)
			if strings.HasPrefix(trimmed, "//") {
				continue
			}
			for _, rule := range zigSecurityRules {
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
					ID: rule.id, Severity: rule.severity, Category: "security",
					File: fileDiff.Path, Line: line.NewNum,
					Message: rule.message, Suggestion: rule.suggestion,
				})
				break
			}
		}
	}
	return issues
}

// --- Nim-specific security detection ---

type nimSecurityRule struct {
	pattern    *regexp.Regexp
	exclusions []*regexp.Regexp
	id         string
	severity   Severity
	message    string
	suggestion string
}

var nimSecurityRules = []nimSecurityRule{
	{
		pattern:    regexp.MustCompile(`\{\.emit:`),
		exclusions: []*regexp.Regexp{},
		id:         "security/nim-code-injection",
		severity:   SeverityError,
		message:    "{.emit:} pragma injects raw code into the output — potential for code injection",
		suggestion: "Avoid {.emit:} with dynamic data; use proper FFI bindings instead",
	},
	{
		pattern:    regexp.MustCompile(`\{\.importc\.\}`),
		exclusions: []*regexp.Regexp{},
		id:         "security/nim-ffi",
		severity:   SeverityInfo,
		message:    "{.importc.} imports C functions without type safety guarantees",
		suggestion: "Ensure the C function signature matches exactly; consider writing a typed wrapper",
	},
	{
		pattern:    regexp.MustCompile(`\bunsafeAddr\b`),
		exclusions: []*regexp.Regexp{},
		id:         "security/nim-unsafe-addr",
		severity:   SeverityWarning,
		message:    "unsafeAddr bypasses Nim's memory safety guarantees",
		suggestion: "Use addr for mutable variables or redesign to avoid taking unsafe addresses",
	},
	{
		pattern:    regexp.MustCompile(`\bcast\[`),
		exclusions: []*regexp.Regexp{},
		id:         "security/nim-unsafe-cast",
		severity:   SeverityWarning,
		message:    "cast[] performs an unsafe type cast bypassing the type system",
		suggestion: "Use converter procs or proper type conversion instead of cast[]",
	},
	{
		pattern:    regexp.MustCompile(`\bexecCmd\b`),
		exclusions: []*regexp.Regexp{},
		id:         "security/nim-command-execution",
		severity:   SeverityWarning,
		message:    "execCmd() executes shell commands — verify input is not user-controlled",
		suggestion: "Use execProcess() with explicit arguments to prevent command injection",
	},
}

func checkNimSecurity(fileDiff git.FileDiff) []Issue {
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
			for _, rule := range nimSecurityRules {
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
					ID: rule.id, Severity: rule.severity, Category: "security",
					File: fileDiff.Path, Line: line.NewNum,
					Message: rule.message, Suggestion: rule.suggestion,
				})
				break
			}
		}
	}
	return issues
}

// --- Crystal-specific security detection ---

type crystalSecurityRule struct {
	pattern    *regexp.Regexp
	exclusions []*regexp.Regexp
	id         string
	severity   Severity
	message    string
	suggestion string
}

var crystalSecurityRules = []crystalSecurityRule{
	{
		pattern:    regexp.MustCompile(`Process\.run\b`),
		exclusions: []*regexp.Regexp{},
		id:         "security/crystal-command-execution",
		severity:   SeverityWarning,
		message:    "Process.run executes system commands — verify input is not user-controlled",
		suggestion: "Validate and sanitize all inputs to Process.run to prevent command injection",
	},
	{
		pattern:    regexp.MustCompile(`\bsystem\s*\(`),
		exclusions: []*regexp.Regexp{},
		id:         "security/crystal-command-execution",
		severity:   SeverityWarning,
		message:    "system() executes shell commands — vulnerable to command injection",
		suggestion: "Use Process.run with explicit argument arrays instead of system()",
	},
	{
		pattern:    regexp.MustCompile(`\b` + "`" + `exec` + "`" + `\s*\(`),
		exclusions: []*regexp.Regexp{},
		id:         "security/crystal-command-execution",
		severity:   SeverityWarning,
		message:    "exec() replaces the current process with a shell command",
		suggestion: "Use Process.run for safer subprocess management",
	},
	{
		pattern:    regexp.MustCompile(`IO\.popen\b`),
		exclusions: []*regexp.Regexp{},
		id:         "security/crystal-command-execution",
		severity:   SeverityWarning,
		message:    "IO.popen executes shell commands — verify input is not user-controlled",
		suggestion: "Validate and sanitize all inputs to IO.popen",
	},
	{
		pattern:    regexp.MustCompile(`\bmacro\s+`),
		exclusions: []*regexp.Regexp{},
		id:         "security/crystal-macro",
		severity:   SeverityInfo,
		message:    "Macros generate code at compile time and can be difficult to audit",
		suggestion: "Keep macros simple and well-documented; prefer methods when possible",
	},
}

func checkCrystalSecurity(fileDiff git.FileDiff) []Issue {
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
			for _, rule := range crystalSecurityRules {
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
					ID: rule.id, Severity: rule.severity, Category: "security",
					File: fileDiff.Path, Line: line.NewNum,
					Message: rule.message, Suggestion: rule.suggestion,
				})
				break
			}
		}
	}
	return issues
}

// --- V language-specific security detection ---

type vlangSecurityRule struct {
	pattern    *regexp.Regexp
	exclusions []*regexp.Regexp
	id         string
	severity   Severity
	message    string
	suggestion string
}

var vlangSecurityRules = []vlangSecurityRule{
	{
		pattern:    regexp.MustCompile(`\bunsafe\s*\{`),
		exclusions: []*regexp.Regexp{},
		id:         "security/vlang-unsafe-block",
		severity:   SeverityWarning,
		message:    "unsafe block bypasses V's memory safety guarantees",
		suggestion: "Minimize unsafe blocks and document the safety invariants",
	},
	{
		pattern:    regexp.MustCompile(`C\.system\s*\(`),
		exclusions: []*regexp.Regexp{},
		id:         "security/vlang-command-execution",
		severity:   SeverityError,
		message:    "C.system() executes shell commands — vulnerable to command injection",
		suggestion: "Use os.execute_or_panic() with explicit arguments instead of C.system()",
	},
	{
		pattern:    regexp.MustCompile(`#flag\b`),
		exclusions: []*regexp.Regexp{},
		id:         "security/vlang-ffi",
		severity:   SeverityInfo,
		message:    "#flag directive modifies compiler flags — can introduce unsafe C dependencies",
		suggestion: "Review #flag directives carefully and document their purpose",
	},
	{
		pattern:    regexp.MustCompile(`#include\b`),
		exclusions: []*regexp.Regexp{},
		id:         "security/vlang-ffi",
		severity:   SeverityInfo,
		message:    "#include directive includes C header files — ensure they are trusted",
		suggestion: "Only include headers from trusted sources; prefer V's standard library",
	},
	{
		pattern:    regexp.MustCompile(`v\.reflection\b`),
		exclusions: []*regexp.Regexp{},
		id:         "security/vlang-reflection",
		severity:   SeverityWarning,
		message:    "v.reflection enables runtime reflection which can be abused",
		suggestion: "Avoid reflection in security-sensitive code; use compile-time generics instead",
	},
}

func checkVlangSecurity(fileDiff git.FileDiff) []Issue {
	issues := make([]Issue, 0)

	for _, hunk := range fileDiff.Hunks {
		for _, line := range hunk.Lines {
			if line.Type != "added" {
				continue
			}
			content := line.Content
			trimmed := strings.TrimSpace(content)
			if strings.HasPrefix(trimmed, "//") || strings.HasPrefix(trimmed, "/*") {
				continue
			}
			for _, rule := range vlangSecurityRules {
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
					ID: rule.id, Severity: rule.severity, Category: "security",
					File: fileDiff.Path, Line: line.NewNum,
					Message: rule.message, Suggestion: rule.suggestion,
				})
				break
			}
		}
	}
	return issues
}

// --- D language-specific security detection ---

type dlangSecurityRule struct {
	pattern    *regexp.Regexp
	exclusions []*regexp.Regexp
	id         string
	severity   Severity
	message    string
	suggestion string
}

var dlangSecurityRules = []dlangSecurityRule{
	{
		pattern:    regexp.MustCompile(`__traits\s*\(\s*getMember`),
		exclusions: []*regexp.Regexp{},
		id:         "security/dlang-reflection",
		severity:   SeverityWarning,
		message:    "__traits(getMember) uses runtime reflection which can be abused",
		suggestion: "Prefer compile-time introspection or explicit method calls over __traits(getMember)",
	},
	{
		pattern:    regexp.MustCompile(`std\.process\.execute\b`),
		exclusions: []*regexp.Regexp{},
		id:         "security/dlang-command-execution",
		severity:   SeverityWarning,
		message:    "std.process.execute runs system commands — verify input is not user-controlled",
		suggestion: "Validate and sanitize all inputs to std.process.execute",
	},
	{
		pattern:    regexp.MustCompile(`\bmixin\s*\(`),
		exclusions: []*regexp.Regexp{},
		id:         "security/dlang-code-injection",
		severity:   SeverityWarning,
		message:    "mixin() evaluates strings as D code — potential for code injection",
		suggestion: "Avoid mixin() with dynamic strings; use template mixins for compile-time code generation",
	},
	{
		pattern:    regexp.MustCompile(`cast\s*\(\s*void\s*\*\s*\)`),
		exclusions: []*regexp.Regexp{},
		id:         "security/dlang-unsafe-cast",
		severity:   SeverityWarning,
		message:    "cast(void*) erases type information — potential for memory corruption",
		suggestion: "Use typed casts or templates instead of casting to void*",
	},
	{
		pattern:    regexp.MustCompile(`@system\b`),
		exclusions: []*regexp.Regexp{},
		id:         "security/dlang-system-code",
		severity:   SeverityInfo,
		message:    "@system attribute marks code as unsafe — allows pointer arithmetic and casts",
		suggestion: "Prefer @safe or @trusted code; minimize @system sections and audit them carefully",
	},
}

func checkDlangSecurity(fileDiff git.FileDiff) []Issue {
	issues := make([]Issue, 0)

	for _, hunk := range fileDiff.Hunks {
		for _, line := range hunk.Lines {
			if line.Type != "added" {
				continue
			}
			content := line.Content
			trimmed := strings.TrimSpace(content)
			if strings.HasPrefix(trimmed, "//") || strings.HasPrefix(trimmed, "/*") || strings.HasPrefix(trimmed, "*") || strings.HasPrefix(trimmed, "/+") {
				continue
			}
			for _, rule := range dlangSecurityRules {
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
					ID: rule.id, Severity: rule.severity, Category: "security",
					File: fileDiff.Path, Line: line.NewNum,
					Message: rule.message, Suggestion: rule.suggestion,
				})
				break
			}
		}
	}
	return issues
}
