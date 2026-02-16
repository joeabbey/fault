package analyzer

import (
	"regexp"
	"strings"

	"github.com/joeabbey/fault/pkg/git"
)

// --- Haskell security ---

type haskellSecurityRule struct {
	pattern    *regexp.Regexp
	exclusions []*regexp.Regexp
	id         string
	severity   Severity
	message    string
	suggestion string
}

var haskellSecurityRules = []haskellSecurityRule{
	{regexp.MustCompile(`\bunsafePerformIO\b`), nil, "security/haskell-unsafe-io", SeverityError, "unsafePerformIO breaks referential transparency and can cause subtle bugs", "Use IO monad instead of unsafePerformIO"},
	{regexp.MustCompile(`\bunsafeCoerce\b`), nil, "security/haskell-unsafe-coerce", SeverityError, "unsafeCoerce bypasses type safety entirely", "Use safe type conversions or newtype wrappers"},
	{regexp.MustCompile(`System\.Process`), nil, "security/haskell-command-injection", SeverityWarning, "System.Process can execute arbitrary system commands", "Validate and sanitize all inputs to process execution functions"},
	{regexp.MustCompile(`\bunsafeDupablePerformIO\b`), nil, "security/haskell-unsafe-io", SeverityError, "unsafeDupablePerformIO is even more dangerous than unsafePerformIO", "Use IO monad for side effects"},
	{regexp.MustCompile(`\bunsafeInterleaveIO\b`), nil, "security/haskell-unsafe-io", SeverityWarning, "unsafeInterleaveIO creates lazy I/O which can cause resource leaks", "Use strict I/O or streaming libraries like conduit/pipes"},
}

func checkHaskellSecurity(fileDiff git.FileDiff) []Issue {
	issues := make([]Issue, 0)
	for _, hunk := range fileDiff.Hunks {
		for _, line := range hunk.Lines {
			if line.Type != "added" { continue }
			trimmed := strings.TrimSpace(line.Content)
			if strings.HasPrefix(trimmed, "--") { continue }
			for _, rule := range haskellSecurityRules {
				if !rule.pattern.MatchString(line.Content) { continue }
				excluded := false
				for _, excl := range rule.exclusions {
					if excl.MatchString(line.Content) { excluded = true; break }
				}
				if excluded { continue }
				issues = append(issues, Issue{ID: rule.id, Severity: rule.severity, Category: "security", File: fileDiff.Path, Line: line.NewNum, Message: rule.message, Suggestion: rule.suggestion})
				break
			}
		}
	}
	return issues
}

// --- Clojure security ---

type clojureSecurityRule struct {
	pattern    *regexp.Regexp
	exclusions []*regexp.Regexp
	id         string
	severity   Severity
	message    string
	suggestion string
}

var clojureSecurityRules = []clojureSecurityRule{
	{regexp.MustCompile(`\(eval\s`), nil, "security/clojure-code-injection", SeverityError, "eval allows arbitrary code execution", "Avoid eval; use safe data-driven alternatives"},
	{regexp.MustCompile(`\(read-string\s`), nil, "security/clojure-code-injection", SeverityError, "read-string can execute arbitrary code via reader macros", "Use clojure.edn/read-string for safe parsing of untrusted data"},
	{regexp.MustCompile(`clojure\.java\.shell/sh\b`), nil, "security/clojure-command-injection", SeverityWarning, "sh executes system commands — verify input is not user-controlled", "Validate and sanitize all inputs to shell commands"},
	{regexp.MustCompile(`\(resolve\s`), nil, "security/clojure-reflection", SeverityWarning, "resolve enables dynamic var lookup which can be abused", "Use direct var references instead of resolve with untrusted input"},
}

func checkClojureSecurity(fileDiff git.FileDiff) []Issue {
	issues := make([]Issue, 0)
	for _, hunk := range fileDiff.Hunks {
		for _, line := range hunk.Lines {
			if line.Type != "added" { continue }
			trimmed := strings.TrimSpace(line.Content)
			if strings.HasPrefix(trimmed, ";") { continue }
			for _, rule := range clojureSecurityRules {
				if !rule.pattern.MatchString(line.Content) { continue }
				excluded := false
				for _, excl := range rule.exclusions {
					if excl.MatchString(line.Content) { excluded = true; break }
				}
				if excluded { continue }
				issues = append(issues, Issue{ID: rule.id, Severity: rule.severity, Category: "security", File: fileDiff.Path, Line: line.NewNum, Message: rule.message, Suggestion: rule.suggestion})
				break
			}
		}
	}
	return issues
}

// --- Erlang security ---

type erlangSecurityRule struct {
	pattern    *regexp.Regexp
	exclusions []*regexp.Regexp
	id         string
	severity   Severity
	message    string
	suggestion string
}

var erlangSecurityRules = []erlangSecurityRule{
	{regexp.MustCompile(`os:cmd\s*\(`), nil, "security/erlang-command-injection", SeverityError, "os:cmd executes shell commands", "Use erlang:open_port with explicit argument lists instead"},
	{regexp.MustCompile(`erlang:apply\s*\(`), nil, "security/erlang-dynamic-apply", SeverityWarning, "erlang:apply with dynamic module/function can execute arbitrary code", "Use direct function calls or validate module/function names"},
	{regexp.MustCompile(`file:eval\s*\(`), nil, "security/erlang-code-injection", SeverityError, "file:eval evaluates arbitrary Erlang expressions from file", "Avoid file:eval; use file:consult for safe data reading"},
	{regexp.MustCompile(`erl_eval:expr`), nil, "security/erlang-code-injection", SeverityError, "erl_eval allows arbitrary expression evaluation", "Avoid dynamic evaluation of untrusted expressions"},
}

func checkErlangSecurity(fileDiff git.FileDiff) []Issue {
	issues := make([]Issue, 0)
	for _, hunk := range fileDiff.Hunks {
		for _, line := range hunk.Lines {
			if line.Type != "added" { continue }
			trimmed := strings.TrimSpace(line.Content)
			if strings.HasPrefix(trimmed, "%") { continue }
			for _, rule := range erlangSecurityRules {
				if !rule.pattern.MatchString(line.Content) { continue }
				excluded := false
				for _, excl := range rule.exclusions {
					if excl.MatchString(line.Content) { excluded = true; break }
				}
				if excluded { continue }
				issues = append(issues, Issue{ID: rule.id, Severity: rule.severity, Category: "security", File: fileDiff.Path, Line: line.NewNum, Message: rule.message, Suggestion: rule.suggestion})
				break
			}
		}
	}
	return issues
}

// --- F# security ---

type fsharpSecurityRule struct {
	pattern    *regexp.Regexp
	exclusions []*regexp.Regexp
	id         string
	severity   Severity
	message    string
	suggestion string
}

var fsharpSecurityRules = []fsharpSecurityRule{
	{regexp.MustCompile(`System\.Diagnostics\.Process`), nil, "security/fsharp-command-injection", SeverityWarning, "System.Diagnostics.Process executes system commands", "Validate and sanitize all inputs to process execution"},
	{regexp.MustCompile(`Assembly\.Load`), nil, "security/fsharp-reflection", SeverityWarning, "Assembly.Load enables dynamic code loading", "Avoid loading assemblies from untrusted sources"},
	{regexp.MustCompile(`Unchecked\.defaultof`), nil, "security/fsharp-null-ref", SeverityWarning, "Unchecked.defaultof creates null references bypassing F# null safety", "Use Option types instead of Unchecked.defaultof"},
	{regexp.MustCompile(`NativePtr\.\w+`), nil, "security/fsharp-unsafe-pointer", SeverityWarning, "NativePtr operations bypass memory safety", "Avoid NativePtr unless interfacing with native code"},
}

func checkFsharpSecurity(fileDiff git.FileDiff) []Issue {
	issues := make([]Issue, 0)
	for _, hunk := range fileDiff.Hunks {
		for _, line := range hunk.Lines {
			if line.Type != "added" { continue }
			trimmed := strings.TrimSpace(line.Content)
			if strings.HasPrefix(trimmed, "//") { continue }
			for _, rule := range fsharpSecurityRules {
				if !rule.pattern.MatchString(line.Content) { continue }
				excluded := false
				for _, excl := range rule.exclusions {
					if excl.MatchString(line.Content) { excluded = true; break }
				}
				if excluded { continue }
				issues = append(issues, Issue{ID: rule.id, Severity: rule.severity, Category: "security", File: fileDiff.Path, Line: line.NewNum, Message: rule.message, Suggestion: rule.suggestion})
				break
			}
		}
	}
	return issues
}

// --- OCaml security ---

type ocamlSecurityRule struct {
	pattern    *regexp.Regexp
	exclusions []*regexp.Regexp
	id         string
	severity   Severity
	message    string
	suggestion string
}

var ocamlSecurityRules = []ocamlSecurityRule{
	{regexp.MustCompile(`Sys\.command\b`), nil, "security/ocaml-command-injection", SeverityError, "Sys.command executes shell commands", "Use Unix.create_process with explicit argument lists"},
	{regexp.MustCompile(`Unix\.system\b`), nil, "security/ocaml-command-injection", SeverityError, "Unix.system executes shell commands via /bin/sh", "Use Unix.create_process with explicit argument lists"},
	{regexp.MustCompile(`Obj\.magic\b`), nil, "security/ocaml-unsafe-cast", SeverityError, "Obj.magic bypasses the type system entirely", "Use proper type conversions or GADT patterns instead"},
	{regexp.MustCompile(`Marshal\.from_string\b`), nil, "security/ocaml-deserialization", SeverityWarning, "Marshal.from_string can deserialize arbitrary data unsafely", "Validate input before deserialization; prefer safe formats like JSON"},
}

func checkOcamlSecurity(fileDiff git.FileDiff) []Issue {
	issues := make([]Issue, 0)
	for _, hunk := range fileDiff.Hunks {
		for _, line := range hunk.Lines {
			if line.Type != "added" { continue }
			trimmed := strings.TrimSpace(line.Content)
			if strings.HasPrefix(trimmed, "(*") { continue }
			for _, rule := range ocamlSecurityRules {
				if !rule.pattern.MatchString(line.Content) { continue }
				excluded := false
				for _, excl := range rule.exclusions {
					if excl.MatchString(line.Content) { excluded = true; break }
				}
				if excluded { continue }
				issues = append(issues, Issue{ID: rule.id, Severity: rule.severity, Category: "security", File: fileDiff.Path, Line: line.NewNum, Message: rule.message, Suggestion: rule.suggestion})
				break
			}
		}
	}
	return issues
}
