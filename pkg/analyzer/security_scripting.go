package analyzer

import (
	"regexp"
	"strings"

	"github.com/joeabbey/fault/pkg/git"
)

// --- Perl security ---

type perlSecurityRule struct {
	pattern    *regexp.Regexp
	exclusions []*regexp.Regexp
	id         string
	severity   Severity
	message    string
	suggestion string
}

var perlSecurityRules = []perlSecurityRule{
	{regexp.MustCompile(`\beval\s*\(`), nil, "security/perl-code-injection", SeverityError, "eval executes arbitrary code", "Avoid eval; use safe alternatives"},
	{regexp.MustCompile(`\bsystem\s*\(`), nil, "security/perl-command-injection", SeverityError, "system() executes shell commands", "Use list form of system() or IPC::Run for safe command execution"},
	{regexp.MustCompile("\\bopen\\s*\\(.*\\|"), nil, "security/perl-command-injection", SeverityError, "open with pipe executes commands", "Use IPC::Open3 or three-argument open instead"},
	{regexp.MustCompile("`[^`]+`"), nil, "security/perl-command-injection", SeverityWarning, "Backtick execution runs shell commands", "Use IPC::Run or capture for safe command execution"},
}

func checkPerlSecurity(fileDiff git.FileDiff) []Issue {
	issues := make([]Issue, 0)
	for _, hunk := range fileDiff.Hunks {
		for _, line := range hunk.Lines {
			if line.Type != "added" {
				continue
			}
			trimmed := strings.TrimSpace(line.Content)
			if strings.HasPrefix(trimmed, "#") {
				continue
			}
			for _, rule := range perlSecurityRules {
				if !rule.pattern.MatchString(line.Content) {
					continue
				}
				excluded := false
				for _, excl := range rule.exclusions {
					if excl.MatchString(line.Content) {
						excluded = true
						break
					}
				}
				if excluded {
					continue
				}
				issues = append(issues, Issue{ID: rule.id, Severity: rule.severity, Category: "security", File: fileDiff.Path, Line: line.NewNum, Message: rule.message, Suggestion: rule.suggestion})
				break
			}
		}
	}
	return issues
}

// --- PowerShell security ---

type powershellSecurityRule struct {
	pattern    *regexp.Regexp
	exclusions []*regexp.Regexp
	id         string
	severity   Severity
	message    string
	suggestion string
}

var powershellSecurityRules = []powershellSecurityRule{
	{regexp.MustCompile(`(?i)Invoke-Expression`), nil, "security/powershell-code-injection", SeverityError, "Invoke-Expression executes arbitrary code", "Avoid Invoke-Expression; use direct cmdlet calls"},
	{regexp.MustCompile(`(?i)Start-Process`), nil, "security/powershell-command-execution", SeverityWarning, "Start-Process launches external processes", "Validate process arguments and avoid user-controlled input"},
	{regexp.MustCompile(`(?i)ConvertTo-SecureString.*-AsPlainText`), nil, "security/powershell-plaintext-secret", SeverityWarning, "Converting plaintext to SecureString exposes secrets in code", "Use credential management or encrypted parameters"},
	{regexp.MustCompile(`(?i)\$ExecutionContext\.InvokeCommand`), nil, "security/powershell-code-injection", SeverityError, "InvokeCommand enables dynamic code execution", "Avoid dynamic command invocation with untrusted input"},
}

func checkPowershellSecurity(fileDiff git.FileDiff) []Issue {
	issues := make([]Issue, 0)
	for _, hunk := range fileDiff.Hunks {
		for _, line := range hunk.Lines {
			if line.Type != "added" {
				continue
			}
			trimmed := strings.TrimSpace(line.Content)
			if strings.HasPrefix(trimmed, "#") {
				continue
			}
			for _, rule := range powershellSecurityRules {
				if !rule.pattern.MatchString(line.Content) {
					continue
				}
				excluded := false
				for _, excl := range rule.exclusions {
					if excl.MatchString(line.Content) {
						excluded = true
						break
					}
				}
				if excluded {
					continue
				}
				issues = append(issues, Issue{ID: rule.id, Severity: rule.severity, Category: "security", File: fileDiff.Path, Line: line.NewNum, Message: rule.message, Suggestion: rule.suggestion})
				break
			}
		}
	}
	return issues
}

// --- Groovy security ---

type groovySecurityRule struct {
	pattern    *regexp.Regexp
	exclusions []*regexp.Regexp
	id         string
	severity   Severity
	message    string
	suggestion string
}

var groovySecurityRules = []groovySecurityRule{
	{regexp.MustCompile(`\.execute\(\)`), nil, "security/groovy-command-injection", SeverityError, "String.execute() runs system commands", "Use ProcessBuilder with explicit argument lists"},
	{regexp.MustCompile(`(?i)GroovyShell\b`), nil, "security/groovy-code-injection", SeverityError, "GroovyShell enables dynamic code execution", "Avoid GroovyShell with untrusted input"},
	{regexp.MustCompile(`(?i)Eval\s*\(`), nil, "security/groovy-code-injection", SeverityError, "Eval executes arbitrary Groovy code", "Avoid eval; use safe alternatives"},
	{regexp.MustCompile(`Runtime\.getRuntime\(\)\.exec`), nil, "security/groovy-command-injection", SeverityError, "Runtime.exec executes system commands", "Use ProcessBuilder with explicit argument lists"},
}

func checkGroovySecurity(fileDiff git.FileDiff) []Issue {
	issues := make([]Issue, 0)
	for _, hunk := range fileDiff.Hunks {
		for _, line := range hunk.Lines {
			if line.Type != "added" {
				continue
			}
			trimmed := strings.TrimSpace(line.Content)
			if strings.HasPrefix(trimmed, "//") {
				continue
			}
			for _, rule := range groovySecurityRules {
				if !rule.pattern.MatchString(line.Content) {
					continue
				}
				excluded := false
				for _, excl := range rule.exclusions {
					if excl.MatchString(line.Content) {
						excluded = true
						break
					}
				}
				if excluded {
					continue
				}
				issues = append(issues, Issue{ID: rule.id, Severity: rule.severity, Category: "security", File: fileDiff.Path, Line: line.NewNum, Message: rule.message, Suggestion: rule.suggestion})
				break
			}
		}
	}
	return issues
}
