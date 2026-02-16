package analyzer

import (
	"regexp"
	"strings"

	"github.com/joeabbey/fault/pkg/git"
)

// --- Visual Basic security ---

type vbSecurityRule struct {
	pattern    *regexp.Regexp
	exclusions []*regexp.Regexp
	id         string
	severity   Severity
	message    string
	suggestion string
}

var vbSecurityRules = []vbSecurityRule{
	{regexp.MustCompile(`(?i)\bShell\s*\(`), nil, "security/vb-shell-injection", SeverityError, "Shell() executes system commands", "Validate all inputs to Shell()"},
	{regexp.MustCompile(`(?i)\bProcess\.Start\s*\(`), nil, "security/vb-process-start", SeverityWarning, "Process.Start executes external programs", "Validate process arguments carefully"},
	{regexp.MustCompile(`(?i)\bMy\.Computer\.FileSystem\.(Delete|Write)`), nil, "security/vb-filesystem-access", SeverityWarning, "Direct filesystem operation", "Validate file paths before operations"},
	{regexp.MustCompile(`(?i)\bCreateObject\s*\(`), nil, "security/vb-createobject", SeverityWarning, "CreateObject can instantiate arbitrary COM objects", "Avoid late-bound COM object creation"},
}

func checkVisualBasicSecurity(fileDiff git.FileDiff) []Issue {
	issues := make([]Issue, 0)
	for _, hunk := range fileDiff.Hunks {
		for _, line := range hunk.Lines {
			if line.Type != "added" {
				continue
			}
			trimmed := strings.TrimSpace(line.Content)
			if strings.HasPrefix(trimmed, "'") {
				continue
			}
			for _, rule := range vbSecurityRules {
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

// --- COBOL security ---

type cobolSecurityRule struct {
	pattern    *regexp.Regexp
	exclusions []*regexp.Regexp
	id         string
	severity   Severity
	message    string
	suggestion string
}

var cobolSecurityRules = []cobolSecurityRule{
	{regexp.MustCompile(`(?i)\bCALL\s+['"]SYSTEM['"]\s+USING`), nil, "security/cobol-system-call", SeverityError, "CALL 'SYSTEM' executes OS commands", "Avoid CALL 'SYSTEM' with untrusted input"},
	{regexp.MustCompile(`(?i)\bACCEPT\s+\w+\s+FROM\s+ENVIRONMENT`), nil, "security/cobol-env-access", SeverityWarning, "ACCEPT FROM ENVIRONMENT reads env variables", "Validate environment variable values"},
}

func checkCobolSecurity(fileDiff git.FileDiff) []Issue {
	issues := make([]Issue, 0)
	for _, hunk := range fileDiff.Hunks {
		for _, line := range hunk.Lines {
			if line.Type != "added" {
				continue
			}
			trimmed := strings.TrimSpace(line.Content)
			if strings.HasPrefix(trimmed, "*>") {
				continue
			}
			// Fixed-form comment check
			raw := line.Content
			if len(raw) > 6 && (raw[6] == '*' || raw[6] == '/') {
				continue
			}
			for _, rule := range cobolSecurityRules {
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

// --- Ada security ---

type adaSecurityRule struct {
	pattern    *regexp.Regexp
	exclusions []*regexp.Regexp
	id         string
	severity   Severity
	message    string
	suggestion string
}

var adaSecurityRules = []adaSecurityRule{
	{regexp.MustCompile(`(?i)\bUnchecked_Conversion\b`), nil, "security/ada-unchecked-conversion", SeverityWarning, "Unchecked_Conversion bypasses type safety", "Use checked type conversions where possible"},
	{regexp.MustCompile(`(?i)\bUnchecked_Deallocation\b`), nil, "security/ada-unchecked-deallocation", SeverityWarning, "Unchecked_Deallocation can cause dangling pointers", "Ensure proper lifetime management"},
	{regexp.MustCompile(`(?i)\bSystem\.Address\b`), nil, "security/ada-raw-address", SeverityWarning, "System.Address bypasses type safety", "Use typed access types instead"},
	{regexp.MustCompile(`(?i)pragma\s+Suppress\s*\(\s*All_Checks`), nil, "security/ada-suppress-checks", SeverityError, "Suppressing all checks removes runtime safety", "Only suppress specific checks when necessary"},
}

func checkAdaSecurity(fileDiff git.FileDiff) []Issue {
	issues := make([]Issue, 0)
	for _, hunk := range fileDiff.Hunks {
		for _, line := range hunk.Lines {
			if line.Type != "added" {
				continue
			}
			trimmed := strings.TrimSpace(line.Content)
			if strings.HasPrefix(trimmed, "--") {
				continue
			}
			for _, rule := range adaSecurityRules {
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

// --- Pascal security ---

type pascalSecurityRule struct {
	pattern    *regexp.Regexp
	exclusions []*regexp.Regexp
	id         string
	severity   Severity
	message    string
	suggestion string
}

var pascalSecurityRules = []pascalSecurityRule{
	{regexp.MustCompile(`(?i)\bExecuteProcess\s*\(`), nil, "security/pascal-execute-process", SeverityError, "ExecuteProcess runs external commands", "Validate all arguments to ExecuteProcess"},
	{regexp.MustCompile(`(?i)\bShellExecute\s*\(`), nil, "security/pascal-shell-execute", SeverityError, "ShellExecute runs external programs", "Validate all arguments to ShellExecute"},
	{regexp.MustCompile(`(?i)\bGetMem\s*\(`), nil, "security/pascal-getmem", SeverityWarning, "GetMem allocates unmanaged memory", "Use managed types or ensure proper FreeMem"},
	{regexp.MustCompile(`(?i)\b(?:absolute|Pointer)\b`), nil, "security/pascal-unsafe-pointer", SeverityWarning, "Direct pointer usage bypasses type safety", "Use managed types when possible"},
}

func checkPascalSecurity(fileDiff git.FileDiff) []Issue {
	issues := make([]Issue, 0)
	for _, hunk := range fileDiff.Hunks {
		for _, line := range hunk.Lines {
			if line.Type != "added" {
				continue
			}
			trimmed := strings.TrimSpace(line.Content)
			if strings.HasPrefix(trimmed, "//") || strings.HasPrefix(trimmed, "{") || strings.HasPrefix(trimmed, "(*") {
				continue
			}
			for _, rule := range pascalSecurityRules {
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
