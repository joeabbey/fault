package analyzer

import (
	"regexp"
	"strings"

	"github.com/joeabbey/fault/pkg/git"
)

// Perl anti-patterns
var (
	perlTwoArgOpen = regexp.MustCompile(`\bopen\s*\(\s*\w+\s*,\s*"[^"]*"\s*\)`)
	perlGlobalVar  = regexp.MustCompile(`\$(?:GLOBAL|main::)\w+`)
)

func checkPerlPatterns(fileDiff git.FileDiff) []Issue {
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
			if perlTwoArgOpen.MatchString(line.Content) {
				issues = append(issues, Issue{ID: "patterns/perl-two-arg-open", Severity: SeverityWarning, Category: "patterns", File: fileDiff.Path, Line: line.NewNum, Message: "Two-argument open is vulnerable to injection", Suggestion: "Use three-argument open: open(my $fh, '<', $file)"})
			}
			if perlGlobalVar.MatchString(line.Content) {
				issues = append(issues, Issue{ID: "patterns/perl-global-var", Severity: SeverityInfo, Category: "patterns", File: fileDiff.Path, Line: line.NewNum, Message: "Global variable usage reduces encapsulation", Suggestion: "Use lexical variables and pass data explicitly"})
			}
		}
	}
	return issues
}

// PowerShell anti-patterns
var (
	psWriteHost = regexp.MustCompile(`(?i)\bWrite-Host\b`)
	psGlobalVar = regexp.MustCompile(`\$global:\w+`)
)

func checkPowershellPatterns(fileDiff git.FileDiff) []Issue {
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
			if psWriteHost.MatchString(line.Content) {
				issues = append(issues, Issue{ID: "patterns/powershell-write-host", FixID: "powershell-debug-print", Severity: SeverityInfo, Category: "patterns", File: fileDiff.Path, Line: line.NewNum, Message: "Write-Host bypasses the pipeline", Suggestion: "Use Write-Output or Write-Verbose for pipeline-compatible output"})
			}
			if psGlobalVar.MatchString(line.Content) {
				issues = append(issues, Issue{ID: "patterns/powershell-global-var", Severity: SeverityWarning, Category: "patterns", File: fileDiff.Path, Line: line.NewNum, Message: "Global variable usage creates hidden dependencies", Suggestion: "Use parameters and return values instead of global state"})
			}
		}
	}
	return issues
}

// Groovy anti-patterns
var (
	groovyPrintln    = regexp.MustCompile(`\bprintln\b`)
	groovyDynamicGet = regexp.MustCompile(`\.\s*getClass\s*\(\s*\)\.`)
)

func checkGroovyPatterns(fileDiff git.FileDiff) []Issue {
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
			if groovyPrintln.MatchString(line.Content) {
				issues = append(issues, Issue{ID: "patterns/groovy-println", FixID: "groovy-debug-print", Severity: SeverityInfo, Category: "patterns", File: fileDiff.Path, Line: line.NewNum, Message: "println used for output", Suggestion: "Use a logging framework (SLF4J/Logback) instead of println"})
			}
			if groovyDynamicGet.MatchString(line.Content) {
				issues = append(issues, Issue{ID: "patterns/groovy-reflection", Severity: SeverityWarning, Category: "patterns", File: fileDiff.Path, Line: line.NewNum, Message: "Reflection via getClass() can bypass security", Suggestion: "Use instanceof checks or typed references instead"})
			}
		}
	}
	return issues
}
