package analyzer

import (
	"regexp"
	"strings"

	"github.com/joeabbey/fault/pkg/git"
)

// --- Zig anti-patterns ---

var (
	zigCatchUnreachable = regexp.MustCompile(`\bcatch\s+unreachable\b`)
	zigIgnoreError      = regexp.MustCompile(`_\s*=.*catch\b`)
	zigDebugPrint       = regexp.MustCompile(`std\.debug\.print\b`)
)

func checkZigAntiPatterns(fileDiff git.FileDiff) []Issue {
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

			if zigCatchUnreachable.MatchString(content) {
				issues = append(issues, Issue{
					ID: "patterns/zig-catch-unreachable", Severity: SeverityWarning, Category: "patterns",
					File: fileDiff.Path, Line: line.NewNum,
					Message:    "catch unreachable will panic on error — handle errors explicitly",
					Suggestion: "Use catch |err| to handle the error or propagate with try",
				})
			}
			if zigIgnoreError.MatchString(content) {
				issues = append(issues, Issue{
					ID: "patterns/zig-ignored-error", Severity: SeverityWarning, Category: "patterns",
					File: fileDiff.Path, Line: line.NewNum,
					Message:    "Error union result assigned to _ — error silently ignored",
					Suggestion: "Handle the error with catch or propagate with try",
				})
			}
			if zigDebugPrint.MatchString(content) {
				issues = append(issues, Issue{
					ID: "patterns/zig-debug-print", FixID: "zig-debug-print", Severity: SeverityWarning, Category: "patterns",
					File: fileDiff.Path, Line: line.NewNum,
					Message:    "std.debug.print left in code — likely a debugging artifact",
					Suggestion: "Use std.log for production logging or remove debug prints",
				})
			}
		}
	}
	return issues
}

// --- Nim anti-patterns ---

var (
	nimDiscardResult = regexp.MustCompile(`^\s*discard\s+`)
	nimVarImmutable  = regexp.MustCompile(`\bvar\s+\w+\s*=\s*[^(]`)
	nimTemplateHeavy = regexp.MustCompile(`\btemplate\s+\w+\*?\s*\(`)
)

func checkNimAntiPatterns(fileDiff git.FileDiff) []Issue {
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

			if nimDiscardResult.MatchString(trimmed) {
				issues = append(issues, Issue{
					ID: "patterns/nim-discard", Severity: SeverityWarning, Category: "patterns",
					File: fileDiff.Path, Line: line.NewNum,
					Message:    "discard may hide important return values or errors",
					Suggestion: "Check if the discarded value indicates an error condition",
				})
			}
		}
	}
	return issues
}

// --- Crystal anti-patterns ---

var (
	crystalPPDebug   = regexp.MustCompile(`\bpp\b`)
	crystalNotNilBang = regexp.MustCompile(`\.not_nil!`)
	crystalUninitialized = regexp.MustCompile(`\buninitialized\b`)
)

func checkCrystalAntiPatterns(fileDiff git.FileDiff) []Issue {
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

			if crystalPPDebug.MatchString(trimmed) && !strings.Contains(trimmed, "pp!") {
				// Only flag standalone pp calls, not pp! macro
				if strings.HasPrefix(trimmed, "pp ") || trimmed == "pp" {
					issues = append(issues, Issue{
						ID: "patterns/crystal-debug-print", FixID: "crystal-debug-print", Severity: SeverityWarning, Category: "patterns",
						File: fileDiff.Path, Line: line.NewNum,
						Message:    "pp debug call left in code",
						Suggestion: "Remove pp calls before committing; use Log for production logging",
					})
				}
			}
			if crystalNotNilBang.MatchString(content) {
				issues = append(issues, Issue{
					ID: "patterns/crystal-not-nil-bang", Severity: SeverityWarning, Category: "patterns",
					File: fileDiff.Path, Line: line.NewNum,
					Message:    ".not_nil! will raise at runtime if value is nil",
					Suggestion: "Use if/unless nil check or .try for safer nil handling",
				})
			}
			if crystalUninitialized.MatchString(content) {
				issues = append(issues, Issue{
					ID: "patterns/crystal-uninitialized", Severity: SeverityWarning, Category: "patterns",
					File: fileDiff.Path, Line: line.NewNum,
					Message:    "uninitialized creates unsafe memory — value is not zero-initialized",
					Suggestion: "Initialize values explicitly; only use uninitialized for performance-critical FFI code",
				})
			}
		}
	}
	return issues
}

// --- V language anti-patterns ---

var (
	vlangUnsafeBlock = regexp.MustCompile(`\bunsafe\s*\{`)
	vlangOrPanic     = regexp.MustCompile(`or\s*\{\s*panic\s*\(`)
	vlangManualFree  = regexp.MustCompile(`\[manualfree\]`)
)

func checkVlangAntiPatterns(fileDiff git.FileDiff) []Issue {
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

			if vlangUnsafeBlock.MatchString(content) {
				issues = append(issues, Issue{
					ID: "patterns/vlang-unsafe", Severity: SeverityWarning, Category: "patterns",
					File: fileDiff.Path, Line: line.NewNum,
					Message:    "unsafe block bypasses V's safety guarantees",
					Suggestion: "Minimize unsafe blocks; document why unsafe is necessary",
				})
			}
			if vlangOrPanic.MatchString(content) {
				issues = append(issues, Issue{
					ID: "patterns/vlang-or-panic", Severity: SeverityWarning, Category: "patterns",
					File: fileDiff.Path, Line: line.NewNum,
					Message:    "or { panic(err) } pattern crashes on error instead of handling it",
					Suggestion: "Handle the error gracefully or propagate with ! operator",
				})
			}
			if vlangManualFree.MatchString(content) {
				issues = append(issues, Issue{
					ID: "patterns/vlang-manualfree", Severity: SeverityWarning, Category: "patterns",
					File: fileDiff.Path, Line: line.NewNum,
					Message:    "[manualfree] attribute disables automatic memory management",
					Suggestion: "Prefer V's automatic memory management; only use [manualfree] for FFI code",
				})
			}
		}
	}
	return issues
}

// --- D language anti-patterns ---

var (
	dlangGoto       = regexp.MustCompile(`\bgoto\s+\w+`)
	dlangDelete     = regexp.MustCompile(`\bdelete\s+`)
	dlangGCDisable  = regexp.MustCompile(`GC\.disable\b`)
	dlangScopeExit  = regexp.MustCompile(`scope\s*\(\s*exit\s*\)`)
	dlangScopeFail  = regexp.MustCompile(`scope\s*\(\s*failure\s*\)`)
)

func checkDlangAntiPatterns(fileDiff git.FileDiff) []Issue {
	issues := make([]Issue, 0)

	hasScopeExit := false
	hasScopeFail := false

	for _, hunk := range fileDiff.Hunks {
		for _, line := range hunk.Lines {
			if line.Type != "added" {
				continue
			}
			content := line.Content
			trimmed := strings.TrimSpace(content)
			if strings.HasPrefix(trimmed, "//") || strings.HasPrefix(trimmed, "/*") || strings.HasPrefix(trimmed, "/+") {
				continue
			}

			if dlangScopeExit.MatchString(content) {
				hasScopeExit = true
			}
			if dlangScopeFail.MatchString(content) {
				hasScopeFail = true
			}

			if dlangGoto.MatchString(content) {
				issues = append(issues, Issue{
					ID: "patterns/dlang-goto", Severity: SeverityWarning, Category: "patterns",
					File: fileDiff.Path, Line: line.NewNum,
					Message:    "goto statement makes control flow harder to follow",
					Suggestion: "Use structured control flow (if/else, while, foreach) instead of goto",
				})
			}
			if dlangDelete.MatchString(content) {
				issues = append(issues, Issue{
					ID: "patterns/dlang-delete", Severity: SeverityWarning, Category: "patterns",
					File: fileDiff.Path, Line: line.NewNum,
					Message:    "delete is deprecated in D — use destroy() or let the GC handle it",
					Suggestion: "Remove explicit delete; use scope guards or RAII patterns instead",
				})
			}
			if dlangGCDisable.MatchString(content) {
				issues = append(issues, Issue{
					ID: "patterns/dlang-gc-disable", Severity: SeverityWarning, Category: "patterns",
					File: fileDiff.Path, Line: line.NewNum,
					Message:    "GC.disable() disables garbage collection — potential for memory leaks",
					Suggestion: "Only disable GC for performance-critical sections; re-enable with GC.enable()",
				})
			}
		}
	}

	// Check for scope(exit) without scope(failure)
	if hasScopeExit && !hasScopeFail {
		issues = append(issues, Issue{
			ID: "patterns/dlang-scope-without-failure", Severity: SeverityInfo, Category: "patterns",
			File: fileDiff.Path, Line: 0,
			Message:    "scope(exit) used without scope(failure) — cleanup may mask errors",
			Suggestion: "Consider adding scope(failure) for error-specific cleanup",
		})
	}

	return issues
}
