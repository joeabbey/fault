package analyzer

import (
	"regexp"
	"strings"

	"github.com/joeabbey/fault/pkg/git"
)

// Haskell anti-patterns
var (
	haskellStringType    = regexp.MustCompile(`\bString\b`)
	haskellHeadTail      = regexp.MustCompile(`\b(head|tail)\s`)
	haskellDebugTrace    = regexp.MustCompile(`\bDebug\.Trace\b|trace\s`)
)

func checkHaskellPatterns(fileDiff git.FileDiff) []Issue {
	issues := make([]Issue, 0)
	for _, hunk := range fileDiff.Hunks {
		for _, line := range hunk.Lines {
			if line.Type != "added" { continue }
			trimmed := strings.TrimSpace(line.Content)
			if strings.HasPrefix(trimmed, "--") { continue }
			if haskellStringType.MatchString(line.Content) && strings.Contains(line.Content, "::") {
				issues = append(issues, Issue{ID: "patterns/haskell-string-type", Severity: SeverityInfo, Category: "patterns", File: fileDiff.Path, Line: line.NewNum, Message: "Using String type (linked list of Char) instead of Text", Suggestion: "Use Data.Text for better performance in production code"})
			}
			if haskellHeadTail.MatchString(line.Content) {
				issues = append(issues, Issue{ID: "patterns/haskell-partial-function", Severity: SeverityWarning, Category: "patterns", File: fileDiff.Path, Line: line.NewNum, Message: "head/tail are partial functions that crash on empty lists", Suggestion: "Use pattern matching or safe alternatives like listToMaybe"})
			}
			if haskellDebugTrace.MatchString(line.Content) {
				issues = append(issues, Issue{ID: "patterns/haskell-debug-trace", Severity: SeverityWarning, Category: "patterns", File: fileDiff.Path, Line: line.NewNum, Message: "Debug.Trace left in code", Suggestion: "Remove Debug.Trace calls before production"})
			}
		}
	}
	return issues
}

// Clojure anti-patterns
var (
	clojureDefInsideDefn = regexp.MustCompile(`^\s+\(def\s`)
	clojureThreadSleep   = regexp.MustCompile(`Thread/sleep`)
)

func checkClojurePatterns(fileDiff git.FileDiff) []Issue {
	issues := make([]Issue, 0)
	for _, hunk := range fileDiff.Hunks {
		for _, line := range hunk.Lines {
			if line.Type != "added" { continue }
			trimmed := strings.TrimSpace(line.Content)
			if strings.HasPrefix(trimmed, ";") { continue }
			if clojureDefInsideDefn.MatchString(line.Content) {
				issues = append(issues, Issue{ID: "patterns/clojure-nested-def", Severity: SeverityWarning, Category: "patterns", File: fileDiff.Path, Line: line.NewNum, Message: "def inside function creates mutable global state", Suggestion: "Use let bindings for local state or atoms for shared state"})
			}
			if clojureThreadSleep.MatchString(line.Content) {
				issues = append(issues, Issue{ID: "patterns/clojure-thread-sleep", Severity: SeverityWarning, Category: "patterns", File: fileDiff.Path, Line: line.NewNum, Message: "Thread/sleep blocks the current thread", Suggestion: "Use core.async timeout channels or scheduled executors"})
			}
		}
	}
	return issues
}

// Erlang anti-patterns
var (
	erlangListConcat = regexp.MustCompile(`\+\+\s*\[`)
	erlangCatchAll   = regexp.MustCompile(`catch\s+_:_\s*->`)
)

func checkErlangPatterns(fileDiff git.FileDiff) []Issue {
	issues := make([]Issue, 0)
	for _, hunk := range fileDiff.Hunks {
		for _, line := range hunk.Lines {
			if line.Type != "added" { continue }
			trimmed := strings.TrimSpace(line.Content)
			if strings.HasPrefix(trimmed, "%") { continue }
			if erlangListConcat.MatchString(line.Content) {
				issues = append(issues, Issue{ID: "patterns/erlang-list-append", Severity: SeverityInfo, Category: "patterns", File: fileDiff.Path, Line: line.NewNum, Message: "List concatenation (++) copies the left-hand list", Suggestion: "Build lists with [H|T] prepend and reverse at the end for better performance"})
			}
			if erlangCatchAll.MatchString(line.Content) {
				issues = append(issues, Issue{ID: "patterns/erlang-catch-all", Severity: SeverityWarning, Category: "patterns", File: fileDiff.Path, Line: line.NewNum, Message: "Catch-all clause hides unexpected errors", Suggestion: "Catch specific exception classes and patterns"})
			}
		}
	}
	return issues
}

// F# anti-patterns
var (
	fsharpMutable = regexp.MustCompile(`\blet\s+mutable\b`)
	fsharpPrintfn = regexp.MustCompile(`\bprintfn\b`)
)

func checkFsharpPatterns(fileDiff git.FileDiff) []Issue {
	issues := make([]Issue, 0)
	for _, hunk := range fileDiff.Hunks {
		for _, line := range hunk.Lines {
			if line.Type != "added" { continue }
			trimmed := strings.TrimSpace(line.Content)
			if strings.HasPrefix(trimmed, "//") { continue }
			if fsharpMutable.MatchString(line.Content) {
				issues = append(issues, Issue{ID: "patterns/fsharp-mutable", Severity: SeverityInfo, Category: "patterns", File: fileDiff.Path, Line: line.NewNum, Message: "Mutable binding reduces referential transparency", Suggestion: "Prefer immutable values; use ref cells or agents if mutation is needed"})
			}
			if fsharpPrintfn.MatchString(line.Content) {
				issues = append(issues, Issue{ID: "patterns/fsharp-printfn", Severity: SeverityInfo, Category: "patterns", File: fileDiff.Path, Line: line.NewNum, Message: "printfn used for output — consider using a logging framework", Suggestion: "Use a structured logging library instead of printfn in production"})
			}
		}
	}
	return issues
}

// OCaml anti-patterns
var (
	ocamlObjMagic   = regexp.MustCompile(`\bObj\.magic\b`)
	ocamlExnFlow    = regexp.MustCompile(`\braise\b.*\bwith\b`)
)

func checkOcamlPatterns(fileDiff git.FileDiff) []Issue {
	issues := make([]Issue, 0)
	for _, hunk := range fileDiff.Hunks {
		for _, line := range hunk.Lines {
			if line.Type != "added" { continue }
			trimmed := strings.TrimSpace(line.Content)
			if strings.HasPrefix(trimmed, "(*") { continue }
			if ocamlObjMagic.MatchString(line.Content) {
				issues = append(issues, Issue{ID: "patterns/ocaml-obj-magic", Severity: SeverityError, Category: "patterns", File: fileDiff.Path, Line: line.NewNum, Message: "Obj.magic bypasses the type system", Suggestion: "Use proper type conversions or GADTs instead of Obj.magic"})
			}
		}
	}
	return issues
}
