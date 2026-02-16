package analyzer

import (
	"os"
	"regexp"
	"strings"

	"github.com/joeabbey/fault/pkg/parser"
)

// --- Haskell stdlib ---

var haskellStdModules = map[string]bool{
	"Prelude": true, "Data.List": true, "Data.Map": true, "Data.Map.Strict": true,
	"Data.Set": true, "Data.Maybe": true, "Data.Either": true, "Data.Char": true,
	"Data.String": true, "Data.Word": true, "Data.Int": true, "Data.Tuple": true,
	"Data.IORef": true, "Data.Typeable": true, "Data.ByteString": true,
	"Data.ByteString.Lazy": true, "Data.Text": true, "Data.Text.IO": true,
	"Control.Monad": true, "Control.Monad.IO.Class": true, "Control.Applicative": true,
	"Control.Concurrent": true, "Control.Exception": true, "Control.DeepSeq": true,
	"System.IO": true, "System.Environment": true, "System.Exit": true,
	"System.FilePath": true, "System.Directory": true, "System.Process": true,
	"GHC.Generics": true, "GHC.IO": true, "GHC.Base": true,
	"Debug.Trace": true, "Text.Read": true, "Text.Show": true,
	"Foreign": true, "Foreign.C": true, "Foreign.Ptr": true,
}

func (h *HallucinationAnalyzer) checkHaskellImports(repoPath, filePath string, pf *parser.ParsedFile) []Issue {
	issues := make([]Issue, 0)
	deps, err := parseCabalFile(repoPath)
	if err != nil {
		return issues
	}
	for _, imp := range pf.Imports {
		modName := imp.Path
		if haskellStdModules[modName] { continue }
		topMod := modName
		if idx := strings.Index(modName, "."); idx != -1 {
			topMod = modName[:idx]
		}
		if haskellStdModules[topMod] { continue }
		if deps[topMod] || deps[strings.ToLower(topMod)] { continue }
		issues = append(issues, Issue{ID: "hallucination/phantom-import", Severity: SeverityError, Category: "hallucination", File: filePath, Line: imp.Line, Message: "Import references unknown module: " + modName, Suggestion: "Verify the module exists and add the package to your .cabal file"})
	}
	return issues
}

func parseCabalFile(repoPath string) (map[string]bool, error) {
	deps := make(map[string]bool)
	entries, err := os.ReadDir(repoPath)
	if err != nil { return nil, err }
	found := false
	for _, e := range entries {
		if e.IsDir() { continue }
		if strings.HasSuffix(e.Name(), ".cabal") {
			found = true
			data, err := os.ReadFile(repoPath + "/" + e.Name())
			if err != nil { continue }
			depRe := regexp.MustCompile(`(?i)build-depends:\s*(.+)`)
			for _, m := range depRe.FindAllStringSubmatch(string(data), -1) {
				for _, dep := range strings.Split(m[1], ",") {
					dep = strings.TrimSpace(dep)
					if idx := strings.IndexAny(dep, " ><="); idx != -1 {
						dep = dep[:idx]
					}
					if dep != "" {
						deps[dep] = true
						parts := strings.Split(dep, "-")
						for _, p := range parts {
							if len(p) > 0 {
								deps[strings.ToUpper(p[:1])+p[1:]] = true
							}
						}
					}
				}
			}
		}
	}
	if !found { return nil, os.ErrNotExist }
	return deps, nil
}

// --- Clojure stdlib ---

var clojureStdNamespaces = map[string]bool{
	"clojure.core": true, "clojure.string": true, "clojure.set": true,
	"clojure.pprint": true, "clojure.java.io": true, "clojure.java.shell": true,
	"clojure.edn": true, "clojure.walk": true, "clojure.zip": true,
	"clojure.data": true, "clojure.test": true, "clojure.spec.alpha": true,
	"clojure.core.async": true, "clojure.repl": true, "clojure.stacktrace": true,
	"clojure.xml": true, "clojure.reflect": true, "clojure.main": true,
}

var clojureStdPrefixes = []string{
	"clojure.", "java.", "javax.",
}

func (h *HallucinationAnalyzer) checkClojureImports(repoPath, filePath string, pf *parser.ParsedFile) []Issue {
	issues := make([]Issue, 0)
	deps, err := parseDepsEdn(repoPath)
	if err != nil {
		deps, err = parseProjectClj(repoPath)
		if err != nil { return issues }
	}
	for _, imp := range pf.Imports {
		ns := imp.Path
		if clojureStdNamespaces[ns] { continue }
		isStd := false
		for _, p := range clojureStdPrefixes {
			if strings.HasPrefix(ns, p) { isStd = true; break }
		}
		if isStd { continue }
		if deps[ns] { continue }
		topNs := ns
		if idx := strings.Index(ns, "."); idx != -1 { topNs = ns[:idx] }
		if deps[topNs] { continue }
		issues = append(issues, Issue{ID: "hallucination/phantom-import", Severity: SeverityError, Category: "hallucination", File: filePath, Line: imp.Line, Message: "Import references unknown namespace: " + ns, Suggestion: "Verify the namespace exists and add the dependency to deps.edn or project.clj"})
	}
	return issues
}

func parseDepsEdn(repoPath string) (map[string]bool, error) {
	path := findFileUp(repoPath, "deps.edn")
	if path == "" { return nil, os.ErrNotExist }
	data, err := os.ReadFile(path)
	if err != nil { return nil, err }
	deps := make(map[string]bool)
	depRe := regexp.MustCompile(`([a-zA-Z][\w.-]+/[\w.-]+|[a-zA-Z][\w.-]+)\s*\{`)
	for _, m := range depRe.FindAllStringSubmatch(string(data), -1) {
		name := m[1]
		deps[name] = true
		if idx := strings.Index(name, "/"); idx != -1 {
			deps[name[idx+1:]] = true
		}
	}
	return deps, nil
}

func parseProjectClj(repoPath string) (map[string]bool, error) {
	path := findFileUp(repoPath, "project.clj")
	if path == "" { return nil, os.ErrNotExist }
	data, err := os.ReadFile(path)
	if err != nil { return nil, err }
	deps := make(map[string]bool)
	depRe := regexp.MustCompile(`\[([a-zA-Z][\w.-]+(?:/[\w.-]+)?)\s+"`)
	for _, m := range depRe.FindAllStringSubmatch(string(data), -1) {
		name := m[1]
		deps[name] = true
		if idx := strings.Index(name, "/"); idx != -1 {
			deps[name[idx+1:]] = true
		}
	}
	return deps, nil
}

// --- Erlang stdlib ---

var erlangStdModules = map[string]bool{
	"lists": true, "maps": true, "io": true, "io_lib": true, "file": true,
	"gen_server": true, "gen_statem": true, "supervisor": true, "application": true,
	"ets": true, "timer": true, "string": true, "proplists": true, "dict": true,
	"sets": true, "queue": true, "logger": true, "crypto": true, "ssl": true,
	"httpc": true, "mnesia": true, "os": true, "filelib": true, "gen_event": true,
	"erlang": true, "unicode": true, "binary": true, "calendar": true,
	"math": true, "re": true, "rand": true, "sys": true, "code": true,
}

func (h *HallucinationAnalyzer) checkErlangImports(repoPath, filePath string, pf *parser.ParsedFile) []Issue {
	issues := make([]Issue, 0)
	// Erlang includes are local files — check they exist relative to repo.
	for _, imp := range pf.Imports {
		if strings.Contains(imp.Path, "/") {
			continue // include_lib paths — skip
		}
		// Check if the file exists in the repo.
		candidate := repoPath + "/" + imp.Path
		if _, err := os.Stat(candidate); err == nil { continue }
		candidate = repoPath + "/include/" + imp.Path
		if _, err := os.Stat(candidate); err == nil { continue }
		issues = append(issues, Issue{ID: "hallucination/phantom-import", Severity: SeverityError, Category: "hallucination", File: filePath, Line: imp.Line, Message: "Include references missing file: " + imp.Path, Suggestion: "Verify the header file exists in the project or include/ directory"})
	}
	return issues
}

// --- F# stdlib ---

var fsharpStdNamespaces = map[string]bool{
	"System": true, "System.IO": true, "System.Collections": true,
	"System.Collections.Generic": true, "System.Linq": true, "System.Text": true,
	"System.Threading": true, "System.Threading.Tasks": true, "System.Net": true,
	"System.Net.Http": true, "System.Diagnostics": true, "System.Runtime": true,
	"Microsoft.FSharp.Core": true, "Microsoft.FSharp.Collections": true,
	"FSharp.Core": true, "FSharp.Collections": true,
}

func (h *HallucinationAnalyzer) checkFsharpImports(repoPath, filePath string, pf *parser.ParsedFile) []Issue {
	issues := make([]Issue, 0)
	deps, err := parseFsproj(repoPath)
	if err != nil { return issues }
	for _, imp := range pf.Imports {
		ns := imp.Path
		if fsharpStdNamespaces[ns] { continue }
		topNs := ns
		if idx := strings.Index(ns, "."); idx != -1 { topNs = ns[:idx] }
		if fsharpStdNamespaces[topNs+"."+ns] || fsharpStdNamespaces[topNs] { continue }
		if deps[ns] || deps[topNs] { continue }
		issues = append(issues, Issue{ID: "hallucination/phantom-import", Severity: SeverityError, Category: "hallucination", File: filePath, Line: imp.Line, Message: "Open references unknown namespace: " + ns, Suggestion: "Verify the namespace exists and add the NuGet package to your .fsproj"})
	}
	return issues
}

func parseFsproj(repoPath string) (map[string]bool, error) {
	deps := make(map[string]bool)
	entries, err := os.ReadDir(repoPath)
	if err != nil { return nil, err }
	found := false
	for _, e := range entries {
		if e.IsDir() { continue }
		if strings.HasSuffix(e.Name(), ".fsproj") {
			found = true
			data, err := os.ReadFile(repoPath + "/" + e.Name())
			if err != nil { continue }
			pkgRe := regexp.MustCompile(`<PackageReference\s+Include="([^"]+)"`)
			for _, m := range pkgRe.FindAllStringSubmatch(string(data), -1) {
				deps[m[1]] = true
			}
		}
	}
	if !found { return nil, os.ErrNotExist }
	return deps, nil
}

// --- OCaml stdlib ---

var ocamlStdModules = map[string]bool{
	"Stdlib": true, "List": true, "Array": true, "String": true, "Bytes": true,
	"Buffer": true, "Printf": true, "Format": true, "Hashtbl": true, "Map": true,
	"Set": true, "Sys": true, "Unix": true, "Filename": true, "Arg": true,
	"Lexing": true, "Parsing": true, "Scanf": true, "Obj": true, "Gc": true,
	"Lazy": true, "Fun": true, "In_channel": true, "Out_channel": true,
	"Seq": true, "Option": true, "Result": true, "Either": true, "Int": true,
	"Float": true, "Bool": true, "Char": true, "Complex": true, "Uchar": true,
	"Random": true, "Printexc": true, "Digest": true, "Marshal": true,
}

func (h *HallucinationAnalyzer) checkOcamlImports(repoPath, filePath string, pf *parser.ParsedFile) []Issue {
	issues := make([]Issue, 0)
	deps, err := parseDuneProject(repoPath)
	if err != nil { return issues }
	for _, imp := range pf.Imports {
		modName := imp.Path
		if ocamlStdModules[modName] { continue }
		if deps[modName] || deps[strings.ToLower(modName)] { continue }
		issues = append(issues, Issue{ID: "hallucination/phantom-import", Severity: SeverityError, Category: "hallucination", File: filePath, Line: imp.Line, Message: "Open references unknown module: " + modName, Suggestion: "Verify the module exists and add the library to your dune-project"})
	}
	return issues
}

func parseDuneProject(repoPath string) (map[string]bool, error) {
	path := findFileUp(repoPath, "dune-project")
	if path == "" {
		// Try .opam files
		entries, err := os.ReadDir(repoPath)
		if err != nil { return nil, os.ErrNotExist }
		deps := make(map[string]bool)
		found := false
		for _, e := range entries {
			if strings.HasSuffix(e.Name(), ".opam") {
				found = true
				data, err := os.ReadFile(repoPath + "/" + e.Name())
				if err != nil { continue }
				depRe := regexp.MustCompile(`"([a-zA-Z][\w-]+)"`)
				for _, m := range depRe.FindAllStringSubmatch(string(data), -1) {
					deps[m[1]] = true
				}
			}
		}
		if !found { return nil, os.ErrNotExist }
		return deps, nil
	}
	data, err := os.ReadFile(path)
	if err != nil { return nil, err }
	deps := make(map[string]bool)
	depRe := regexp.MustCompile(`\(depends[^)]*\)`)
	if m := depRe.FindString(string(data)); m != "" {
		nameRe := regexp.MustCompile(`([a-zA-Z][\w-]+)`)
		for _, n := range nameRe.FindAllStringSubmatch(m, -1) {
			deps[n[1]] = true
		}
	}
	return deps, nil
}
