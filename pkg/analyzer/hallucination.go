package analyzer

import (
	"bufio"
	"os"
	"path/filepath"
	"regexp"
	"strings"

	"github.com/joeabbey/fault/pkg/git"
	"github.com/joeabbey/fault/pkg/parser"
)

// HallucinationAnalyzer detects AI hallucinations: phantom imports,
// stub implementations, and references to nonexistent files.
type HallucinationAnalyzer struct{}

// NewHallucinationAnalyzer creates a new hallucination analyzer.
func NewHallucinationAnalyzer() *HallucinationAnalyzer {
	return &HallucinationAnalyzer{}
}

// Name returns the analyzer name.
func (h *HallucinationAnalyzer) Name() string {
	return "hallucination"
}

// Analyze scans the diff for phantom imports, stub functions, and missing file references.
func (h *HallucinationAnalyzer) Analyze(ctx *AnalysisContext) ([]Issue, error) {
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

		// Skip vendor/node_modules.
		normalized := filepath.ToSlash(fileDiff.Path)
		if strings.HasPrefix(normalized, "vendor/") || strings.HasPrefix(normalized, "node_modules/") ||
			strings.Contains(normalized, "/vendor/") || strings.Contains(normalized, "/node_modules/") {
			continue
		}

		issues = append(issues, h.checkPhantomImports(ctx, fileDiff)...)

		// Stub detection skips test files.
		if !isTestFile(fileDiff.Path) {
			issues = append(issues, h.checkStubFunctions(fileDiff)...)
		}
	}

	return issues, nil
}

// --- Phantom import detection ---

// goStdlib is a set of common Go standard library top-level packages.
var goStdlib = map[string]bool{
	"archive": true, "bufio": true, "bytes": true, "cmp": true,
	"compress": true, "container": true, "context": true, "crypto": true,
	"database": true, "debug": true, "embed": true, "encoding": true,
	"errors": true, "expvar": true, "flag": true, "fmt": true,
	"go": true, "hash": true, "html": true, "image": true,
	"index": true, "io": true, "iter": true, "log": true,
	"maps": true, "math": true, "mime": true, "net": true,
	"os": true, "path": true, "plugin": true, "reflect": true,
	"regexp": true, "runtime": true, "slices": true, "sort": true,
	"strconv": true, "strings": true, "sync": true, "syscall": true,
	"testing": true, "text": true, "time": true, "unicode": true,
	"unsafe": true, "internal": true,
}

// nodeBuiltins is a set of Node.js built-in modules.
var nodeBuiltins = map[string]bool{
	"assert": true, "buffer": true, "child_process": true, "cluster": true,
	"console": true, "constants": true, "crypto": true, "dgram": true,
	"dns": true, "domain": true, "events": true, "fs": true,
	"http": true, "http2": true, "https": true, "module": true,
	"net": true, "os": true, "path": true, "perf_hooks": true,
	"process": true, "punycode": true, "querystring": true, "readline": true,
	"repl": true, "stream": true, "string_decoder": true, "sys": true,
	"timers": true, "tls": true, "tty": true, "url": true,
	"util": true, "v8": true, "vm": true, "wasi": true,
	"worker_threads": true, "zlib": true,
	"node:assert": true, "node:buffer": true, "node:child_process": true,
	"node:cluster": true, "node:console": true, "node:crypto": true,
	"node:dgram": true, "node:dns": true, "node:events": true, "node:fs": true,
	"node:http": true, "node:http2": true, "node:https": true, "node:module": true,
	"node:net": true, "node:os": true, "node:path": true, "node:perf_hooks": true,
	"node:process": true, "node:querystring": true, "node:readline": true,
	"node:stream": true, "node:string_decoder": true, "node:timers": true,
	"node:tls": true, "node:tty": true, "node:url": true,
	"node:util": true, "node:v8": true, "node:vm": true, "node:wasi": true,
	"node:worker_threads": true, "node:zlib": true,
	"node:fs/promises": true, "fs/promises": true, "node:path/posix": true,
	"node:path/win32": true, "path/posix": true, "path/win32": true,
	"node:test": true, "test": true,
}

// pythonStdlib is a set of common Python standard library modules.
var pythonStdlib = map[string]bool{
	"abc": true, "aifc": true, "argparse": true, "array": true,
	"ast": true, "asynchat": true, "asyncio": true, "asyncore": true,
	"atexit": true, "base64": true, "bdb": true, "binascii": true,
	"binhex": true, "bisect": true, "builtins": true, "bz2": true,
	"calendar": true, "cgi": true, "cgitb": true, "chunk": true,
	"cmath": true, "cmd": true, "code": true, "codecs": true,
	"codeop": true, "collections": true, "colorsys": true, "compileall": true,
	"concurrent": true, "configparser": true, "contextlib": true,
	"contextvars": true, "copy": true, "copyreg": true, "cProfile": true,
	"crypt": true, "csv": true, "ctypes": true, "curses": true,
	"dataclasses": true, "datetime": true, "dbm": true, "decimal": true,
	"difflib": true, "dis": true, "distutils": true, "doctest": true,
	"email": true, "encodings": true, "enum": true, "errno": true,
	"faulthandler": true, "fcntl": true, "filecmp": true, "fileinput": true,
	"fnmatch": true, "fractions": true, "ftplib": true, "functools": true,
	"gc": true, "getopt": true, "getpass": true, "gettext": true,
	"glob": true, "grp": true, "gzip": true, "hashlib": true,
	"heapq": true, "hmac": true, "html": true, "http": true,
	"idlelib": true, "imaplib": true, "imghdr": true, "imp": true,
	"importlib": true, "inspect": true, "io": true, "ipaddress": true,
	"itertools": true, "json": true, "keyword": true, "lib2to3": true,
	"linecache": true, "locale": true, "logging": true, "lzma": true,
	"mailbox": true, "mailcap": true, "marshal": true, "math": true,
	"mimetypes": true, "mmap": true, "modulefinder": true, "multiprocessing": true,
	"netrc": true, "nis": true, "nntplib": true, "numbers": true,
	"operator": true, "optparse": true, "os": true, "ossaudiodev": true,
	"pathlib": true, "pdb": true, "pickle": true, "pickletools": true,
	"pipes": true, "pkgutil": true, "platform": true, "plistlib": true,
	"poplib": true, "posix": true, "posixpath": true, "pprint": true,
	"profile": true, "pstats": true, "pty": true, "pwd": true,
	"py_compile": true, "pyclbr": true, "pydoc": true, "queue": true,
	"quopri": true, "random": true, "re": true, "readline": true,
	"reprlib": true, "resource": true, "rlcompleter": true, "runpy": true,
	"sched": true, "secrets": true, "select": true, "selectors": true,
	"shelve": true, "shlex": true, "shutil": true, "signal": true,
	"site": true, "smtpd": true, "smtplib": true, "sndhdr": true,
	"socket": true, "socketserver": true, "spwd": true, "sqlite3": true,
	"ssl": true, "stat": true, "statistics": true, "string": true,
	"stringprep": true, "struct": true, "subprocess": true, "sunau": true,
	"symtable": true, "sys": true, "sysconfig": true, "syslog": true,
	"tabnanny": true, "tarfile": true, "telnetlib": true, "tempfile": true,
	"termios": true, "textwrap": true, "threading": true, "time": true,
	"timeit": true, "tkinter": true, "token": true, "tokenize": true,
	"tomllib": true, "trace": true, "traceback": true, "tracemalloc": true,
	"tty": true, "turtle": true, "turtledemo": true, "types": true,
	"typing": true, "unicodedata": true, "unittest": true, "urllib": true,
	"uu": true, "uuid": true, "venv": true, "warnings": true,
	"wave": true, "weakref": true, "webbrowser": true, "winreg": true,
	"winsound": true, "wsgiref": true, "xdrlib": true, "xml": true,
	"xmlrpc": true, "zipapp": true, "zipfile": true, "zipimport": true,
	"zlib": true, "_thread": true, "__future__": true,
}

func (h *HallucinationAnalyzer) checkPhantomImports(ctx *AnalysisContext, fileDiff git.FileDiff) []Issue {
	issues := make([]Issue, 0)

	pf, ok := ctx.ParsedFiles[fileDiff.Path]
	if !ok || pf == nil || len(pf.Imports) == 0 {
		return issues
	}

	ext := strings.ToLower(filepath.Ext(fileDiff.Path))

	switch {
	case ext == ".go":
		issues = append(issues, h.checkGoImports(ctx.RepoPath, fileDiff.Path, pf)...)
	case ext == ".ts" || ext == ".tsx" || ext == ".js" || ext == ".jsx" || ext == ".mjs" || ext == ".cjs":
		issues = append(issues, h.checkJSImports(ctx.RepoPath, fileDiff.Path, pf)...)
	case ext == ".py":
		issues = append(issues, h.checkPythonImports(ctx.RepoPath, fileDiff.Path, pf)...)
	case ext == ".java":
		issues = append(issues, h.checkJavaImports(ctx.RepoPath, fileDiff.Path, pf)...)
	case ext == ".rs":
		issues = append(issues, h.checkRustImports(ctx.RepoPath, fileDiff.Path, pf)...)
	case ext == ".rb" || ext == ".rake":
		issues = append(issues, h.checkRubyImports(ctx.RepoPath, fileDiff.Path, pf)...)
	case ext == ".kt" || ext == ".kts":
		issues = append(issues, h.checkKotlinImports(ctx.RepoPath, fileDiff.Path, pf)...)
	case ext == ".cs":
		issues = append(issues, h.checkCSharpImports(ctx.RepoPath, fileDiff.Path, pf)...)
	case ext == ".php":
		issues = append(issues, h.checkPHPImports(ctx.RepoPath, fileDiff.Path, pf)...)
	case ext == ".swift":
		issues = append(issues, h.checkSwiftImports(ctx.RepoPath, fileDiff.Path, pf)...)
	case ext == ".c" || ext == ".h":
		issues = append(issues, h.checkCImports(ctx.RepoPath, fileDiff.Path, pf)...)
	case ext == ".cpp" || ext == ".cc" || ext == ".cxx" || ext == ".hpp" || ext == ".hxx":
		issues = append(issues, h.checkCppImports(ctx.RepoPath, fileDiff.Path, pf)...)
	case ext == ".m" || ext == ".mm":
		issues = append(issues, h.checkObjCImports(ctx.RepoPath, fileDiff.Path, pf)...)
	case ext == ".sh" || ext == ".bash":
		// Bash has no manifest-based imports — skip phantom check
	case ext == ".sql":
		// SQL has no imports — skip phantom check
	case ext == ".dart":
		issues = append(issues, h.checkDartImports(ctx.RepoPath, fileDiff.Path, pf)...)
	case ext == ".scala" || ext == ".sc":
		issues = append(issues, h.checkScalaImports(ctx.RepoPath, fileDiff.Path, pf)...)
	case ext == ".r":
		issues = append(issues, h.checkRImports(ctx.RepoPath, fileDiff.Path, pf)...)
	case ext == ".ex" || ext == ".exs":
		issues = append(issues, h.checkElixirImports(ctx.RepoPath, fileDiff.Path, pf)...)
	case ext == ".lua":
		issues = append(issues, h.checkLuaImports(ctx.RepoPath, fileDiff.Path, pf)...)
	case ext == ".pl" || ext == ".pm":
		issues = append(issues, h.checkPerlImports(ctx.RepoPath, fileDiff.Path, pf)...)
	case ext == ".ps1" || ext == ".psm1":
		issues = append(issues, h.checkPowershellImports(ctx.RepoPath, fileDiff.Path, pf)...)
	case ext == ".groovy" || ext == ".gvy":
		issues = append(issues, h.checkGroovyImports(ctx.RepoPath, fileDiff.Path, pf)...)
	}

	return issues
}

// checkGoImports validates Go imports against go.mod and the stdlib.
func (h *HallucinationAnalyzer) checkGoImports(repoPath, filePath string, pf *parser.ParsedFile) []Issue {
	issues := make([]Issue, 0)

	// Read go.mod to get module path and require directives.
	goModPath := filepath.Join(repoPath, "go.mod")
	modulePath, requires, err := parseGoMod(goModPath)
	if err != nil {
		// Can't read go.mod — fail open.
		return issues
	}

	for _, imp := range pf.Imports {
		importPath := imp.Path

		// Standard library: check top-level package name.
		topPkg := importPath
		if idx := strings.Index(importPath, "/"); idx != -1 {
			topPkg = importPath[:idx]
		}
		if goStdlib[topPkg] {
			continue
		}

		// Internal module import (same module).
		if modulePath != "" && strings.HasPrefix(importPath, modulePath) {
			continue
		}

		// Check if the import matches any require directive.
		found := false
		for _, req := range requires {
			if importPath == req || strings.HasPrefix(importPath, req+"/") {
				found = true
				break
			}
		}

		if !found {
			issues = append(issues, Issue{
				ID:         "hallucination/phantom-import",
				Severity:   SeverityError,
				Category:   "hallucination",
				File:       filePath,
				Line:       imp.Line,
				Message:    "Import references unknown package: " + importPath,
				Suggestion: "Verify the package exists and add it to go.mod with 'go get " + importPath + "'",
			})
		}
	}

	return issues
}

// parseGoMod reads a go.mod file and returns the module path and require paths.
func parseGoMod(path string) (string, []string, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return "", nil, err
	}

	var modulePath string
	requires := make([]string, 0)

	lines := strings.Split(string(data), "\n")
	inRequireBlock := false
	for _, line := range lines {
		line = strings.TrimSpace(line)

		if strings.HasPrefix(line, "module ") {
			modulePath = strings.TrimSpace(strings.TrimPrefix(line, "module"))
			continue
		}

		if line == "require (" {
			inRequireBlock = true
			continue
		}
		if inRequireBlock && line == ")" {
			inRequireBlock = false
			continue
		}

		if inRequireBlock {
			// Lines look like: github.com/foo/bar v1.2.3
			parts := strings.Fields(line)
			if len(parts) >= 2 && !strings.HasPrefix(line, "//") {
				requires = append(requires, parts[0])
			}
			continue
		}

		// Single-line require: require github.com/foo/bar v1.2.3
		if strings.HasPrefix(line, "require ") && !strings.Contains(line, "(") {
			rest := strings.TrimPrefix(line, "require ")
			parts := strings.Fields(rest)
			if len(parts) >= 2 {
				requires = append(requires, parts[0])
			}
		}
	}

	return modulePath, requires, nil
}

// checkJSImports validates JS/TS imports against package.json.
func (h *HallucinationAnalyzer) checkJSImports(repoPath, filePath string, pf *parser.ParsedFile) []Issue {
	issues := make([]Issue, 0)

	pkgJSONPath := filepath.Join(repoPath, "package.json")
	deps, err := parsePackageJSON(pkgJSONPath)
	if err != nil {
		// Can't read package.json — fail open.
		return issues
	}

	for _, imp := range pf.Imports {
		importPath := imp.Path

		// Relative imports: check file existence.
		if strings.HasPrefix(importPath, ".") || strings.HasPrefix(importPath, "/") {
			dir := filepath.Dir(filepath.Join(repoPath, filePath))
			if !resolveRelativeImportJS(dir, importPath) {
				issues = append(issues, Issue{
					ID:         "hallucination/missing-import-target",
					Severity:   SeverityError,
					Category:   "hallucination",
					File:       filePath,
					Line:       imp.Line,
					Message:    "Import target does not exist: " + importPath,
					Suggestion: "Check the file path — the imported module could not be found on disk",
				})
			}
			continue
		}

		// Node built-in.
		if nodeBuiltins[importPath] {
			continue
		}

		// Scoped or bare package: extract top-level name.
		pkgName := importPath
		if strings.HasPrefix(importPath, "@") {
			// Scoped: @scope/pkg or @scope/pkg/subpath
			parts := strings.SplitN(importPath, "/", 3)
			if len(parts) >= 2 {
				pkgName = parts[0] + "/" + parts[1]
			}
		} else {
			// Bare: pkg or pkg/subpath
			if idx := strings.Index(importPath, "/"); idx != -1 {
				pkgName = importPath[:idx]
			}
		}

		if !deps[pkgName] {
			issues = append(issues, Issue{
				ID:         "hallucination/phantom-import",
				Severity:   SeverityError,
				Category:   "hallucination",
				File:       filePath,
				Line:       imp.Line,
				Message:    "Import references unknown package: " + importPath,
				Suggestion: "Verify the package exists and install it with 'npm install " + pkgName + "'",
			})
		}
	}

	return issues
}

// parsePackageJSON reads a package.json and returns a set of dependency names.
func parsePackageJSON(path string) (map[string]bool, error) {
	deps := make(map[string]bool)

	f, err := os.Open(path)
	if err != nil {
		return nil, err
	}
	defer f.Close()

	// Simple line-based parsing to avoid encoding/json dependency for large files.
	// We look for keys within "dependencies", "devDependencies", "peerDependencies" blocks.
	scanner := bufio.NewScanner(f)
	inDepsBlock := false
	braceDepth := 0

	depsKeyPattern := regexp.MustCompile(`"(dependencies|devDependencies|peerDependencies)"\s*:\s*\{`)
	pkgNamePattern := regexp.MustCompile(`^\s*"([^"]+)"\s*:`)

	for scanner.Scan() {
		line := scanner.Text()

		if !inDepsBlock {
			if depsKeyPattern.MatchString(line) {
				inDepsBlock = true
				braceDepth = 1
				// Check if there's already a package on this line after the opening brace.
				afterBrace := line[strings.Index(line, "{")+1:]
				if m := pkgNamePattern.FindStringSubmatch(afterBrace); len(m) >= 2 {
					deps[m[1]] = true
				}
			}
			continue
		}

		// Track brace depth within the dependency block.
		for _, ch := range line {
			if ch == '{' {
				braceDepth++
			} else if ch == '}' {
				braceDepth--
				if braceDepth <= 0 {
					inDepsBlock = false
					break
				}
			}
		}

		if inDepsBlock {
			if m := pkgNamePattern.FindStringSubmatch(line); len(m) >= 2 {
				deps[m[1]] = true
			}
		}
	}

	return deps, nil
}

// resolveRelativeImportJS checks if a relative JS/TS import target exists.
func resolveRelativeImportJS(fromDir, importPath string) bool {
	target := filepath.Join(fromDir, importPath)

	// Try exact path first.
	if fileExists(target) {
		return true
	}

	// Try with extensions.
	for _, ext := range []string{".ts", ".tsx", ".js", ".jsx", ".mjs", ".cjs"} {
		if fileExists(target + ext) {
			return true
		}
	}

	// Try index files.
	for _, idx := range []string{"index.ts", "index.tsx", "index.js", "index.jsx"} {
		if fileExists(filepath.Join(target, idx)) {
			return true
		}
	}

	return false
}

// checkPythonImports validates Python imports against requirements and stdlib.
func (h *HallucinationAnalyzer) checkPythonImports(repoPath, filePath string, pf *parser.ParsedFile) []Issue {
	issues := make([]Issue, 0)

	deps, err := parsePythonDeps(repoPath)
	if err != nil {
		// Can't read requirements — fail open.
		return issues
	}

	for _, imp := range pf.Imports {
		importPath := imp.Path

		// Relative imports.
		if strings.HasPrefix(importPath, ".") {
			continue
		}

		// Get top-level module name.
		topModule := importPath
		if idx := strings.Index(importPath, "."); idx != -1 {
			topModule = importPath[:idx]
		}

		if pythonStdlib[topModule] {
			continue
		}

		// Normalize: package names use hyphens, import names use underscores.
		normalizedModule := strings.ReplaceAll(topModule, "_", "-")
		normalizedModuleUnderscore := strings.ReplaceAll(topModule, "-", "_")

		if deps[topModule] || deps[normalizedModule] || deps[normalizedModuleUnderscore] {
			continue
		}

		// Check if it's a local package (directory with __init__.py).
		localPkg := filepath.Join(repoPath, topModule)
		if fileExists(filepath.Join(localPkg, "__init__.py")) || fileExists(localPkg+".py") {
			continue
		}

		issues = append(issues, Issue{
			ID:         "hallucination/phantom-import",
			Severity:   SeverityError,
			Category:   "hallucination",
			File:       filePath,
			Line:       imp.Line,
			Message:    "Import references unknown package: " + importPath,
			Suggestion: "Verify the package exists and add it to requirements.txt or pyproject.toml",
		})
	}

	return issues
}

// rustStdCrates is a set of well-known Rust standard library and core crates.
var rustStdCrates = map[string]bool{
	"std": true, "core": true, "alloc": true, "proc_macro": true,
	"crate": true, "super": true, "self": true,
}

// checkRustImports validates Rust use statements against Cargo.toml.
func (h *HallucinationAnalyzer) checkRustImports(repoPath, filePath string, pf *parser.ParsedFile) []Issue {
	issues := make([]Issue, 0)

	deps, err := parseCargoToml(repoPath)
	if err != nil {
		// Can't read Cargo.toml — fail open.
		return issues
	}

	for _, imp := range pf.Imports {
		importPath := imp.Path

		// Get the top-level crate name (first path segment before ::).
		topCrate := importPath
		if idx := strings.Index(importPath, "::"); idx != -1 {
			topCrate = importPath[:idx]
		}

		// Standard/built-in crate paths.
		if rustStdCrates[topCrate] {
			continue
		}

		// Check if the crate matches any dependency in Cargo.toml.
		// Cargo normalizes hyphens to underscores in crate names.
		normalizedCrate := strings.ReplaceAll(topCrate, "-", "_")
		if deps[topCrate] || deps[normalizedCrate] {
			continue
		}

		issues = append(issues, Issue{
			ID:         "hallucination/phantom-import",
			Severity:   SeverityError,
			Category:   "hallucination",
			File:       filePath,
			Line:       imp.Line,
			Message:    "Import references unknown crate: " + importPath,
			Suggestion: "Verify the crate exists and add it to Cargo.toml with 'cargo add " + topCrate + "'",
		})
	}

	return issues
}

// parseCargoToml reads a Cargo.toml and returns a set of dependency crate names.
func parseCargoToml(repoPath string) (map[string]bool, error) {
	cargoPath := filepath.Join(repoPath, "Cargo.toml")
	data, err := os.ReadFile(cargoPath)
	if err != nil {
		return nil, err
	}

	deps := make(map[string]bool)
	inDeps := false

	for _, line := range strings.Split(string(data), "\n") {
		trimmed := strings.TrimSpace(line)

		// Detect [dependencies], [dev-dependencies], [build-dependencies] sections.
		if strings.HasPrefix(trimmed, "[") {
			inDeps = strings.Contains(trimmed, "dependencies")
			continue
		}

		if !inDeps {
			continue
		}

		// Lines like: serde = "1.0" or serde = { version = "1.0", features = [...] }
		if idx := strings.Index(trimmed, "="); idx > 0 {
			name := strings.TrimSpace(trimmed[:idx])
			if name != "" && !strings.HasPrefix(name, "#") {
				deps[name] = true
				// Also add underscore-normalized form.
				deps[strings.ReplaceAll(name, "-", "_")] = true
			}
		}
	}

	return deps, nil
}

// parsePythonDeps reads requirements.txt and/or pyproject.toml for dependency names.
func parsePythonDeps(repoPath string) (map[string]bool, error) {
	deps := make(map[string]bool)
	found := false

	// Try requirements.txt.
	reqPath := filepath.Join(repoPath, "requirements.txt")
	if data, err := os.ReadFile(reqPath); err == nil {
		found = true
		for _, line := range strings.Split(string(data), "\n") {
			line = strings.TrimSpace(line)
			if line == "" || strings.HasPrefix(line, "#") || strings.HasPrefix(line, "-") {
				continue
			}
			// Package names end at version specifiers: ==, >=, <=, ~=, !=, [
			name := line
			for _, sep := range []string{"==", ">=", "<=", "~=", "!=", "[", ";", " "} {
				if idx := strings.Index(name, sep); idx != -1 {
					name = name[:idx]
				}
			}
			name = strings.TrimSpace(name)
			if name != "" {
				// Normalize: both hyphen and underscore forms.
				deps[strings.ToLower(name)] = true
				deps[strings.ToLower(strings.ReplaceAll(name, "-", "_"))] = true
				deps[strings.ToLower(strings.ReplaceAll(name, "_", "-"))] = true
			}
		}
	}

	// Try pyproject.toml (simple parsing for dependencies list).
	pyprojectPath := filepath.Join(repoPath, "pyproject.toml")
	if data, err := os.ReadFile(pyprojectPath); err == nil {
		found = true
		// Look for dependencies = [...] section.
		inDeps := false
		for _, line := range strings.Split(string(data), "\n") {
			trimmed := strings.TrimSpace(line)

			if strings.HasPrefix(trimmed, "dependencies") && strings.Contains(trimmed, "=") {
				inDeps = true
				// Check for inline single-line list.
				if strings.Contains(trimmed, "[") && strings.Contains(trimmed, "]") {
					extractPyprojectDeps(trimmed, deps)
					inDeps = false
				}
				continue
			}

			if inDeps {
				if strings.HasPrefix(trimmed, "]") {
					inDeps = false
					continue
				}
				extractPyprojectDeps(trimmed, deps)
			}
		}
	}

	if !found {
		return nil, os.ErrNotExist
	}

	return deps, nil
}

// extractPyprojectDeps extracts package names from a pyproject.toml dependencies line.
func extractPyprojectDeps(line string, deps map[string]bool) {
	// Lines look like: "requests>=2.28", or "flask",
	depNameRe := regexp.MustCompile(`"([a-zA-Z0-9_-]+)`)
	matches := depNameRe.FindAllStringSubmatch(line, -1)
	for _, m := range matches {
		if len(m) >= 2 {
			name := strings.ToLower(m[1])
			deps[name] = true
			deps[strings.ReplaceAll(name, "-", "_")] = true
			deps[strings.ReplaceAll(name, "_", "-")] = true
		}
	}
}

// --- Java phantom import detection ---

// javaStdlib is a set of Java standard library package prefixes.
var javaStdlib = map[string]bool{
	"java.lang":     true,
	"java.util":     true,
	"java.io":       true,
	"java.nio":      true,
	"java.net":      true,
	"java.math":     true,
	"java.text":     true,
	"java.time":     true,
	"java.security": true,
	"java.sql":      true,
	"java.beans":    true,
	"java.applet":   true,
	"java.awt":      true,
	"java.rmi":      true,
	"java.logging":  false, // This is NOT a real package — see phantomJavaPackages
	"javax.swing":   true,
	"javax.servlet": true,
	"javax.xml":     true,
	"javax.crypto":  true,
	"javax.net":     true,
	"javax.sql":     true,
	"javax.naming":  true,
	"javax.script":  true,
	"javax.sound":   true,
	"javax.imageio": true,
	"javax.print":   true,
	"javax.annotation": true,
	"jakarta.servlet":  true,
	"jakarta.xml":      true,
	"jakarta.json":     true,
	"jakarta.inject":   true,
	"jakarta.ws":       true,
	"jakarta.persistence": true,
	"jakarta.validation":  true,
	"jakarta.annotation":  true,
	"org.w3c.dom":    true,
	"org.xml.sax":    true,
	"org.ietf.jgss":  true,
}

// javaStdlibPrefixes are top-level Java standard library package prefixes
// for broad matching when the specific subpackage isn't in javaStdlib.
var javaStdlibPrefixes = []string{
	"java.", "javax.", "jakarta.",
	"org.w3c.", "org.xml.", "org.ietf.",
}

// commonJavaLibPrefixes are well-known third-party library prefixes that
// we consider "known" and don't flag as phantom.
var commonJavaLibPrefixes = []string{
	"org.springframework.", "org.apache.", "org.hibernate.",
	"com.google.", "com.fasterxml.", "com.amazonaws.",
	"org.junit.", "org.mockito.", "org.assertj.",
	"org.slf4j.", "ch.qos.logback.",
	"io.netty.", "io.grpc.", "io.micrometer.",
	"lombok.", "org.projectlombok.",
	"com.squareup.", "io.reactivex.",
	"org.jetbrains.", "kotlin.",
}

// phantomJavaPackages are AI-hallucinated package patterns that don't exist.
var phantomJavaPackages = []string{
	"java.utils.",       // Common hallucination — correct is java.util
	"java.collections.", // Doesn't exist — correct is java.util
	"java.strings.",     // Doesn't exist — correct is java.lang.String
	"javax.json.bind.",  // Often confused with jakarta.json.bind
	"java.concurrent.",  // Correct is java.util.concurrent
	"java.http.",        // Correct is java.net.http
	"java.logging.",     // Correct is java.util.logging
	"org.spring.",       // Correct is org.springframework
}

// parsePomXml walks up from repoPath to find pom.xml and extracts groupId
// prefixes from <dependency> blocks. Returns a map of groupId prefixes.
func parsePomXml(repoPath string) (map[string]bool, error) {
	pomPath := findFileUp(repoPath, "pom.xml")
	if pomPath == "" {
		return nil, os.ErrNotExist
	}

	data, err := os.ReadFile(pomPath)
	if err != nil {
		return nil, err
	}

	deps := make(map[string]bool)
	lines := strings.Split(string(data), "\n")
	inDependency := false
	groupIDRe := regexp.MustCompile(`<groupId>\s*([^<]+?)\s*</groupId>`)

	for _, line := range lines {
		trimmed := strings.TrimSpace(line)

		if strings.Contains(trimmed, "<dependency>") {
			inDependency = true
			continue
		}
		if strings.Contains(trimmed, "</dependency>") {
			inDependency = false
			continue
		}

		if inDependency {
			if m := groupIDRe.FindStringSubmatch(trimmed); len(m) >= 2 {
				groupID := m[1]
				deps[groupID] = true
			}
		}
	}

	return deps, nil
}

// parseBuildGradle walks up from repoPath to find build.gradle or build.gradle.kts
// and extracts dependency group prefixes. Returns a map of group prefixes.
func parseBuildGradle(repoPath string) (map[string]bool, error) {
	gradlePath := findFileUp(repoPath, "build.gradle")
	if gradlePath == "" {
		gradlePath = findFileUp(repoPath, "build.gradle.kts")
	}
	if gradlePath == "" {
		return nil, os.ErrNotExist
	}

	data, err := os.ReadFile(gradlePath)
	if err != nil {
		return nil, err
	}

	deps := make(map[string]bool)
	depKeywords := []string{
		"implementation", "api", "compile", "compileOnly",
		"runtimeOnly", "testImplementation", "testCompile",
		"testCompileOnly", "testRuntimeOnly", "annotationProcessor",
		"kapt", "ksp",
	}

	colonNotation := regexp.MustCompile(`['"]([^'":\s]+):([^'":\s]+)(?::([^'":\s]+))?['"]`)
	groupNotation := regexp.MustCompile(`group:\s*['"]([^'"]+)['"]`)

	for _, line := range strings.Split(string(data), "\n") {
		trimmed := strings.TrimSpace(line)

		if strings.HasPrefix(trimmed, "//") || strings.HasPrefix(trimmed, "/*") || strings.HasPrefix(trimmed, "*") {
			continue
		}

		isDep := false
		for _, kw := range depKeywords {
			if strings.HasPrefix(trimmed, kw+" ") || strings.HasPrefix(trimmed, kw+"(") {
				isDep = true
				break
			}
		}
		if !isDep {
			continue
		}

		if m := colonNotation.FindStringSubmatch(trimmed); len(m) >= 3 {
			deps[m[1]] = true
			continue
		}

		if m := groupNotation.FindStringSubmatch(trimmed); len(m) >= 2 {
			deps[m[1]] = true
		}
	}

	return deps, nil
}

// findFileUp walks up the directory tree from startDir looking for a file with
// the given name. Returns the full path or empty string if not found.
func findFileUp(startDir, filename string) string {
	dir, err := filepath.Abs(startDir)
	if err != nil {
		return ""
	}

	for {
		candidate := filepath.Join(dir, filename)
		if _, err := os.Stat(candidate); err == nil {
			return candidate
		}

		parent := filepath.Dir(dir)
		if parent == dir {
			return ""
		}
		dir = parent
	}
}

func (h *HallucinationAnalyzer) checkJavaImports(repoPath, filePath string, pf *parser.ParsedFile) []Issue {
	issues := make([]Issue, 0)

	// Try to parse manifest files for dependency validation.
	manifestDeps := make(map[string]bool)
	hasManifest := false

	if pomDeps, err := parsePomXml(repoPath); err == nil {
		hasManifest = true
		for k, v := range pomDeps {
			manifestDeps[k] = v
		}
	}
	if gradleDeps, err := parseBuildGradle(repoPath); err == nil {
		hasManifest = true
		for k, v := range gradleDeps {
			manifestDeps[k] = v
		}
	}

	for _, imp := range pf.Imports {
		importPath := imp.Path

		// Remove trailing .* for wildcard imports
		checkPath := strings.TrimSuffix(importPath, ".*")

		// Check known phantom patterns first
		isPhantom := false
		for _, phantom := range phantomJavaPackages {
			if strings.HasPrefix(checkPath, phantom) || strings.HasPrefix(checkPath+".", phantom) {
				issues = append(issues, Issue{
					ID:         "hallucination/phantom-import",
					FixID:      "import-broken-java",
					Severity:   SeverityError,
					Category:   "hallucination",
					File:       filePath,
					Line:       imp.Line,
					Message:    "Import references likely hallucinated package: " + importPath,
					Suggestion: "This package does not exist in the Java standard library. Check the correct package name",
				})
				isPhantom = true
				break
			}
		}
		if isPhantom {
			continue
		}

		// Standard library imports — check broad prefix match.
		isStdlib := false
		for _, prefix := range javaStdlibPrefixes {
			if strings.HasPrefix(checkPath, prefix) {
				isStdlib = true
				break
			}
		}
		if isStdlib {
			continue
		}

		// If no manifest found, fail open — don't flag unknown packages.
		if !hasManifest {
			continue
		}

		// Check if the import's groupId prefix matches any manifest dependency.
		found := false
		for depGroup := range manifestDeps {
			if strings.HasPrefix(checkPath, depGroup) {
				found = true
				break
			}
		}

		if !found {
			issues = append(issues, Issue{
				ID:         "hallucination/phantom-import",
				Severity:   SeverityError,
				Category:   "hallucination",
				File:       filePath,
				Line:       imp.Line,
				Message:    "Import references unknown package: " + importPath,
				Suggestion: "Verify the package exists and add it to pom.xml or build.gradle",
			})
		}
	}

	return issues
}

// --- Kotlin phantom import detection ---

// kotlinStdlib is a set of Kotlin standard library and kotlinx package prefixes.
var kotlinStdlib = map[string]bool{
	"kotlin":                   true,
	"kotlin.annotation":        true,
	"kotlin.collections":       true,
	"kotlin.comparisons":       true,
	"kotlin.concurrent":        true,
	"kotlin.contracts":         true,
	"kotlin.coroutines":        true,
	"kotlin.io":                true,
	"kotlin.jvm":               true,
	"kotlin.math":              true,
	"kotlin.properties":        true,
	"kotlin.random":            true,
	"kotlin.ranges":            true,
	"kotlin.reflect":           true,
	"kotlin.sequences":         true,
	"kotlin.streams":           true,
	"kotlin.system":            true,
	"kotlin.text":              true,
	"kotlin.time":              true,
	"kotlinx.coroutines":       true,
	"kotlinx.serialization":    true,
	"kotlinx.datetime":         true,
	"kotlinx.io":               true,
}

// checkKotlinImports validates Kotlin imports against manifests, Java stdlib, and Kotlin stdlib.
func (h *HallucinationAnalyzer) checkKotlinImports(repoPath, filePath string, pf *parser.ParsedFile) []Issue {
	issues := make([]Issue, 0)

	// Kotlin reuses Java manifests (pom.xml / build.gradle).
	manifestDeps := make(map[string]bool)
	hasManifest := false

	if pomDeps, err := parsePomXml(repoPath); err == nil {
		hasManifest = true
		for k, v := range pomDeps {
			manifestDeps[k] = v
		}
	}
	if gradleDeps, err := parseBuildGradle(repoPath); err == nil {
		hasManifest = true
		for k, v := range gradleDeps {
			manifestDeps[k] = v
		}
	}

	for _, imp := range pf.Imports {
		importPath := imp.Path
		checkPath := strings.TrimSuffix(importPath, ".*")

		// Check Kotlin stdlib — match by prefix.
		isKotlinStd := false
		for prefix := range kotlinStdlib {
			if checkPath == prefix || strings.HasPrefix(checkPath, prefix+".") {
				isKotlinStd = true
				break
			}
		}
		if isKotlinStd {
			continue
		}

		// Check Java stdlib — Kotlin can import Java classes.
		isJavaStd := false
		for _, prefix := range javaStdlibPrefixes {
			if strings.HasPrefix(checkPath, prefix) {
				isJavaStd = true
				break
			}
		}
		if isJavaStd {
			continue
		}

		// Check known Java phantom patterns.
		isPhantom := false
		for _, phantom := range phantomJavaPackages {
			if strings.HasPrefix(checkPath, phantom) || strings.HasPrefix(checkPath+".", phantom) {
				issues = append(issues, Issue{
					ID:         "hallucination/phantom-import",
					FixID:      "import-broken-java",
					Severity:   SeverityError,
					Category:   "hallucination",
					File:       filePath,
					Line:       imp.Line,
					Message:    "Import references likely hallucinated package: " + importPath,
					Suggestion: "This package does not exist. Check the correct package name",
				})
				isPhantom = true
				break
			}
		}
		if isPhantom {
			continue
		}

		// If no manifest found, fail open.
		if !hasManifest {
			continue
		}

		// Check if the import matches any manifest dependency.
		found := false
		for depGroup := range manifestDeps {
			if strings.HasPrefix(checkPath, depGroup) {
				found = true
				break
			}
		}

		if !found {
			issues = append(issues, Issue{
				ID:         "hallucination/phantom-import",
				Severity:   SeverityError,
				Category:   "hallucination",
				File:       filePath,
				Line:       imp.Line,
				Message:    "Import references unknown package: " + importPath,
				Suggestion: "Verify the package exists and add it to pom.xml or build.gradle",
			})
		}
	}

	return issues
}

// --- C# phantom import detection ---

// csharpStdNamespaces is a set of well-known C# standard library / .NET namespaces.
var csharpStdNamespaces = map[string]bool{
	"System":                          true,
	"System.Buffers":                  true,
	"System.Collections":              true,
	"System.Collections.Concurrent":   true,
	"System.Collections.Generic":      true,
	"System.Collections.Immutable":    true,
	"System.Collections.ObjectModel":  true,
	"System.Collections.Specialized":  true,
	"System.ComponentModel":           true,
	"System.Configuration":            true,
	"System.Data":                     true,
	"System.Diagnostics":              true,
	"System.Drawing":                  true,
	"System.Dynamic":                  true,
	"System.Globalization":            true,
	"System.IO":                       true,
	"System.IO.Compression":           true,
	"System.IO.Pipes":                 true,
	"System.Linq":                     true,
	"System.Linq.Expressions":         true,
	"System.Math":                     true,
	"System.Net":                      true,
	"System.Net.Http":                 true,
	"System.Net.Sockets":              true,
	"System.Numerics":                 true,
	"System.Reflection":               true,
	"System.Resources":                true,
	"System.Runtime":                  true,
	"System.Runtime.CompilerServices": true,
	"System.Runtime.InteropServices":  true,
	"System.Runtime.Serialization":    true,
	"System.Security":                 true,
	"System.Security.Claims":          true,
	"System.Security.Cryptography":    true,
	"System.Text":                     true,
	"System.Text.Encoding":            true,
	"System.Text.Json":                true,
	"System.Text.RegularExpressions":  true,
	"System.Threading":                true,
	"System.Threading.Channels":       true,
	"System.Threading.Tasks":          true,
	"System.Xml":                      true,
	"System.Xml.Linq":                 true,
	"System.Xml.Serialization":        true,
}

// commonCSharpLibPrefixes are well-known third-party and framework namespace prefixes.
var commonCSharpLibPrefixes = []string{
	"Microsoft.AspNetCore.",
	"Microsoft.Extensions.",
	"Microsoft.EntityFrameworkCore.",
	"Microsoft.Identity.",
	"Microsoft.Azure.",
	"Microsoft.Data.",
	"Microsoft.VisualStudio.",
	"Newtonsoft.Json",
	"Serilog.",
	"AutoMapper.",
	"FluentValidation.",
	"MediatR.",
	"Polly.",
	"Dapper.",
	"NUnit.",
	"Xunit.",
	"Moq.",
	"FluentAssertions.",
}

// phantomCSharpNamespaces are AI-hallucinated namespace patterns that don't exist.
var phantomCSharpNamespaces = []string{
	"System.Collections.Generics.",  // Common hallucination — correct is System.Collections.Generic
	"System.Collection.",            // Doesn't exist — correct is System.Collections
	"System.Util.",                  // Doesn't exist
	"System.Utils.",                 // Doesn't exist
	"System.Networking.",            // Correct is System.Net
	"System.Threading.Task.",        // Correct is System.Threading.Tasks
	"System.Text.Jsons.",            // Correct is System.Text.Json
	"Microsoft.AspNet.",             // Correct is Microsoft.AspNetCore
}

func (h *HallucinationAnalyzer) checkCSharpImports(repoPath, filePath string, pf *parser.ParsedFile) []Issue {
	issues := make([]Issue, 0)

	deps, _ := parseCsproj(repoPath)

	for _, imp := range pf.Imports {
		namespacePath := imp.Path

		// Check known phantom patterns first
		for _, phantom := range phantomCSharpNamespaces {
			if strings.HasPrefix(namespacePath, phantom) || strings.HasPrefix(namespacePath+".", phantom) {
				issues = append(issues, Issue{
					ID:         "hallucination/phantom-import",
					FixID:      "import-broken-csharp",
					Severity:   SeverityError,
					Category:   "hallucination",
					File:       filePath,
					Line:       imp.Line,
					Message:    "Import references likely hallucinated namespace: " + namespacePath,
					Suggestion: "This namespace does not exist in .NET. Check the correct namespace name",
				})
				break
			}
		}

		// Check standard library namespaces
		if csharpStdNamespaces[namespacePath] {
			continue
		}
		// Check if it's under a known System.* prefix
		if strings.HasPrefix(namespacePath, "System.") {
			continue
		}

		// Check known third-party library prefixes
		isKnown := false
		for _, prefix := range commonCSharpLibPrefixes {
			if strings.HasPrefix(namespacePath, prefix) {
				isKnown = true
				break
			}
		}
		if isKnown {
			continue
		}

		// Check against project references from .csproj
		if deps != nil && len(deps) > 0 {
			found := false
			for dep := range deps {
				if namespacePath == dep || strings.HasPrefix(namespacePath, dep+".") {
					found = true
					break
				}
			}
			if !found {
				// Not in stdlib, not in known libs, not in project deps — may be phantom
				// But only flag if we successfully parsed .csproj (otherwise fail open)
			}
		}
	}

	return issues
}

// parseCsproj reads .csproj files in the repo root for PackageReference elements.
func parseCsproj(repoPath string) (map[string]bool, error) {
	deps := make(map[string]bool)
	found := false

	entries, err := os.ReadDir(repoPath)
	if err != nil {
		return nil, err
	}

	pkgRefPattern := regexp.MustCompile(`<PackageReference\s+Include="([^"]+)"`)
	pkgConfigPattern := regexp.MustCompile(`<package\s+id="([^"]+)"`)

	for _, entry := range entries {
		if entry.IsDir() {
			continue
		}

		name := entry.Name()

		if strings.HasSuffix(name, ".csproj") {
			data, err := os.ReadFile(filepath.Join(repoPath, name))
			if err != nil {
				continue
			}
			found = true
			for _, match := range pkgRefPattern.FindAllStringSubmatch(string(data), -1) {
				if len(match) >= 2 {
					deps[match[1]] = true
				}
			}
		}

		if name == "packages.config" {
			data, err := os.ReadFile(filepath.Join(repoPath, name))
			if err != nil {
				continue
			}
			found = true
			for _, match := range pkgConfigPattern.FindAllStringSubmatch(string(data), -1) {
				if len(match) >= 2 {
					deps[match[1]] = true
				}
			}
		}
	}

	if !found {
		return nil, os.ErrNotExist
	}

	return deps, nil
}

// --- PHP phantom import detection ---

// checkPHPImports validates PHP use statements against composer.json.
func (h *HallucinationAnalyzer) checkPHPImports(repoPath, filePath string, pf *parser.ParsedFile) []Issue {
	issues := make([]Issue, 0)

	deps, err := parseComposerJSON(repoPath)
	if err != nil {
		return issues
	}

	for _, imp := range pf.Imports {
		importPath := imp.Path

		topVendor := importPath
		if idx := strings.Index(importPath, `\`); idx != -1 {
			topVendor = importPath[:idx]
		}

		if phpBuiltinNamespaces[topVendor] {
			continue
		}

		normalizedVendor := strings.ToLower(topVendor)
		if deps[normalizedVendor] {
			continue
		}

		parts := strings.SplitN(importPath, `\`, 3)
		if len(parts) >= 2 {
			composerKey := strings.ToLower(parts[0]) + "/" + strings.ToLower(parts[1])
			if deps[composerKey] {
				continue
			}
		}

		if phpAppNamespaces[topVendor] {
			continue
		}

		issues = append(issues, Issue{
			ID:         "hallucination/phantom-import",
			Severity:   SeverityError,
			Category:   "hallucination",
			File:       filePath,
			Line:       imp.Line,
			Message:    "Import references unknown package: " + importPath,
			Suggestion: "Verify the package exists and add it to composer.json with 'composer require'",
		})
	}

	return issues
}

// phpBuiltinNamespaces are top-level PHP namespaces that are always available.
var phpBuiltinNamespaces = map[string]bool{
	"PHP":           true,
	"Ds":            true,
	"FFI":           true,
	"Fiber":         true,
	"Random":        true,
	"IntlException": true,
}

// phpAppNamespaces are common application-level namespace prefixes that are local code.
var phpAppNamespaces = map[string]bool{
	"App":      true,
	"Src":      true,
	"Tests":    true,
	"Test":     true,
	"Database": true,
	"Modules":  true,
	"Domain":   true,
	"Http":     true,
	"Console":  true,
}

// parseComposerJSON reads a composer.json and returns a set of dependency identifiers.
func parseComposerJSON(repoPath string) (map[string]bool, error) {
	composerPath := filepath.Join(repoPath, "composer.json")
	f, err := os.Open(composerPath)
	if err != nil {
		return nil, err
	}
	defer f.Close()

	deps := make(map[string]bool)

	scanner := bufio.NewScanner(f)
	inDepsBlock := false
	braceDepth := 0

	depsKeyPattern := regexp.MustCompile(`"(require|require-dev)"\s*:\s*\{`)
	pkgNamePattern := regexp.MustCompile(`^\s*"([^"]+)"\s*:`)

	for scanner.Scan() {
		line := scanner.Text()

		if !inDepsBlock {
			if depsKeyPattern.MatchString(line) {
				inDepsBlock = true
				braceDepth = 1
				afterBrace := line[strings.Index(line, "{")+1:]
				if m := pkgNamePattern.FindStringSubmatch(afterBrace); len(m) >= 2 {
					addComposerDep(deps, m[1])
				}
			}
			continue
		}

		for _, ch := range line {
			if ch == '{' {
				braceDepth++
			} else if ch == '}' {
				braceDepth--
				if braceDepth <= 0 {
					inDepsBlock = false
					break
				}
			}
		}

		if inDepsBlock {
			if m := pkgNamePattern.FindStringSubmatch(line); len(m) >= 2 {
				addComposerDep(deps, m[1])
			}
		}
	}

	return deps, nil
}

// addComposerDep adds a composer dependency name in multiple lookup forms.
func addComposerDep(deps map[string]bool, name string) {
	if name == "php" || strings.HasPrefix(name, "ext-") || strings.HasPrefix(name, "lib-") {
		return
	}

	deps[strings.ToLower(name)] = true

	if idx := strings.Index(name, "/"); idx != -1 {
		vendor := strings.ToLower(name[:idx])
		deps[vendor] = true
	}
}

// --- Swift phantom import detection ---

// swiftStdModules is a set of well-known Swift standard library and Apple framework modules.
var swiftStdModules = map[string]bool{
	"Foundation": true, "UIKit": true, "SwiftUI": true, "Combine": true,
	"CoreData": true, "CoreGraphics": true, "CoreLocation": true, "MapKit": true,
	"AVFoundation": true, "CoreImage": true, "CoreML": true, "Metal": true,
	"SceneKit": true, "SpriteKit": true, "ARKit": true, "RealityKit": true,
	"Observation": true, "Swift": true, "_Concurrency": true, "os": true,
	"AppKit": true, "WatchKit": true, "WidgetKit": true, "GameKit": true,
	"StoreKit": true, "CloudKit": true, "WebKit": true, "SafariServices": true,
	"AuthenticationServices": true, "CryptoKit": true, "Network": true,
	"NaturalLanguage": true, "Vision": true, "CoreBluetooth": true,
	"CoreMotion": true, "CoreTelephony": true, "CoreText": true,
	"Dispatch": true, "ObjectiveC": true, "Darwin": true, "XCTest": true,
}

// checkSwiftImports validates Swift imports against Package.swift and Podfile.
func (h *HallucinationAnalyzer) checkSwiftImports(repoPath, filePath string, pf *parser.ParsedFile) []Issue {
	issues := make([]Issue, 0)

	deps := make(map[string]bool)
	pkgDeps, pkgErr := parsePackageSwift(repoPath)
	if pkgErr == nil {
		for k, v := range pkgDeps {
			deps[k] = v
		}
	}
	podDeps, podErr := parsePodfile(repoPath)
	if podErr == nil {
		for k, v := range podDeps {
			deps[k] = v
		}
	}

	if pkgErr != nil && podErr != nil {
		return issues
	}

	for _, imp := range pf.Imports {
		moduleName := imp.Path

		if swiftStdModules[moduleName] {
			continue
		}

		if deps[moduleName] {
			continue
		}

		issues = append(issues, Issue{
			ID:         "hallucination/phantom-import",
			Severity:   SeverityError,
			Category:   "hallucination",
			File:       filePath,
			Line:       imp.Line,
			Message:    "Import references unknown module: " + moduleName,
			Suggestion: "Verify the module exists and add it to Package.swift or Podfile",
		})
	}

	return issues
}

// parsePackageSwift reads a Package.swift file and returns dependency names.
func parsePackageSwift(repoPath string) (map[string]bool, error) {
	pkgPath := filepath.Join(repoPath, "Package.swift")
	data, err := os.ReadFile(pkgPath)
	if err != nil {
		return nil, err
	}

	deps := make(map[string]bool)

	rePackageURL := regexp.MustCompile(`\.package\s*\(\s*url\s*:\s*"[^"]*?/([^/"]+?)(?:\.git)?"\s*,`)
	matches := rePackageURL.FindAllStringSubmatch(string(data), -1)
	for _, m := range matches {
		if len(m) >= 2 {
			deps[m[1]] = true
		}
	}

	rePackageName := regexp.MustCompile(`\.package\s*\(\s*name\s*:\s*"([^"]+)"`)
	matches = rePackageName.FindAllStringSubmatch(string(data), -1)
	for _, m := range matches {
		if len(m) >= 2 {
			deps[m[1]] = true
		}
	}

	reProductName := regexp.MustCompile(`\.product\s*\(\s*name\s*:\s*"([^"]+)"`)
	matches = reProductName.FindAllStringSubmatch(string(data), -1)
	for _, m := range matches {
		if len(m) >= 2 {
			deps[m[1]] = true
		}
	}

	return deps, nil
}

// parsePodfile reads a Podfile and returns pod names.
func parsePodfile(repoPath string) (map[string]bool, error) {
	podfilePath := filepath.Join(repoPath, "Podfile")
	data, err := os.ReadFile(podfilePath)
	if err != nil {
		return nil, err
	}

	deps := make(map[string]bool)

	rePod := regexp.MustCompile(`pod\s+['"]([^'"]+)['"]`)
	matches := rePod.FindAllStringSubmatch(string(data), -1)
	for _, m := range matches {
		if len(m) >= 2 {
			name := m[1]
			if idx := strings.Index(name, "/"); idx != -1 {
				name = name[:idx]
			}
			deps[name] = true
		}
	}

	return deps, nil
}

// --- Stub function detection ---

// Go function declaration.
var goFuncPattern = regexp.MustCompile(`^func\s+`)

// TS/JS function declarations.
var tsFuncPatterns = []*regexp.Regexp{
	regexp.MustCompile(`^(export\s+)?(async\s+)?function\s+\w+`),
	regexp.MustCompile(`^(export\s+)?(const|let|var)\s+\w+\s*=\s*(async\s+)?\(`),
	regexp.MustCompile(`^(export\s+)?(const|let|var)\s+\w+\s*=\s*(async\s+)?\w+\s*=>`),
}

// Python function declaration.
var pyFuncPattern = regexp.MustCompile(`^def\s+\w+\(`)

// Java method declaration (for stub detection).
var javaFuncPattern = regexp.MustCompile(`^\s*(?:public|protected|private)?\s*(?:static\s+)?(?:final\s+)?(?:\w[\w<>\[\],\s?]*?\s+)(\w+)\s*\(`)

// Rust function declaration.
var rustFuncPattern = regexp.MustCompile(`^(?:pub(?:\((?:crate|super)\))?\s+)?(?:async\s+)?fn\s+\w+`)

// Kotlin function declaration (for stub detection).
var kotlinFuncPattern = regexp.MustCompile(`^\s*(?:(?:public|protected|private|internal)\s+)?(?:override\s+)?(?:suspend\s+)?(?:inline\s+)?fun\s+`)

// C# method declaration (for stub detection).
var csharpFuncPattern = regexp.MustCompile(`^\s*(?:public|protected|private|internal)?\s*(?:static\s+)?(?:async\s+)?(?:virtual\s+)?(?:override\s+)?(?:\w[\w<>\[\],\s?]*?\s+)(\w+)\s*\(`)

// PHP function/method declaration.
var phpFuncPattern = regexp.MustCompile(`^\s*(?:public|protected|private)?\s*(?:static\s+)?function\s+\w+`)

// Swift function declaration.
var swiftFuncPattern = regexp.MustCompile(`^(?:open|public|internal|fileprivate|private)?\s*(?:static\s+|class\s+)?(?:override\s+)?func\s+\w+`)

// Stub body patterns.
var stubPatterns = []*regexp.Regexp{
	regexp.MustCompile(`^\s*return\s+(nil|""|0|false|\[\]|\{\}|None|null)\s*;?\s*$`),
	regexp.MustCompile(`^\s*return;?\s*$`),
	regexp.MustCompile(`^\s*return\s+\[\]\s*;?\s*$`),
	regexp.MustCompile(`^\s*return\s+\{\}\s*;?\s*$`),
	regexp.MustCompile(`^\s*pass\s*$`),
	regexp.MustCompile(`(?i)^\s*throw\s+new\s+Error\(\s*["'](not implemented|TODO)`),
	regexp.MustCompile(`(?i)^\s*throw\s+new\s+UnsupportedOperationException\s*\(`),
	regexp.MustCompile(`(?i)^\s*panic\(\s*"(not implemented|TODO)`),
	regexp.MustCompile(`(?i)^\s*//\s*TODO\b`),
	regexp.MustCompile(`(?i)^\s*#\s*TODO\b`),
	regexp.MustCompile(`^\s*todo!\(`),
	regexp.MustCompile(`^\s*unimplemented!\(`),
}

func (h *HallucinationAnalyzer) checkStubFunctions(fileDiff git.FileDiff) []Issue {
	issues := make([]Issue, 0)

	ext := strings.ToLower(filepath.Ext(fileDiff.Path))

	for _, hunk := range fileDiff.Hunks {
		lines := hunk.Lines

		for i := 0; i < len(lines); i++ {
			line := lines[i]
			if line.Type != "added" {
				continue
			}

			content := strings.TrimSpace(line.Content)
			funcLine := line.NewNum

			isFuncDecl := false
			switch {
			case ext == ".go":
				isFuncDecl = goFuncPattern.MatchString(content)
			case ext == ".ts" || ext == ".tsx" || ext == ".js" || ext == ".jsx" || ext == ".mjs" || ext == ".cjs":
				for _, p := range tsFuncPatterns {
					if p.MatchString(content) {
						isFuncDecl = true
						break
					}
				}
			case ext == ".py":
				isFuncDecl = pyFuncPattern.MatchString(content)
			case ext == ".java":
				isFuncDecl = javaFuncPattern.MatchString(content)
			case ext == ".rs":
				isFuncDecl = rustFuncPattern.MatchString(content)
			case ext == ".kt" || ext == ".kts":
				isFuncDecl = kotlinFuncPattern.MatchString(content)
			case ext == ".cs":
				isFuncDecl = csharpFuncPattern.MatchString(content)
			case ext == ".php":
				isFuncDecl = phpFuncPattern.MatchString(content)
			case ext == ".swift":
				isFuncDecl = swiftFuncPattern.MatchString(content)
			case ext == ".c" || ext == ".h" || ext == ".cpp" || ext == ".cc" || ext == ".cxx" || ext == ".hpp" || ext == ".hxx":
				isFuncDecl = cFuncPattern.MatchString(content)
			case ext == ".m" || ext == ".mm":
				isFuncDecl = objcMethodPattern.MatchString(content)
			case ext == ".dart":
				isFuncDecl = dartFuncPattern.MatchString(content)
			case ext == ".scala" || ext == ".sc":
				isFuncDecl = scalaFuncPattern.MatchString(content)
			case ext == ".r":
				isFuncDecl = rFuncPattern.MatchString(content)
			case ext == ".ex" || ext == ".exs":
				isFuncDecl = elixirFuncPattern.MatchString(content)
			case ext == ".lua":
				isFuncDecl = luaFuncPattern.MatchString(content)
			case ext == ".sh" || ext == ".bash":
				isFuncDecl = bashFuncPattern.MatchString(content)
			}

			if !isFuncDecl {
				continue
			}

			// Collect the body lines (next added lines until we see a line that
			// closes the block or is not added).
			bodyLines := make([]string, 0)
			for j := i + 1; j < len(lines) && len(bodyLines) < 5; j++ {
				next := lines[j]
				if next.Type != "added" {
					break
				}

				nextTrimmed := strings.TrimSpace(next.Content)

				// Stop at closing brace for Go/JS/TS.
				if nextTrimmed == "}" || nextTrimmed == "})" || nextTrimmed == "};" {
					break
				}

				// Stop at next function declaration or class declaration.
				if goFuncPattern.MatchString(nextTrimmed) || pyFuncPattern.MatchString(nextTrimmed) {
					break
				}

				// Skip blank lines.
				if nextTrimmed == "" || nextTrimmed == "{" {
					continue
				}

				bodyLines = append(bodyLines, nextTrimmed)
			}

			// A stub is a function with 1 meaningful body line that matches a stub pattern.
			if len(bodyLines) == 1 {
				for _, pat := range stubPatterns {
					if pat.MatchString(bodyLines[0]) {
						issues = append(issues, Issue{
							ID:         "hallucination/stub-implementation",
							Severity:   SeverityWarning,
							Category:   "hallucination",
							File:       fileDiff.Path,
							Line:       funcLine,
							Message:    "Possibly incomplete implementation: " + content,
							Suggestion: "This function appears to be a stub — implement the actual logic or remove it",
						})
						break
					}
				}
			}
		}
	}

	return issues
}

// --- Ruby import validation ---

// rubyStdlib contains Ruby standard library modules/gems.
var rubyStdlib = map[string]bool{
	"abbrev": true, "base64": true, "benchmark": true, "bigdecimal": true,
	"cgi": true, "cmath": true, "coverage": true, "csv": true,
	"date": true, "dbm": true, "debug": true, "delegate": true,
	"digest": true, "drb": true, "english": true, "erb": true,
	"etc": true, "expect": true, "fcntl": true, "fiber": true,
	"fiddle": true, "fileutils": true, "find": true, "forwardable": true,
	"gdbm": true, "getoptlong": true, "io/console": true, "io/nonblock": true,
	"io/wait": true, "ipaddr": true, "irb": true, "json": true,
	"logger": true, "matrix": true, "minitest": true, "monitor": true,
	"mutex_m": true, "net/ftp": true, "net/http": true, "net/imap": true,
	"net/pop": true, "net/smtp": true, "nkf": true, "objspace": true,
	"observer": true, "open-uri": true, "open3": true, "openssl": true,
	"optparse": true, "ostruct": true, "pathname": true, "pp": true,
	"prettyprint": true, "prime": true, "pstore": true, "psych": true,
	"pty": true, "racc": true, "readline": true, "reline": true,
	"resolv": true, "ripper": true, "rss": true, "ruby2_keywords": true,
	"securerandom": true, "set": true, "shellwords": true, "singleton": true,
	"socket": true, "stringio": true, "strscan": true, "syslog": true,
	"tempfile": true, "time": true, "timeout": true, "tmpdir": true,
	"tracer": true, "tsort": true, "un": true, "unicode_normalize": true,
	"uri": true, "weakref": true, "webrick": true, "yaml": true, "zlib": true,
	// Commonly available built-in requires
	"thread": true, "rbconfig": true, "mkmf": true, "rubygems": true,
	"bundler": true, "bundler/setup": true, "rake": true,
}

func (h *HallucinationAnalyzer) checkRubyImports(repoPath, filePath string, pf *parser.ParsedFile) []Issue {
	issues := make([]Issue, 0)

	gems, err := parseGemfile(repoPath)
	if err != nil {
		// Can't read Gemfile -- fail open.
		return issues
	}

	for _, imp := range pf.Imports {
		importPath := imp.Path

		// Skip relative imports (require_relative).
		if strings.HasPrefix(importPath, ".") {
			continue
		}

		// Check standard library.
		if rubyStdlib[importPath] {
			continue
		}

		// Normalize: some gems use hyphens but imports use slashes or underscores.
		topGem := importPath
		if idx := strings.Index(importPath, "/"); idx != -1 {
			topGem = importPath[:idx]
		}
		normalizedGem := strings.ReplaceAll(topGem, "-", "_")

		if gems[topGem] || gems[normalizedGem] {
			continue
		}
		// Also try with hyphens swapped to underscores and vice versa.
		hyphenGem := strings.ReplaceAll(topGem, "_", "-")
		if gems[hyphenGem] {
			continue
		}

		issues = append(issues, Issue{
			ID:         "hallucination/phantom-import",
			Severity:   SeverityError,
			Category:   "hallucination",
			File:       filePath,
			Line:       imp.Line,
			Message:    "Import references unknown gem: " + importPath,
			Suggestion: "Verify the gem exists and add it to your Gemfile with 'gem \"" + topGem + "\"'",
		})
	}

	return issues
}

// parseGemfile reads a Gemfile and returns a set of gem names.
func parseGemfile(repoPath string) (map[string]bool, error) {
	gems := make(map[string]bool)

	gemfilePath := filepath.Join(repoPath, "Gemfile")
	f, err := os.Open(gemfilePath)
	if err != nil {
		return nil, err
	}
	defer f.Close()

	gemPattern := regexp.MustCompile(`^\s*gem\s+['"]([^'"]+)['"]`)
	scanner := bufio.NewScanner(f)
	for scanner.Scan() {
		line := scanner.Text()
		if m := gemPattern.FindStringSubmatch(line); len(m) >= 2 {
			gems[m[1]] = true
		}
	}

	return gems, nil
}

// --- Helpers ---

// fileExists returns true if the path exists and is a regular file or directory.
func fileExists(path string) bool {
	_, err := os.Stat(path)
	return err == nil
}
