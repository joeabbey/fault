package analyzer

import (
	"os"
	"path/filepath"
	"regexp"
	"strings"

	"github.com/joeabbey/fault/pkg/parser"
)

// --- Zig phantom import detection ---

var zigStdModules = map[string]bool{
	"std": true, "builtin": true,
	"std.mem": true, "std.fmt": true, "std.fs": true, "std.io": true,
	"std.net": true, "std.http": true, "std.json": true, "std.math": true,
	"std.os": true, "std.debug": true, "std.heap": true, "std.time": true,
	"std.testing": true, "std.crypto": true, "std.rand": true, "std.log": true,
	"std.sort": true, "std.unicode": true, "std.process": true, "std.Thread": true,
	"std.ArrayList": true, "std.AutoHashMap": true, "std.StringHashMap": true,
}

func (h *HallucinationAnalyzer) checkZigImports(repoPath, filePath string, pf *parser.ParsedFile) []Issue {
	issues := make([]Issue, 0)

	deps, err := parseBuildZigZon(repoPath)
	if err != nil {
		return issues
	}

	for _, imp := range pf.Imports {
		importPath := imp.Path
		if zigStdModules[importPath] {
			continue
		}
		if strings.HasPrefix(importPath, "std.") || importPath == "std" || importPath == "builtin" {
			continue
		}
		// Relative imports
		if strings.HasPrefix(importPath, ".") || strings.HasPrefix(importPath, "/") {
			continue
		}
		if deps[importPath] {
			continue
		}
		issues = append(issues, Issue{
			ID: "hallucination/phantom-import", Severity: SeverityError, Category: "hallucination",
			File: filePath, Line: imp.Line,
			Message:    "Import references unknown Zig module: " + importPath,
			Suggestion: "Verify the module exists and add it to build.zig.zon dependencies",
		})
	}
	return issues
}

func parseBuildZigZon(repoPath string) (map[string]bool, error) {
	zonPath := findFileUp(repoPath, "build.zig.zon")
	if zonPath == "" {
		return nil, os.ErrNotExist
	}
	data, err := os.ReadFile(zonPath)
	if err != nil {
		return nil, err
	}
	deps := make(map[string]bool)
	depRe := regexp.MustCompile(`\.(\w+)\s*=\s*\.{`)
	for _, m := range depRe.FindAllStringSubmatch(string(data), -1) {
		if len(m) >= 2 {
			deps[m[1]] = true
		}
	}
	return deps, nil
}

// --- Nim phantom import detection ---

var nimStdModules = map[string]bool{
	"system": true, "os": true, "strutils": true, "sequtils": true,
	"tables": true, "sets": true, "algorithm": true, "math": true,
	"json": true, "httpclient": true, "asyncdispatch": true, "net": true,
	"streams": true, "unicode": true, "parseutils": true, "times": true,
	"threadpool": true, "locks": true, "channels": true, "macros": true,
	"sugar": true, "options": true, "strformat": true, "re": true,
	"nre": true, "osproc": true, "parseopt": true, "terminal": true,
	"unittest": true, "logging": true, "hashes": true, "marshal": true,
}

func (h *HallucinationAnalyzer) checkNimImports(repoPath, filePath string, pf *parser.ParsedFile) []Issue {
	issues := make([]Issue, 0)

	deps, err := parseNimble(repoPath)
	if err != nil {
		return issues
	}

	for _, imp := range pf.Imports {
		importPath := imp.Path
		if nimStdModules[importPath] {
			continue
		}
		if strings.HasPrefix(importPath, ".") || strings.HasPrefix(importPath, "/") {
			continue
		}
		if deps[importPath] {
			continue
		}
		issues = append(issues, Issue{
			ID: "hallucination/phantom-import", Severity: SeverityError, Category: "hallucination",
			File: filePath, Line: imp.Line,
			Message:    "Import references unknown Nim module: " + importPath,
			Suggestion: "Verify the module exists and add it to your .nimble file requires",
		})
	}
	return issues
}

func parseNimble(repoPath string) (map[string]bool, error) {
	deps := make(map[string]bool)
	found := false
	entries, err := os.ReadDir(repoPath)
	if err != nil {
		return nil, err
	}
	for _, entry := range entries {
		if entry.IsDir() || !strings.HasSuffix(entry.Name(), ".nimble") {
			continue
		}
		data, err := os.ReadFile(filepath.Join(repoPath, entry.Name()))
		if err != nil {
			continue
		}
		found = true
		reqRe := regexp.MustCompile(`requires\s+"(\w+)`)
		for _, m := range reqRe.FindAllStringSubmatch(string(data), -1) {
			if len(m) >= 2 {
				deps[m[1]] = true
			}
		}
	}
	if !found {
		return nil, os.ErrNotExist
	}
	return deps, nil
}

// --- Crystal phantom import detection ---

var crystalStdModules = map[string]bool{
	"comparable": true, "enumerable": true, "io": true, "file": true,
	"dir": true, "http": true, "json": true, "yaml": true,
	"socket": true, "process": true, "regex": true, "time": true,
	"random": true, "math": true, "spec": true, "log": true,
	"option": true, "channel": true, "fiber": true, "mutex": true,
	"xml": true, "csv": true, "base64": true, "digest": true,
	"openssl": true, "uri": true, "uuid": true, "benchmark": true,
	"colorize": true, "html": true, "markdown": true, "mime": true,
	"http/server": true, "http/client": true,
}

func (h *HallucinationAnalyzer) checkCrystalImports(repoPath, filePath string, pf *parser.ParsedFile) []Issue {
	issues := make([]Issue, 0)

	deps, err := parseShardYml(repoPath)
	if err != nil {
		return issues
	}

	for _, imp := range pf.Imports {
		importPath := imp.Path
		if crystalStdModules[importPath] {
			continue
		}
		if strings.HasPrefix(importPath, ".") || strings.HasPrefix(importPath, "/") {
			continue
		}
		if deps[importPath] {
			continue
		}
		topModule := importPath
		if idx := strings.Index(importPath, "/"); idx != -1 {
			topModule = importPath[:idx]
		}
		if crystalStdModules[topModule] || deps[topModule] {
			continue
		}
		issues = append(issues, Issue{
			ID: "hallucination/phantom-import", Severity: SeverityError, Category: "hallucination",
			File: filePath, Line: imp.Line,
			Message:    "Require references unknown Crystal shard: " + importPath,
			Suggestion: "Verify the shard exists and add it to shard.yml dependencies",
		})
	}
	return issues
}

func parseShardYml(repoPath string) (map[string]bool, error) {
	shardPath := findFileUp(repoPath, "shard.yml")
	if shardPath == "" {
		return nil, os.ErrNotExist
	}
	data, err := os.ReadFile(shardPath)
	if err != nil {
		return nil, err
	}
	deps := make(map[string]bool)
	inDeps := false
	for _, line := range strings.Split(string(data), "\n") {
		trimmed := strings.TrimSpace(line)
		if !strings.HasPrefix(line, " ") && !strings.HasPrefix(line, "\t") && strings.HasSuffix(trimmed, ":") {
			sectionName := strings.TrimSuffix(trimmed, ":")
			inDeps = sectionName == "dependencies" || sectionName == "development_dependencies"
			continue
		}
		if !inDeps {
			continue
		}
		if len(line) > 0 && line[0] != ' ' && line[0] != '\t' {
			inDeps = false
			continue
		}
		if idx := strings.Index(trimmed, ":"); idx > 0 {
			name := strings.TrimSpace(trimmed[:idx])
			indent := len(line) - len(strings.TrimLeft(line, " \t"))
			if indent <= 4 && name != "" && !strings.HasPrefix(name, "#") {
				deps[name] = true
			}
		}
	}
	return deps, nil
}

// --- V language phantom import detection ---

var vlangStdModules = map[string]bool{
	"os": true, "math": true, "json": true, "net": true,
	"net.http": true, "sync": true, "time": true, "io": true,
	"crypto": true, "rand": true, "arrays": true, "strings": true,
	"encoding": true, "regex": true, "log": true, "cli": true,
	"flag": true, "term": true, "datatypes": true, "dl": true,
	"builtin": true, "v.ast": true, "v.fmt": true, "v.parser": true,
	"v.reflection": true, "compress": true, "crypto.md5": true,
	"crypto.sha256": true, "encoding.csv": true, "encoding.utf8": true,
	"net.urllib": true, "net.websocket": true,
}

func (h *HallucinationAnalyzer) checkVlangImports(repoPath, filePath string, pf *parser.ParsedFile) []Issue {
	issues := make([]Issue, 0)

	deps, err := parseVMod(repoPath)
	if err != nil {
		return issues
	}

	for _, imp := range pf.Imports {
		importPath := imp.Path
		if vlangStdModules[importPath] {
			continue
		}
		if strings.HasPrefix(importPath, ".") || strings.HasPrefix(importPath, "/") {
			continue
		}
		topModule := importPath
		if idx := strings.Index(importPath, "."); idx != -1 {
			topModule = importPath[:idx]
		}
		if vlangStdModules[topModule] {
			continue
		}
		if deps[importPath] || deps[topModule] {
			continue
		}
		issues = append(issues, Issue{
			ID: "hallucination/phantom-import", Severity: SeverityError, Category: "hallucination",
			File: filePath, Line: imp.Line,
			Message:    "Import references unknown V module: " + importPath,
			Suggestion: "Verify the module exists and add it to v.mod dependencies",
		})
	}
	return issues
}

func parseVMod(repoPath string) (map[string]bool, error) {
	vmodPath := findFileUp(repoPath, "v.mod")
	if vmodPath == "" {
		return nil, os.ErrNotExist
	}
	data, err := os.ReadFile(vmodPath)
	if err != nil {
		return nil, err
	}
	deps := make(map[string]bool)
	depRe := regexp.MustCompile(`'([\w.]+)'`)
	inDeps := false
	for _, line := range strings.Split(string(data), "\n") {
		trimmed := strings.TrimSpace(line)
		if strings.Contains(trimmed, "dependencies:") {
			inDeps = true
		}
		if inDeps {
			for _, m := range depRe.FindAllStringSubmatch(trimmed, -1) {
				if len(m) >= 2 {
					deps[m[1]] = true
				}
			}
			if strings.Contains(trimmed, "]") {
				inDeps = false
			}
		}
	}
	return deps, nil
}

// --- D language phantom import detection ---

var dlangStdModules = map[string]bool{
	"std.stdio": true, "std.string": true, "std.conv": true,
	"std.algorithm": true, "std.range": true, "std.array": true,
	"std.format": true, "std.file": true, "std.path": true,
	"std.json": true, "std.net": true, "std.regex": true,
	"std.math": true, "std.datetime": true, "std.concurrency": true,
	"std.parallelism": true, "std.container": true, "std.traits": true,
	"std.uni": true, "std.process": true, "core.memory": true,
	"std.typecons": true, "std.exception": true, "std.functional": true,
	"std.getopt": true, "std.bitmanip": true, "std.digest": true,
	"std.random": true, "std.signals": true, "std.socket": true,
	"std.variant": true, "std.xml": true, "std.zip": true,
	"core.thread": true, "core.sync": true, "core.stdc": true,
}

var dlangStdPrefixes = []string{
	"std.", "core.", "etc.",
}

func (h *HallucinationAnalyzer) checkDlangImports(repoPath, filePath string, pf *parser.ParsedFile) []Issue {
	issues := make([]Issue, 0)

	deps, err := parseDubManifest(repoPath)
	if err != nil {
		return issues
	}

	for _, imp := range pf.Imports {
		importPath := imp.Path
		if dlangStdModules[importPath] {
			continue
		}
		isStd := false
		for _, prefix := range dlangStdPrefixes {
			if strings.HasPrefix(importPath, prefix) {
				isStd = true
				break
			}
		}
		if isStd {
			continue
		}
		if strings.HasPrefix(importPath, ".") || strings.HasPrefix(importPath, "/") {
			continue
		}
		topModule := importPath
		if idx := strings.Index(importPath, "."); idx != -1 {
			topModule = importPath[:idx]
		}
		if deps[importPath] || deps[topModule] {
			continue
		}
		issues = append(issues, Issue{
			ID: "hallucination/phantom-import", Severity: SeverityError, Category: "hallucination",
			File: filePath, Line: imp.Line,
			Message:    "Import references unknown D module: " + importPath,
			Suggestion: "Verify the module exists and add it to dub.json or dub.sdl dependencies",
		})
	}
	return issues
}

func parseDubManifest(repoPath string) (map[string]bool, error) {
	deps := make(map[string]bool)
	hasManifest := false

	// Try dub.json
	dubJsonPath := findFileUp(repoPath, "dub.json")
	if dubJsonPath != "" {
		data, err := os.ReadFile(dubJsonPath)
		if err == nil {
			hasManifest = true
			depRe := regexp.MustCompile(`"(\w[\w-]*)"`)
			inDeps := false
			for _, line := range strings.Split(string(data), "\n") {
				trimmed := strings.TrimSpace(line)
				if strings.Contains(trimmed, "\"dependencies\"") {
					inDeps = true
				}
				if inDeps {
					for _, m := range depRe.FindAllStringSubmatch(trimmed, -1) {
						if len(m) >= 2 && m[1] != "dependencies" {
							deps[m[1]] = true
						}
					}
					if strings.Contains(trimmed, "}") && !strings.Contains(trimmed, "\"dependencies\"") {
						inDeps = false
					}
				}
			}
		}
	}

	// Try dub.sdl
	dubSdlPath := findFileUp(repoPath, "dub.sdl")
	if dubSdlPath != "" {
		data, err := os.ReadFile(dubSdlPath)
		if err == nil {
			hasManifest = true
			depRe := regexp.MustCompile(`dependency\s+"(\w[\w-]*)"`)
			for _, m := range depRe.FindAllStringSubmatch(string(data), -1) {
				if len(m) >= 2 {
					deps[m[1]] = true
				}
			}
		}
	}

	if !hasManifest {
		return nil, os.ErrNotExist
	}
	return deps, nil
}
