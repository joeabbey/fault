package analyzer

import (
	"bufio"
	"os"
	"path/filepath"
	"regexp"
	"strings"

	"github.com/joeabbey/fault/pkg/parser"
)

// --- C phantom import detection ---

// cStdHeaders is a set of C standard library headers (C11).
var cStdHeaders = map[string]bool{
	"assert.h": true, "complex.h": true, "ctype.h": true, "errno.h": true,
	"fenv.h": true, "float.h": true, "inttypes.h": true, "iso646.h": true,
	"limits.h": true, "locale.h": true, "math.h": true, "setjmp.h": true,
	"signal.h": true, "stdalign.h": true, "stdarg.h": true, "stdatomic.h": true,
	"stdbool.h": true, "stddef.h": true, "stdint.h": true, "stdio.h": true,
	"stdlib.h": true, "stdnoreturn.h": true, "string.h": true, "tgmath.h": true,
	"threads.h": true, "time.h": true, "uchar.h": true, "wchar.h": true,
	"wctype.h": true,
	// Common POSIX headers.
	"unistd.h": true, "fcntl.h": true, "sys/types.h": true, "sys/stat.h": true,
	"sys/socket.h": true, "sys/wait.h": true, "sys/mman.h": true, "sys/time.h": true,
	"sys/ioctl.h": true, "sys/select.h": true, "netinet/in.h": true,
	"arpa/inet.h": true, "netdb.h": true, "pthread.h": true, "dirent.h": true,
	"dlfcn.h": true, "poll.h": true, "semaphore.h": true, "termios.h": true,
	"syslog.h": true, "grp.h": true, "pwd.h": true,
}

// checkCImports validates C #include directives against stdlib and CMakeLists.txt.
func (h *HallucinationAnalyzer) checkCImports(repoPath, filePath string, pf *parser.ParsedFile) []Issue {
	issues := make([]Issue, 0)

	deps, err := parseCMakeLists(repoPath)
	if err != nil {
		// Can't read CMakeLists.txt — fail open.
		return issues
	}

	for _, imp := range pf.Imports {
		importPath := imp.Path

		// Local includes (#include "...") — skip phantom check.
		// The parser typically stores the path without quotes, but if it has
		// a relative-looking path or refers to a project header, skip it.
		// We only check angle-bracket system includes.
		if imp.IsType {
			// IsType is used by some parsers to distinguish local vs system includes.
			// If it's a local include, skip.
			continue
		}

		// Check standard headers.
		if cStdHeaders[importPath] {
			continue
		}

		// Check if the header belongs to a known CMake dependency.
		// Extract the top-level directory/prefix from the header path.
		topDir := importPath
		if idx := strings.Index(importPath, "/"); idx != -1 {
			topDir = importPath[:idx]
		}
		// Strip .h suffix for library name matching.
		headerBase := strings.TrimSuffix(importPath, ".h")
		headerBase = strings.TrimSuffix(headerBase, ".hpp")

		if deps[topDir] || deps[headerBase] || deps[strings.ToLower(topDir)] || deps[strings.ToLower(headerBase)] {
			continue
		}

		issues = append(issues, Issue{
			ID:         "hallucination/phantom-import",
			Severity:   SeverityError,
			Category:   "hallucination",
			File:       filePath,
			Line:       imp.Line,
			Message:    "Include references unknown header: " + importPath,
			Suggestion: "Verify the header exists and add the dependency to CMakeLists.txt",
		})
	}

	return issues
}

// parseCMakeLists reads CMakeLists.txt and extracts find_package and target_link_libraries names.
func parseCMakeLists(repoPath string) (map[string]bool, error) {
	cmakePath := findFileUp(repoPath, "CMakeLists.txt")
	if cmakePath == "" {
		return nil, os.ErrNotExist
	}

	data, err := os.ReadFile(cmakePath)
	if err != nil {
		return nil, err
	}

	deps := make(map[string]bool)

	findPkgRe := regexp.MustCompile(`find_package\s*\(\s*(\w+)`)
	targetLinkRe := regexp.MustCompile(`target_link_libraries\s*\([^)]*`)

	for _, m := range findPkgRe.FindAllStringSubmatch(string(data), -1) {
		if len(m) >= 2 {
			deps[m[1]] = true
			deps[strings.ToLower(m[1])] = true
		}
	}

	for _, m := range targetLinkRe.FindAllStringSubmatch(string(data), -1) {
		// Extract all words from target_link_libraries(...).
		// First word is the target, rest are libraries.
		words := strings.Fields(m[0])
		for i, w := range words {
			if i == 0 {
				continue // skip "target_link_libraries(target"
			}
			w = strings.TrimRight(w, ")")
			w = strings.TrimLeft(w, "(")
			if w == "" || w == "PUBLIC" || w == "PRIVATE" || w == "INTERFACE" {
				continue
			}
			deps[w] = true
			deps[strings.ToLower(w)] = true
		}
	}

	return deps, nil
}

// --- C++ phantom import detection ---

// cppStdHeaders is a set of C++ standard library headers (C++20).
var cppStdHeaders = map[string]bool{
	"algorithm": true, "any": true, "array": true, "atomic": true,
	"barrier": true, "bit": true, "bitset": true, "cassert": true,
	"ccomplex": true, "cctype": true, "cerrno": true, "cfenv": true,
	"cfloat": true, "charconv": true, "chrono": true, "cinttypes": true,
	"climits": true, "clocale": true, "cmath": true, "codecvt": true,
	"compare": true, "complex": true, "concepts": true, "condition_variable": true,
	"coroutine": true, "csetjmp": true, "csignal": true, "cstdarg": true,
	"cstddef": true, "cstdint": true, "cstdio": true, "cstdlib": true,
	"cstring": true, "ctgmath": true, "ctime": true, "cuchar": true,
	"cwchar": true, "cwctype": true, "deque": true, "exception": true,
	"execution": true, "expected": true, "filesystem": true, "format": true,
	"forward_list": true, "fstream": true, "functional": true, "future": true,
	"initializer_list": true, "iomanip": true, "ios": true, "iosfwd": true,
	"iostream": true, "istream": true, "iterator": true, "latch": true,
	"limits": true, "list": true, "locale": true, "map": true,
	"memory": true, "memory_resource": true, "mutex": true, "new": true,
	"numbers": true, "numeric": true, "optional": true, "ostream": true,
	"print": true, "queue": true, "random": true, "ranges": true,
	"ratio": true, "regex": true, "scoped_allocator": true, "semaphore": true,
	"set": true, "shared_mutex": true, "source_location": true, "span": true,
	"spanstream": true, "sstream": true, "stack": true, "stacktrace": true,
	"stdexcept": true, "stop_token": true, "streambuf": true, "string": true,
	"string_view": true, "strstream": true, "syncstream": true, "system_error": true,
	"thread": true, "tuple": true, "type_traits": true, "typeindex": true,
	"typeinfo": true, "unordered_map": true, "unordered_set": true, "utility": true,
	"valarray": true, "variant": true, "vector": true, "version": true,
}

// checkCppImports validates C++ includes against stdlib, C headers, and CMakeLists.txt.
func (h *HallucinationAnalyzer) checkCppImports(repoPath, filePath string, pf *parser.ParsedFile) []Issue {
	issues := make([]Issue, 0)

	deps, err := parseCMakeLists(repoPath)
	if err != nil {
		// Can't read CMakeLists.txt — fail open.
		return issues
	}

	for _, imp := range pf.Imports {
		importPath := imp.Path

		// Local includes — skip phantom check.
		if imp.IsType {
			continue
		}

		// Check C++ standard headers.
		if cppStdHeaders[importPath] {
			continue
		}

		// Check C standard headers (C++ can use them).
		if cStdHeaders[importPath] {
			continue
		}

		// Check if the header belongs to a known CMake dependency.
		topDir := importPath
		if idx := strings.Index(importPath, "/"); idx != -1 {
			topDir = importPath[:idx]
		}
		headerBase := strings.TrimSuffix(importPath, ".h")
		headerBase = strings.TrimSuffix(headerBase, ".hpp")
		headerBase = strings.TrimSuffix(headerBase, ".hxx")

		if deps[topDir] || deps[headerBase] || deps[strings.ToLower(topDir)] || deps[strings.ToLower(headerBase)] {
			continue
		}

		issues = append(issues, Issue{
			ID:         "hallucination/phantom-import",
			Severity:   SeverityError,
			Category:   "hallucination",
			File:       filePath,
			Line:       imp.Line,
			Message:    "Include references unknown header: " + importPath,
			Suggestion: "Verify the header exists and add the dependency to CMakeLists.txt",
		})
	}

	return issues
}

// --- Objective-C phantom import detection ---

// objcFrameworks is a set of well-known Apple frameworks available in Objective-C.
var objcFrameworks = map[string]bool{
	"Foundation": true, "UIKit": true, "AppKit": true, "CoreGraphics": true,
	"CoreFoundation": true, "CoreData": true, "CoreLocation": true, "MapKit": true,
	"AVFoundation": true, "CoreImage": true, "CoreML": true, "Metal": true,
	"SceneKit": true, "SpriteKit": true, "ARKit": true, "RealityKit": true,
	"QuartzCore": true, "CoreAnimation": true, "CoreText": true,
	"CoreBluetooth": true, "CoreMotion": true, "CoreTelephony": true,
	"CoreMedia": true, "CoreAudio": true, "CoreVideo": true,
	"Security": true, "SystemConfiguration": true, "CFNetwork": true,
	"Accelerate": true, "GameKit": true, "StoreKit": true, "CloudKit": true,
	"WebKit": true, "SafariServices": true, "AuthenticationServices": true,
	"CryptoKit": true, "Network": true, "Dispatch": true, "ObjectiveC": true,
	"Darwin": true, "XCTest": true, "os": true, "MobileCoreServices": true,
	"AddressBook": true, "AssetsLibrary": true, "AudioToolbox": true,
	"MediaPlayer": true, "MessageUI": true, "EventKit": true,
	"Contacts": true, "ContactsUI": true, "HealthKit": true,
	"HomeKit": true, "MultipeerConnectivity": true, "NotificationCenter": true,
	"Photos": true, "PhotosUI": true, "Social": true, "WatchConnectivity": true,
}

// checkObjCImports validates Objective-C imports against Apple frameworks, Podfile, and Package.swift.
func (h *HallucinationAnalyzer) checkObjCImports(repoPath, filePath string, pf *parser.ParsedFile) []Issue {
	issues := make([]Issue, 0)

	// Gather dependencies from Podfile and Package.swift (reuse Swift parsers).
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
		// No manifest files found — fail open.
		return issues
	}

	for _, imp := range pf.Imports {
		importPath := imp.Path

		// Local includes (#import "...") — skip phantom check.
		if imp.IsType {
			continue
		}

		// Extract the framework name from #import <Framework/Header.h>.
		frameworkName := importPath
		if idx := strings.Index(importPath, "/"); idx != -1 {
			frameworkName = importPath[:idx]
		}
		// Strip .h suffix if it's a bare header import.
		frameworkName = strings.TrimSuffix(frameworkName, ".h")

		// Check Apple frameworks.
		if objcFrameworks[frameworkName] {
			continue
		}

		// Check manifest dependencies.
		if deps[frameworkName] {
			continue
		}

		issues = append(issues, Issue{
			ID:         "hallucination/phantom-import",
			Severity:   SeverityError,
			Category:   "hallucination",
			File:       filePath,
			Line:       imp.Line,
			Message:    "Import references unknown framework: " + importPath,
			Suggestion: "Verify the framework exists and add it to Podfile or Package.swift",
		})
	}

	return issues
}

// --- Dart phantom import detection ---

// dartStdLibs is a set of Dart standard library packages (dart: prefix).
var dartStdLibs = map[string]bool{
	"dart:async":     true, "dart:collection": true, "dart:convert": true,
	"dart:core":      true, "dart:developer": true, "dart:io": true,
	"dart:isolate":   true, "dart:math": true, "dart:mirrors": true,
	"dart:typed_data": true, "dart:ffi": true, "dart:html": true,
	"dart:js":        true, "dart:js_util": true,
}

// checkDartImports validates Dart imports against dart: stdlib and pubspec.yaml.
func (h *HallucinationAnalyzer) checkDartImports(repoPath, filePath string, pf *parser.ParsedFile) []Issue {
	issues := make([]Issue, 0)

	deps, err := parsePubspecYaml(repoPath)
	if err != nil {
		// Can't read pubspec.yaml — fail open.
		return issues
	}

	for _, imp := range pf.Imports {
		importPath := imp.Path

		// dart: standard library imports.
		if strings.HasPrefix(importPath, "dart:") {
			if dartStdLibs[importPath] {
				continue
			}
			// Unknown dart: lib — flag it.
			issues = append(issues, Issue{
				ID:         "hallucination/phantom-import",
				Severity:   SeverityError,
				Category:   "hallucination",
				File:       filePath,
				Line:       imp.Line,
				Message:    "Import references unknown Dart library: " + importPath,
				Suggestion: "Verify the dart: library exists — see https://api.dart.dev/",
			})
			continue
		}

		// Relative imports — skip phantom check.
		if strings.HasPrefix(importPath, ".") || strings.HasPrefix(importPath, "/") {
			continue
		}

		// Package imports: import 'package:name/...'
		if strings.HasPrefix(importPath, "package:") {
			withoutPrefix := strings.TrimPrefix(importPath, "package:")
			pkgName := withoutPrefix
			if idx := strings.Index(withoutPrefix, "/"); idx != -1 {
				pkgName = withoutPrefix[:idx]
			}

			if deps[pkgName] {
				continue
			}

			issues = append(issues, Issue{
				ID:         "hallucination/phantom-import",
				Severity:   SeverityError,
				Category:   "hallucination",
				File:       filePath,
				Line:       imp.Line,
				Message:    "Import references unknown package: " + importPath,
				Suggestion: "Verify the package exists and add it to pubspec.yaml dependencies",
			})
		}
	}

	return issues
}

// parsePubspecYaml reads pubspec.yaml and extracts dependency names.
func parsePubspecYaml(repoPath string) (map[string]bool, error) {
	pubspecPath := findFileUp(repoPath, "pubspec.yaml")
	if pubspecPath == "" {
		return nil, os.ErrNotExist
	}

	data, err := os.ReadFile(pubspecPath)
	if err != nil {
		return nil, err
	}

	deps := make(map[string]bool)

	// Simple line-based YAML parsing for dependencies and dev_dependencies sections.
	inDeps := false
	lines := strings.Split(string(data), "\n")

	for _, line := range lines {
		trimmed := strings.TrimSpace(line)

		// Detect top-level sections (no leading whitespace).
		if !strings.HasPrefix(line, " ") && !strings.HasPrefix(line, "\t") && strings.HasSuffix(trimmed, ":") {
			sectionName := strings.TrimSuffix(trimmed, ":")
			inDeps = sectionName == "dependencies" || sectionName == "dev_dependencies"
			continue
		}

		if !inDeps {
			continue
		}

		// A new top-level key (no indent) ends the deps section.
		if len(line) > 0 && line[0] != ' ' && line[0] != '\t' {
			inDeps = false
			continue
		}

		// Dependency lines look like "  package_name: ^1.0.0" or "  package_name:".
		if idx := strings.Index(trimmed, ":"); idx > 0 {
			name := strings.TrimSpace(trimmed[:idx])
			// Skip nested keys (indented more than one level).
			indent := len(line) - len(strings.TrimLeft(line, " \t"))
			if indent <= 4 && name != "" && !strings.HasPrefix(name, "#") {
				deps[name] = true
			}
		}
	}

	// Also add the project's own name (self-imports are valid).
	for _, line := range lines {
		trimmed := strings.TrimSpace(line)
		if strings.HasPrefix(trimmed, "name:") {
			projectName := strings.TrimSpace(strings.TrimPrefix(trimmed, "name:"))
			deps[projectName] = true
			break
		}
	}

	return deps, nil
}

// --- Scala phantom import detection ---

// scalaStdPrefixes are standard library and JDK package prefixes valid in Scala.
var scalaStdPrefixes = []string{
	"scala.", "java.", "javax.",
}

// checkScalaImports validates Scala imports against stdlib, build.sbt, pom.xml, and build.gradle.
func (h *HallucinationAnalyzer) checkScalaImports(repoPath, filePath string, pf *parser.ParsedFile) []Issue {
	issues := make([]Issue, 0)

	// Gather manifest dependencies.
	manifestDeps := make(map[string]bool)
	hasManifest := false

	if sbtDeps, err := parseBuildSbt(repoPath); err == nil {
		hasManifest = true
		for k, v := range sbtDeps {
			manifestDeps[k] = v
		}
	}
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

	if !hasManifest {
		// No manifest — fail open.
		return issues
	}

	for _, imp := range pf.Imports {
		importPath := imp.Path

		// Remove trailing ._ or .{...} for wildcard imports.
		checkPath := importPath
		if strings.HasSuffix(checkPath, "._") {
			checkPath = strings.TrimSuffix(checkPath, "._")
		}

		// Check standard library prefixes.
		isStd := false
		for _, prefix := range scalaStdPrefixes {
			if strings.HasPrefix(checkPath, prefix) {
				isStd = true
				break
			}
		}
		if isStd {
			continue
		}

		// Check if import matches a manifest dependency groupId.
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
				Suggestion: "Verify the package exists and add it to build.sbt, pom.xml, or build.gradle",
			})
		}
	}

	return issues
}

// parseBuildSbt reads build.sbt and extracts libraryDependencies groupIds.
func parseBuildSbt(repoPath string) (map[string]bool, error) {
	sbtPath := findFileUp(repoPath, "build.sbt")
	if sbtPath == "" {
		return nil, os.ErrNotExist
	}

	data, err := os.ReadFile(sbtPath)
	if err != nil {
		return nil, err
	}

	deps := make(map[string]bool)

	// Match patterns like: "org.scalatest" %% "scalatest" % "3.2.0"
	// or: "com.typesafe.akka" %% "akka-actor" % "2.6.0"
	depRe := regexp.MustCompile(`"([a-zA-Z0-9._-]+)"\s*%%?\s*"([a-zA-Z0-9._-]+)"`)
	matches := depRe.FindAllStringSubmatch(string(data), -1)
	for _, m := range matches {
		if len(m) >= 3 {
			// Add the groupId (org prefix).
			deps[m[1]] = true
		}
	}

	return deps, nil
}

// --- R phantom import detection ---

// rBasePackages is a set of R base and recommended packages.
var rBasePackages = map[string]bool{
	"base": true, "compiler": true, "datasets": true, "grDevices": true,
	"graphics": true, "grid": true, "methods": true, "parallel": true,
	"splines": true, "stats": true, "stats4": true, "tcltk": true,
	"tools": true, "utils": true,
	// Recommended packages (shipped with R).
	"boot": true, "class": true, "cluster": true, "codetools": true,
	"foreign": true, "KernSmooth": true, "lattice": true, "MASS": true,
	"Matrix": true, "mgcv": true, "nlme": true, "nnet": true,
	"rpart": true, "spatial": true, "survival": true,
}

// checkRImports validates R library() / require() calls against base packages and DESCRIPTION.
func (h *HallucinationAnalyzer) checkRImports(repoPath, filePath string, pf *parser.ParsedFile) []Issue {
	issues := make([]Issue, 0)

	deps, err := parseRDescription(repoPath)
	if err != nil {
		// Can't read DESCRIPTION — fail open.
		return issues
	}

	for _, imp := range pf.Imports {
		pkgName := imp.Path

		// Check base packages.
		if rBasePackages[pkgName] {
			continue
		}

		// Check DESCRIPTION dependencies.
		if deps[pkgName] {
			continue
		}

		issues = append(issues, Issue{
			ID:         "hallucination/phantom-import",
			Severity:   SeverityError,
			Category:   "hallucination",
			File:       filePath,
			Line:       imp.Line,
			Message:    "Import references unknown R package: " + pkgName,
			Suggestion: "Verify the package exists and add it to DESCRIPTION Imports or Depends field",
		})
	}

	return issues
}

// parseRDescription reads a DESCRIPTION file and extracts packages from Imports and Depends fields.
func parseRDescription(repoPath string) (map[string]bool, error) {
	descPath := findFileUp(repoPath, "DESCRIPTION")
	if descPath == "" {
		return nil, os.ErrNotExist
	}

	data, err := os.ReadFile(descPath)
	if err != nil {
		return nil, err
	}

	deps := make(map[string]bool)

	// DESCRIPTION files use "Field: value" format.
	// Multi-line values are indented with whitespace.
	lines := strings.Split(string(data), "\n")
	inDepsField := false

	for _, line := range lines {
		trimmed := strings.TrimSpace(line)

		// Check for Imports: or Depends: field start.
		if strings.HasPrefix(trimmed, "Imports:") || strings.HasPrefix(trimmed, "Depends:") {
			inDepsField = true
			// Extract packages on the same line after the colon.
			afterColon := ""
			if strings.HasPrefix(trimmed, "Imports:") {
				afterColon = strings.TrimPrefix(trimmed, "Imports:")
			} else {
				afterColon = strings.TrimPrefix(trimmed, "Depends:")
			}
			extractRPackageNames(afterColon, deps)
			continue
		}

		// Continuation lines start with whitespace.
		if inDepsField {
			if len(line) > 0 && (line[0] == ' ' || line[0] == '\t') {
				extractRPackageNames(trimmed, deps)
			} else {
				// New field starts — stop collecting.
				inDepsField = false
			}
		}
	}

	return deps, nil
}

// extractRPackageNames parses comma-separated package names (with optional version constraints).
func extractRPackageNames(text string, deps map[string]bool) {
	// Packages are comma-separated, possibly with version constraints in parens.
	// e.g., "dplyr (>= 1.0.0), ggplot2, tidyr"
	parts := strings.Split(text, ",")
	for _, part := range parts {
		part = strings.TrimSpace(part)
		// Remove version constraint in parens.
		if idx := strings.Index(part, "("); idx != -1 {
			part = strings.TrimSpace(part[:idx])
		}
		if part != "" && part != "R" {
			deps[part] = true
		}
	}
}

// --- Elixir phantom import detection ---

// elixirStdModules is a set of Elixir standard library modules.
var elixirStdModules = map[string]bool{
	"Enum": true, "String": true, "Map": true, "List": true,
	"Keyword": true, "IO": true, "File": true, "Path": true,
	"GenServer": true, "Supervisor": true, "Agent": true, "Task": true,
	"Registry": true, "DynamicSupervisor": true,
	"Kernel": true, "Module": true, "Process": true, "Node": true,
	"System": true, "Code": true, "Macro": true, "Protocol": true,
	"Stream": true, "Range": true, "Regex": true, "URI": true,
	"Base": true, "Bitwise": true, "DateTime": true, "NaiveDateTime": true,
	"Date": true, "Time": true, "Calendar": true,
	"Tuple": true, "MapSet": true, "Access": true, "Atom": true,
	"Integer": true, "Float": true, "Function": true, "Port": true,
	"StringIO": true, "OptionParser": true, "EEx": true,
	"Logger": true, "Application": true, "Config": true,
	"Mix": true, "ExUnit": true, "IEx": true,
	// Erlang stdlib modules commonly used from Elixir (via :module syntax, but
	// sometimes imported).
	"GenEvent": true, "Collectable": true, "Enumerable": true,
	"Inspect": true, "Exception": true, "Behaviour": true,
}

// elixirStdPrefixes are top-level module prefixes that belong to Elixir/Erlang stdlib.
var elixirStdPrefixes = []string{
	"Kernel.", "String.", "Enum.", "Map.", "List.", "IO.", "File.", "Path.",
	"GenServer.", "Supervisor.", "Task.", "Agent.", "Registry.",
	"Logger.", "Application.", "System.", "Code.", "Macro.", "Module.",
	"Process.", "Node.", "Mix.", "ExUnit.", "IEx.", "EEx.",
	"Calendar.", "DateTime.", "NaiveDateTime.", "Date.", "Time.",
	"Stream.", "Range.", "Regex.", "URI.", "Base.", "Protocol.",
	"DynamicSupervisor.", "Port.", "Inspect.", "Exception.",
}

// checkElixirImports validates Elixir imports/aliases against stdlib and mix.exs deps.
func (h *HallucinationAnalyzer) checkElixirImports(repoPath, filePath string, pf *parser.ParsedFile) []Issue {
	issues := make([]Issue, 0)

	deps, err := parseMixExs(repoPath)
	if err != nil {
		// Can't read mix.exs — fail open.
		return issues
	}

	for _, imp := range pf.Imports {
		moduleName := imp.Path

		// Get the top-level module.
		topModule := moduleName
		if idx := strings.Index(moduleName, "."); idx != -1 {
			topModule = moduleName[:idx]
		}

		// Check Elixir stdlib modules.
		if elixirStdModules[topModule] {
			continue
		}

		// Check stdlib prefixes.
		isStd := false
		for _, prefix := range elixirStdPrefixes {
			if strings.HasPrefix(moduleName, prefix) {
				isStd = true
				break
			}
		}
		if isStd {
			continue
		}

		// Check mix.exs dependencies.
		if deps[topModule] || deps[strings.ToLower(topModule)] || deps[moduleName] {
			continue
		}

		// Convert module name to snake_case for dep matching (e.g., Phoenix -> phoenix).
		snakeCase := toSnakeCase(topModule)
		if deps[snakeCase] {
			continue
		}

		issues = append(issues, Issue{
			ID:         "hallucination/phantom-import",
			Severity:   SeverityError,
			Category:   "hallucination",
			File:       filePath,
			Line:       imp.Line,
			Message:    "Import references unknown module: " + moduleName,
			Suggestion: "Verify the module exists and add the dependency to mix.exs",
		})
	}

	return issues
}

// toSnakeCase converts a CamelCase string to snake_case.
func toSnakeCase(s string) string {
	var result strings.Builder
	for i, r := range s {
		if r >= 'A' && r <= 'Z' {
			if i > 0 {
				result.WriteByte('_')
			}
			result.WriteByte(byte(r - 'A' + 'a'))
		} else {
			result.WriteRune(r)
		}
	}
	return result.String()
}

// parseMixExs reads a mix.exs file and extracts dependency names from the deps list.
func parseMixExs(repoPath string) (map[string]bool, error) {
	mixPath := findFileUp(repoPath, "mix.exs")
	if mixPath == "" {
		return nil, os.ErrNotExist
	}

	data, err := os.ReadFile(mixPath)
	if err != nil {
		return nil, err
	}

	deps := make(map[string]bool)

	// Match patterns like: {:phoenix, "~> 1.7"} or {:ecto_sql, ">= 0.0.0"}
	depRe := regexp.MustCompile(`\{:(\w+)\s*,`)
	matches := depRe.FindAllStringSubmatch(string(data), -1)
	for _, m := range matches {
		if len(m) >= 2 {
			depName := m[1]
			deps[depName] = true
			// Also add CamelCase form (elixir convention: dep name -> module name).
			deps[toCamelCase(depName)] = true
		}
	}

	return deps, nil
}

// toCamelCase converts a snake_case string to CamelCase.
func toCamelCase(s string) string {
	parts := strings.Split(s, "_")
	var result strings.Builder
	for _, part := range parts {
		if len(part) > 0 {
			result.WriteByte(byte(part[0]-'a') + 'A')
			result.WriteString(part[1:])
		}
	}
	return result.String()
}

// --- Lua phantom import detection ---

// luaStdLibs is a set of Lua standard library modules.
var luaStdLibs = map[string]bool{
	"table": true, "string": true, "math": true, "io": true,
	"os": true, "debug": true, "coroutine": true, "package": true,
	"utf8": true, "bit32": true,
}

// checkLuaImports validates Lua require() calls against stdlib and .rockspec files.
func (h *HallucinationAnalyzer) checkLuaImports(repoPath, filePath string, pf *parser.ParsedFile) []Issue {
	issues := make([]Issue, 0)

	deps, err := parseLuaRockspec(repoPath)
	if err != nil {
		// Can't find .rockspec — fail open.
		return issues
	}

	for _, imp := range pf.Imports {
		moduleName := imp.Path

		// Get the top-level module name (before dots).
		topModule := moduleName
		if idx := strings.Index(moduleName, "."); idx != -1 {
			topModule = moduleName[:idx]
		}

		// Check Lua standard library.
		if luaStdLibs[topModule] {
			continue
		}

		// Relative requires — skip phantom check.
		if strings.HasPrefix(moduleName, ".") {
			continue
		}

		// Check rockspec dependencies.
		if deps[topModule] || deps[moduleName] {
			continue
		}

		// Normalize: replace dots with underscores and hyphens for dep matching.
		normalized := strings.ReplaceAll(topModule, ".", "-")
		if deps[normalized] {
			continue
		}
		normalized = strings.ReplaceAll(topModule, ".", "_")
		if deps[normalized] {
			continue
		}

		issues = append(issues, Issue{
			ID:         "hallucination/phantom-import",
			Severity:   SeverityError,
			Category:   "hallucination",
			File:       filePath,
			Line:       imp.Line,
			Message:    "Require references unknown module: " + moduleName,
			Suggestion: "Verify the module exists and add it to your .rockspec dependencies",
		})
	}

	return issues
}

// parseLuaRockspec finds .rockspec files in the repo and extracts dependency names.
func parseLuaRockspec(repoPath string) (map[string]bool, error) {
	deps := make(map[string]bool)
	found := false

	entries, err := os.ReadDir(repoPath)
	if err != nil {
		return nil, err
	}

	for _, entry := range entries {
		if entry.IsDir() {
			continue
		}
		if !strings.HasSuffix(entry.Name(), ".rockspec") {
			continue
		}

		data, err := os.ReadFile(filepath.Join(repoPath, entry.Name()))
		if err != nil {
			continue
		}
		found = true

		// Look for dependencies block and extract package names.
		// Format: dependencies = { "lua >= 5.1", "luasocket >= 3.0" }
		inDeps := false
		for _, line := range strings.Split(string(data), "\n") {
			trimmed := strings.TrimSpace(line)

			if strings.HasPrefix(trimmed, "dependencies") && strings.Contains(trimmed, "=") {
				inDeps = true
			}

			if inDeps {
				// Extract quoted dependency names.
				depRe := regexp.MustCompile(`"(\w[\w.-]*)`)
				matches := depRe.FindAllStringSubmatch(trimmed, -1)
				for _, m := range matches {
					if len(m) >= 2 {
						name := m[1]
						// Skip "lua" itself as it's the runtime.
						if name == "lua" {
							continue
						}
						deps[name] = true
					}
				}

				// End of dependencies block.
				if strings.Contains(trimmed, "}") && inDeps && strings.Contains(trimmed, "dependencies") || (inDeps && strings.TrimSpace(line) == "}") {
					inDeps = false
				}
			}
		}
	}

	if !found {
		return nil, os.ErrNotExist
	}

	return deps, nil
}

// --- Stub function patterns for new languages ---

// C/C++ function declaration.
var cFuncPattern = regexp.MustCompile(`^(?:static\s+)?(?:inline\s+)?(?:extern\s+)?(?:const\s+)?(?:\w[\w*\s]*?\s+\*?)(\w+)\s*\(`)

// Objective-C method declaration.
var objcMethodPattern = regexp.MustCompile(`^[-+]\s*\(`)

// Dart function/method declaration.
var dartFuncPattern = regexp.MustCompile(`^\s*(?:static\s+)?(?:Future\s*<[^>]*>\s+|void\s+|int\s+|double\s+|String\s+|bool\s+|List\s*<[^>]*>\s+|Map\s*<[^>]*>\s+|\w+\s+)(\w+)\s*\(`)

// Scala function declaration.
var scalaFuncPattern = regexp.MustCompile(`^\s*(?:(?:private|protected|override)\s+)*def\s+\w+`)

// R function declaration.
var rFuncPattern = regexp.MustCompile(`^\s*\w+\s*<-\s*function\s*\(`)

// Elixir function declaration.
var elixirFuncPattern = regexp.MustCompile(`^\s*(?:def|defp)\s+\w+`)

// Lua function declaration.
var luaFuncPattern = regexp.MustCompile(`^(?:local\s+)?function\s+\w+`)

// --- Stub function patterns for shell/SQL ---

// Bash function declaration.
var bashFuncPattern = regexp.MustCompile(`^(?:\w+\s*\(\)\s*\{|function\s+\w+)`)

// parseLuaRockspecScanner is a helper that parses using a scanner for large files.
// This exists for consistency with other parsers that use bufio.Scanner.
func parseLuaRockspecScanner(path string) (map[string]bool, error) {
	f, err := os.Open(path)
	if err != nil {
		return nil, err
	}
	defer f.Close()

	deps := make(map[string]bool)
	scanner := bufio.NewScanner(f)
	inDeps := false
	depRe := regexp.MustCompile(`"(\w[\w.-]*)`)

	for scanner.Scan() {
		line := scanner.Text()
		trimmed := strings.TrimSpace(line)

		if strings.HasPrefix(trimmed, "dependencies") && strings.Contains(trimmed, "=") {
			inDeps = true
		}

		if inDeps {
			matches := depRe.FindAllStringSubmatch(trimmed, -1)
			for _, m := range matches {
				if len(m) >= 2 && m[1] != "lua" {
					deps[m[1]] = true
				}
			}

			if strings.Contains(trimmed, "}") {
				inDeps = false
			}
		}
	}

	return deps, nil
}
