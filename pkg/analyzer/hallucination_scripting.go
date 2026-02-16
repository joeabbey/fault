package analyzer

import (
	"os"
	"regexp"
	"strings"

	"github.com/joeabbey/fault/pkg/parser"
)

// --- Perl stdlib ---

var perlStdModules = map[string]bool{
	"strict": true, "warnings": true, "Carp": true, "Exporter": true,
	"File::Basename": true, "File::Path": true, "File::Spec": true, "File::Find": true,
	"File::Copy": true, "File::Temp": true, "Getopt::Long": true, "Getopt::Std": true,
	"Data::Dumper": true, "Storable": true, "POSIX": true, "Cwd": true,
	"Scalar::Util": true, "List::Util": true, "Encode": true, "IO::File": true,
	"IO::Socket": true, "Socket": true, "Time::HiRes": true, "Time::Local": true,
	"Digest::MD5": true, "Digest::SHA": true, "MIME::Base64": true,
	"JSON::PP": true, "HTTP::Tiny": true, "Test::More": true, "Test::Simple": true,
	"FindBin": true, "lib": true, "constant": true, "vars": true, "utf8": true,
	"overload": true, "parent": true, "base": true, "feature": true,
}

func (h *HallucinationAnalyzer) checkPerlImports(repoPath, filePath string, pf *parser.ParsedFile) []Issue {
	issues := make([]Issue, 0)
	deps, err := parseCpanfile(repoPath)
	if err != nil {
		return issues
	}
	for _, imp := range pf.Imports {
		modName := imp.Path
		if perlStdModules[modName] {
			continue
		}
		topMod := modName
		if idx := strings.Index(modName, "::"); idx != -1 {
			topMod = modName[:idx]
		}
		if perlStdModules[topMod] {
			continue
		}
		if deps[modName] || deps[topMod] {
			continue
		}
		issues = append(issues, Issue{ID: "hallucination/phantom-import", Severity: SeverityError, Category: "hallucination", File: filePath, Line: imp.Line, Message: "Import references unknown module: " + modName, Suggestion: "Verify the module exists and add it to your cpanfile"})
	}
	return issues
}

func parseCpanfile(repoPath string) (map[string]bool, error) {
	path := findFileUp(repoPath, "cpanfile")
	if path == "" {
		return nil, os.ErrNotExist
	}
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	deps := make(map[string]bool)
	depRe := regexp.MustCompile(`(?m)requires\s+['"]([^'"]+)['"]`)
	for _, m := range depRe.FindAllStringSubmatch(string(data), -1) {
		deps[m[1]] = true
	}
	return deps, nil
}

// --- PowerShell stdlib ---

var powershellStdModules = map[string]bool{
	"Microsoft.PowerShell.Management": true, "Microsoft.PowerShell.Utility": true,
	"Microsoft.PowerShell.Security": true, "Microsoft.PowerShell.Archive": true,
	"Microsoft.PowerShell.Host": true, "Microsoft.PowerShell.Diagnostics": true,
	"PSReadLine": true, "PackageManagement": true, "PowerShellGet": true,
	"ThreadJob": true, "Microsoft.WSMan.Management": true,
	"ActiveDirectory": true, "PSDesiredStateConfiguration": true,
}

func (h *HallucinationAnalyzer) checkPowershellImports(repoPath, filePath string, pf *parser.ParsedFile) []Issue {
	issues := make([]Issue, 0)
	for _, imp := range pf.Imports {
		modName := imp.Path
		if powershellStdModules[modName] {
			continue
		}
		if strings.HasSuffix(modName, ".ps1") || strings.HasSuffix(modName, ".psm1") {
			continue
		}
		issues = append(issues, Issue{ID: "hallucination/phantom-import", Severity: SeverityWarning, Category: "hallucination", File: filePath, Line: imp.Line, Message: "Import references module that may not be available: " + modName, Suggestion: "Verify the module is installed or available in the PSModulePath"})
	}
	return issues
}

// --- Groovy stdlib ---

var groovyStdPrefixes = []string{
	"groovy.", "java.", "javax.", "org.codehaus.groovy.",
}

func (h *HallucinationAnalyzer) checkGroovyImports(repoPath, filePath string, pf *parser.ParsedFile) []Issue {
	issues := make([]Issue, 0)
	deps, err := parseBuildGradle(repoPath)
	if err != nil {
		return issues
	}
	for _, imp := range pf.Imports {
		pkg := imp.Path
		isStd := false
		for _, p := range groovyStdPrefixes {
			if strings.HasPrefix(pkg, p) {
				isStd = true
				break
			}
		}
		if isStd {
			continue
		}
		if deps[pkg] {
			continue
		}
		topPkg := pkg
		if idx := strings.LastIndex(pkg, "."); idx != -1 {
			topPkg = pkg[:idx]
		}
		if deps[topPkg] {
			continue
		}
		issues = append(issues, Issue{ID: "hallucination/phantom-import", Severity: SeverityError, Category: "hallucination", File: filePath, Line: imp.Line, Message: "Import references unknown package: " + pkg, Suggestion: "Verify the package exists and add the dependency to build.gradle"})
	}
	return issues
}
