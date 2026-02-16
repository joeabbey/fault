package analyzer

import (
	"os"
	"regexp"
	"strings"

	"github.com/joeabbey/fault/pkg/parser"
)

// --- Julia stdlib ---

var juliaStdModules = map[string]bool{
	"Base": true, "Core": true, "Main": true, "LinearAlgebra": true,
	"Statistics": true, "Random": true, "Dates": true, "Printf": true,
	"REPL": true, "Test": true, "Pkg": true, "Sockets": true,
	"Distributed": true, "SharedArrays": true, "SparseArrays": true,
	"DelimitedFiles": true, "Markdown": true, "UUIDs": true,
	"InteractiveUtils": true, "Logging": true, "Serialization": true,
	"Unicode": true, "TOML": true, "Libdl": true, "Profile": true,
}

func (h *HallucinationAnalyzer) checkJuliaImports(repoPath, filePath string, pf *parser.ParsedFile) []Issue {
	issues := make([]Issue, 0)
	deps, err := parseProjectToml(repoPath)
	if err != nil {
		return issues
	}
	for _, imp := range pf.Imports {
		modName := imp.Path
		if strings.HasSuffix(modName, ".jl") {
			continue
		} // include files
		if juliaStdModules[modName] {
			continue
		}
		topMod := modName
		if idx := strings.Index(modName, "."); idx != -1 {
			topMod = modName[:idx]
		}
		if juliaStdModules[topMod] {
			continue
		}
		if deps[modName] || deps[topMod] {
			continue
		}
		issues = append(issues, Issue{
			ID:         "hallucination/phantom-import",
			Severity:   SeverityError,
			Category:   "hallucination",
			File:       filePath,
			Line:       imp.Line,
			Message:    "Import references unknown module: " + modName,
			Suggestion: "Verify the module exists and add it to Project.toml",
		})
	}
	return issues
}

func parseProjectToml(repoPath string) (map[string]bool, error) {
	path := findFileUp(repoPath, "Project.toml")
	if path == "" {
		return nil, os.ErrNotExist
	}
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	deps := make(map[string]bool)
	depRe := regexp.MustCompile(`(?m)^(\w+)\s*=\s*"`)
	inDeps := false
	for _, line := range strings.Split(string(data), "\n") {
		trimmed := strings.TrimSpace(line)
		if trimmed == "[deps]" {
			inDeps = true
			continue
		}
		if strings.HasPrefix(trimmed, "[") {
			inDeps = false
			continue
		}
		if inDeps {
			if m := depRe.FindStringSubmatch(line); m != nil {
				deps[m[1]] = true
			}
		}
	}
	return deps, nil
}

// --- Solidity stdlib ---

var solidityStdImports = map[string]bool{
	"@openzeppelin": true, "hardhat": true, "forge-std": true,
}

func (h *HallucinationAnalyzer) checkSolidityImports(repoPath, filePath string, pf *parser.ParsedFile) []Issue {
	issues := make([]Issue, 0)
	for _, imp := range pf.Imports {
		path := imp.Path
		if strings.HasPrefix(path, "./") || strings.HasPrefix(path, "../") {
			continue
		}
		isStd := false
		for prefix := range solidityStdImports {
			if strings.HasPrefix(path, prefix) {
				isStd = true
				break
			}
		}
		if isStd {
			continue
		}
		issues = append(issues, Issue{
			ID:         "hallucination/phantom-import",
			Severity:   SeverityWarning,
			Category:   "hallucination",
			File:       filePath,
			Line:       imp.Line,
			Message:    "Import references external path: " + path,
			Suggestion: "Verify the import path and install the dependency",
		})
	}
	return issues
}

// --- Terraform: no traditional imports to check ---

func (h *HallucinationAnalyzer) checkTerraformImports(repoPath, filePath string, pf *parser.ParsedFile) []Issue {
	return make([]Issue, 0)
}

// --- Protobuf: check import file existence ---

func (h *HallucinationAnalyzer) checkProtobufImports(repoPath, filePath string, pf *parser.ParsedFile) []Issue {
	issues := make([]Issue, 0)
	for _, imp := range pf.Imports {
		path := imp.Path
		if strings.HasPrefix(path, "google/protobuf/") {
			continue
		}
		if strings.HasPrefix(path, "google/api/") {
			continue
		}
		candidate := repoPath + "/" + path
		if _, err := os.Stat(candidate); err == nil {
			continue
		}
		issues = append(issues, Issue{
			ID:         "hallucination/phantom-import",
			Severity:   SeverityWarning,
			Category:   "hallucination",
			File:       filePath,
			Line:       imp.Line,
			Message:    "Import references file not found in repo: " + path,
			Suggestion: "Verify the proto file exists in the project",
		})
	}
	return issues
}

// --- Fortran: no package manager, skip ---

func (h *HallucinationAnalyzer) checkFortranImports(repoPath, filePath string, pf *parser.ParsedFile) []Issue {
	return make([]Issue, 0)
}
