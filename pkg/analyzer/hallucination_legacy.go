package analyzer

import (
	"github.com/joeabbey/fault/pkg/parser"
)

// --- Visual Basic: no standard package manager to verify ---

func (h *HallucinationAnalyzer) checkVisualBasicImports(repoPath, filePath string, pf *parser.ParsedFile) []Issue {
	return make([]Issue, 0)
}

// --- COBOL: COPY statements reference copybooks, no package manager ---

func (h *HallucinationAnalyzer) checkCobolImports(repoPath, filePath string, pf *parser.ParsedFile) []Issue {
	return make([]Issue, 0)
}

// --- Ada: with statements reference packages, no standard manifest to check ---

func (h *HallucinationAnalyzer) checkAdaImports(repoPath, filePath string, pf *parser.ParsedFile) []Issue {
	return make([]Issue, 0)
}

// --- Pascal: uses statements reference units, no standard manifest to check ---

func (h *HallucinationAnalyzer) checkPascalImports(repoPath, filePath string, pf *parser.ParsedFile) []Issue {
	return make([]Issue, 0)
}
