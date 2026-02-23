package analyzer

import (
	"fmt"
	"log"
	"path/filepath"
	"strings"

	"github.com/joeabbey/fault/pkg/spec"
)

// SpecAnalyzer validates code against a .fault-spec.yaml specification file.
// It checks for orphaned anchors, unanchored requirements, modified anchored code,
// and target mismatches.
type SpecAnalyzer struct{}

// NewSpecAnalyzer creates a new spec validation analyzer.
func NewSpecAnalyzer() *SpecAnalyzer {
	return &SpecAnalyzer{}
}

// Name returns the analyzer name.
func (a *SpecAnalyzer) Name() string {
	return "spec"
}

// Analyze runs spec validation on the given context.
func (a *SpecAnalyzer) Analyze(ctx *AnalysisContext) ([]Issue, error) {
	issues := make([]Issue, 0)

	if ctx.Config == nil || ctx.Config.LLM.SpecFile == "" {
		return issues, nil
	}

	// Resolve spec file path
	specPath := ctx.Config.LLM.SpecFile
	if !filepath.IsAbs(specPath) {
		specPath = filepath.Join(ctx.RepoPath, specPath)
	}

	// Load and parse the spec
	s, err := spec.LoadSpec(specPath)
	if err != nil {
		log.Printf("warning: could not load spec file %s: %v", specPath, err)
		return issues, nil
	}

	// Extract anchors from the entire repo
	allAnchors, err := spec.ExtractAnchorsFromDir(ctx.RepoPath)
	if err != nil {
		log.Printf("warning: could not scan repo for spec anchors: %v", err)
		return issues, nil
	}
	anchorsByReq := spec.GroupAnchorsByReqID(allAnchors)

	// Build set of known requirement IDs
	knownIDs := make(map[string]bool, len(s.Requirements))
	for _, req := range s.Requirements {
		knownIDs[req.ID] = true
	}

	// 1. Orphaned anchors: spec:FOO-001 in code but no matching requirement
	for _, anchor := range allAnchors {
		if !knownIDs[anchor.ReqID] {
			issues = append(issues, Issue{
				ID:       fmt.Sprintf("spec-orphan-%s-%s-%d", anchor.ReqID, anchor.File, anchor.Line),
				FixID:    "spec-orphaned-anchor",
				Severity: SeverityError,
				Category: "spec",
				File:     anchor.File,
				Line:     anchor.Line,
				Message:  fmt.Sprintf("Orphaned spec anchor: spec:%s has no matching requirement in %s", anchor.ReqID, ctx.Config.LLM.SpecFile),
				Suggestion: fmt.Sprintf("Add requirement %s to the spec file or remove this anchor", anchor.ReqID),
			})
		}
	}

	// 2. Unanchored requirements: requirement exists but no code anchor
	for _, req := range s.Requirements {
		if _, hasAnchor := anchorsByReq[req.ID]; !hasAnchor {
			sev := severityForPriority(req.Priority)
			issues = append(issues, Issue{
				ID:       fmt.Sprintf("spec-unanchored-%s", req.ID),
				Severity: sev,
				Category: "spec",
				Message:  fmt.Sprintf("Requirement %s (%s) has no code anchor", req.ID, req.Description),
				Suggestion: fmt.Sprintf("Add a spec:%s comment in the implementing code", req.ID),
			})
		}
	}

	// 3. Modified anchored code: diff touches lines near a spec anchor
	if ctx.Diff != nil {
		changedFiles := make(map[string]bool, len(ctx.Diff.Files))
		for _, fd := range ctx.Diff.Files {
			changedFiles[fd.Path] = true
		}

		for _, anchor := range allAnchors {
			if changedFiles[anchor.File] {
				if diffTouchesNearLine(ctx, anchor.File, anchor.Line, 5) {
					req := s.RequirementByID(anchor.ReqID)
					desc := anchor.ReqID
					if req != nil {
						desc = fmt.Sprintf("%s (%s)", req.ID, req.Description)
					}
					issues = append(issues, Issue{
						ID:       fmt.Sprintf("spec-modified-%s-%s-%d", anchor.ReqID, anchor.File, anchor.Line),
						Severity: SeverityInfo,
						Category: "spec",
						File:     anchor.File,
						Line:     anchor.Line,
						Message:  fmt.Sprintf("Code near spec anchor for %s was modified", desc),
						Suggestion: "Verify the requirement is still satisfied after this change",
					})
				}
			}
		}
	}

	// 4. Target mismatch: anchors found outside specified target globs
	for _, req := range s.Requirements {
		if len(req.Targets) == 0 {
			continue
		}

		anchors, ok := anchorsByReq[req.ID]
		if !ok {
			continue
		}

		for _, anchor := range anchors {
			if !matchesAnyTarget(anchor.File, req.Targets) {
				issues = append(issues, Issue{
					ID:       fmt.Sprintf("spec-target-%s-%s-%d", req.ID, anchor.File, anchor.Line),
					Severity: SeverityWarning,
					Category: "spec",
					File:     anchor.File,
					Line:     anchor.Line,
					Message:  fmt.Sprintf("Spec anchor for %s is outside its target paths %v", req.ID, req.Targets),
					Suggestion: fmt.Sprintf("Move this code to match targets %v or update the spec targets", req.Targets),
				})
			}
		}
	}

	return issues, nil
}

// severityForPriority maps requirement priority to issue severity.
func severityForPriority(priority string) Severity {
	switch priority {
	case "high":
		return SeverityWarning
	default:
		return SeverityInfo
	}
}

// diffTouchesNearLine checks if any diff hunk in the given file touches lines
// within `proximity` lines of the target line.
func diffTouchesNearLine(ctx *AnalysisContext, file string, line, proximity int) bool {
	if ctx.Diff == nil {
		return false
	}

	for _, fd := range ctx.Diff.Files {
		if fd.Path != file {
			continue
		}
		for _, hunk := range fd.Hunks {
			hunkStart := hunk.NewStart
			hunkEnd := hunk.NewStart + hunk.NewCount
			if line >= hunkStart-proximity && line <= hunkEnd+proximity {
				return true
			}
		}
	}
	return false
}

// matchesAnyTarget checks if a file path matches any of the target glob patterns.
func matchesAnyTarget(file string, targets []string) bool {
	for _, pattern := range targets {
		if matched, _ := filepath.Match(pattern, file); matched {
			return true
		}
		// Also try matching against just the path with forward slashes
		normalized := strings.ReplaceAll(file, "\\", "/")
		if matched, _ := filepath.Match(pattern, normalized); matched {
			return true
		}
	}
	return false
}
