package spec

import (
	"bufio"
	"os"
	"path/filepath"
	"regexp"
	"strings"
)

// AnchorPattern matches spec anchors in source code comments: spec:REQ-001
var AnchorPattern = regexp.MustCompile(`spec:([A-Za-z][A-Za-z0-9_-]*-\d+)`)

// Anchor represents a spec anchor found in source code.
type Anchor struct {
	ReqID string // the requirement ID referenced (e.g. "REQ-001")
	File  string // file path relative to repo root
	Line  int    // 1-based line number
}

// ExtractAnchorsFromFile scans a single file for spec anchors.
func ExtractAnchorsFromFile(path string) ([]Anchor, error) {
	f, err := os.Open(path)
	if err != nil {
		return nil, err
	}
	defer f.Close()

	anchors := make([]Anchor, 0)
	scanner := bufio.NewScanner(f)
	lineNum := 0

	for scanner.Scan() {
		lineNum++
		line := scanner.Text()
		matches := AnchorPattern.FindAllStringSubmatch(line, -1)
		for _, match := range matches {
			anchors = append(anchors, Anchor{
				ReqID: match[1],
				File:  path,
				Line:  lineNum,
			})
		}
	}

	if err := scanner.Err(); err != nil {
		return nil, err
	}

	return anchors, nil
}

// ExtractAnchorsFromDir walks a directory tree and extracts all spec anchors.
// It skips binary files, vendor dirs, node_modules, and .git.
func ExtractAnchorsFromDir(root string) ([]Anchor, error) {
	allAnchors := make([]Anchor, 0)

	err := filepath.Walk(root, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			return nil // skip inaccessible paths
		}

		if info.IsDir() {
			name := info.Name()
			if name == ".git" || name == "node_modules" || name == "vendor" || name == ".next" || name == "dist" {
				return filepath.SkipDir
			}
			return nil
		}

		// Skip binary and non-source files
		if !isSourceFile(path) {
			return nil
		}

		relPath, relErr := filepath.Rel(root, path)
		if relErr != nil {
			relPath = path
		}

		anchors, scanErr := ExtractAnchorsFromFile(path)
		if scanErr != nil {
			return nil // skip unreadable files
		}

		// Rewrite paths to be relative
		for i := range anchors {
			anchors[i].File = relPath
		}
		allAnchors = append(allAnchors, anchors...)

		return nil
	})

	return allAnchors, err
}

// GroupAnchorsByReqID groups anchors by their requirement ID.
func GroupAnchorsByReqID(anchors []Anchor) map[string][]Anchor {
	grouped := make(map[string][]Anchor)
	for _, a := range anchors {
		grouped[a.ReqID] = append(grouped[a.ReqID], a)
	}
	return grouped
}

// isSourceFile returns true if the file extension suggests a text source file.
func isSourceFile(path string) bool {
	ext := strings.ToLower(filepath.Ext(path))
	switch ext {
	case ".go", ".ts", ".tsx", ".js", ".jsx", ".mjs", ".cjs",
		".py", ".rs", ".java", ".rb", ".kt", ".kts",
		".cs", ".php", ".swift", ".c", ".h", ".cpp", ".cc", ".cxx", ".hpp",
		".dart", ".scala", ".groovy", ".zig", ".cr", ".d",
		".ex", ".exs", ".erl", ".hrl",
		".hs", ".clj", ".cljs", ".cljc",
		".fs", ".fsx", ".ml", ".mli",
		".pl", ".pm", ".ps1", ".psm1",
		".jl", ".f90", ".f95", ".f03",
		".sol", ".tf", ".proto",
		".vb", ".cob", ".cbl", ".adb", ".ads", ".pas",
		".lua", ".nim", ".v",
		".r", ".R",
		".sql", ".sh", ".bash", ".zsh",
		".yaml", ".yml", ".toml", ".json", ".xml",
		".md", ".txt", ".rst",
		".svelte", ".vue", ".html", ".css", ".scss", ".less":
		return true
	}
	return false
}
