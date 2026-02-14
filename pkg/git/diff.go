package git

import (
	"fmt"
	"strconv"
	"strings"
)

// Diff represents a parsed git diff.
type Diff struct {
	Files []FileDiff
	Mode  string // "staged", "unstaged", "branch"
}

// FileDiff represents changes to a single file.
type FileDiff struct {
	Path     string
	OldPath  string // for renames
	Status   string // added, modified, deleted, renamed
	Hunks    []Hunk
	IsBinary bool
}

// Hunk represents a contiguous block of changes.
type Hunk struct {
	OldStart int
	OldCount int
	NewStart int
	NewCount int
	Lines    []Line
}

// Line represents a single line in a diff hunk.
type Line struct {
	Type    string // added, removed, context
	Content string
	OldNum  int
	NewNum  int
}

// ParseDiff parses unified diff output into structured data.
func ParseDiff(raw string, mode string) (*Diff, error) {
	diff := &Diff{
		Files: make([]FileDiff, 0),
		Mode:  mode,
	}

	if strings.TrimSpace(raw) == "" {
		return diff, nil
	}

	// Split into per-file sections by "diff --git" header
	sections := splitDiffSections(raw)

	for _, section := range sections {
		fd, err := parseFileDiff(section)
		if err != nil {
			return nil, fmt.Errorf("parsing file diff: %w", err)
		}
		if fd != nil {
			diff.Files = append(diff.Files, *fd)
		}
	}

	return diff, nil
}

// splitDiffSections splits raw diff output into per-file sections.
func splitDiffSections(raw string) []string {
	sections := make([]string, 0)
	// Trim trailing whitespace to avoid phantom empty lines
	raw = strings.TrimRight(raw, "\n\r ")
	lines := strings.Split(raw, "\n")

	var current []string
	for _, line := range lines {
		if strings.HasPrefix(line, "diff --git ") {
			if len(current) > 0 {
				sections = append(sections, strings.Join(current, "\n"))
			}
			current = []string{line}
		} else if len(current) > 0 {
			current = append(current, line)
		}
	}
	if len(current) > 0 {
		sections = append(sections, strings.Join(current, "\n"))
	}

	return sections
}

// parseFileDiff parses a single file's diff section.
func parseFileDiff(section string) (*FileDiff, error) {
	lines := strings.Split(section, "\n")
	if len(lines) == 0 {
		return nil, nil
	}

	fd := &FileDiff{
		Hunks: make([]Hunk, 0),
	}

	// Parse header: "diff --git a/path b/path"
	header := lines[0]
	if strings.HasPrefix(header, "diff --git ") {
		parts := strings.SplitN(header[len("diff --git "):], " ", 2)
		if len(parts) == 2 {
			fd.Path = strings.TrimPrefix(parts[1], "b/")
			oldPath := strings.TrimPrefix(parts[0], "a/")
			if oldPath != fd.Path {
				fd.OldPath = oldPath
			}
		}
	}

	// Scan header lines for status indicators
	fd.Status = "modified" // default
	i := 1
	for i < len(lines) {
		line := lines[i]
		if strings.HasPrefix(line, "@@") {
			break
		}
		if strings.HasPrefix(line, "Binary files") {
			fd.IsBinary = true
			return fd, nil
		}
		if strings.HasPrefix(line, "new file mode") {
			fd.Status = "added"
		}
		if strings.HasPrefix(line, "deleted file mode") {
			fd.Status = "deleted"
		}
		if strings.HasPrefix(line, "rename from ") {
			fd.Status = "renamed"
			fd.OldPath = strings.TrimPrefix(line, "rename from ")
		}
		if strings.HasPrefix(line, "rename to ") {
			fd.Path = strings.TrimPrefix(line, "rename to ")
		}
		if strings.HasPrefix(line, "similarity index") {
			fd.Status = "renamed"
		}
		i++
	}

	// Parse hunks
	for i < len(lines) {
		if strings.HasPrefix(lines[i], "@@") {
			hunk, advance, err := parseHunk(lines[i:])
			if err != nil {
				return nil, err
			}
			fd.Hunks = append(fd.Hunks, *hunk)
			i += advance
		} else {
			i++
		}
	}

	return fd, nil
}

// parseHunk parses a single hunk starting with an @@ header.
func parseHunk(lines []string) (*Hunk, int, error) {
	if len(lines) == 0 || !strings.HasPrefix(lines[0], "@@") {
		return nil, 0, fmt.Errorf("expected @@ header, got: %q", lines[0])
	}

	hunk := &Hunk{
		Lines: make([]Line, 0),
	}

	// Parse @@ -old,count +new,count @@
	header := lines[0]
	err := parseHunkHeader(header, hunk)
	if err != nil {
		return nil, 0, err
	}

	oldNum := hunk.OldStart
	newNum := hunk.NewStart
	i := 1

	for i < len(lines) {
		line := lines[i]

		// Stop at next hunk or next file
		if strings.HasPrefix(line, "@@") || strings.HasPrefix(line, "diff --git") {
			break
		}

		if len(line) == 0 {
			// Empty line in diff = context line with empty content
			hunk.Lines = append(hunk.Lines, Line{
				Type:    "context",
				Content: "",
				OldNum:  oldNum,
				NewNum:  newNum,
			})
			oldNum++
			newNum++
		} else {
			switch line[0] {
			case '+':
				hunk.Lines = append(hunk.Lines, Line{
					Type:    "added",
					Content: line[1:],
					NewNum:  newNum,
				})
				newNum++
			case '-':
				hunk.Lines = append(hunk.Lines, Line{
					Type:    "removed",
					Content: line[1:],
					OldNum:  oldNum,
				})
				oldNum++
			case ' ':
				hunk.Lines = append(hunk.Lines, Line{
					Type:    "context",
					Content: line[1:],
					OldNum:  oldNum,
					NewNum:  newNum,
				})
				oldNum++
				newNum++
			case '\\':
				// "\ No newline at end of file" — skip
			default:
				// Treat as context
				hunk.Lines = append(hunk.Lines, Line{
					Type:    "context",
					Content: line,
					OldNum:  oldNum,
					NewNum:  newNum,
				})
				oldNum++
				newNum++
			}
		}
		i++
	}

	return hunk, i, nil
}

// parseHunkHeader parses "@@ -old,count +new,count @@" into a Hunk.
func parseHunkHeader(header string, hunk *Hunk) error {
	// Find the range part between @@ markers
	start := strings.Index(header, "@@")
	if start == -1 {
		return fmt.Errorf("invalid hunk header: %q", header)
	}
	rest := header[start+2:]
	end := strings.Index(rest, "@@")
	if end == -1 {
		return fmt.Errorf("invalid hunk header: %q", header)
	}
	rangePart := strings.TrimSpace(rest[:end])

	parts := strings.Fields(rangePart)
	if len(parts) < 2 {
		return fmt.Errorf("invalid hunk range: %q", rangePart)
	}

	// Parse old range: -start,count
	oldRange := strings.TrimPrefix(parts[0], "-")
	oldStart, oldCount, err := parseRange(oldRange)
	if err != nil {
		return fmt.Errorf("parsing old range: %w", err)
	}
	hunk.OldStart = oldStart
	hunk.OldCount = oldCount

	// Parse new range: +start,count
	newRange := strings.TrimPrefix(parts[1], "+")
	newStart, newCount, err := parseRange(newRange)
	if err != nil {
		return fmt.Errorf("parsing new range: %w", err)
	}
	hunk.NewStart = newStart
	hunk.NewCount = newCount

	return nil
}

// parseRange parses "start,count" or "start" into start and count values.
func parseRange(s string) (int, int, error) {
	if idx := strings.Index(s, ","); idx != -1 {
		start, err := strconv.Atoi(s[:idx])
		if err != nil {
			return 0, 0, err
		}
		count, err := strconv.Atoi(s[idx+1:])
		if err != nil {
			return 0, 0, err
		}
		return start, count, nil
	}

	start, err := strconv.Atoi(s)
	if err != nil {
		return 0, 0, err
	}
	return start, 1, nil
}
