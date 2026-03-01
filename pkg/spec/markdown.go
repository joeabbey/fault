package spec

import (
	"fmt"
	"regexp"
	"strings"
)

// headingPattern matches Markdown ATX headings: # Title, ## Section, ### Subsection
var headingPattern = regexp.MustCompile(`^(#{1,6})\s+(.+)$`)

// numberedHeadingPattern matches section-numbered headings like "1. Lexical Structure" or "1.1 Keywords"
var numberedHeadingPattern = regexp.MustCompile(`^(\d+(?:\.\d+)*)\.?\s+(.+)$`)

// markdownSection represents a parsed Markdown heading and its body content.
type markdownSection struct {
	level int    // heading level: 1=H1, 2=H2, etc.
	title string // heading text (without the # prefix)
	body  string // content below this heading, before the next heading
}

// ParseMarkdownSpec parses a Markdown document into a Spec.
//
// The first H1 heading becomes the spec title.
// H2 headings become top-level requirements (SEC-1, SEC-2, ...).
// H3 headings become sub-requirements (SEC-1-1, SEC-1-2, ...).
// Each section's body text becomes the requirement description.
func ParseMarkdownSpec(data []byte) (*Spec, error) {
	sections := parseMarkdownSections(string(data))
	if len(sections) == 0 {
		return nil, fmt.Errorf("no headings found in Markdown spec")
	}

	s := &Spec{
		Version:      1,
		Requirements: make([]Requirement, 0),
	}

	// Extract title from first H1
	for _, sec := range sections {
		if sec.level == 1 {
			s.Title = sec.title
			break
		}
	}
	if s.Title == "" {
		// Fall back to first heading of any level
		s.Title = sections[0].title
	}

	// Convert H2/H3 sections into requirements.
	// Track H2 index for sub-requirement numbering.
	h2Index := 0
	h3Counts := make(map[int]int) // h2Index → count of H3s under it

	for _, sec := range sections {
		switch sec.level {
		case 2:
			h2Index++
			id := fmt.Sprintf("SEC-%d", h2Index)
			desc := truncateDescription(sec.body)
			if desc == "" {
				desc = sec.title
			}
			s.Requirements = append(s.Requirements, Requirement{
				ID:          id,
				Description: fmt.Sprintf("%s: %s", sec.title, desc),
			})

		case 3:
			if h2Index == 0 {
				// H3 before any H2 — treat as top-level
				h2Index++
				h3Counts[h2Index] = 0
			}
			h3Counts[h2Index]++
			subIdx := h3Counts[h2Index]
			id := fmt.Sprintf("SEC-%d-%d", h2Index, subIdx)
			desc := truncateDescription(sec.body)
			if desc == "" {
				desc = sec.title
			}
			s.Requirements = append(s.Requirements, Requirement{
				ID:          id,
				Description: fmt.Sprintf("%s: %s", sec.title, desc),
			})
		}
	}

	if len(s.Requirements) == 0 {
		return nil, fmt.Errorf("no H2 or H3 sections found in Markdown spec")
	}

	return s, nil
}

// parseMarkdownSections splits a Markdown document into sections by headings.
func parseMarkdownSections(content string) []markdownSection {
	lines := strings.Split(content, "\n")
	sections := make([]markdownSection, 0)

	var currentBody strings.Builder
	inFencedBlock := false

	for _, line := range lines {
		trimmed := strings.TrimSpace(line)

		// Track fenced code blocks so we don't treat # inside them as headings
		if strings.HasPrefix(trimmed, "```") {
			inFencedBlock = !inFencedBlock
			if len(sections) > 0 {
				currentBody.WriteString(line)
				currentBody.WriteByte('\n')
			}
			continue
		}

		if inFencedBlock {
			if len(sections) > 0 {
				currentBody.WriteString(line)
				currentBody.WriteByte('\n')
			}
			continue
		}

		if m := headingPattern.FindStringSubmatch(trimmed); m != nil {
			// Close previous section
			if len(sections) > 0 {
				sections[len(sections)-1].body = strings.TrimSpace(currentBody.String())
				currentBody.Reset()
			}

			level := len(m[1])
			title := strings.TrimSpace(m[2])

			// Strip leading section numbers (e.g., "1.2 Keywords" → "Keywords")
			if nm := numberedHeadingPattern.FindStringSubmatch(title); nm != nil {
				title = strings.TrimSpace(nm[2])
			}

			sections = append(sections, markdownSection{
				level: level,
				title: title,
			})
			continue
		}

		// Accumulate body text for the current section
		if len(sections) > 0 {
			currentBody.WriteString(line)
			currentBody.WriteByte('\n')
		}
	}

	// Close the last section
	if len(sections) > 0 {
		sections[len(sections)-1].body = strings.TrimSpace(currentBody.String())
	}

	return sections
}

// truncateDescription returns the first meaningful paragraph of a section body,
// truncated to a reasonable length for a requirement description.
func truncateDescription(body string) string {
	if body == "" {
		return ""
	}

	// Take the first non-empty paragraph
	paragraphs := strings.Split(body, "\n\n")
	for _, p := range paragraphs {
		p = strings.TrimSpace(p)
		if p == "" {
			continue
		}
		// Skip code blocks
		if strings.HasPrefix(p, "```") {
			continue
		}
		// Collapse whitespace
		p = strings.Join(strings.Fields(p), " ")

		// Truncate at 300 chars
		if len(p) > 300 {
			p = p[:297] + "..."
		}
		return p
	}

	return ""
}
