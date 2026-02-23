package analyzer

import "fmt"

// CompliancePack defines a set of CWE IDs that map to a compliance standard.
type CompliancePack struct {
	ID          string   // e.g., "owasp-top-10-2021"
	Name        string   // e.g., "OWASP Top 10 (2021)"
	Description string
	CWEIDs      []string // e.g., ["CWE-79", "CWE-89", ...]
}

// CompliancePacks maps pack IDs to their definitions.
var CompliancePacks = map[string]CompliancePack{
	"owasp-top-10-2021": {
		ID:          "owasp-top-10-2021",
		Name:        "OWASP Top 10 (2021)",
		Description: "The OWASP Top 10 is a standard awareness document for web application security, representing the most critical security risks.",
		CWEIDs: []string{
			"CWE-79",  // Cross-site Scripting (XSS)
			"CWE-89",  // SQL Injection
			"CWE-22",  // Path Traversal
			"CWE-78",  // OS Command Injection
			"CWE-94",  // Code Injection
			"CWE-611", // XML External Entity Reference
			"CWE-502", // Deserialization of Untrusted Data
			"CWE-918", // Server-Side Request Forgery (SSRF)
			"CWE-601", // URL Redirection to Untrusted Site
			"CWE-798", // Use of Hard-coded Credentials
			"CWE-327", // Use of a Broken or Risky Cryptographic Algorithm
			"CWE-295", // Improper Certificate Validation
			"CWE-330", // Use of Insufficiently Random Values
			"CWE-862", // Missing Authorization
			"CWE-863", // Incorrect Authorization
		},
	},
	"cwe-top-25-2023": {
		ID:          "cwe-top-25-2023",
		Name:        "CWE Top 25 (2023)",
		Description: "The CWE Top 25 Most Dangerous Software Weaknesses is a list of the most common and impactful software vulnerabilities.",
		CWEIDs: []string{
			"CWE-79",  // Cross-site Scripting (XSS)
			"CWE-89",  // SQL Injection
			"CWE-22",  // Path Traversal
			"CWE-78",  // OS Command Injection
			"CWE-787", // Out-of-bounds Write
			"CWE-416", // Use After Free
			"CWE-476", // NULL Pointer Dereference
			"CWE-20",  // Improper Input Validation
			"CWE-125", // Out-of-bounds Read
			"CWE-190", // Integer Overflow or Wraparound
			"CWE-502", // Deserialization of Untrusted Data
			"CWE-798", // Use of Hard-coded Credentials
			"CWE-862", // Missing Authorization
			"CWE-863", // Incorrect Authorization
			"CWE-306", // Missing Authentication for Critical Function
			"CWE-434", // Unrestricted Upload of File with Dangerous Type
			"CWE-611", // XML External Entity Reference
			"CWE-918", // Server-Side Request Forgery (SSRF)
			"CWE-77",  // Command Injection
			"CWE-362", // Race Condition
			"CWE-269", // Improper Privilege Management
			"CWE-94",  // Code Injection
			"CWE-119", // Improper Restriction of Operations within the Bounds of a Memory Buffer
			"CWE-276", // Incorrect Default Permissions
			"CWE-732", // Incorrect Permission Assignment for Critical Resource
		},
	},
}

// IssueIDToCWE maps a Fault issue ID to its CWE ID, if known.
// This duplicates the mapping from reporter/sarif.go for use by the analyzer package.
var IssueIDToCWE = map[string]string{
	// Security analyzer issues
	"security-sql-injection":         "CWE-89",
	"security-xss":                   "CWE-79",
	"security-path-traversal":        "CWE-22",
	"security-hardcoded-credentials": "CWE-798",
	"security-weak-crypto":           "CWE-327",
	"security-command-injection":     "CWE-78",
	"security-code-injection":        "CWE-94",
	"security-xxe":                   "CWE-611",
	"security-deserialization":       "CWE-502",
	"security-ssrf":                  "CWE-918",
	"security-open-redirect":         "CWE-601",
	"security-weak-random":           "CWE-330",
	"security-insecure-tls":          "CWE-295",
	"security-sensitive-log":         "CWE-532",
	"security-eval":                  "CWE-95",
	"security-prototype-pollution":   "CWE-1321",
	"security-regex-dos":             "CWE-1333",
	"security-unsafe-reflection":     "CWE-470",
	// Cross-cutting analyzer issues
	"concurrency-race":              "CWE-362",
	"concurrency-deadlock":          "CWE-833",
	"resource-leak":                 "CWE-404",
	"resource-unclosed":             "CWE-404",
	"error-handling-unchecked":      "CWE-252",
	"error-handling-swallowed":      "CWE-390",
	"import-nonexistent":            "CWE-829",
	"complexity-high":               "CWE-1121",
	"hallucination-nonexistent-api": "CWE-476",
}

// ComplianceResult summarizes compliance check findings.
type ComplianceResult struct {
	PackID       string                `json:"pack_id"`
	PackName     string                `json:"pack_name"`
	TotalCWEs    int                   `json:"total_cwes"`
	ViolatedCWEs int                   `json:"violated_cwes"`
	Violations   []ComplianceViolation `json:"violations"`
	Compliant    bool                  `json:"compliant"`
}

// ComplianceViolation records a specific CWE violated by issues.
type ComplianceViolation struct {
	CWEID   string `json:"cwe_id"`
	IssueID string `json:"issue_id"`
	Count   int    `json:"count"`
}

// CheckCompliance runs compliance checking against analysis results.
func CheckCompliance(packID string, issues []Issue) (*ComplianceResult, error) {
	pack, ok := CompliancePacks[packID]
	if !ok {
		return nil, fmt.Errorf("unknown compliance pack: %q", packID)
	}

	// Build set of CWE IDs in this pack
	packCWEs := make(map[string]bool, len(pack.CWEIDs))
	for _, cwe := range pack.CWEIDs {
		packCWEs[cwe] = true
	}

	// Count violations per (CWE, issue ID) pair
	type violationKey struct {
		cweID   string
		issueID string
	}
	counts := make(map[violationKey]int)

	for _, issue := range issues {
		cwe, ok := IssueIDToCWE[issue.ID]
		if !ok {
			continue
		}
		if !packCWEs[cwe] {
			continue
		}
		counts[violationKey{cweID: cwe, issueID: issue.ID}]++
	}

	violations := make([]ComplianceViolation, 0, len(counts))
	violatedCWESet := make(map[string]bool)
	for key, count := range counts {
		violations = append(violations, ComplianceViolation{
			CWEID:   key.cweID,
			IssueID: key.issueID,
			Count:   count,
		})
		violatedCWESet[key.cweID] = true
	}

	return &ComplianceResult{
		PackID:       pack.ID,
		PackName:     pack.Name,
		TotalCWEs:    len(pack.CWEIDs),
		ViolatedCWEs: len(violatedCWESet),
		Violations:   violations,
		Compliant:    len(violations) == 0,
	}, nil
}
