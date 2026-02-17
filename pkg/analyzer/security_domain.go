package analyzer

import (
	"regexp"
	"strings"

	"github.com/joeabbey/fault/pkg/git"
)

// --- Julia security ---

type juliaSecurityRule struct {
	pattern    *regexp.Regexp
	exclusions []*regexp.Regexp
	id         string
	severity   Severity
	message    string
	suggestion string
}

var juliaSecurityRules = []juliaSecurityRule{
	{regexp.MustCompile(`\beval\s*\(`), nil, "security/julia-code-injection", SeverityError, "eval executes arbitrary Julia code", "Avoid eval with untrusted input"},
	{regexp.MustCompile(`\brun\s*\(`), nil, "security/julia-command-injection", SeverityWarning, "run() executes system commands", "Validate all inputs to run()"},
	{regexp.MustCompile(`\bccall\s*\(`), nil, "security/julia-unsafe-ccall", SeverityWarning, "ccall invokes C functions directly", "Ensure ccall arguments are validated"},
	{regexp.MustCompile(`\bunsafe_\w+`), nil, "security/julia-unsafe-operation", SeverityWarning, "Unsafe operation bypasses safety checks", "Use safe alternatives when possible"},
}

func checkJuliaSecurity(fileDiff git.FileDiff) []Issue {
	issues := make([]Issue, 0)
	for _, hunk := range fileDiff.Hunks {
		for _, line := range hunk.Lines {
			if line.Type != "added" {
				continue
			}
			trimmed := strings.TrimSpace(line.Content)
			if strings.HasPrefix(trimmed, "#") {
				continue
			}
			for _, rule := range juliaSecurityRules {
				if !rule.pattern.MatchString(line.Content) {
					continue
				}
				excluded := false
				for _, excl := range rule.exclusions {
					if excl.MatchString(line.Content) {
						excluded = true
						break
					}
				}
				if excluded {
					continue
				}
				issues = append(issues, Issue{
					ID:         rule.id,
					Severity:   rule.severity,
					Category:   "security",
					File:       fileDiff.Path,
					Line:       line.NewNum,
					Message:    rule.message,
					Suggestion: rule.suggestion,
				})
				break
			}
		}
	}
	return issues
}

// --- Solidity security ---

type soliditySecurityRule struct {
	pattern    *regexp.Regexp
	exclusions []*regexp.Regexp
	id         string
	severity   Severity
	message    string
	suggestion string
}

var soliditySecurityRules = []soliditySecurityRule{
	{regexp.MustCompile(`\.call\{value:`), nil, "security/solidity-reentrancy", SeverityError, "External call with value transfer is vulnerable to reentrancy", "Use checks-effects-interactions pattern or ReentrancyGuard"},
	{regexp.MustCompile(`tx\.origin`), nil, "security/solidity-tx-origin", SeverityError, "tx.origin for authorization is vulnerable to phishing", "Use msg.sender instead of tx.origin"},
	{regexp.MustCompile(`selfdestruct\s*\(`), nil, "security/solidity-selfdestruct", SeverityError, "selfdestruct can permanently destroy the contract", "Avoid selfdestruct; use pausable patterns instead"},
	{regexp.MustCompile(`delegatecall\s*\(`), nil, "security/solidity-delegatecall", SeverityWarning, "delegatecall executes code in the caller's context", "Ensure delegatecall targets are trusted and immutable"},
	{regexp.MustCompile(`block\.timestamp`), nil, "security/solidity-timestamp-dependence", SeverityWarning, "block.timestamp can be manipulated by miners", "Avoid using block.timestamp for critical logic"},
}

func checkSoliditySecurity(fileDiff git.FileDiff) []Issue {
	issues := make([]Issue, 0)
	for _, hunk := range fileDiff.Hunks {
		for _, line := range hunk.Lines {
			if line.Type != "added" {
				continue
			}
			trimmed := strings.TrimSpace(line.Content)
			if strings.HasPrefix(trimmed, "//") {
				continue
			}
			for _, rule := range soliditySecurityRules {
				if !rule.pattern.MatchString(line.Content) {
					continue
				}
				excluded := false
				for _, excl := range rule.exclusions {
					if excl.MatchString(line.Content) {
						excluded = true
						break
					}
				}
				if excluded {
					continue
				}
				issues = append(issues, Issue{
					ID:         rule.id,
					Severity:   rule.severity,
					Category:   "security",
					File:       fileDiff.Path,
					Line:       line.NewNum,
					Message:    rule.message,
					Suggestion: rule.suggestion,
				})
				break
			}
		}
	}
	return issues
}

// --- Terraform security ---

type terraformSecurityRule struct {
	pattern    *regexp.Regexp
	exclusions []*regexp.Regexp
	id         string
	severity   Severity
	message    string
	suggestion string
}

var terraformSecurityRules = []terraformSecurityRule{
	{regexp.MustCompile(`cidr_blocks\s*=\s*\[\s*"0\.0\.0\.0/0"\s*\]`), nil, "security/terraform-open-cidr", SeverityWarning, "Security group allows traffic from all IPs", "Restrict CIDR blocks to known IP ranges"},
	{regexp.MustCompile(`(?i)password\s*=\s*"[^"]+"`), nil, "security/terraform-hardcoded-secret", SeverityError, "Hardcoded password in Terraform config", "Use variables or a secrets manager"},
	{regexp.MustCompile(`(?i)secret\s*=\s*"[^"]+"`), nil, "security/terraform-hardcoded-secret", SeverityError, "Hardcoded secret in Terraform config", "Use variables or a secrets manager"},
	{regexp.MustCompile(`encrypted\s*=\s*false`), nil, "security/terraform-unencrypted", SeverityWarning, "Resource encryption is disabled", "Enable encryption for data at rest"},
}

// --- Fortran security ---

type fortranSecurityRule struct {
	pattern    *regexp.Regexp
	exclusions []*regexp.Regexp
	id         string
	severity   Severity
	message    string
	suggestion string
}

var fortranSecurityRules = []fortranSecurityRule{
	{regexp.MustCompile(`(?i)\bCALL\s+SYSTEM\s*\(`), nil, "security/fortran-command-injection", SeverityError, "CALL SYSTEM executes OS commands", "Avoid CALL SYSTEM with untrusted input"},
	{regexp.MustCompile(`(?i)\bEQUIVALENCE\s*\(`), nil, "security/fortran-equivalence", SeverityWarning, "EQUIVALENCE allows memory aliasing abuse", "Use explicit type conversions instead of EQUIVALENCE"},
	{regexp.MustCompile(`(?i)\bFORMAT\s*\(.*\bA\b`), nil, "security/fortran-format-injection", SeverityWarning, "FORMAT with character input may allow format string injection", "Validate user-controlled data before FORMAT"},
	{regexp.MustCompile(`(?i)\bOPEN\s*\(.*FILE\s*=\s*['"][^'"]+['"]\s*`), nil, "security/fortran-hardcoded-path", SeverityWarning, "Hardcoded file path in OPEN statement", "Use variables or configuration for file paths"},
}

func checkFortranSecurity(fileDiff git.FileDiff) []Issue {
	issues := make([]Issue, 0)
	for _, hunk := range fileDiff.Hunks {
		for _, line := range hunk.Lines {
			if line.Type != "added" {
				continue
			}
			trimmed := strings.TrimSpace(line.Content)
			if strings.HasPrefix(trimmed, "!") {
				continue
			}
			for _, rule := range fortranSecurityRules {
				if !rule.pattern.MatchString(line.Content) {
					continue
				}
				excluded := false
				for _, excl := range rule.exclusions {
					if excl.MatchString(line.Content) {
						excluded = true
						break
					}
				}
				if excluded {
					continue
				}
				issues = append(issues, Issue{
					ID:         rule.id,
					Severity:   rule.severity,
					Category:   "security",
					File:       fileDiff.Path,
					Line:       line.NewNum,
					Message:    rule.message,
					Suggestion: rule.suggestion,
				})
				break
			}
		}
	}
	return issues
}

// --- Protobuf security ---

type protobufSecurityRule struct {
	pattern    *regexp.Regexp
	exclusions []*regexp.Regexp
	id         string
	severity   Severity
	message    string
	suggestion string
}

var protobufSecurityRules = []protobufSecurityRule{
	{regexp.MustCompile(`option\s+java_package\s*=\s*".*\.(internal|impl)\b`), nil, "security/protobuf-internal-package", SeverityWarning, "java_package references internal package", "Avoid exposing internal packages in protobuf definitions"},
	{regexp.MustCompile(`\bservice\s+\w+\s*\{`), []*regexp.Regexp{regexp.MustCompile(`(?i)(auth|authenticated|interceptor)`)}, "security/protobuf-unauthenticated-service", SeverityWarning, "Service defined without authentication annotation", "Add authentication interceptor or annotation to service"},
	{regexp.MustCompile(`\bgoogle\.protobuf\.Any\b`), nil, "security/protobuf-any-type", SeverityWarning, "google.protobuf.Any allows arbitrary types (type confusion risk)", "Use specific message types instead of Any when possible"},
}

func checkProtobufSecurity(fileDiff git.FileDiff) []Issue {
	issues := make([]Issue, 0)
	for _, hunk := range fileDiff.Hunks {
		for _, line := range hunk.Lines {
			if line.Type != "added" {
				continue
			}
			trimmed := strings.TrimSpace(line.Content)
			if strings.HasPrefix(trimmed, "//") {
				continue
			}
			for _, rule := range protobufSecurityRules {
				if !rule.pattern.MatchString(line.Content) {
					continue
				}
				excluded := false
				for _, excl := range rule.exclusions {
					if excl.MatchString(line.Content) {
						excluded = true
						break
					}
				}
				if excluded {
					continue
				}
				issues = append(issues, Issue{
					ID:         rule.id,
					Severity:   rule.severity,
					Category:   "security",
					File:       fileDiff.Path,
					Line:       line.NewNum,
					Message:    rule.message,
					Suggestion: rule.suggestion,
				})
				break
			}
		}
	}
	return issues
}

func checkTerraformSecurity(fileDiff git.FileDiff) []Issue {
	issues := make([]Issue, 0)
	for _, hunk := range fileDiff.Hunks {
		for _, line := range hunk.Lines {
			if line.Type != "added" {
				continue
			}
			trimmed := strings.TrimSpace(line.Content)
			if strings.HasPrefix(trimmed, "#") || strings.HasPrefix(trimmed, "//") {
				continue
			}
			for _, rule := range terraformSecurityRules {
				if !rule.pattern.MatchString(line.Content) {
					continue
				}
				excluded := false
				for _, excl := range rule.exclusions {
					if excl.MatchString(line.Content) {
						excluded = true
						break
					}
				}
				if excluded {
					continue
				}
				issues = append(issues, Issue{
					ID:         rule.id,
					Severity:   rule.severity,
					Category:   "security",
					File:       fileDiff.Path,
					Line:       line.NewNum,
					Message:    rule.message,
					Suggestion: rule.suggestion,
				})
				break
			}
		}
	}
	return issues
}
