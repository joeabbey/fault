package fixer

import (
	"strings"
	"testing"

	"github.com/joeabbey/fault/pkg/analyzer"
)

func TestFixHardcodedSecretGo(t *testing.T) {
	dir := t.TempDir()
	writeFile(t, dir, "config.go", "package config\n\nvar apiKey = \"sk_live_abc123def456ghi789jkl\"\n")

	issue := analyzer.Issue{
		ID:    "security/hardcoded-secret",
		FixID: "sec-hardcoded-secret",
		File:  "config.go",
		Line:  3,
	}

	fixer := NewSecurityFixer()
	fix := fixer.GenerateFix(issue, dir)
	if fix == nil {
		t.Fatal("expected fix, got nil")
	}

	if err := Apply(fix, dir); err != nil {
		t.Fatalf("Apply: %v", err)
	}

	content := readFile(t, dir, "config.go")
	if !strings.Contains(content, `os.Getenv("API_KEY")`) {
		t.Errorf("expected os.Getenv replacement, got:\n%s", content)
	}
	if strings.Contains(content, "sk_live_") {
		t.Errorf("hardcoded secret should be removed, got:\n%s", content)
	}
}

func TestFixHardcodedSecretTS(t *testing.T) {
	dir := t.TempDir()
	writeFile(t, dir, "config.ts", "const stripeKey = 'sk_live_abc123def456ghi789jkl'\n")

	issue := analyzer.Issue{
		ID:    "security/hardcoded-secret",
		FixID: "sec-hardcoded-secret",
		File:  "config.ts",
		Line:  1,
	}

	fixer := NewSecurityFixer()
	fix := fixer.GenerateFix(issue, dir)
	if fix == nil {
		t.Fatal("expected fix, got nil")
	}

	if err := Apply(fix, dir); err != nil {
		t.Fatalf("Apply: %v", err)
	}

	content := readFile(t, dir, "config.ts")
	if !strings.Contains(content, "process.env.STRIPE_KEY") {
		t.Errorf("expected process.env replacement, got:\n%s", content)
	}
}

func TestFixHardcodedSecretPython(t *testing.T) {
	dir := t.TempDir()
	writeFile(t, dir, "config.py", "api_key = 'sk_live_abc123def456ghi789jkl'\n")

	issue := analyzer.Issue{
		ID:    "security/hardcoded-secret",
		FixID: "sec-hardcoded-secret",
		File:  "config.py",
		Line:  1,
	}

	fixer := NewSecurityFixer()
	fix := fixer.GenerateFix(issue, dir)
	if fix == nil {
		t.Fatal("expected fix, got nil")
	}

	if err := Apply(fix, dir); err != nil {
		t.Fatalf("Apply: %v", err)
	}

	content := readFile(t, dir, "config.py")
	if !strings.Contains(content, `os.environ.get("API_KEY")`) {
		t.Errorf("expected os.environ.get replacement, got:\n%s", content)
	}
}

func TestFixInsecureRandomJS(t *testing.T) {
	dir := t.TempDir()
	writeFile(t, dir, "utils.ts", "const id = Math.random().toString(36)\n")

	issue := analyzer.Issue{
		ID:    "security/insecure-crypto",
		FixID: "sec-insecure-random",
		File:  "utils.ts",
		Line:  1,
	}

	fixer := NewSecurityFixer()
	fix := fixer.GenerateFix(issue, dir)
	if fix == nil {
		t.Fatal("expected fix, got nil")
	}

	if err := Apply(fix, dir); err != nil {
		t.Fatalf("Apply: %v", err)
	}

	content := readFile(t, dir, "utils.ts")
	if !strings.Contains(content, "crypto.randomUUID()") {
		t.Errorf("expected crypto.randomUUID replacement, got:\n%s", content)
	}
	if strings.Contains(content, "Math.random()") {
		t.Errorf("Math.random should be replaced, got:\n%s", content)
	}
}

func TestVarNameInference(t *testing.T) {
	tests := []struct {
		name   string
		input  string
		expect string
	}{
		{"camelCase", "API_KEY", "API_KEY"},
		{"camelCase2", "apiKey", "API_KEY"},
		{"snake_case", "stripe_key", "STRIPE_KEY"},
		{"PascalCase", "StripeKey", "STRIPE_KEY"},
		{"with-dash", "api-key", "API_KEY"},
		{"simple", "secret", "SECRET"},
		{"mixed", "myAPIKey", "MY_API_KEY"},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := toEnvVarName(tt.input)
			if got != tt.expect {
				t.Errorf("toEnvVarName(%q) = %q, want %q", tt.input, got, tt.expect)
			}
		})
	}
}
