package config

import (
	"os"
	"path/filepath"
	"testing"
)

func TestDefaultConfig(t *testing.T) {
	cfg := DefaultConfig()

	if cfg.Version != 1 {
		t.Errorf("expected version 1, got %d", cfg.Version)
	}
	if cfg.BlockOn != "error" {
		t.Errorf("expected block_on=error, got %q", cfg.BlockOn)
	}
	if !cfg.Analyzers.Imports {
		t.Error("expected imports analyzer enabled by default")
	}
	if !cfg.Analyzers.Consistency {
		t.Error("expected consistency analyzer enabled by default")
	}
	if !cfg.Analyzers.References {
		t.Error("expected references analyzer enabled by default")
	}
	if !cfg.Analyzers.Tests {
		t.Error("expected tests analyzer enabled by default")
	}
	if !cfg.Analyzers.Patterns {
		t.Error("expected patterns analyzer enabled by default")
	}
	if !cfg.Analyzers.Security {
		t.Error("expected security analyzer enabled by default")
	}
	if !cfg.Analyzers.Hallucination {
		t.Error("expected hallucination analyzer enabled by default")
	}
	if !cfg.Analyzers.Complexity {
		t.Error("expected complexity analyzer enabled by default")
	}
	if !cfg.Analyzers.Concurrency {
		t.Error("expected concurrency analyzer enabled by default")
	}
	if !cfg.Analyzers.Resource {
		t.Error("expected resource analyzer enabled by default")
	}
	if !cfg.Analyzers.Migration {
		t.Error("expected migration analyzer enabled by default")
	}
	if !cfg.Analyzers.DocDrift {
		t.Error("expected docdrift analyzer enabled by default")
	}
	if cfg.LLM.Enabled {
		t.Error("expected LLM disabled by default")
	}
	if len(cfg.Languages) != 41 {
		t.Errorf("expected 41 languages, got %d", len(cfg.Languages))
	}
	if len(cfg.Ignore) != 4 {
		t.Errorf("expected 4 ignore patterns, got %d", len(cfg.Ignore))
	}
}

func TestLoadValidConfig(t *testing.T) {
	dir := t.TempDir()
	configContent := `version: 1
languages: [go, python]
block_on: warning
analyzers:
  imports: true
  consistency: false
  references: true
  tests: false
  patterns: true
llm:
  enabled: false
  spec_file: ""
ignore:
  - "vendor/"
`
	err := os.WriteFile(filepath.Join(dir, ConfigFileName), []byte(configContent), 0644)
	if err != nil {
		t.Fatal(err)
	}

	cfg, err := Load(dir)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	if cfg.BlockOn != "warning" {
		t.Errorf("expected block_on=warning, got %q", cfg.BlockOn)
	}
	if len(cfg.Languages) != 2 {
		t.Errorf("expected 2 languages, got %d", len(cfg.Languages))
	}
	if cfg.Analyzers.Consistency {
		t.Error("expected consistency disabled")
	}
	if cfg.Analyzers.Tests {
		t.Error("expected tests disabled")
	}
	if len(cfg.Ignore) != 1 {
		t.Errorf("expected 1 ignore pattern, got %d", len(cfg.Ignore))
	}
}

func TestLoadInvalidConfig(t *testing.T) {
	tests := []struct {
		name    string
		content string
		wantErr string
	}{
		{
			name:    "invalid version",
			content: "version: 2\nblock_on: error\nlanguages: [go]",
			wantErr: "unsupported config version",
		},
		{
			name:    "invalid block_on",
			content: "version: 1\nblock_on: never\nlanguages: [go]",
			wantErr: "invalid block_on value",
		},
		{
			name:    "unsupported language",
			content: "version: 1\nblock_on: error\nlanguages: [brainfuck]",
			wantErr: "unsupported language",
		},
		{
			name:    "llm enabled without key",
			content: "version: 1\nblock_on: error\nlanguages: [go]\nllm:\n  enabled: true",
			wantErr: "FAULT_API_KEY not set",
		},
		{
			name:    "invalid yaml",
			content: "version: [[[invalid",
			wantErr: "parsing config file",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Clear any env var that might interfere
			t.Setenv("FAULT_API_KEY", "")

			dir := t.TempDir()
			err := os.WriteFile(filepath.Join(dir, ConfigFileName), []byte(tt.content), 0644)
			if err != nil {
				t.Fatal(err)
			}

			_, err = Load(dir)
			if err == nil {
				t.Fatal("expected error, got nil")
			}
			if !contains(err.Error(), tt.wantErr) {
				t.Errorf("error %q does not contain %q", err.Error(), tt.wantErr)
			}
		})
	}
}

func TestLoadMissingConfigUsesDefaults(t *testing.T) {
	dir := t.TempDir()

	cfg, err := Load(dir)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	defaults := DefaultConfig()
	if cfg.Version != defaults.Version {
		t.Errorf("expected default version %d, got %d", defaults.Version, cfg.Version)
	}
	if cfg.BlockOn != defaults.BlockOn {
		t.Errorf("expected default block_on %q, got %q", defaults.BlockOn, cfg.BlockOn)
	}
}

func TestEnvVarOverride(t *testing.T) {
	t.Setenv("FAULT_API_KEY", "test-key-123")

	dir := t.TempDir()
	configContent := `version: 1
languages: [go]
block_on: error
llm:
  enabled: true
`
	err := os.WriteFile(filepath.Join(dir, ConfigFileName), []byte(configContent), 0644)
	if err != nil {
		t.Fatal(err)
	}

	cfg, err := Load(dir)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	if cfg.LLM.APIKey != "test-key-123" {
		t.Errorf("expected API key from env, got %q", cfg.LLM.APIKey)
	}
}

func TestConfigWalksUpDirectories(t *testing.T) {
	root := t.TempDir()
	sub := filepath.Join(root, "a", "b", "c")
	err := os.MkdirAll(sub, 0755)
	if err != nil {
		t.Fatal(err)
	}

	configContent := `version: 1
languages: [go]
block_on: error
`
	err = os.WriteFile(filepath.Join(root, ConfigFileName), []byte(configContent), 0644)
	if err != nil {
		t.Fatal(err)
	}

	cfg, err := Load(sub)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	if cfg.Version != 1 {
		t.Errorf("expected version 1, got %d", cfg.Version)
	}
}

func TestIsIgnored(t *testing.T) {
	cfg := DefaultConfig()

	tests := []struct {
		path    string
		ignored bool
	}{
		{"vendor/lib/foo.go", true},
		{"node_modules/react/index.js", true},
		{"foo.generated.ts", true},
		{"bundle.min.js", true},
		{"src/main.go", false},
		{"pkg/config/config.go", false},
	}

	for _, tt := range tests {
		t.Run(tt.path, func(t *testing.T) {
			got := cfg.IsIgnored(tt.path)
			if got != tt.ignored {
				t.Errorf("IsIgnored(%q) = %v, want %v", tt.path, got, tt.ignored)
			}
		})
	}
}

func contains(s, substr string) bool {
	return len(s) >= len(substr) && searchString(s, substr)
}

func searchString(s, substr string) bool {
	for i := 0; i <= len(s)-len(substr); i++ {
		if s[i:i+len(substr)] == substr {
			return true
		}
	}
	return false
}
