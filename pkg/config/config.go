package config

import (
	"fmt"
	"os"
	"path/filepath"
	"time"

	"gopkg.in/yaml.v3"
)

const ConfigFileName = ".fault.yaml"

// Config represents the fault configuration file.
type Config struct {
	Version   int              `yaml:"version" json:"version"`
	Languages []string         `yaml:"languages" json:"languages"`
	BlockOn   string           `yaml:"block_on" json:"block_on"`
	Analyzers AnalyzersConfig  `yaml:"analyzers" json:"analyzers"`
	LLM       LLMConfig        `yaml:"llm" json:"llm"`
	Watch     WatchConfig      `yaml:"watch" json:"watch"`
	Ignore    []string         `yaml:"ignore" json:"ignore"`
}

// AnalyzersConfig controls which analyzers are enabled.
type AnalyzersConfig struct {
	Imports       bool `yaml:"imports" json:"imports"`
	Consistency   bool `yaml:"consistency" json:"consistency"`
	References    bool `yaml:"references" json:"references"`
	Tests         bool `yaml:"tests" json:"tests"`
	Patterns      bool `yaml:"patterns" json:"patterns"`
	Security      bool `yaml:"security" json:"security"`
	Hallucination bool `yaml:"hallucination" json:"hallucination"`
}

// LLMConfig controls LLM-assisted analysis.
type LLMConfig struct {
	Enabled  bool   `yaml:"enabled" json:"enabled"`
	SpecFile string `yaml:"spec_file" json:"spec_file"`
	APIKey   string `yaml:"-" json:"-"` // loaded from env, never serialized
}

// WatchConfig controls watch mode behavior.
type WatchConfig struct {
	PollInterval string `yaml:"poll_interval" json:"poll_interval"`
	Debounce     string `yaml:"debounce" json:"debounce"`
}

// DefaultConfig returns a Config with sensible defaults.
func DefaultConfig() *Config {
	return &Config{
		Version:   1,
		Languages: []string{"typescript", "python", "go"},
		BlockOn:   "error",
		Analyzers: AnalyzersConfig{
			Imports:       true,
			Consistency:   true,
			References:    true,
			Tests:         true,
			Patterns:      true,
			Security:      true,
			Hallucination: true,
		},
		LLM: LLMConfig{
			Enabled:  false,
			SpecFile: "",
		},
		Watch: WatchConfig{
			PollInterval: "500ms",
			Debounce:     "200ms",
		},
		Ignore: []string{
			"vendor/",
			"node_modules/",
			"*.generated.*",
			"*.min.js",
		},
	}
}

// Load finds and loads a .fault.yaml config, walking up from startDir.
// If no config file is found, returns defaults.
func Load(startDir string) (*Config, error) {
	configPath, err := findConfig(startDir)
	if err != nil {
		// No config found — use defaults
		cfg := DefaultConfig()
		applyEnvOverrides(cfg)
		return cfg, nil
	}

	return LoadFromFile(configPath)
}

// LoadFromFile loads config from a specific file path.
func LoadFromFile(path string) (*Config, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, fmt.Errorf("reading config file: %w", err)
	}

	cfg := DefaultConfig()
	if err := yaml.Unmarshal(data, cfg); err != nil {
		return nil, fmt.Errorf("parsing config file: %w", err)
	}

	applyEnvOverrides(cfg)

	if err := cfg.Validate(); err != nil {
		return nil, fmt.Errorf("invalid config: %w", err)
	}

	return cfg, nil
}

// Validate checks the config for errors.
func (c *Config) Validate() error {
	if c.Version != 1 {
		return fmt.Errorf("unsupported config version: %d (expected 1)", c.Version)
	}

	switch c.BlockOn {
	case "error", "warning":
		// valid
	default:
		return fmt.Errorf("invalid block_on value: %q (expected \"error\" or \"warning\")", c.BlockOn)
	}

	validLangs := map[string]bool{
		"go":         true,
		"typescript": true,
		"python":     true,
	}
	for _, lang := range c.Languages {
		if !validLangs[lang] {
			return fmt.Errorf("unsupported language: %q", lang)
		}
	}

	if c.LLM.Enabled && c.LLM.APIKey == "" {
		return fmt.Errorf("LLM enabled but FAULT_API_KEY not set")
	}

	if c.Watch.PollInterval != "" {
		if _, err := time.ParseDuration(c.Watch.PollInterval); err != nil {
			return fmt.Errorf("invalid watch.poll_interval: %w", err)
		}
	}
	if c.Watch.Debounce != "" {
		if _, err := time.ParseDuration(c.Watch.Debounce); err != nil {
			return fmt.Errorf("invalid watch.debounce: %w", err)
		}
	}

	return nil
}

// findConfig walks up from startDir looking for .fault.yaml.
func findConfig(startDir string) (string, error) {
	dir, err := filepath.Abs(startDir)
	if err != nil {
		return "", err
	}

	for {
		candidate := filepath.Join(dir, ConfigFileName)
		if _, err := os.Stat(candidate); err == nil {
			return candidate, nil
		}

		parent := filepath.Dir(dir)
		if parent == dir {
			// Reached filesystem root
			return "", fmt.Errorf("no %s found", ConfigFileName)
		}
		dir = parent
	}
}

// applyEnvOverrides applies environment variable overrides to the config.
func applyEnvOverrides(cfg *Config) {
	if apiKey := os.Getenv("FAULT_API_KEY"); apiKey != "" {
		cfg.LLM.APIKey = apiKey
	}
}

// IsIgnored checks if a file path matches any of the ignore patterns.
func (c *Config) IsIgnored(path string) bool {
	for _, pattern := range c.Ignore {
		// Check if the pattern is a directory prefix
		if pattern[len(pattern)-1] == '/' {
			dir := pattern[:len(pattern)-1]
			if matchesDir(path, dir) {
				return true
			}
			continue
		}

		// Check glob pattern against basename and full path
		base := filepath.Base(path)
		if matched, _ := filepath.Match(pattern, base); matched {
			return true
		}
		if matched, _ := filepath.Match(pattern, path); matched {
			return true
		}
	}
	return false
}

// matchesDir checks if a path is inside a directory.
func matchesDir(path, dir string) bool {
	// Check if any path component matches the directory name
	parts := splitPath(path)
	for _, part := range parts {
		if part == dir {
			return true
		}
	}
	return false
}

// splitPath splits a path into its components.
func splitPath(path string) []string {
	var parts []string
	dir := path
	for {
		d, f := filepath.Split(dir)
		if f != "" {
			parts = append([]string{f}, parts...)
		}
		if d == "" || d == dir {
			break
		}
		dir = d[:len(d)-1] // remove trailing slash
	}
	return parts
}
