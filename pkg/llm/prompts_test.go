package llm

import (
	"testing"
)

func TestLoadPrompt_Confidence(t *testing.T) {
	prompt := LoadPrompt("confidence")
	if prompt == "" {
		t.Fatal("LoadPrompt(\"confidence\") returned empty string")
	}
	if len(prompt) < 100 {
		t.Errorf("confidence prompt seems too short: %d bytes", len(prompt))
	}
}

func TestLoadPrompt_SpecCompare(t *testing.T) {
	prompt := LoadPrompt("spec_compare")
	if prompt == "" {
		t.Fatal("LoadPrompt(\"spec_compare\") returned empty string")
	}
	if len(prompt) < 100 {
		t.Errorf("spec_compare prompt seems too short: %d bytes", len(prompt))
	}
}

func TestLoadPrompt_NonExistent(t *testing.T) {
	prompt := LoadPrompt("nonexistent_prompt")
	if prompt != "" {
		t.Errorf("LoadPrompt(\"nonexistent_prompt\") = %q, want empty string", prompt)
	}
}

func TestLoadPrompt_Cached(t *testing.T) {
	// First call loads from FS
	first := LoadPrompt("confidence")
	// Second call should return cached
	second := LoadPrompt("confidence")

	if first != second {
		t.Error("cached prompt differs from initial load")
	}
}

func TestFormatPrompt(t *testing.T) {
	tests := []struct {
		name     string
		template string
		vars     map[string]string
		want     string
	}{
		{
			name:     "single variable",
			template: "Hello {{name}}!",
			vars:     map[string]string{"name": "World"},
			want:     "Hello World!",
		},
		{
			name:     "multiple variables",
			template: "{{greeting}} {{name}}, welcome to {{place}}.",
			vars:     map[string]string{"greeting": "Hello", "name": "Alice", "place": "Wonderland"},
			want:     "Hello Alice, welcome to Wonderland.",
		},
		{
			name:     "repeated variable",
			template: "{{x}} and {{x}} again",
			vars:     map[string]string{"x": "value"},
			want:     "value and value again",
		},
		{
			name:     "no variables",
			template: "plain text",
			vars:     map[string]string{},
			want:     "plain text",
		},
		{
			name:     "missing variable stays as-is",
			template: "Hello {{name}}!",
			vars:     map[string]string{},
			want:     "Hello {{name}}!",
		},
		{
			name:     "nil vars map",
			template: "Hello {{name}}!",
			vars:     nil,
			want:     "Hello {{name}}!",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := FormatPrompt(tt.template, tt.vars)
			if got != tt.want {
				t.Errorf("FormatPrompt() = %q, want %q", got, tt.want)
			}
		})
	}
}

func TestAvailablePrompts(t *testing.T) {
	prompts := AvailablePrompts()
	if len(prompts) < 2 {
		t.Errorf("AvailablePrompts() returned %d prompts, expected at least 2", len(prompts))
	}

	// Check that our known prompts are present
	found := make(map[string]bool)
	for _, p := range prompts {
		found[p] = true
	}

	for _, expected := range []string{"confidence", "spec_compare"} {
		if !found[expected] {
			t.Errorf("AvailablePrompts() missing %q", expected)
		}
	}
}
