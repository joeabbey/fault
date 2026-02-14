package main

import (
	"fmt"
	"os"

	"github.com/spf13/cobra"
)

// Version is set at build time via -ldflags
var Version = "0.1.0-dev"

func main() {
	rootCmd := &cobra.Command{
		Use:   "fault",
		Short: "Pre-commit validation for AI agent output",
		Long:  "Fault catches structural errors in AI-generated code before they reach your commit history.",
	}

	rootCmd.AddCommand(checkCmd())
	rootCmd.AddCommand(initCmd())
	rootCmd.AddCommand(versionCmd())

	if err := rootCmd.Execute(); err != nil {
		os.Exit(1)
	}
}

func checkCmd() *cobra.Command {
	cmd := &cobra.Command{
		Use:   "check",
		Short: "Run analyzers on changed files",
		RunE: func(cmd *cobra.Command, args []string) error {
			fmt.Println("no analyzers configured")
			return nil
		},
	}
	return cmd
}

func initCmd() *cobra.Command {
	cmd := &cobra.Command{
		Use:   "init",
		Short: "Generate a .fault.yaml config file",
		RunE: func(cmd *cobra.Command, args []string) error {
			defaultConfig := `version: 1
languages: [typescript, python, go]
block_on: error
analyzers:
  imports: true
  consistency: true
  references: true
  tests: true
  patterns: true
llm:
  enabled: false
  spec_file: ""
ignore:
  - "vendor/"
  - "node_modules/"
  - "*.generated.*"
  - "*.min.js"
`
			if _, err := os.Stat(".fault.yaml"); err == nil {
				return fmt.Errorf(".fault.yaml already exists")
			}
			if err := os.WriteFile(".fault.yaml", []byte(defaultConfig), 0644); err != nil {
				return fmt.Errorf("failed to write .fault.yaml: %w", err)
			}
			fmt.Println("Created .fault.yaml")
			return nil
		},
	}
	return cmd
}

func versionCmd() *cobra.Command {
	cmd := &cobra.Command{
		Use:   "version",
		Short: "Print the version",
		Run: func(cmd *cobra.Command, args []string) {
			fmt.Printf("fault %s\n", Version)
		},
	}
	return cmd
}
