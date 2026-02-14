package main

import (
	"fmt"
	"log"
	"os"

	"github.com/spf13/cobra"

	"github.com/joeabbey/fault/pkg/analyzer"
	"github.com/joeabbey/fault/pkg/config"
	"github.com/joeabbey/fault/pkg/git"
	"github.com/joeabbey/fault/pkg/parser"
	"github.com/joeabbey/fault/pkg/reporter"
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
	rootCmd.AddCommand(hookCmd())
	rootCmd.AddCommand(versionCmd())

	if err := rootCmd.Execute(); err != nil {
		os.Exit(1)
	}
}

func checkCmd() *cobra.Command {
	var (
		staged   bool
		unstaged bool
		branch   string
		noColor  bool
	)

	cmd := &cobra.Command{
		Use:   "check",
		Short: "Run analyzers on changed files",
		RunE: func(cmd *cobra.Command, args []string) error {
			return runCheck(staged, unstaged, branch, noColor)
		},
	}

	cmd.Flags().BoolVar(&staged, "staged", false, "Check staged changes only")
	cmd.Flags().BoolVar(&unstaged, "unstaged", false, "Check unstaged changes only")
	cmd.Flags().StringVar(&branch, "branch", "", "Check changes since branch (e.g., main)")
	cmd.Flags().BoolVar(&noColor, "no-color", false, "Disable colored output")

	return cmd
}

func runCheck(staged, unstaged bool, branch string, noColor bool) error {
	// 1. Load config
	cwd, err := os.Getwd()
	if err != nil {
		return fmt.Errorf("getting working directory: %w", err)
	}

	cfg, err := config.Load(cwd)
	if err != nil {
		return fmt.Errorf("loading config: %w", err)
	}

	// 2. Open git repo
	repo, err := git.NewRepo(cwd)
	if err != nil {
		return fmt.Errorf("opening repo: %w", err)
	}

	// 3. Get diff based on mode
	diff, err := getDiff(repo, staged, unstaged, branch)
	if err != nil {
		return fmt.Errorf("getting diff: %w", err)
	}

	if len(diff.Files) == 0 {
		fmt.Println("No changes detected")
		return nil
	}

	// 4. Set up parser registry
	reg := parser.NewRegistry()
	reg.Register(parser.NewGoParser())
	reg.Register(parser.NewTypeScriptParser())
	reg.Register(parser.NewPythonParser())

	// 5. Parse changed files
	parsedFiles := parseChangedFiles(repo, diff, reg, cfg)

	// 6. Run analyzers
	// No concrete analyzers yet — Phase 2 will add them.
	// The runner will run with an empty analyzer list for now.
	analyzers := make([]analyzer.Analyzer, 0)
	runner := analyzer.NewRunner(cfg, analyzers)

	repoRoot, _ := repo.RepoRoot()
	result := runner.Run(repoRoot, diff, parsedFiles)

	// Set branch info if available
	if currentBranch, err := repo.CurrentBranch(); err == nil {
		result.Branch = currentBranch
	}

	// 7. Report results
	rep := reporter.NewTerminalReporter(noColor)
	exitCode := rep.Report(result, cfg.BlockOn)

	if exitCode != 0 {
		os.Exit(exitCode)
	}

	return nil
}

// getDiff returns the appropriate diff based on flags.
func getDiff(repo *git.Repo, staged, unstaged bool, branch string) (*git.Diff, error) {
	switch {
	case branch != "":
		return repo.BranchDiff(branch)
	case staged:
		return repo.StagedDiff()
	case unstaged:
		return repo.UnstagedDiff()
	default:
		return repo.AutoDiff()
	}
}

// parseChangedFiles parses all non-ignored changed files.
func parseChangedFiles(repo *git.Repo, diff *git.Diff, reg *parser.Registry, cfg *config.Config) map[string]*parser.ParsedFile {
	parsed := make(map[string]*parser.ParsedFile)

	for _, fileDiff := range diff.Files {
		path := fileDiff.Path

		// Skip ignored files
		if cfg.IsIgnored(path) {
			continue
		}

		// Skip deleted and binary files
		if fileDiff.Status == "deleted" || fileDiff.IsBinary {
			continue
		}

		// Detect language
		lang := parser.DetectLanguage(path)
		if lang == "" {
			continue
		}

		// Check if language is in config
		if !languageEnabled(cfg, lang) {
			continue
		}

		// Read file content
		content, err := repo.FileContent(path, "")
		if err != nil {
			log.Printf("warning: could not read %s: %v", path, err)
			continue
		}

		// Parse
		pf, err := reg.ParseFile(path, content)
		if err != nil {
			log.Printf("warning: could not parse %s: %v", path, err)
			continue
		}
		if pf != nil {
			parsed[path] = pf
		}
	}

	return parsed
}

// languageEnabled checks if a language is in the config's language list.
func languageEnabled(cfg *config.Config, lang string) bool {
	for _, l := range cfg.Languages {
		if l == lang {
			return true
		}
	}
	return false
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

func hookCmd() *cobra.Command {
	cmd := &cobra.Command{
		Use:   "hook",
		Short: "Manage the git pre-commit hook",
	}

	cmd.AddCommand(hookInstallCmd())
	cmd.AddCommand(hookUninstallCmd())
	cmd.AddCommand(hookStatusCmd())

	return cmd
}

func hookInstallCmd() *cobra.Command {
	var force bool

	cmd := &cobra.Command{
		Use:   "install",
		Short: "Install the pre-commit hook",
		RunE: func(cmd *cobra.Command, args []string) error {
			cwd, err := os.Getwd()
			if err != nil {
				return fmt.Errorf("getting working directory: %w", err)
			}

			repo, err := git.NewRepo(cwd)
			if err != nil {
				return fmt.Errorf("opening repo: %w", err)
			}

			repoRoot, err := repo.RepoRoot()
			if err != nil {
				return fmt.Errorf("getting repo root: %w", err)
			}

			if err := git.InstallHook(repoRoot, force); err != nil {
				return err
			}

			fmt.Println("Installed pre-commit hook")
			return nil
		},
	}

	cmd.Flags().BoolVar(&force, "force", false, "Overwrite existing hook")

	return cmd
}

func hookUninstallCmd() *cobra.Command {
	cmd := &cobra.Command{
		Use:   "uninstall",
		Short: "Remove the pre-commit hook",
		RunE: func(cmd *cobra.Command, args []string) error {
			cwd, err := os.Getwd()
			if err != nil {
				return fmt.Errorf("getting working directory: %w", err)
			}

			repo, err := git.NewRepo(cwd)
			if err != nil {
				return fmt.Errorf("opening repo: %w", err)
			}

			repoRoot, err := repo.RepoRoot()
			if err != nil {
				return fmt.Errorf("getting repo root: %w", err)
			}

			if err := git.UninstallHook(repoRoot); err != nil {
				return err
			}

			fmt.Println("Removed pre-commit hook")
			return nil
		},
	}

	return cmd
}

func hookStatusCmd() *cobra.Command {
	cmd := &cobra.Command{
		Use:   "status",
		Short: "Check if the pre-commit hook is installed",
		RunE: func(cmd *cobra.Command, args []string) error {
			cwd, err := os.Getwd()
			if err != nil {
				return fmt.Errorf("getting working directory: %w", err)
			}

			repo, err := git.NewRepo(cwd)
			if err != nil {
				return fmt.Errorf("opening repo: %w", err)
			}

			repoRoot, err := repo.RepoRoot()
			if err != nil {
				return fmt.Errorf("getting repo root: %w", err)
			}

			if git.IsHookInstalled(repoRoot) {
				fmt.Println("Pre-commit hook is installed")
			} else {
				fmt.Println("Pre-commit hook is not installed")
			}

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
