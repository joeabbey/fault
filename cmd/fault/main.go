package main

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"log"
	"net/http"
	"os"
	"path/filepath"
	"strings"

	"github.com/spf13/cobra"

	"github.com/joeabbey/fault/pkg/analyzer"
	"github.com/joeabbey/fault/pkg/config"
	"github.com/joeabbey/fault/pkg/git"
	"github.com/joeabbey/fault/pkg/index"
	"github.com/joeabbey/fault/pkg/llm"
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
	rootCmd.AddCommand(baselineCmd())
	rootCmd.AddCommand(versionCmd())
	rootCmd.AddCommand(signupCmd())
	rootCmd.AddCommand(loginCmd())

	if err := rootCmd.Execute(); err != nil {
		os.Exit(1)
	}
}

func checkCmd() *cobra.Command {
	var (
		staged      bool
		unstaged    bool
		branch      string
		noColor     bool
		format      string
		useBaseline bool
		compact     bool
	)

	cmd := &cobra.Command{
		Use:   "check",
		Short: "Run analyzers on changed files",
		RunE: func(cmd *cobra.Command, args []string) error {
			return runCheck(staged, unstaged, branch, noColor, format, useBaseline, compact)
		},
	}

	cmd.Flags().BoolVar(&staged, "staged", false, "Check staged changes only")
	cmd.Flags().BoolVar(&unstaged, "unstaged", false, "Check unstaged changes only")
	cmd.Flags().StringVar(&branch, "branch", "", "Check changes since branch (e.g., main)")
	cmd.Flags().BoolVar(&noColor, "no-color", false, "Disable colored output")
	cmd.Flags().StringVar(&format, "format", "terminal", "Output format: terminal, json, sarif")
	cmd.Flags().BoolVar(&useBaseline, "baseline", false, "Only report issues not in .fault-baseline.json")
	cmd.Flags().BoolVar(&compact, "compact", false, "Compact single-line output (for CI/hooks)")

	return cmd
}

func runCheck(staged, unstaged bool, branch string, noColor bool, format string, useBaseline bool, compact bool) error {
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

	// 6. Build or load repo index for cross-file analysis
	repoRoot, _ := repo.RepoRoot()
	var repoIndex *index.Index
	idx := index.NewIndex(repoRoot, cfg)
	if err := idx.BuildOrLoad(reg); err != nil {
		log.Printf("warning: could not build repo index: %v", err)
	} else {
		repoIndex = idx
	}

	// 7. Run analyzers
	analyzers := []analyzer.Analyzer{
		analyzer.NewImportAnalyzer(),
		analyzer.NewConsistencyAnalyzer(),
		analyzer.NewReferenceAnalyzer(),
		analyzer.NewTestImpactAnalyzer(),
		analyzer.NewAntiPatternAnalyzer(),
		analyzer.NewSecurityAnalyzer(),
		analyzer.NewHallucinationAnalyzer(),
	}
	runner := analyzer.NewRunner(cfg, analyzers)

	result := runner.Run(repoRoot, diff, parsedFiles, repoIndex)

	// Set branch info if available
	if currentBranch, err := repo.CurrentBranch(); err == nil {
		result.Branch = currentBranch
	}

	// 8. LLM-assisted analysis (optional)
	if cfg.LLM.Enabled {
		runLLMAnalysis(cfg, diff, parsedFiles, result, repoRoot)
	}

	// 8b. Filter baseline issues if requested
	if useBaseline {
		baseline, err := analyzer.LoadBaseline(filepath.Join(repoRoot, ".fault-baseline.json"))
		if err == nil {
			result.Issues = analyzer.FilterBaseline(result.Issues, baseline)
		}
	}

	// 9. Report results
	rep := selectReporter(format, noColor, compact)
	if tr, ok := rep.(*reporter.TerminalReporter); ok && diff != nil {
		tr.SetDiff(diff)
	}
	exitCode := rep.Report(result, cfg.BlockOn)

	if exitCode != 0 {
		os.Exit(exitCode)
	}

	return nil
}

// runLLMAnalysis performs LLM-assisted confidence scoring and spec comparison.
func runLLMAnalysis(cfg *config.Config, diff *git.Diff, parsedFiles map[string]*parser.ParsedFile, result *analyzer.AnalysisResult, repoRoot string) {
	ctx := context.Background()
	client := llm.NewClient(cfg.LLM.APIKey)

	// Confidence scoring
	confidenceResult, err := llm.ScoreConfidence(ctx, client, diff, parsedFiles)
	if err != nil {
		log.Printf("warning: LLM confidence scoring failed: %v", err)
	} else {
		result.Confidence = &analyzer.Confidence{
			Score:   confidenceResult.Overall,
			Factors: []string{confidenceResult.Reasoning},
		}
	}

	// Spec comparison (if spec file is configured)
	if cfg.LLM.SpecFile != "" {
		specPath := cfg.LLM.SpecFile
		if !filepath.IsAbs(specPath) {
			specPath = filepath.Join(repoRoot, specPath)
		}

		specContent, err := os.ReadFile(specPath)
		if err != nil {
			log.Printf("warning: could not read spec file %s: %v", specPath, err)
			return
		}

		specResult, err := llm.CompareSpec(ctx, client, string(specContent), diff)
		if err != nil {
			log.Printf("warning: LLM spec comparison failed: %v", err)
			return
		}

		// Convert spec results to issues and append
		specIssues := llm.SpecResultToIssues(specResult)
		result.Issues = append(result.Issues, specIssues...)
	}
}

// selectReporter returns the appropriate reporter based on the format flag.
func selectReporter(format string, noColor bool, compact bool) reporter.Reporter {
	switch format {
	case "json":
		return reporter.NewJSONReporter()
	case "sarif":
		return reporter.NewSARIFReporter()
	default:
		return reporter.NewTerminalReporter(noColor, compact)
	}
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
  security: true
  hallucination: true
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

func signupCmd() *cobra.Command {
	var (
		email  string
		apiURL string
	)

	cmd := &cobra.Command{
		Use:   "signup",
		Short: "Sign up for Fault Pro and get an API key",
		Long:  "Creates a Fault account and returns an API key for LLM-powered analysis features.",
		RunE: func(cmd *cobra.Command, args []string) error {
			return runSignup(email, apiURL)
		},
	}

	cmd.Flags().StringVar(&email, "email", "", "Email address for your account (required)")
	cmd.MarkFlagRequired("email")
	cmd.Flags().StringVar(&apiURL, "api-url", "https://fault.jabbey.io", "Fault cloud API URL")

	return cmd
}

func loginCmd() *cobra.Command {
	cmd := signupCmd()
	cmd.Use = "login"
	cmd.Short = "Log in to Fault and get a new API key"
	cmd.Long = "Generates a new API key for your Fault account. Creates an account if one doesn't exist."
	return cmd
}

func runSignup(email, apiURL string) error {
	reqBody, err := json.Marshal(map[string]string{"email": email})
	if err != nil {
		return fmt.Errorf("encoding request: %w", err)
	}

	url := strings.TrimRight(apiURL, "/") + "/api/v1/signup"
	resp, err := http.Post(url, "application/json", bytes.NewReader(reqBody))
	if err != nil {
		return fmt.Errorf("connecting to Fault API: %w", err)
	}
	defer resp.Body.Close()

	var result struct {
		APIKey string `json:"api_key"`
		Email  string `json:"email"`
		Error  string `json:"error"`
	}
	if err := json.NewDecoder(resp.Body).Decode(&result); err != nil {
		return fmt.Errorf("parsing API response: %w", err)
	}

	if result.Error != "" {
		return fmt.Errorf("signup failed: %s", result.Error)
	}

	fmt.Printf(`Account created for %s

Your API key:
  %s

Save this key — it won't be shown again.

To use LLM-powered analysis, add to your shell profile:
  export FAULT_API_KEY=%s

Then enable in .fault.yaml:
  llm:
    enabled: true
`, result.Email, result.APIKey, result.APIKey)

	return nil
}

func baselineCmd() *cobra.Command {
	var (
		staged   bool
		unstaged bool
		branch   string
	)

	cmd := &cobra.Command{
		Use:   "baseline",
		Short: "Capture current issues as known baseline",
		Long:  "Runs analysis and saves all current issues to .fault-baseline.json. Future runs with --baseline will only report NEW issues not in the baseline.",
		RunE: func(cmd *cobra.Command, args []string) error {
			cwd, err := os.Getwd()
			if err != nil {
				return fmt.Errorf("getting working directory: %w", err)
			}

			cfg, err := config.Load(cwd)
			if err != nil {
				return fmt.Errorf("loading config: %w", err)
			}

			repo, err := git.NewRepo(cwd)
			if err != nil {
				return fmt.Errorf("opening repo: %w", err)
			}

			diff, err := getDiff(repo, staged, unstaged, branch)
			if err != nil {
				return fmt.Errorf("getting diff: %w", err)
			}

			if len(diff.Files) == 0 {
				fmt.Println("No changes detected")
				return nil
			}

			reg := parser.NewRegistry()
			reg.Register(parser.NewGoParser())
			reg.Register(parser.NewTypeScriptParser())
			reg.Register(parser.NewPythonParser())

			parsedFiles := parseChangedFiles(repo, diff, reg, cfg)

			repoRoot, _ := repo.RepoRoot()
			var repoIndex *index.Index
			idx := index.NewIndex(repoRoot, cfg)
			if err := idx.BuildOrLoad(reg); err != nil {
				log.Printf("warning: could not build repo index: %v", err)
			} else {
				repoIndex = idx
			}

			analyzers := []analyzer.Analyzer{
				analyzer.NewImportAnalyzer(),
				analyzer.NewConsistencyAnalyzer(),
				analyzer.NewReferenceAnalyzer(),
				analyzer.NewTestImpactAnalyzer(),
				analyzer.NewAntiPatternAnalyzer(),
				analyzer.NewSecurityAnalyzer(),
				analyzer.NewHallucinationAnalyzer(),
			}
			runner := analyzer.NewRunner(cfg, analyzers)

			result := runner.Run(repoRoot, diff, parsedFiles, repoIndex)

			baselinePath := filepath.Join(repoRoot, ".fault-baseline.json")
			if err := analyzer.SaveBaseline(baselinePath, result); err != nil {
				return fmt.Errorf("saving baseline: %w", err)
			}

			fmt.Printf("Baseline saved: %d issues suppressed\n", len(result.Issues))
			fmt.Printf("File: %s\n", baselinePath)
			return nil
		},
	}

	cmd.Flags().BoolVar(&staged, "staged", false, "Check staged changes only")
	cmd.Flags().BoolVar(&unstaged, "unstaged", false, "Check unstaged changes only")
	cmd.Flags().StringVar(&branch, "branch", "", "Check changes since branch (e.g., main)")

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
