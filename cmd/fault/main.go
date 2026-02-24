package main

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"log"
	"net/http"
	"os"
	"os/signal"
	"path/filepath"
	"strings"
	"syscall"
	"time"

	"github.com/spf13/cobra"

	"github.com/joeabbey/fault/pkg/analyzer"
	"github.com/joeabbey/fault/pkg/cloudapi"
	"github.com/joeabbey/fault/pkg/config"
	"github.com/joeabbey/fault/pkg/fixer"
	"github.com/joeabbey/fault/pkg/git"
	"github.com/joeabbey/fault/pkg/index"
	"github.com/joeabbey/fault/pkg/llm"
	"github.com/joeabbey/fault/pkg/parser"
	"github.com/joeabbey/fault/pkg/reporter"
	"github.com/joeabbey/fault/pkg/spec"
	"github.com/joeabbey/fault/pkg/watcher"
)

// Version is set at build time via -ldflags
var Version = "0.7.0-dev"

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
	rootCmd.AddCommand(fixCmd())
	rootCmd.AddCommand(watchCmd())
	rootCmd.AddCommand(auditCmd())
	rootCmd.AddCommand(configCmd())

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
		specFile    string
		compliance  string
	)

	cmd := &cobra.Command{
		Use:   "check",
		Short: "Run analyzers on changed files",
		RunE: func(cmd *cobra.Command, args []string) error {
			return runCheck(staged, unstaged, branch, noColor, format, useBaseline, compact, specFile, compliance)
		},
	}

	cmd.Flags().BoolVar(&staged, "staged", false, "Check staged changes only")
	cmd.Flags().BoolVar(&unstaged, "unstaged", false, "Check unstaged changes only")
	cmd.Flags().StringVar(&branch, "branch", "", "Check changes since branch (e.g., main)")
	cmd.Flags().BoolVar(&noColor, "no-color", false, "Disable colored output")
	cmd.Flags().StringVar(&format, "format", "terminal", "Output format: terminal, json, sarif, github")
	cmd.Flags().BoolVar(&useBaseline, "baseline", false, "Only report issues not in .fault-baseline.json")
	cmd.Flags().BoolVar(&compact, "compact", false, "Compact single-line output (for CI/hooks)")
	cmd.Flags().StringVar(&specFile, "spec", "", "Path to .fault-spec.yaml for requirements validation")
	cmd.Flags().StringVar(&compliance, "compliance", "", "Compliance pack to check (e.g., owasp-top-10-2021, cwe-top-25-2023)")

	return cmd
}

func runCheck(staged, unstaged bool, branch string, noColor bool, format string, useBaseline bool, compact bool, specFile string, compliance string) error {
	// 1. Load config
	cwd, err := os.Getwd()
	if err != nil {
		return fmt.Errorf("getting working directory: %w", err)
	}

	cfg, err := config.Load(cwd)
	if err != nil {
		return fmt.Errorf("loading config: %w", err)
	}

	// Override spec file from CLI flag
	if specFile != "" {
		cfg.LLM.SpecFile = specFile
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
	registerAllParsers(reg)

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
		analyzer.NewComplexityAnalyzer(),
		analyzer.NewConcurrencyAnalyzer(),
		analyzer.NewResourceAnalyzer(),
		analyzer.NewMigrationAnalyzer(),
		analyzer.NewDocDriftAnalyzer(),
		analyzer.NewErrorHandlingAnalyzer(),
		analyzer.NewDepGraphAnalyzer(),
		analyzer.NewDeadCodeAnalyzer(),
		analyzer.NewSpecAnalyzer(),
	}

	// Register custom rule analyzer if configured
	if len(cfg.CustomRules) > 0 {
		rules := make([]analyzer.CustomRuleConfig, len(cfg.CustomRules))
		for i, r := range cfg.CustomRules {
			rules[i] = analyzer.CustomRuleConfig{
				ID: r.ID, Pattern: r.Pattern, Files: r.Files,
				Severity: r.Severity, Message: r.Message,
			}
		}
		analyzers = append(analyzers, analyzer.NewCustomRuleAnalyzer(rules))
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

	// 8a. Filter inline suppressed issues
	fileLines := loadFileLines(repoRoot, result.Issues)
	result.Issues = analyzer.FilterSuppressed(result.Issues, fileLines)

	// 8b. Filter baseline issues if requested
	if useBaseline {
		baseline, err := analyzer.LoadBaseline(filepath.Join(repoRoot, ".fault-baseline.json"))
		if err == nil {
			result.Issues = analyzer.FilterBaseline(result.Issues, baseline)
		}
	}

	// 8c. Compliance checking
	complianceFailed := false
	compliancePacks := collectCompliancePacks(compliance, cfg)
	for _, packID := range compliancePacks {
		compResult, err := analyzer.CheckCompliance(packID, result.Issues)
		if err != nil {
			log.Printf("warning: compliance check failed for %s: %v", packID, err)
			continue
		}
		printComplianceResult(compResult)
		if !compResult.Compliant && cfg.Compliance.FailOnViolation {
			complianceFailed = true
		}
	}

	// 9. Report results
	rep := selectReporter(format, noColor, compact)
	if tr, ok := rep.(*reporter.TerminalReporter); ok && diff != nil {
		tr.SetDiff(diff)
	}
	exitCode := rep.Report(result, cfg.BlockOn)

	if complianceFailed && exitCode == 0 {
		exitCode = 1
	}

	if exitCode != 0 {
		os.Exit(exitCode)
	}

	return nil
}

// runLLMAnalysis performs LLM-assisted confidence scoring and spec comparison.
func runLLMAnalysis(cfg *config.Config, diff *git.Diff, parsedFiles map[string]*parser.ParsedFile, result *analyzer.AnalysisResult, repoRoot string) {
	ctx := context.Background()
	client := cloudapi.New(cfg.LLM.APIURL, cfg.LLM.APIKey)
	diffSummary := llm.BuildDiffSummary(diff)

	// Confidence scoring
	confidenceResult, err := client.AnalyzeConfidence(ctx, diffSummary)
	if err != nil {
		log.Printf("warning: LLM confidence scoring failed: %v", err)
	} else {
		result.Confidence = &analyzer.Confidence{
			Score:   confidenceResult.Overall,
			Factors: []string{confidenceResult.Reasoning},
			PerFile: confidenceResult.PerFile,
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

		// Try structured spec (YAML with requirements) first
		if _, parseErr := spec.ParseSpec(specContent); parseErr == nil {
			specTitle := filepath.Base(specPath)
			structuredResult, err := client.AnalyzeSpecStructured(ctx, string(specContent), diffSummary, specTitle)
			if err != nil {
				log.Printf("warning: LLM structured spec comparison failed: %v", err)
				return
			}
			specIssues := llm.StructuredSpecResultToIssues(structuredResult)
			result.Issues = append(result.Issues, specIssues...)
			return
		}

		// Fall back to unstructured spec comparison
		specResult, err := client.AnalyzeSpec(ctx, string(specContent), diffSummary)
		if err != nil {
			log.Printf("warning: LLM spec comparison failed: %v", err)
			return
		}

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
	case "github":
		return reporter.NewGitHubReporter()
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

func loadFileLines(repoRoot string, issues []analyzer.Issue) map[string][]string {
	files := make(map[string]struct{})
	for _, issue := range issues {
		if issue.File == "" || issue.Line <= 0 {
			continue
		}
		files[issue.File] = struct{}{}
	}
	if len(files) == 0 {
		return nil
	}

	fileLines := make(map[string][]string, len(files))
	for file := range files {
		absPath := filepath.Join(repoRoot, file)
		data, err := os.ReadFile(absPath)
		if err != nil {
			continue
		}

		// Normalize CRLF to LF for stable line indexing.
		content := strings.ReplaceAll(string(data), "\r\n", "\n")
		fileLines[file] = strings.Split(content, "\n")
	}

	return fileLines
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
  api_url: "https://fault.jabbey.io"
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

			// Best-effort: ensure cache dir isn't accidentally committed.
			if repo, err := git.NewRepo("."); err == nil {
				if repoRoot, err := repo.RepoRoot(); err == nil {
					if err := index.EnsureGitignore(repoRoot); err != nil {
						log.Printf("warning: could not update .gitignore: %v", err)
					}
				}
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

			// Best-effort: ensure cache dir isn't accidentally committed.
			if err := index.EnsureGitignore(repoRoot); err != nil {
				log.Printf("warning: could not update .gitignore: %v", err)
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

If you're using a self-hosted Fault Cloud, also set:
  export FAULT_API_URL=http://localhost:8082

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
			registerAllParsers(reg)

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
				analyzer.NewComplexityAnalyzer(),
				analyzer.NewConcurrencyAnalyzer(),
				analyzer.NewResourceAnalyzer(),
				analyzer.NewMigrationAnalyzer(),
				analyzer.NewDocDriftAnalyzer(),
				analyzer.NewErrorHandlingAnalyzer(),
				analyzer.NewDepGraphAnalyzer(),
				analyzer.NewDeadCodeAnalyzer(),
				analyzer.NewSpecAnalyzer(),
			}

			// Register custom rule analyzer if configured
			if len(cfg.CustomRules) > 0 {
				rules := make([]analyzer.CustomRuleConfig, len(cfg.CustomRules))
				for i, r := range cfg.CustomRules {
					rules[i] = analyzer.CustomRuleConfig{
						ID: r.ID, Pattern: r.Pattern, Files: r.Files,
						Severity: r.Severity, Message: r.Message,
					}
				}
				analyzers = append(analyzers, analyzer.NewCustomRuleAnalyzer(rules))
			}

			runner := analyzer.NewRunner(cfg, analyzers)

			result := runner.Run(repoRoot, diff, parsedFiles, repoIndex)
			fileLines := loadFileLines(repoRoot, result.Issues)
			result.Issues = analyzer.FilterSuppressed(result.Issues, fileLines)

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

func fixCmd() *cobra.Command {
	var (
		dryRun   bool
		staged   bool
		unstaged bool
		branch   string
	)

	cmd := &cobra.Command{
		Use:   "fix",
		Short: "Auto-fix issues in changed files",
		Long:  "Runs analysis, then automatically generates and applies fixes for issues that have known fix strategies. Use --dry-run to preview changes without modifying files.",
		RunE: func(cmd *cobra.Command, args []string) error {
			return runFix(dryRun, staged, unstaged, branch)
		},
	}

	cmd.Flags().BoolVar(&dryRun, "dry-run", false, "Preview fixes without applying them")
	cmd.Flags().BoolVar(&staged, "staged", false, "Fix staged changes only")
	cmd.Flags().BoolVar(&unstaged, "unstaged", false, "Fix unstaged changes only")
	cmd.Flags().StringVar(&branch, "branch", "", "Fix changes since branch (e.g., main)")

	return cmd
}

func runFix(dryRun, staged, unstaged bool, branch string) error {
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
	registerAllParsers(reg)

	// 5. Parse changed files
	parsedFiles := parseChangedFiles(repo, diff, reg, cfg)

	// 6. Build or load repo index
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
		analyzer.NewComplexityAnalyzer(),
		analyzer.NewConcurrencyAnalyzer(),
		analyzer.NewResourceAnalyzer(),
		analyzer.NewMigrationAnalyzer(),
		analyzer.NewDocDriftAnalyzer(),
		analyzer.NewErrorHandlingAnalyzer(),
		analyzer.NewDepGraphAnalyzer(),
		analyzer.NewDeadCodeAnalyzer(),
		analyzer.NewSpecAnalyzer(),
	}

	// Register custom rule analyzer if configured
	if len(cfg.CustomRules) > 0 {
		rules := make([]analyzer.CustomRuleConfig, len(cfg.CustomRules))
		for i, r := range cfg.CustomRules {
			rules[i] = analyzer.CustomRuleConfig{
				ID: r.ID, Pattern: r.Pattern, Files: r.Files,
				Severity: r.Severity, Message: r.Message,
			}
		}
		analyzers = append(analyzers, analyzer.NewCustomRuleAnalyzer(rules))
	}

	runner := analyzer.NewRunner(cfg, analyzers)
	result := runner.Run(repoRoot, diff, parsedFiles, repoIndex)
	fileLines := loadFileLines(repoRoot, result.Issues)
	result.Issues = analyzer.FilterSuppressed(result.Issues, fileLines)

	// 8. Build fixer registry
	fixRegistry := fixer.NewRegistry()
	fixRegistry.Register(fixer.NewImportFixer())
	fixRegistry.Register(fixer.NewSecurityFixer())
	fixRegistry.Register(fixer.NewPatternFixer())
	fixRegistry.Register(fixer.NewJavaFixer())
	fixRegistry.Register(fixer.NewRustFixer())
	fixRegistry.Register(fixer.NewTypeScriptFixer())
	fixRegistry.Register(fixer.NewKotlinFixer())
	fixRegistry.Register(fixer.NewRubyFixer())
	fixRegistry.Register(fixer.NewPHPFixer())
	fixRegistry.Register(fixer.NewCSharpFixer())
	fixRegistry.Register(fixer.NewSwiftFixer())
	fixRegistry.Register(fixer.NewCFixer())
	fixRegistry.Register(fixer.NewCppFixer())
	fixRegistry.Register(fixer.NewPerlFixer())
	fixRegistry.Register(fixer.NewPowerShellFixer())
	fixRegistry.Register(fixer.NewGroovyFixer())
	fixRegistry.Register(fixer.NewScalaFixer())
	fixRegistry.Register(fixer.NewRFixer())
	fixRegistry.Register(fixer.NewObjCFixer())
	fixRegistry.Register(fixer.NewDartFixer())
	fixRegistry.Register(fixer.NewBashFixer())
	fixRegistry.Register(fixer.NewElixirFixer())
	fixRegistry.Register(fixer.NewLuaFixer())
	fixRegistry.Register(fixer.NewZigFixer())
	fixRegistry.Register(fixer.NewNimFixer())
	fixRegistry.Register(fixer.NewCrystalFixer())
	fixRegistry.Register(fixer.NewVlangFixer())
	fixRegistry.Register(fixer.NewDlangFixer())
	fixRegistry.Register(fixer.NewHaskellFixer())
	fixRegistry.Register(fixer.NewClojureFixer())
	fixRegistry.Register(fixer.NewErlangFixer())
	fixRegistry.Register(fixer.NewFSharpFixer())
	fixRegistry.Register(fixer.NewOCamlFixer())
	fixRegistry.Register(fixer.NewJuliaFixer())
	fixRegistry.Register(fixer.NewSolidityFixer())
	fixRegistry.Register(fixer.NewTerraformFixer())
	fixRegistry.Register(fixer.NewFortranFixer())
	fixRegistry.Register(fixer.NewVisualBasicFixer())
	fixRegistry.Register(fixer.NewCOBOLFixer())
	fixRegistry.Register(fixer.NewAdaFixer())
	fixRegistry.Register(fixer.NewPascalFixer())
	fixRegistry.Register(fixer.NewSQLFixer())

	// 9. Generate fixes for fixable issues
	var fixes []*fixer.Fix
	fixableCount := 0
	for _, issue := range result.Issues {
		if issue.FixID == "" {
			continue
		}
		fixableCount++
		fix := fixRegistry.GenerateFix(issue, repoRoot)
		if fix != nil {
			fixes = append(fixes, fix)
		}
	}

	if len(fixes) == 0 {
		if fixableCount == 0 {
			fmt.Printf("Found %d issues, none are auto-fixable\n", len(result.Issues))
		} else {
			fmt.Printf("Found %d issues (%d fixable), but could not generate fixes\n", len(result.Issues), fixableCount)
		}
		return nil
	}

	// 10. Apply or preview fixes
	if dryRun {
		fmt.Printf("Found %d issues, %d fixes available (dry run):\n\n", len(result.Issues), len(fixes))
		for _, fix := range fixes {
			fmt.Println(fixer.DryRun(fix))
		}
		return nil
	}

	fmt.Printf("Found %d issues, applying %d fixes...\n\n", len(result.Issues), len(fixes))
	applied := 0
	for _, fix := range fixes {
		if err := fixer.Apply(fix, repoRoot); err != nil {
			fmt.Printf("  FAIL %s:%d - %s (%v)\n", fix.File, fix.Edits[0].Line, fix.Description, err)
		} else {
			fmt.Printf("  OK   %s - %s\n", fix.File, fix.Description)
			applied++
		}
	}

	fmt.Printf("\nApplied %d/%d fixes\n", applied, len(fixes))

	// 11. Re-run analysis to show remaining issues
	if applied > 0 {
		diff2, err := getDiff(repo, staged, unstaged, branch)
		if err != nil {
			return nil // best effort
		}
		if len(diff2.Files) == 0 {
			fmt.Println("\nAll issues resolved!")
			return nil
		}

		parsedFiles2 := parseChangedFiles(repo, diff2, reg, cfg)
		result2 := runner.Run(repoRoot, diff2, parsedFiles2, repoIndex)
		fileLines2 := loadFileLines(repoRoot, result2.Issues)
		result2.Issues = analyzer.FilterSuppressed(result2.Issues, fileLines2)

		remaining := len(result2.Issues)
		if remaining == 0 {
			fmt.Println("\nAll issues resolved!")
		} else {
			fmt.Printf("\n%d issues remaining:\n", remaining)
			rep := reporter.NewTerminalReporter(false, false)
			rep.Report(result2, "")
		}
	}

	return nil
}

// registerAllParsers registers all 42 language parsers in the registry.
func registerAllParsers(reg *parser.Registry) {
	// Original 10
	reg.Register(parser.NewGoParser())
	reg.Register(parser.NewTypeScriptParser())
	reg.Register(parser.NewPythonParser())
	reg.Register(parser.NewJavaParser())
	reg.Register(parser.NewRustParser())
	reg.Register(parser.NewRubyParser())
	reg.Register(parser.NewKotlinParser())
	reg.Register(parser.NewCSharpParser())
	reg.Register(parser.NewPHPParser())
	reg.Register(parser.NewSwiftParser())
	// v3 expansion
	reg.Register(parser.NewCParser())
	reg.Register(parser.NewCppParser())
	reg.Register(parser.NewObjCParser())
	reg.Register(parser.NewBashParser())
	reg.Register(parser.NewSQLParser())
	reg.Register(parser.NewDartParser())
	reg.Register(parser.NewScalaParser())
	reg.Register(parser.NewRParser())
	reg.Register(parser.NewElixirParser())
	reg.Register(parser.NewLuaParser())
	reg.Register(parser.NewPerlParser())
	reg.Register(parser.NewPowershellParser())
	reg.Register(parser.NewGroovyParser())
	// v4 modern systems
	reg.Register(parser.NewZigParser())
	reg.Register(parser.NewNimParser())
	reg.Register(parser.NewCrystalParser())
	reg.Register(parser.NewVlangParser())
	reg.Register(parser.NewDlangParser())
	// v4 functional
	reg.Register(parser.NewHaskellParser())
	reg.Register(parser.NewClojureParser())
	reg.Register(parser.NewErlangParser())
	reg.Register(parser.NewFsharpParser())
	reg.Register(parser.NewOcamlParser())
	// v4 domain
	reg.Register(parser.NewJuliaParser())
	reg.Register(parser.NewFortranParser())
	reg.Register(parser.NewSolidityParser())
	reg.Register(parser.NewTerraformParser())
	reg.Register(parser.NewProtobufParser())
	// v4 legacy
	reg.Register(parser.NewVisualBasicParser())
	reg.Register(parser.NewCobolParser())
	reg.Register(parser.NewAdaParser())
	reg.Register(parser.NewPascalParser())
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

func watchCmd() *cobra.Command {
	var noColor bool

	cmd := &cobra.Command{
		Use:   "watch",
		Short: "Watch for file changes and run analysis continuously",
		Long:  "Monitors the working tree for file changes and automatically runs Fault analysis. Provides near-instant feedback as you edit.",
		RunE: func(cmd *cobra.Command, args []string) error {
			return runWatch(noColor)
		},
	}

	cmd.Flags().BoolVar(&noColor, "no-color", false, "Disable colored output")
	return cmd
}

func runWatch(noColor bool) error {
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

	// 3. Set up parser registry
	reg := parser.NewRegistry()
	registerAllParsers(reg)

	// 4. Build or load the index
	repoRoot, _ := repo.RepoRoot()
	idx := index.NewIndex(repoRoot, cfg)
	if err := idx.BuildOrLoad(reg); err != nil {
		log.Printf("warning: could not build repo index: %v", err)
	}

	// 5. Create analyzers
	analyzers := []analyzer.Analyzer{
		analyzer.NewImportAnalyzer(),
		analyzer.NewConsistencyAnalyzer(),
		analyzer.NewReferenceAnalyzer(),
		analyzer.NewTestImpactAnalyzer(),
		analyzer.NewAntiPatternAnalyzer(),
		analyzer.NewSecurityAnalyzer(),
		analyzer.NewHallucinationAnalyzer(),
		analyzer.NewComplexityAnalyzer(),
		analyzer.NewConcurrencyAnalyzer(),
		analyzer.NewResourceAnalyzer(),
		analyzer.NewMigrationAnalyzer(),
		analyzer.NewDocDriftAnalyzer(),
		analyzer.NewErrorHandlingAnalyzer(),
		analyzer.NewDepGraphAnalyzer(),
		analyzer.NewDeadCodeAnalyzer(),
		analyzer.NewSpecAnalyzer(),
	}

	// Register custom rule analyzer if configured
	if len(cfg.CustomRules) > 0 {
		rules := make([]analyzer.CustomRuleConfig, len(cfg.CustomRules))
		for i, r := range cfg.CustomRules {
			rules[i] = analyzer.CustomRuleConfig{
				ID: r.ID, Pattern: r.Pattern, Files: r.Files,
				Severity: r.Severity, Message: r.Message,
			}
		}
		analyzers = append(analyzers, analyzer.NewCustomRuleAnalyzer(rules))
	}

	// 6. Parse watch config
	pollInterval := 500 * time.Millisecond
	debounce := 200 * time.Millisecond

	if cfg.Watch.PollInterval != "" {
		if d, err := time.ParseDuration(cfg.Watch.PollInterval); err == nil {
			pollInterval = d
		}
	}
	if cfg.Watch.Debounce != "" {
		if d, err := time.ParseDuration(cfg.Watch.Debounce); err == nil {
			debounce = d
		}
	}

	// 7. Create display
	display := watcher.NewDisplay(os.Stdout, noColor)

	// 8. Track stats
	totalRuns := 0
	totalIssues := 0
	watchStart := time.Now()

	// 9. Create watcher
	onChange := func(r watcher.RunResult) {
		totalRuns++
		totalIssues += len(r.Result.Issues)
		display.Clear()
		display.RenderHeader(repoRoot, Version)
		display.RenderResult(r)
		display.RenderWaiting()
	}

	onError := func(err error) {
		log.Printf("watch error: %v", err)
	}

	opts := watcher.Options{
		PollInterval: pollInterval,
		Debounce:     debounce,
	}

	w := watcher.New(repoRoot, repo, cfg, reg, analyzers, idx, opts, onChange, onError)

	// 10. Signal handling
	ctx, cancel := signal.NotifyContext(context.Background(), os.Interrupt, syscall.SIGTERM)
	defer cancel()

	// 11. Show initial state
	display.RenderHeader(repoRoot, Version)
	display.RenderWaiting()

	// 12. Run watcher (blocks until ctx cancelled)
	err = w.Watch(ctx)

	// 13. Show exit summary
	display.RenderExit(totalRuns, totalIssues, time.Since(watchStart))

	return err
}

func auditCmd() *cobra.Command {
	var (
		from        string
		to          string
		commits     int
		since       string
		noColor     bool
		format      string
		useBaseline bool
		compact     bool
		upload      bool
		blockOn     string
	)

	cmd := &cobra.Command{
		Use:   "audit",
		Short: "Run post-merge regression detection on committed changes",
		Long: `Audit analyzes already-committed changes between two refs (commits, tags, or branches).
Unlike 'check' which looks at uncommitted changes, 'audit' examines merged code to detect
regressions introduced by recent commits. Ideal for CI pipelines and post-merge quality gates.`,
		RunE: func(cmd *cobra.Command, args []string) error {
			return runAudit(from, to, commits, since, noColor, format, useBaseline, compact, upload, blockOn)
		},
	}

	cmd.Flags().StringVar(&from, "from", "", "Base ref to compare from (commit SHA, tag, or branch)")
	cmd.Flags().StringVar(&to, "to", "HEAD", "Head ref to compare to (default: HEAD)")
	cmd.Flags().IntVar(&commits, "commits", 0, "Audit the last N commits (shorthand for --from HEAD~N)")
	cmd.Flags().StringVar(&since, "since", "", "Audit commits since duration (e.g., 24h, 7d)")
	cmd.Flags().BoolVar(&noColor, "no-color", false, "Disable colored output")
	cmd.Flags().StringVar(&format, "format", "terminal", "Output format: terminal, json, sarif, github")
	cmd.Flags().BoolVar(&useBaseline, "baseline", false, "Only report issues not in .fault-baseline.json")
	cmd.Flags().BoolVar(&compact, "compact", false, "Compact single-line output (for CI/hooks)")
	cmd.Flags().BoolVar(&upload, "upload", false, "Upload results to Fault Cloud")
	cmd.Flags().StringVar(&blockOn, "block", "", "Override block_on level (error, warning)")

	return cmd
}

func runAudit(from, to string, commits int, since string, noColor bool, format string, useBaseline bool, compact bool, upload bool, blockOn string) error {
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

	repoRoot, _ := repo.RepoRoot()

	// 3. Resolve the base ref
	baseRef, err := resolveBaseRef(repo, from, commits, since)
	if err != nil {
		return fmt.Errorf("resolving base ref: %w", err)
	}

	// 4. Get diff between refs
	diff, err := repo.RefDiff(baseRef, to)
	if err != nil {
		return fmt.Errorf("getting ref diff: %w", err)
	}

	if len(diff.Files) == 0 {
		fmt.Println("No changes detected between refs")
		return nil
	}

	// 5. Set up parser registry
	reg := parser.NewRegistry()
	registerAllParsers(reg)

	// 6. Parse changed files
	parsedFiles := parseChangedFiles(repo, diff, reg, cfg)

	// 7. Build or load repo index
	var repoIndex *index.Index
	idx := index.NewIndex(repoRoot, cfg)
	if err := idx.BuildOrLoad(reg); err != nil {
		log.Printf("warning: could not build repo index: %v", err)
	} else {
		repoIndex = idx
	}

	// 8. Run analyzers
	analyzers := []analyzer.Analyzer{
		analyzer.NewImportAnalyzer(),
		analyzer.NewConsistencyAnalyzer(),
		analyzer.NewReferenceAnalyzer(),
		analyzer.NewTestImpactAnalyzer(),
		analyzer.NewAntiPatternAnalyzer(),
		analyzer.NewSecurityAnalyzer(),
		analyzer.NewHallucinationAnalyzer(),
		analyzer.NewComplexityAnalyzer(),
		analyzer.NewConcurrencyAnalyzer(),
		analyzer.NewResourceAnalyzer(),
		analyzer.NewMigrationAnalyzer(),
		analyzer.NewDocDriftAnalyzer(),
		analyzer.NewErrorHandlingAnalyzer(),
		analyzer.NewDepGraphAnalyzer(),
		analyzer.NewDeadCodeAnalyzer(),
		analyzer.NewSpecAnalyzer(),
	}

	// Register custom rule analyzer if configured
	if len(cfg.CustomRules) > 0 {
		rules := make([]analyzer.CustomRuleConfig, len(cfg.CustomRules))
		for i, r := range cfg.CustomRules {
			rules[i] = analyzer.CustomRuleConfig{
				ID: r.ID, Pattern: r.Pattern, Files: r.Files,
				Severity: r.Severity, Message: r.Message,
			}
		}
		analyzers = append(analyzers, analyzer.NewCustomRuleAnalyzer(rules))
	}

	runner := analyzer.NewRunner(cfg, analyzers)

	result := runner.Run(repoRoot, diff, parsedFiles, repoIndex)

	// Set branch and commit range info
	if currentBranch, err := repo.CurrentBranch(); err == nil {
		result.Branch = currentBranch
	}
	result.CommitRange = baseRef + "..." + to

	// 9. LLM-assisted analysis (optional)
	if cfg.LLM.Enabled {
		runLLMAnalysis(cfg, diff, parsedFiles, result, repoRoot)
	}

	// 10. Filter inline suppressed issues
	fileLines := loadFileLines(repoRoot, result.Issues)
	result.Issues = analyzer.FilterSuppressed(result.Issues, fileLines)

	// 11. Filter baseline issues if requested
	if useBaseline {
		baseline, err := analyzer.LoadBaseline(filepath.Join(repoRoot, ".fault-baseline.json"))
		if err == nil {
			result.Issues = analyzer.FilterBaseline(result.Issues, baseline)
		}
	}

	// 12. Upload results to Fault Cloud (optional)
	if upload && cfg.LLM.APIKey != "" {
		uploadAuditRun(cfg, result, repo)
	}

	// 13. Report results
	effectiveBlockOn := cfg.BlockOn
	if blockOn != "" {
		effectiveBlockOn = blockOn
	}

	rep := selectReporter(format, noColor, compact)
	if tr, ok := rep.(*reporter.TerminalReporter); ok && diff != nil {
		tr.SetDiff(diff)
	}
	exitCode := rep.Report(result, effectiveBlockOn)

	if exitCode != 0 {
		os.Exit(exitCode)
	}

	return nil
}

// resolveBaseRef determines the base ref for audit based on flags.
func resolveBaseRef(repo *git.Repo, from string, commits int, since string) (string, error) {
	if from != "" {
		return from, nil
	}

	if commits > 0 {
		return fmt.Sprintf("HEAD~%d", commits), nil
	}

	if since != "" {
		// Use git rev-list with --since to find the oldest commit in the range
		output, err := repo.CommitLog("HEAD")
		if err != nil {
			return "", fmt.Errorf("getting commit log: %w", err)
		}
		// For --since, use git directly
		_ = output
		return fmt.Sprintf("HEAD@{%s}", since), nil
	}

	return "", fmt.Errorf("one of --from, --commits, or --since is required")
}

// uploadAuditRun sends audit results to the Fault Cloud API.
func uploadAuditRun(cfg *config.Config, result *analyzer.AnalysisResult, repo *git.Repo) {
	ctx := context.Background()
	client := cloudapi.New(cfg.LLM.APIURL, cfg.LLM.APIKey)

	headSHA, _ := repo.HeadSHA()
	remoteURL := repo.RemoteURL()

	upload := &cloudapi.RunUpload{
		RepoURL:      remoteURL,
		Branch:       result.Branch,
		CommitSHA:    headSHA,
		CommitRange:  result.CommitRange,
		Duration:     result.Duration,
		FilesChanged: result.FilesChanged,
		Issues:       result.Issues,
		Summary:      result.Summary,
	}

	if result.Confidence != nil {
		score := result.Confidence.Score
		upload.ConfidenceScore = &score
		upload.Metadata = map[string]any{
			"confidence_factors":  result.Confidence.Factors,
			"confidence_per_file": result.Confidence.PerFile,
		}
	}

	if err := client.UploadRun(ctx, upload); err != nil {
		log.Printf("warning: failed to upload audit run: %v", err)
	} else {
		fmt.Println("Audit results uploaded to Fault Cloud")
	}
}

func configCmd() *cobra.Command {
	cmd := &cobra.Command{
		Use:   "config",
		Short: "Manage Fault configuration",
	}
	cmd.AddCommand(configPullCmd())
	return cmd
}

func configPullCmd() *cobra.Command {
	var orgSlug string

	cmd := &cobra.Command{
		Use:   "pull",
		Short: "Pull shared team config from Fault Cloud",
		Long:  "Fetches the organization's shared configuration and saves it as .fault.team.yaml. This config is used as a base layer under your local .fault.yaml overrides.",
		RunE: func(cmd *cobra.Command, args []string) error {
			return runConfigPull(orgSlug)
		},
	}

	cmd.Flags().StringVar(&orgSlug, "org", "", "Organization slug (required)")
	cmd.MarkFlagRequired("org")
	return cmd
}

func runConfigPull(orgSlug string) error {
	cwd, err := os.Getwd()
	if err != nil {
		return fmt.Errorf("getting working directory: %w", err)
	}

	cfg, err := config.Load(cwd)
	if err != nil {
		return fmt.Errorf("loading config: %w", err)
	}

	if cfg.LLM.APIKey == "" {
		return fmt.Errorf("FAULT_API_KEY is not set")
	}

	client := cloudapi.New(cfg.LLM.APIURL, cfg.LLM.APIKey)
	yaml, err := client.PullOrgConfig(context.Background(), orgSlug)
	if err != nil {
		return fmt.Errorf("pulling org config: %w", err)
	}

	if yaml == "" {
		fmt.Println("No team config found for this organization")
		return nil
	}

	teamConfigPath := filepath.Join(cwd, ".fault.team.yaml")
	if err := os.WriteFile(teamConfigPath, []byte(yaml), 0644); err != nil {
		return fmt.Errorf("writing team config: %w", err)
	}

	fmt.Printf("Team config saved to %s\n", teamConfigPath)
	return nil
}

// collectCompliancePacks gathers active compliance pack IDs from flag and config.
func collectCompliancePacks(flagValue string, cfg *config.Config) []string {
	seen := make(map[string]bool)
	packs := make([]string, 0)

	// Flag value takes priority
	if flagValue != "" {
		seen[flagValue] = true
		packs = append(packs, flagValue)
	}

	// Add packs from config
	for _, p := range cfg.Compliance.Packs {
		if !seen[p] {
			seen[p] = true
			packs = append(packs, p)
		}
	}

	return packs
}

// printComplianceResult prints a compliance check summary to stdout.
func printComplianceResult(result *analyzer.ComplianceResult) {
	if result.Compliant {
		fmt.Printf("Compliance [%s]: PASS (%d/%d CWEs clean)\n",
			result.PackName, result.TotalCWEs, result.TotalCWEs)
		return
	}

	fmt.Printf("Compliance [%s]: FAIL (%d/%d CWEs violated)\n",
		result.PackName, result.ViolatedCWEs, result.TotalCWEs)
	for _, v := range result.Violations {
		fmt.Printf("  %s: %s (%d occurrences)\n", v.CWEID, v.IssueID, v.Count)
	}
}
