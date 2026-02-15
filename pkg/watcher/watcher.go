package watcher

import (
	"context"
	"log"
	"os"
	"path/filepath"
	"time"

	"github.com/joeabbey/fault/pkg/analyzer"
	"github.com/joeabbey/fault/pkg/config"
	"github.com/joeabbey/fault/pkg/git"
	"github.com/joeabbey/fault/pkg/index"
	"github.com/joeabbey/fault/pkg/parser"
)

// RunResult holds the output of a single analysis cycle.
type RunResult struct {
	Result    *analyzer.AnalysisResult
	Timestamp time.Time
	Duration  time.Duration
	Trigger   string // which file triggered the run
}

// Options configures the watcher behavior.
type Options struct {
	PollInterval time.Duration // how often to check for changes (default 500ms)
	Debounce     time.Duration // wait this long after last change before running (default 200ms)
}

// Watcher monitors the git working tree and runs analysis on changes.
type Watcher struct {
	repoRoot  string
	repo      *git.Repo
	cfg       *config.Config
	reg       *parser.Registry
	analyzers []analyzer.Analyzer
	idx       *index.Index
	opts      Options

	onChange func(RunResult) // called after each analysis cycle
	onError  func(error)     // called on non-fatal errors

	// internal state
	modTimes map[string]time.Time
}

// New creates a new Watcher.
func New(repoRoot string, repo *git.Repo, cfg *config.Config, reg *parser.Registry,
	analyzers []analyzer.Analyzer, idx *index.Index, opts Options,
	onChange func(RunResult), onError func(error)) *Watcher {

	if opts.PollInterval == 0 {
		opts.PollInterval = 500 * time.Millisecond
	}
	if opts.Debounce == 0 {
		opts.Debounce = 200 * time.Millisecond
	}

	return &Watcher{
		repoRoot:  repoRoot,
		repo:      repo,
		cfg:       cfg,
		reg:       reg,
		analyzers: analyzers,
		idx:       idx,
		opts:      opts,
		onChange:   onChange,
		onError:   onError,
		modTimes:  make(map[string]time.Time),
	}
}

// Watch starts the monitoring loop. Blocks until ctx is cancelled.
func (w *Watcher) Watch(ctx context.Context) error {
	// Initial scan to record mod times
	w.scanFiles()

	// Run initial analysis if there are unstaged changes
	w.runAnalysis("")

	var lastChangeTime time.Time
	var pendingTrigger string
	debouncing := false

	for {
		select {
		case <-ctx.Done():
			return nil
		case <-time.After(w.opts.PollInterval):
		}

		changed, trigger := w.detectChanges()
		if changed {
			lastChangeTime = time.Now()
			pendingTrigger = trigger
			debouncing = true
		}

		if debouncing && time.Since(lastChangeTime) > w.opts.Debounce {
			debouncing = false
			w.runAnalysis(pendingTrigger)
			pendingTrigger = ""
		}
	}
}

// scanFiles walks the repo and records mod times of source files.
func (w *Watcher) scanFiles() {
	newTimes := make(map[string]time.Time)

	w.walkSourceFiles(func(relPath string, info os.FileInfo) {
		newTimes[relPath] = info.ModTime()
	})

	w.modTimes = newTimes
}

// detectChanges compares current file mod times against recorded state.
// Returns true if changes were found, and the path of the first changed file.
func (w *Watcher) detectChanges() (bool, string) {
	changed := false
	trigger := ""

	currentFiles := make(map[string]time.Time)

	w.walkSourceFiles(func(relPath string, info os.FileInfo) {
		currentFiles[relPath] = info.ModTime()

		prevTime, exists := w.modTimes[relPath]
		if !exists || !info.ModTime().Equal(prevTime) {
			changed = true
			if trigger == "" {
				trigger = relPath
			}
		}
	})

	// Check for deleted files
	for path := range w.modTimes {
		if _, exists := currentFiles[path]; !exists {
			changed = true
			if trigger == "" {
				trigger = path
			}
		}
	}

	// Update mod times to current state
	w.modTimes = currentFiles

	return changed, trigger
}

// walkSourceFiles walks the repo recursively, calling fn for each source file
// that is not ignored.
func (w *Watcher) walkSourceFiles(fn func(relPath string, info os.FileInfo)) {
	w.walkDir(w.repoRoot, fn)
}

func (w *Watcher) walkDir(dir string, fn func(relPath string, info os.FileInfo)) {
	entries, err := os.ReadDir(dir)
	if err != nil {
		return
	}

	for _, entry := range entries {
		absPath := filepath.Join(dir, entry.Name())
		relPath, err := filepath.Rel(w.repoRoot, absPath)
		if err != nil {
			continue
		}

		if entry.IsDir() {
			name := entry.Name()
			// Skip .git and cache directories
			if name == ".git" || name == ".fault-cache" {
				continue
			}
			// Skip ignored directories
			if w.cfg != nil && w.cfg.IsIgnored(relPath+"/") {
				continue
			}
			w.walkDir(absPath, fn)
			continue
		}

		// Skip non-source files
		if parser.DetectLanguage(relPath) == "" {
			continue
		}

		// Skip ignored files
		if w.cfg != nil && w.cfg.IsIgnored(relPath) {
			continue
		}

		info, err := entry.Info()
		if err != nil {
			continue
		}

		fn(relPath, info)
	}
}

// runAnalysis performs a full analysis cycle.
func (w *Watcher) runAnalysis(trigger string) {
	start := time.Now()

	// Get the diff (auto = staged if any, else unstaged)
	diff, err := w.repo.AutoDiff()
	if err != nil {
		if w.onError != nil {
			w.onError(err)
		}
		return
	}

	// Parse changed files
	parsedFiles := w.parseChangedFiles(diff)

	// Run analyzers
	runner := analyzer.NewRunner(w.cfg, w.analyzers)
	result := runner.Run(w.repoRoot, diff, parsedFiles, w.idx)

	// Set branch info
	if branch, err := w.repo.CurrentBranch(); err == nil {
		result.Branch = branch
	}

	duration := time.Since(start)

	if w.onChange != nil {
		w.onChange(RunResult{
			Result:    result,
			Timestamp: start,
			Duration:  duration,
			Trigger:   trigger,
		})
	}
}

// parseChangedFiles parses files from the diff (mirrors main.go logic).
func (w *Watcher) parseChangedFiles(diff *git.Diff) map[string]*parser.ParsedFile {
	parsed := make(map[string]*parser.ParsedFile)

	for _, fileDiff := range diff.Files {
		path := fileDiff.Path

		if w.cfg.IsIgnored(path) {
			continue
		}

		if fileDiff.Status == "deleted" || fileDiff.IsBinary {
			continue
		}

		lang := parser.DetectLanguage(path)
		if lang == "" {
			continue
		}

		if !w.languageEnabled(lang) {
			continue
		}

		content, err := w.repo.FileContent(path, "")
		if err != nil {
			log.Printf("warning: could not read %s: %v", path, err)
			continue
		}

		pf, err := w.reg.ParseFile(path, content)
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

// languageEnabled checks if a language is enabled in config.
func (w *Watcher) languageEnabled(lang string) bool {
	for _, l := range w.cfg.Languages {
		if l == lang {
			return true
		}
	}
	return false
}
