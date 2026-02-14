package index

import (
	"encoding/json"
	"fmt"
	"os"
	"path/filepath"
	"sort"
	"strings"
	"sync"
	"time"

	"github.com/joeabbey/fault/pkg/config"
	"github.com/joeabbey/fault/pkg/parser"
)

const (
	// CacheDirName is the directory used for fault's cache files.
	CacheDirName = ".fault-cache"
	// CacheFileName is the index cache file name.
	CacheFileName = "index.json"
)

// Index holds a lightweight index of all source files in a repository.
// It maps file paths to their structural metadata (exports, imports) so
// analyzers can cross-reference the full codebase, not just changed files.
type Index struct {
	Files    map[string]*FileEntry `json:"files"`
	RepoRoot string                `json:"repo_root"`
	CacheDir string                `json:"-"`

	mu  sync.RWMutex
	cfg *config.Config
}

// FileEntry holds parsed metadata for a single source file.
type FileEntry struct {
	Path     string          `json:"path"`
	Language string          `json:"language"`
	Exports  []parser.Export `json:"exports"`
	Imports  []parser.Import `json:"imports"`
	ModTime  time.Time       `json:"mod_time"`
	Size     int64           `json:"size"`
}

// cacheData is the on-disk representation of the index cache.
type cacheData struct {
	Version  int                    `json:"version"`
	RepoRoot string                 `json:"repo_root"`
	Files    map[string]*FileEntry  `json:"files"`
	SavedAt  time.Time              `json:"saved_at"`
}

// NewIndex creates a new Index for the given repository root.
func NewIndex(repoRoot string, cfg *config.Config) *Index {
	return &Index{
		Files:    make(map[string]*FileEntry),
		RepoRoot: repoRoot,
		CacheDir: filepath.Join(repoRoot, CacheDirName),
		cfg:      cfg,
	}
}

// Build walks the repository directory tree, parses all source files,
// and populates the index. It skips .git/, ignored directories, and
// files that are not recognized source files.
func (idx *Index) Build(reg *parser.Registry) error {
	idx.mu.Lock()
	defer idx.mu.Unlock()

	// Reset files
	idx.Files = make(map[string]*FileEntry)

	err := filepath.Walk(idx.RepoRoot, func(absPath string, info os.FileInfo, err error) error {
		if err != nil {
			return nil // skip files we cannot stat
		}

		// Compute relative path
		relPath, err := filepath.Rel(idx.RepoRoot, absPath)
		if err != nil {
			return nil
		}

		// Skip .git directory
		if info.IsDir() {
			base := filepath.Base(absPath)
			if base == ".git" || base == CacheDirName {
				return filepath.SkipDir
			}
			// Check if directory matches ignore patterns
			if idx.cfg != nil && idx.cfg.IsIgnored(relPath+"/") {
				return filepath.SkipDir
			}
			return nil
		}

		// Skip non-source files
		lang := parser.DetectLanguage(relPath)
		if lang == "" {
			return nil
		}

		// Skip ignored files
		if idx.cfg != nil && idx.cfg.IsIgnored(relPath) {
			return nil
		}

		// Read and parse the file
		content, err := os.ReadFile(absPath)
		if err != nil {
			return nil // skip unreadable files
		}

		entry := &FileEntry{
			Path:     relPath,
			Language: lang,
			Exports:  make([]parser.Export, 0),
			Imports:  make([]parser.Import, 0),
			ModTime:  info.ModTime(),
			Size:     info.Size(),
		}

		parsed, err := reg.ParseFile(relPath, content)
		if err == nil && parsed != nil {
			entry.Exports = parsed.Exports
			entry.Imports = parsed.Imports
			// Ensure nil slices are empty for JSON serialization
			if entry.Exports == nil {
				entry.Exports = make([]parser.Export, 0)
			}
			if entry.Imports == nil {
				entry.Imports = make([]parser.Import, 0)
			}
		}

		idx.Files[relPath] = entry
		return nil
	})

	return err
}

// GetFile returns the FileEntry for the given relative path, or nil.
func (idx *Index) GetFile(path string) *FileEntry {
	idx.mu.RLock()
	defer idx.mu.RUnlock()
	return idx.Files[path]
}

// FindExport returns the relative paths of all files that export a symbol
// with the given name. If lang is non-empty, results are filtered to that language.
func (idx *Index) FindExport(name string, lang string) []string {
	idx.mu.RLock()
	defer idx.mu.RUnlock()

	results := make([]string, 0)
	for path, entry := range idx.Files {
		if lang != "" && entry.Language != lang {
			continue
		}
		for _, exp := range entry.Exports {
			if exp.Name == name {
				results = append(results, path)
				break
			}
		}
	}

	sort.Strings(results)
	return results
}

// AllFiles returns all indexed FileEntry values sorted by path.
func (idx *Index) AllFiles() []*FileEntry {
	idx.mu.RLock()
	defer idx.mu.RUnlock()

	entries := make([]*FileEntry, 0, len(idx.Files))
	for _, entry := range idx.Files {
		entries = append(entries, entry)
	}

	sort.Slice(entries, func(i, j int) bool {
		return entries[i].Path < entries[j].Path
	})

	return entries
}

// SaveCache writes the index to .fault-cache/index.json.
func (idx *Index) SaveCache() error {
	idx.mu.RLock()
	defer idx.mu.RUnlock()

	if err := os.MkdirAll(idx.CacheDir, 0755); err != nil {
		return fmt.Errorf("creating cache directory: %w", err)
	}

	data := cacheData{
		Version:  1,
		RepoRoot: idx.RepoRoot,
		Files:    idx.Files,
		SavedAt:  time.Now(),
	}

	jsonBytes, err := json.MarshalIndent(data, "", "  ")
	if err != nil {
		return fmt.Errorf("marshaling index: %w", err)
	}

	cachePath := filepath.Join(idx.CacheDir, CacheFileName)
	if err := os.WriteFile(cachePath, jsonBytes, 0644); err != nil {
		return fmt.Errorf("writing cache file: %w", err)
	}

	return nil
}

// LoadCache reads the index from .fault-cache/index.json.
// Returns an error if the cache file does not exist or is corrupt.
func (idx *Index) LoadCache() error {
	idx.mu.Lock()
	defer idx.mu.Unlock()

	cachePath := filepath.Join(idx.CacheDir, CacheFileName)
	jsonBytes, err := os.ReadFile(cachePath)
	if err != nil {
		return fmt.Errorf("reading cache file: %w", err)
	}

	var data cacheData
	if err := json.Unmarshal(jsonBytes, &data); err != nil {
		return fmt.Errorf("parsing cache file: %w", err)
	}

	if data.Version != 1 {
		return fmt.Errorf("unsupported cache version: %d", data.Version)
	}

	idx.Files = data.Files
	if idx.Files == nil {
		idx.Files = make(map[string]*FileEntry)
	}

	return nil
}

// IsCacheValid checks whether the cached index is still valid by comparing
// file count and modification times against the current filesystem state.
func (idx *Index) IsCacheValid() bool {
	// Must have a loaded cache to validate
	if len(idx.Files) == 0 {
		return false
	}

	// Walk the repo and collect current file state
	currentFiles := make(map[string]os.FileInfo)
	err := filepath.Walk(idx.RepoRoot, func(absPath string, info os.FileInfo, err error) error {
		if err != nil {
			return nil
		}

		relPath, err := filepath.Rel(idx.RepoRoot, absPath)
		if err != nil {
			return nil
		}

		if info.IsDir() {
			base := filepath.Base(absPath)
			if base == ".git" || base == CacheDirName {
				return filepath.SkipDir
			}
			if idx.cfg != nil && idx.cfg.IsIgnored(relPath+"/") {
				return filepath.SkipDir
			}
			return nil
		}

		lang := parser.DetectLanguage(relPath)
		if lang == "" {
			return nil
		}

		if idx.cfg != nil && idx.cfg.IsIgnored(relPath) {
			return nil
		}

		currentFiles[relPath] = info
		return nil
	})
	if err != nil {
		return false
	}

	// Check file count
	if len(currentFiles) != len(idx.Files) {
		return false
	}

	// Check each file's mod time and size
	for path, info := range currentFiles {
		entry, ok := idx.Files[path]
		if !ok {
			return false
		}
		if !info.ModTime().Equal(entry.ModTime) {
			return false
		}
		if info.Size() != entry.Size {
			return false
		}
	}

	return true
}

// BuildOrLoad tries to load the cache first. If the cache is valid, it
// uses the cached data. Otherwise, it rebuilds the index and saves the cache.
func (idx *Index) BuildOrLoad(reg *parser.Registry) error {
	// Try loading cache
	if err := idx.LoadCache(); err == nil {
		if idx.IsCacheValid() {
			return nil
		}
	}

	// Cache invalid or missing — rebuild
	if err := idx.Build(reg); err != nil {
		return err
	}

	// Save new cache (best-effort)
	_ = idx.SaveCache()

	return nil
}

// EnsureGitignore checks if .fault-cache/ is in the repo's .gitignore
// and adds it if missing.
func EnsureGitignore(repoRoot string) error {
	gitignorePath := filepath.Join(repoRoot, ".gitignore")

	content, err := os.ReadFile(gitignorePath)
	if err != nil && !os.IsNotExist(err) {
		return fmt.Errorf("reading .gitignore: %w", err)
	}

	entry := CacheDirName + "/"
	if strings.Contains(string(content), entry) {
		return nil // already present
	}

	// Append the entry
	var newContent string
	if len(content) > 0 && content[len(content)-1] != '\n' {
		newContent = string(content) + "\n" + entry + "\n"
	} else {
		newContent = string(content) + entry + "\n"
	}

	if err := os.WriteFile(gitignorePath, []byte(newContent), 0644); err != nil {
		return fmt.Errorf("writing .gitignore: %w", err)
	}

	return nil
}
