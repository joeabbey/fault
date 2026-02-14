package index

import (
	"os"
	"path/filepath"
	"testing"
	"time"

	"github.com/joeabbey/fault/pkg/config"
	"github.com/joeabbey/fault/pkg/parser"
)

// setupTestDir creates a temporary directory with source files for testing.
func setupTestDir(t *testing.T) (string, *config.Config, *parser.Registry) {
	t.Helper()
	dir := t.TempDir()

	// Create directory structure
	for _, sub := range []string{"src", "vendor", "node_modules"} {
		if err := os.MkdirAll(filepath.Join(dir, sub), 0755); err != nil {
			t.Fatal(err)
		}
	}

	// Create source files
	files := map[string]string{
		"src/main.go": `package main

import "fmt"

func Main() {
	fmt.Println("hello")
}
`,
		"src/util.go": `package main

func Helper() string {
	return "help"
}

func Internal() {}
`,
		"src/app.ts": `import { foo } from './foo';

export function App() { return 'app'; }
export const VERSION = '1.0';
`,
		"src/foo.ts": `export function foo() { return 42; }
`,
		"src/lib.py": `import os

def process():
    pass

class Handler:
    pass
`,
		// Files that should be ignored
		"vendor/dep.go":            `package dep`,
		"node_modules/pkg/index.js": `export default {};`,
	}

	for path, content := range files {
		absPath := filepath.Join(dir, path)
		if err := os.MkdirAll(filepath.Dir(absPath), 0755); err != nil {
			t.Fatal(err)
		}
		if err := os.WriteFile(absPath, []byte(content), 0644); err != nil {
			t.Fatal(err)
		}
	}

	cfg := config.DefaultConfig()
	reg := parser.NewRegistry()
	reg.Register(parser.NewGoParser())
	reg.Register(parser.NewTypeScriptParser())
	reg.Register(parser.NewPythonParser())

	return dir, cfg, reg
}

func TestBuildIndex(t *testing.T) {
	dir, cfg, reg := setupTestDir(t)
	idx := NewIndex(dir, cfg)

	if err := idx.Build(reg); err != nil {
		t.Fatalf("Build failed: %v", err)
	}

	// Should include source files but not ignored ones
	expectedFiles := []string{
		"src/main.go",
		"src/util.go",
		"src/app.ts",
		"src/foo.ts",
		"src/lib.py",
	}

	for _, path := range expectedFiles {
		entry := idx.GetFile(path)
		if entry == nil {
			t.Errorf("expected file %s in index", path)
			continue
		}
		if entry.Language == "" {
			t.Errorf("file %s has empty language", path)
		}
	}

	// Should NOT include ignored files
	ignoredFiles := []string{
		"vendor/dep.go",
		"node_modules/pkg/index.js",
	}

	for _, path := range ignoredFiles {
		if entry := idx.GetFile(path); entry != nil {
			t.Errorf("expected file %s to be excluded from index", path)
		}
	}
}

func TestBuildIndexFileCount(t *testing.T) {
	dir, cfg, reg := setupTestDir(t)
	idx := NewIndex(dir, cfg)

	if err := idx.Build(reg); err != nil {
		t.Fatal(err)
	}

	files := idx.AllFiles()
	if len(files) != 5 {
		t.Errorf("expected 5 files, got %d", len(files))
		for _, f := range files {
			t.Logf("  %s (%s)", f.Path, f.Language)
		}
	}
}

func TestBuildIndexLanguageDetection(t *testing.T) {
	dir, cfg, reg := setupTestDir(t)
	idx := NewIndex(dir, cfg)

	if err := idx.Build(reg); err != nil {
		t.Fatal(err)
	}

	tests := map[string]string{
		"src/main.go": "go",
		"src/app.ts":  "typescript",
		"src/lib.py":  "python",
	}

	for path, expectedLang := range tests {
		entry := idx.GetFile(path)
		if entry == nil {
			t.Errorf("file %s not found", path)
			continue
		}
		if entry.Language != expectedLang {
			t.Errorf("file %s: expected language %q, got %q", path, expectedLang, entry.Language)
		}
	}
}

func TestBuildIndexExports(t *testing.T) {
	dir, cfg, reg := setupTestDir(t)
	idx := NewIndex(dir, cfg)

	if err := idx.Build(reg); err != nil {
		t.Fatal(err)
	}

	// Check Go exports
	mainEntry := idx.GetFile("src/main.go")
	if mainEntry == nil {
		t.Fatal("src/main.go not found")
	}
	foundMain := false
	for _, exp := range mainEntry.Exports {
		if exp.Name == "Main" {
			foundMain = true
		}
	}
	if !foundMain {
		t.Error("expected Main export in src/main.go")
	}

	// Check TypeScript exports
	appEntry := idx.GetFile("src/app.ts")
	if appEntry == nil {
		t.Fatal("src/app.ts not found")
	}
	foundApp := false
	foundVersion := false
	for _, exp := range appEntry.Exports {
		if exp.Name == "App" {
			foundApp = true
		}
		if exp.Name == "VERSION" {
			foundVersion = true
		}
	}
	if !foundApp {
		t.Error("expected App export in src/app.ts")
	}
	if !foundVersion {
		t.Error("expected VERSION export in src/app.ts")
	}
}

func TestFindExport(t *testing.T) {
	dir, cfg, reg := setupTestDir(t)
	idx := NewIndex(dir, cfg)

	if err := idx.Build(reg); err != nil {
		t.Fatal(err)
	}

	// Find a Go export
	results := idx.FindExport("Helper", "go")
	if len(results) != 1 || results[0] != "src/util.go" {
		t.Errorf("FindExport(Helper, go) = %v, expected [src/util.go]", results)
	}

	// Find a TypeScript export
	results = idx.FindExport("foo", "typescript")
	if len(results) != 1 || results[0] != "src/foo.ts" {
		t.Errorf("FindExport(foo, typescript) = %v, expected [src/foo.ts]", results)
	}

	// Find across all languages (empty lang)
	results = idx.FindExport("App", "")
	if len(results) != 1 || results[0] != "src/app.ts" {
		t.Errorf("FindExport(App, '') = %v, expected [src/app.ts]", results)
	}

	// Nonexistent export
	results = idx.FindExport("DoesNotExist", "")
	if len(results) != 0 {
		t.Errorf("FindExport(DoesNotExist) = %v, expected empty", results)
	}
}

func TestAllFilesSorted(t *testing.T) {
	dir, cfg, reg := setupTestDir(t)
	idx := NewIndex(dir, cfg)

	if err := idx.Build(reg); err != nil {
		t.Fatal(err)
	}

	files := idx.AllFiles()
	for i := 1; i < len(files); i++ {
		if files[i].Path < files[i-1].Path {
			t.Errorf("AllFiles not sorted: %s before %s", files[i-1].Path, files[i].Path)
		}
	}
}

func TestCacheSaveAndLoad(t *testing.T) {
	dir, cfg, reg := setupTestDir(t)
	idx := NewIndex(dir, cfg)

	if err := idx.Build(reg); err != nil {
		t.Fatal(err)
	}

	// Save cache
	if err := idx.SaveCache(); err != nil {
		t.Fatalf("SaveCache failed: %v", err)
	}

	// Verify cache file exists
	cachePath := filepath.Join(dir, CacheDirName, CacheFileName)
	if _, err := os.Stat(cachePath); os.IsNotExist(err) {
		t.Fatal("cache file was not created")
	}

	// Load into new index
	idx2 := NewIndex(dir, cfg)
	if err := idx2.LoadCache(); err != nil {
		t.Fatalf("LoadCache failed: %v", err)
	}

	// Verify round-trip
	if len(idx2.Files) != len(idx.Files) {
		t.Errorf("cache round-trip: expected %d files, got %d", len(idx.Files), len(idx2.Files))
	}

	for path, entry := range idx.Files {
		loaded, ok := idx2.Files[path]
		if !ok {
			t.Errorf("cache round-trip: missing file %s", path)
			continue
		}
		if loaded.Language != entry.Language {
			t.Errorf("cache round-trip: file %s language %q != %q", path, loaded.Language, entry.Language)
		}
		if len(loaded.Exports) != len(entry.Exports) {
			t.Errorf("cache round-trip: file %s exports count %d != %d", path, len(loaded.Exports), len(entry.Exports))
		}
	}
}

func TestCacheInvalidOnFileChange(t *testing.T) {
	dir, cfg, reg := setupTestDir(t)
	idx := NewIndex(dir, cfg)

	if err := idx.Build(reg); err != nil {
		t.Fatal(err)
	}
	if err := idx.SaveCache(); err != nil {
		t.Fatal(err)
	}

	// Verify cache is valid initially
	if !idx.IsCacheValid() {
		t.Fatal("cache should be valid immediately after build")
	}

	// Modify a file (ensure mod time changes)
	time.Sleep(50 * time.Millisecond)
	filePath := filepath.Join(dir, "src", "main.go")
	if err := os.WriteFile(filePath, []byte("package main\n\nfunc Changed() {}\n"), 0644); err != nil {
		t.Fatal(err)
	}

	// Cache should be invalid
	if idx.IsCacheValid() {
		t.Error("cache should be invalid after file modification")
	}
}

func TestCacheInvalidOnNewFile(t *testing.T) {
	dir, cfg, reg := setupTestDir(t)
	idx := NewIndex(dir, cfg)

	if err := idx.Build(reg); err != nil {
		t.Fatal(err)
	}
	if err := idx.SaveCache(); err != nil {
		t.Fatal(err)
	}

	// Add a new file
	newFile := filepath.Join(dir, "src", "new.go")
	if err := os.WriteFile(newFile, []byte("package main\n"), 0644); err != nil {
		t.Fatal(err)
	}

	if idx.IsCacheValid() {
		t.Error("cache should be invalid after adding a new file")
	}
}

func TestCacheInvalidOnDeletedFile(t *testing.T) {
	dir, cfg, reg := setupTestDir(t)
	idx := NewIndex(dir, cfg)

	if err := idx.Build(reg); err != nil {
		t.Fatal(err)
	}
	if err := idx.SaveCache(); err != nil {
		t.Fatal(err)
	}

	// Delete a file
	if err := os.Remove(filepath.Join(dir, "src", "foo.ts")); err != nil {
		t.Fatal(err)
	}

	if idx.IsCacheValid() {
		t.Error("cache should be invalid after deleting a file")
	}
}

func TestBuildOrLoad(t *testing.T) {
	dir, cfg, reg := setupTestDir(t)

	// First call: no cache, should build
	idx := NewIndex(dir, cfg)
	if err := idx.BuildOrLoad(reg); err != nil {
		t.Fatal(err)
	}
	if len(idx.Files) == 0 {
		t.Fatal("index should have files after BuildOrLoad")
	}

	// Second call: cache exists, should load
	idx2 := NewIndex(dir, cfg)
	if err := idx2.BuildOrLoad(reg); err != nil {
		t.Fatal(err)
	}
	if len(idx2.Files) != len(idx.Files) {
		t.Errorf("cached load: expected %d files, got %d", len(idx.Files), len(idx2.Files))
	}
}

func TestLoadCacheNotFound(t *testing.T) {
	dir := t.TempDir()
	cfg := config.DefaultConfig()
	idx := NewIndex(dir, cfg)

	err := idx.LoadCache()
	if err == nil {
		t.Fatal("expected error when cache does not exist")
	}
}

func TestGetFileNotFound(t *testing.T) {
	dir := t.TempDir()
	cfg := config.DefaultConfig()
	idx := NewIndex(dir, cfg)

	if entry := idx.GetFile("nonexistent.go"); entry != nil {
		t.Error("expected nil for nonexistent file")
	}
}

func TestEnsureGitignore(t *testing.T) {
	dir := t.TempDir()

	// Create a .gitignore
	gitignore := filepath.Join(dir, ".gitignore")
	if err := os.WriteFile(gitignore, []byte("*.log\n"), 0644); err != nil {
		t.Fatal(err)
	}

	if err := EnsureGitignore(dir); err != nil {
		t.Fatal(err)
	}

	content, err := os.ReadFile(gitignore)
	if err != nil {
		t.Fatal(err)
	}

	if got := string(content); got != "*.log\n.fault-cache/\n" {
		t.Errorf("unexpected .gitignore content: %q", got)
	}

	// Calling again should be idempotent
	if err := EnsureGitignore(dir); err != nil {
		t.Fatal(err)
	}

	content, err = os.ReadFile(gitignore)
	if err != nil {
		t.Fatal(err)
	}
	if got := string(content); got != "*.log\n.fault-cache/\n" {
		t.Errorf("idempotent check: unexpected .gitignore content: %q", got)
	}
}

func TestEnsureGitignoreNewFile(t *testing.T) {
	dir := t.TempDir()

	if err := EnsureGitignore(dir); err != nil {
		t.Fatal(err)
	}

	gitignore := filepath.Join(dir, ".gitignore")
	content, err := os.ReadFile(gitignore)
	if err != nil {
		t.Fatal(err)
	}

	if got := string(content); got != ".fault-cache/\n" {
		t.Errorf("unexpected .gitignore content: %q", got)
	}
}
