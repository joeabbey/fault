package git

import (
	"os"
	"path/filepath"
	"testing"
)

func TestInstallHook(t *testing.T) {
	dir := setupTestRepo(t)

	err := InstallHook(dir, false)
	if err != nil {
		t.Fatalf("InstallHook failed: %v", err)
	}

	hookPath := filepath.Join(dir, ".git", "hooks", "pre-commit")
	content, err := os.ReadFile(hookPath)
	if err != nil {
		t.Fatalf("reading hook: %v", err)
	}

	if !isOurHook(string(content)) {
		t.Error("installed hook does not contain fault marker")
	}

	// Check executable permission
	info, err := os.Stat(hookPath)
	if err != nil {
		t.Fatal(err)
	}
	if info.Mode().Perm()&0111 == 0 {
		t.Error("hook is not executable")
	}
}

func TestInstallHookIdempotent(t *testing.T) {
	dir := setupTestRepo(t)

	// Install twice should succeed (overwriting our own hook)
	if err := InstallHook(dir, false); err != nil {
		t.Fatalf("first install: %v", err)
	}
	if err := InstallHook(dir, false); err != nil {
		t.Fatalf("second install: %v", err)
	}
}

func TestInstallHookExistingForeignHook(t *testing.T) {
	dir := setupTestRepo(t)

	// Create a foreign hook
	hookDir := filepath.Join(dir, ".git", "hooks")
	if err := os.MkdirAll(hookDir, 0755); err != nil {
		t.Fatal(err)
	}
	hookPath := filepath.Join(hookDir, "pre-commit")
	if err := os.WriteFile(hookPath, []byte("#!/bin/sh\necho 'custom hook'\n"), 0755); err != nil {
		t.Fatal(err)
	}

	// Install without force should fail
	err := InstallHook(dir, false)
	if err == nil {
		t.Fatal("expected error when foreign hook exists")
	}

	// Install with force should succeed
	err = InstallHook(dir, true)
	if err != nil {
		t.Fatalf("InstallHook with force failed: %v", err)
	}

	content, err := os.ReadFile(hookPath)
	if err != nil {
		t.Fatal(err)
	}
	if !isOurHook(string(content)) {
		t.Error("hook was not overwritten with fault hook")
	}
}

func TestUninstallHook(t *testing.T) {
	dir := setupTestRepo(t)

	// Install first
	if err := InstallHook(dir, false); err != nil {
		t.Fatal(err)
	}

	// Uninstall
	if err := UninstallHook(dir); err != nil {
		t.Fatalf("UninstallHook failed: %v", err)
	}

	hookPath := filepath.Join(dir, ".git", "hooks", "pre-commit")
	if _, err := os.Stat(hookPath); !os.IsNotExist(err) {
		t.Error("hook file still exists after uninstall")
	}
}

func TestUninstallHookNotInstalled(t *testing.T) {
	dir := setupTestRepo(t)

	err := UninstallHook(dir)
	if err == nil {
		t.Fatal("expected error when no hook installed")
	}
}

func TestUninstallHookForeignHook(t *testing.T) {
	dir := setupTestRepo(t)

	// Create a foreign hook
	hookDir := filepath.Join(dir, ".git", "hooks")
	if err := os.MkdirAll(hookDir, 0755); err != nil {
		t.Fatal(err)
	}
	hookPath := filepath.Join(hookDir, "pre-commit")
	if err := os.WriteFile(hookPath, []byte("#!/bin/sh\necho 'custom'\n"), 0755); err != nil {
		t.Fatal(err)
	}

	err := UninstallHook(dir)
	if err == nil {
		t.Fatal("expected error when uninstalling foreign hook")
	}

	// Foreign hook should still exist
	if _, err := os.Stat(hookPath); os.IsNotExist(err) {
		t.Error("foreign hook was removed")
	}
}

func TestIsHookInstalled(t *testing.T) {
	dir := setupTestRepo(t)

	// Not installed
	if IsHookInstalled(dir) {
		t.Error("expected false when no hook installed")
	}

	// Install
	if err := InstallHook(dir, false); err != nil {
		t.Fatal(err)
	}

	// Now installed
	if !IsHookInstalled(dir) {
		t.Error("expected true after install")
	}

	// Uninstall
	if err := UninstallHook(dir); err != nil {
		t.Fatal(err)
	}

	// Not installed again
	if IsHookInstalled(dir) {
		t.Error("expected false after uninstall")
	}
}

func TestIsHookInstalledForeignHook(t *testing.T) {
	dir := setupTestRepo(t)

	// Create a foreign hook
	hookDir := filepath.Join(dir, ".git", "hooks")
	if err := os.MkdirAll(hookDir, 0755); err != nil {
		t.Fatal(err)
	}
	hookPath := filepath.Join(hookDir, "pre-commit")
	if err := os.WriteFile(hookPath, []byte("#!/bin/sh\necho 'custom'\n"), 0755); err != nil {
		t.Fatal(err)
	}

	if IsHookInstalled(dir) {
		t.Error("expected false for foreign hook")
	}
}

func TestIsHookInstalledNotGitRepo(t *testing.T) {
	dir := t.TempDir()
	if IsHookInstalled(dir) {
		t.Error("expected false for non-git directory")
	}
}

func TestHookFilePath(t *testing.T) {
	dir := setupTestRepo(t)

	path, err := hookFilePath(dir)
	if err != nil {
		t.Fatal(err)
	}

	expected := filepath.Join(dir, ".git", "hooks", "pre-commit")
	if path != expected {
		t.Errorf("expected %s, got %s", expected, path)
	}
}

func TestHookFilePathNotGitRepo(t *testing.T) {
	dir := t.TempDir()
	_, err := hookFilePath(dir)
	if err == nil {
		t.Fatal("expected error for non-git directory")
	}
}
