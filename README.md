# Fault

Pre-commit validation for AI agent output. Catches structural errors in AI-generated code before they reach your commit history.

## What it does

Fault sits between your AI coding agent (Claude Code, aider, Cursor) and `git commit`. It validates multi-file changes, catching:

- **Broken imports** — new imports referencing nonexistent exports, removed exports still imported elsewhere
- **Cross-file inconsistencies** — function signatures changed without updating callers, type mismatches
- **Broken references** — deleted/renamed files still referenced, stale symbol references
- **Missing tests** — changed functions without corresponding test updates
- **Anti-patterns** — TODO placeholders, hardcoded credentials, debug artifacts, commented-out code

## Install

```bash
# From source
go install github.com/joeabbey/fault/cmd/fault@latest

# Or download binary
curl -sSf https://fault.jabbey.io/install.sh | sh
```

## Quick start

```bash
# Initialize config
fault init

# Run on staged changes
fault check --staged

# Run on unstaged changes
fault check

# Install as pre-commit hook
fault hook install
```

## Output formats

```bash
fault check                    # Terminal (colored, human-readable)
fault check --format=json      # JSON (for scripts/CI)
fault check --format=sarif     # SARIF v2.1.0 (GitHub Code Scanning, VS Code)
fault check --no-color         # Terminal without colors (CI-friendly)
```

## Configuration

Create `.fault.yaml` in your project root (or run `fault init`):

```yaml
version: 1
languages: [typescript, python, go]
block_on: error  # "error" or "warning"

analyzers:
  imports: true
  consistency: true
  references: true
  tests: true
  patterns: true

llm:
  enabled: false       # Set true for AI-powered confidence scoring
  api_url: "https://fault.jabbey.io"  # Fault Cloud API base URL (optional if using default)
  spec_file: ""        # Path to spec file for comparison

ignore:
  - "vendor/"
  - "node_modules/"
  - "*.generated.*"
  - "*.min.js"
```

## LLM features (Pro)

Set `FAULT_API_KEY` to enable AI-powered analysis.

If you're using a self-hosted Fault Cloud, also set `FAULT_API_URL` (for example `http://localhost:8082`).

- **Confidence scoring** — per-file confidence scores with reasoning
- **Spec comparison** — compare changes against a requirements spec

## Development

```bash
make build          # Build CLI binary
make build-cloud    # Build cloud API server
make test           # Run all tests
make release        # Cross-compile for all platforms
```

## Architecture

```
fault check
    |
    v
  Git Diff Parser (staged/unstaged/branch)
    |
    v
  Language Parsers (Go, TypeScript, Python)
    |
    v
  Analyzer Runner (parallel execution)
    |-- Import/Export Analyzer
    |-- Consistency Analyzer
    |-- Reference Analyzer
    |-- Test Impact Analyzer
    |-- Anti-Pattern Analyzer
    |
    v
  Reporter (terminal / JSON / SARIF)
```
