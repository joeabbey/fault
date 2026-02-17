# Getting Started with Fault

Fault is a pre-commit validation tool for AI agent output. It catches structural errors in AI-generated code -- broken imports, stale references, cross-file inconsistencies -- before they reach your commit history.

## Installation

### From source (requires Go 1.21+)

```bash
go install github.com/joeabbey/fault/cmd/fault@latest
```

### Download binary

```bash
curl -sSf https://fault.jabbey.io/install.sh | sh
```

The installer detects your OS and architecture, downloads the latest release from GitHub, and places the binary in `/usr/local/bin` (or `~/.local/bin` if `/usr/local/bin` is not writable).

### Manual download

Download a binary from the [GitHub Releases](https://github.com/joeabbey/fault/releases) page. Available for Linux (amd64, arm64) and macOS (amd64, arm64).

## Configuration

Run `fault init` in your project root to generate a `.fault.yaml` config file:

```bash
cd your-project
fault init
```

This creates the following default configuration:

```yaml
version: 1
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
```

### Configuration options

| Option | Values | Description |
|--------|--------|-------------|
| `languages` | `go`, `typescript`, `python`, [42 total](./languages.md) | Languages to analyze |
| `block_on` | `error`, `warning` | Minimum severity to block commits |
| `analyzers.*` | `true`, `false` | Enable/disable individual analyzers |
| `ignore` | glob patterns | Files and directories to skip |
| `llm.enabled` | `true`, `false` | Enable LLM-powered analysis (requires `FAULT_API_KEY`) |
| `llm.api_url` | URL | Fault Cloud API base URL (default: `https://fault.jabbey.io`) |
| `llm.spec_file` | file path | Spec file for LLM comparison |

Fault searches for `.fault.yaml` starting from the current directory and walking up to the filesystem root. If no config is found, defaults are used.

## Running your first check

After making changes in a git repository, run:

```bash
fault check
```

By default, `fault check` auto-detects the right diff to analyze. You can also be explicit:

```bash
# Check only staged changes (what would be committed)
fault check --staged

# Check only unstaged changes (working tree modifications)
fault check --unstaged

# Check all changes since a branch point
fault check --branch main
```

## Understanding the output

Fault reports issues at two severity levels:

- **error** -- Definite structural problems (broken imports, missing exports, stale references). These block commits by default.
- **warning** -- Potential issues that deserve review (missing test updates, TODO placeholders, debug artifacts). These do not block commits unless `block_on` is set to `warning`.

Example output:

```
fault check --staged

  pkg/api/handlers.go
    45:3  error  [imports]       Import 'models/preferences' not found
    82:1  error  [consistency]   Function 'GetPreferences' signature changed but caller not updated

  pkg/api/routes.go
    12:5  warning  [references]  Reference to deleted function 'OldHandler'

  pkg/api/handlers_test.go
    -:-  warning  [tests]        Changed function 'GetPreferences' has no corresponding test update

  2 errors, 2 warnings
  Commit blocked (block_on: error)
```

Each line shows: file, line:column, severity, analyzer name, and a description of the issue.

### Output formats

```bash
# Terminal output with colors (default)
fault check

# Terminal output without colors (for CI logs)
fault check --no-color

# JSON output (for scripts and tooling)
fault check --format=json

# SARIF v2.1.0 (GitHub Code Scanning, VS Code SARIF Viewer)
fault check --format=sarif
```

## Setting up the pre-commit hook

The most effective way to use Fault is as a git pre-commit hook. This ensures every commit is validated automatically:

```bash
fault hook install
```

This writes a pre-commit hook to `.git/hooks/pre-commit` that runs `fault check --staged` before each commit. If Fault finds errors, the commit is blocked.

To check if the hook is installed:

```bash
fault hook status
```

To remove the hook:

```bash
fault hook uninstall
```

To bypass the hook for a single commit (use sparingly):

```bash
git commit --no-verify
```

If a pre-commit hook already exists that was not installed by Fault, the install command will refuse to overwrite it. Use `--force` to replace it:

```bash
fault hook install --force
```

## Configuring for your project

### Go projects

```yaml
version: 1
languages: [go]
block_on: error
analyzers:
  imports: true
  consistency: true
  references: true
  tests: true
  patterns: true
ignore:
  - "vendor/"
  - "*.pb.go"
  - "*.generated.go"
```

### TypeScript/JavaScript projects

```yaml
version: 1
languages: [typescript]
block_on: error
analyzers:
  imports: true
  consistency: true
  references: true
  tests: true
  patterns: true
ignore:
  - "node_modules/"
  - "dist/"
  - "build/"
  - "*.min.js"
  - "*.generated.*"
```

### Python projects

```yaml
version: 1
languages: [python]
block_on: error
analyzers:
  imports: true
  consistency: true
  references: true
  tests: true
  patterns: true
ignore:
  - ".venv/"
  - "__pycache__/"
  - "*.pyc"
```

### Multi-language projects

```yaml
version: 1
languages: [go, typescript, python]
block_on: error
analyzers:
  imports: true
  consistency: true
  references: true
  tests: true
  patterns: true
ignore:
  - "vendor/"
  - "node_modules/"
  - "dist/"
  - ".venv/"
  - "*.generated.*"
  - "*.min.js"
```

## Analyzers

Fault runs 12 analyzers in parallel, each targeting a different class of structural error:

### imports

Validates import/export relationships across files. Catches new imports referencing nonexistent exports and removed exports that are still imported elsewhere.

### consistency

Detects cross-file inconsistencies. When a function signature changes, this analyzer checks that all callers are updated to match.

### references

Finds stale references to deleted or renamed files and symbols.

### tests

Identifies changed functions that lack corresponding test updates. This is a warning-level check -- it reminds you to update tests but does not block commits by default.

### patterns

Catches common anti-patterns in AI-generated code: TODO/FIXME placeholders left behind, debug statements (`console.log`, `fmt.Println` used as debugging), and commented-out code blocks.

### security

Detects hardcoded credentials, SQL injection via string concatenation, path traversal vulnerabilities, insecure crypto usage, and other security anti-patterns.

### hallucination

Identifies calls to APIs, functions, or modules that don't exist in your codebase -- a common artifact of AI-generated code.

### complexity

Flags excessive cyclomatic complexity in changed functions.

### concurrency

Detects race conditions, deadlock patterns, and unsafe shared state access.

### resource

Catches unclosed file handles, leaked database connections, and missing cleanup.

### migration

Identifies schema changes without corresponding migration files.

### doc-drift

Flags code changes that invalidate existing documentation.

## LLM-powered analysis (Pro)

For additional coverage, Fault can use LLM analysis to score confidence in changes and compare them against a specification document.

```bash
export FAULT_API_KEY=fk_your_key_here
```

Then update `.fault.yaml`:

```yaml
llm:
  enabled: true
  spec_file: "SPEC.md"  # optional
```

With LLM analysis enabled, Fault provides:

- **Confidence scoring** -- Per-file confidence scores indicating how likely changes are to be correct, with reasoning.
- **Spec comparison** -- If a spec file is configured, Fault compares the diff against requirements and flags deviations.

Get an API key at [fault.jabbey.io](https://fault.jabbey.io).

## What's next

- [Claude Code Integration Guide](./claude-code-integration.md) -- Using Fault with Claude Code
- [aider Integration Guide](./aider-integration.md) -- Using Fault with aider
