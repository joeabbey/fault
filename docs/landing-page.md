# Landing Page Copy -- fault.jabbey.io

## Hero

**Headline:** Catch AI agent mistakes before they hit your codebase

**Subhead:** Fault validates AI-generated code before and after it ships -- broken imports, swallowed errors, hallucinated APIs, spec drift. Pre-commit, post-merge, and CI.

**CTA:** Get started -- it's free

```bash
curl -sSf https://fault.jabbey.io/install.sh | sh
```

---

## The problem

AI coding agents are fast. They can scaffold an entire feature across a dozen files in seconds. But they make structural mistakes that are easy to miss during review:

- An import references a module that was never created
- A function signature changed but not all callers were updated
- A file was renamed but old references remain in three other files
- TODO placeholders were left behind as "implementation notes"

These errors compile, pass a quick scan, and slip into your commit history. You find them later when something breaks in production.

---

## Features

### Static analysis that works offline

Sixteen analyzers run locally against your git diff. No API calls, no network access, no data leaves your machine. Works on planes, in air-gapped environments, and in CI.

### Post-merge regression detection (New in v6)

Run `fault audit --commits 5` to scan code that already shipped to main. Upload results to the cloud dashboard with historical trend analytics. Runs automatically in CI after every merge.

### Spec validation (New in v6)

Define requirements in `.fault-spec.yaml`, anchor them in code with `// spec:REQ-001`. The spec analyzer tracks orphaned anchors, unanchored requirements, and spec drift across all 42 languages.

### Multi-language support

Parses and validates 42 languages. Understands imports, exports, function signatures, and cross-file references for each language.

### Git native

Installs as a pre-commit hook with `fault hook install`. Reads staged, unstaged, or branch diffs directly from git. Outputs terminal, JSON, or SARIF (GitHub Code Scanning compatible).

### AI-enhanced validation (Pro)

LLM-powered confidence scoring rates how likely each changed file is to be correct. Structured spec comparison analyzes each requirement individually with evidence and confidence scores.

---

## How it works

### 1. AI agent makes changes

Claude Code, aider, or Cursor edits files across your project -- new endpoints, refactored modules, updated types.

### 2. Fault validates the diff

When you commit, `fault check --staged` runs automatically via the pre-commit hook. It parses every changed file, resolves imports and references, and runs five analyzers in parallel.

### 3. Fix issues or commit with confidence

If Fault finds errors, the commit is blocked and you see exactly what to fix. If everything passes, the commit proceeds. No false confidence, no missed breakage.

---

## Built for AI agent workflows

Fault is designed specifically for the multi-file, cross-cutting changes that AI agents produce. Traditional linters check syntax within a single file. Fault checks the relationships between files.

| Check | Single-file linter | Fault |
|-------|-------------------|-------|
| Syntax errors | Yes | No (use your linter) |
| Broken imports across files | No | Yes |
| Signature changes with stale callers | No | Yes |
| References to deleted files | No | Yes |
| Missing test updates | No | Yes |
| Agent artifacts (TODOs, debug logs) | Partial | Yes |

---

## Works with your tools

- **Claude Code** -- Pre-commit hook or Claude Code hooks integration
- **aider** -- Pre-commit hook or `--lint-cmd` integration
- **Cursor** -- Pre-commit hook
- **GitHub Actions** -- SARIF output for Code Scanning
- **Any git workflow** -- If it commits to git, Fault can validate it

---

## Pricing

### Free

All static analyzers. Unlimited local use. No account required.

- 16 analyzers including spec validation, concurrency, resource leaks
- 42 languages with full cross-file analysis
- Terminal, JSON, SARIF output
- Pre-commit hook
- Works offline

### Pro -- $15/mo

LLM-powered analysis for deeper validation.

- Everything in Free
- Confidence scoring per file
- Structured spec comparison (per-requirement LLM analysis)
- Post-merge audit with `fault audit --upload`
- Historical trend dashboard with issues-over-time charts
- Spec compliance dashboard with coverage tracking
- Priority support

### Team -- $30/user/mo

Shared rules and visibility across your team.

- Everything in Pro
- Shared analyzer configurations
- Team dashboard with change audit trail
- Custom analyzer rules
- SSO integration

---

## Get started

```bash
# Install
curl -sSf https://fault.jabbey.io/install.sh | sh

# Set up your project
cd your-project
fault init
fault hook install

# That's it. Next time you commit, Fault validates your changes.
```

[Read the docs](https://fault.jabbey.io/docs) | [View on GitHub](https://github.com/joeabbey/fault)
