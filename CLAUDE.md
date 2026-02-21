# CLAUDE.md

Context for Claude Code when working with the Fault project.

## What is Fault?

Fault is a **pre-commit validation CLI for AI agent output**. It catches structural errors in AI-generated code (broken imports, stale references, missing test updates, security issues, hallucinated APIs) before they reach the commit history. Works as a git pre-commit hook or CI step, supports 42 languages with 15 analyzers running in parallel.

## Architecture

Two binaries:
- **`fault`** (CLI) — local tool, runs analyzers against git diffs
- **`fault-cloud`** — SaaS backend for Pro features (LLM confidence scoring, spec comparison, billing)

```
┌──────────────────────────────────────────────────┐
│                  fault CLI                        │
│  check | fix | watch | hook | init | baseline    │
└─────────────────────┬────────────────────────────┘
                      │ git diff
┌─────────────────────▼────────────────────────────┐
│            pkg/git + pkg/parser                   │
│  Parse git diff → ParsedFile per lang (42 langs) │
└─────────────────────┬────────────────────────────┘
                      │
┌─────────────────────▼────────────────────────────┐
│          pkg/analyzer (15 analyzers)              │
│  Imports | Consistency | References | Tests       │
│  Security | Patterns | Hallucination | Complexity │
│  Concurrency | Resource | Migration | DocDrift    │
│  ErrorHandling | DepGraph | DeadCode              │
└─────────────────────┬────────────────────────────┘
                      │ optional (Pro)
┌─────────────────────▼────────────────────────────┐
│       fault-cloud API (port 8082)                 │
│  PostgreSQL | Claude API | Stripe billing         │
│  Google OAuth | SvelteKit dashboard               │
└──────────────────────────────────────────────────┘
```

## File Structure

```
fault/
├── cmd/
│   ├── fault/              # CLI entry point
│   └── fault-cloud/        # Cloud API server
├── pkg/
│   ├── analyzer/           # 15 analyzers + runner + baseline
│   ├── fixer/              # Auto-fix strategies per language
│   ├── parser/             # 42 language parsers
│   ├── config/             # .fault.yaml loading
│   ├── git/                # Git diff parsing, hook management
│   ├── index/              # Cross-file symbol index
│   ├── reporter/           # Output: terminal, JSON, SARIF, GitHub Actions
│   ├── watcher/            # Watch mode: file polling, debounce
│   ├── llm/                # Anthropic API client, prompts
│   ├── cloud/              # Cloud server: handlers, auth, billing, store
│   └── cloudapi/           # CLI HTTP client for fault-cloud
├── web/                    # SvelteKit dashboard
│   └── vendor/atlas/       # Atlas design system (vendored)
├── docs/                   # User documentation
├── k8s/                    # Kubernetes manifests
├── testdata/               # Test fixtures
├── Makefile
└── Dockerfile
```

## Development Commands

```bash
make build          # Build CLI → ./fault
make build-cloud    # Build cloud server → ./fault-cloud
make test           # Tests with race detection
make lint           # golangci-lint
make release        # Cross-compile for all platforms

# Frontend (fault-cloud dashboard)
source ~/.nvm/nvm.sh && nvm use 22
cd web && npm install && npm run dev
```

## Dependencies

- **Magma** (`github.com/joeabbey/magma`) — Billing/Stripe, via Go module. Requires `GOPRIVATE=github.com/joeabbey/magma`
- **Atlas** — UI design system, vendored at `web/vendor/atlas/`
- **Core** — Infrastructure patterns for k8s deployment

## CLI Commands

```bash
fault check                    # Check unstaged changes
fault check --staged           # Check staged changes
fault check --branch main      # Check changes since branch
fault check --format sarif     # SARIF output for GitHub
fault check --baseline         # Only new issues (not in .fault-baseline.json)
fault check --compact          # Single-line output for CI
fault fix                      # Auto-fix issues
fault fix --dry-run            # Preview fixes
fault baseline                 # Snapshot current issues
fault hook install             # Install git pre-commit hook
fault watch                    # Re-run on file changes
```

## Analyzers (15)

| Analyzer | What it catches |
|----------|-----------------|
| Imports | Broken imports, nonexistent exports |
| Consistency | Changed signatures without updating callers |
| References | Deleted/renamed files still referenced |
| Tests | Changed functions without test updates |
| Patterns | TODOs, debug artifacts, commented-out code |
| Security | Hardcoded credentials, injection risks |
| Hallucination | Calls to nonexistent APIs/functions |
| Complexity | Excessive cyclomatic complexity |
| Concurrency | Race conditions, deadlock patterns |
| Resource | Unclosed handles, leaked connections |
| Migration | Schema changes without migration files |
| DocDrift | Code changes invalidating documentation |
| ErrorHandling | Unchecked errors, swallowed panics |
| DepGraph | Circular dependencies |
| DeadCode | Unreachable code, unused exports |

## Cloud API Routes

### Public
```
GET  /install.sh                        Install script
GET  /api/health                        Health check
POST /api/v1/signup                     Create account → returns fk_xxx key
```

### Protected (API key)
```
POST /api/v1/analyze/confidence         LLM confidence scoring
POST /api/v1/analyze/spec               LLM spec comparison
GET  /api/v1/usage                      Usage + plan limits
POST /api/v1/api-keys/rotate            Rotate API key
```

### Auth (Google OAuth)
```
GET  /api/auth/google/login             Start OAuth
GET  /api/auth/google/callback          Callback
GET  /api/auth/me                       Current user
POST /api/auth/logout                   Logout
```

### Billing (Stripe)
```
POST /api/v1/billing/checkout           Stripe checkout session
POST /api/v1/billing/portal             Billing portal session
GET  /api/v1/billing/subscription       Subscription status
POST /api/v1/billing/webhook            Stripe webhook
```

## Database Schema (fault-cloud)

```sql
users (id, email, plan, api_key_hash, stripe_customer_id,
       google_id, name, picture_url, created_at, last_active_at)

usage (id, user_id, month, llm_calls, tokens_input, tokens_output, analyses)

subscriptions (id, user_id, stripe_subscription_id, stripe_customer_id,
               plan, status, current_period_start, current_period_end,
               created_at, updated_at)
```

Migrations run automatically on startup via embedded SQL.

## Environment Variables

```bash
# CLI
FAULT_API_KEY=fk_xxx                     # Pro features
FAULT_API_URL=https://fault.jabbey.io    # Cloud API URL

# Cloud server
DATABASE_URL=postgres://...              # PostgreSQL
ANTHROPIC_API_KEY=sk-ant-xxx             # LLM analysis
GOOGLE_CLIENT_ID=xxx                     # OAuth (optional)
GOOGLE_CLIENT_SECRET=xxx
JWT_SECRET=xxx
STRIPE_SECRET_KEY=sk_xxx                 # Billing (optional)
STRIPE_WEBHOOK_SECRET=whsec_xxx
STRIPE_PRICE_ID_PRO=price_xxx
PORT=8082
```

## Deployment

**Auto-deploys via GitHub Actions on push to master.** Do NOT manually deploy.

- CI: https://github.com/joeabbey/fault/actions
- Cloud runs on VM 104 (10.10.10.104), k3s namespace `fault`
- Public URL: https://fault.jabbey.io

```bash
ssh fault-vm                                    # Access VM
kubectl --context=fault get pods -n fault       # Check pods
kubectl --context=fault logs -n fault deployment/fault --tail=50
```

## Go Coding Guidelines

- Initialize slices with `make()` for JSON responses (nil → `null`, empty → `[]`)
- File size limit: ~500 lines per file
- `GOPRIVATE=github.com/joeabbey/magma` required for private module

## Common Tasks

### Add a New Analyzer
1. Create `pkg/analyzer/{name}.go` implementing `analyzer.Analyzer`
2. Register in `runCheck()`, `runFix()`, `runWatch()`, `baselineCmd()` in `cmd/fault/main.go`
3. Add tests and fixtures in `testdata/`

### Add a New Language
1. Create `pkg/parser/{lang}.go` implementing `Parser`
2. Add `_test.go` with fixtures
3. Register via `reg.Register()` in `registerAllParsers()`

### Issue Suppression
Add `// fault:ignore` inline comment to suppress a specific issue.
