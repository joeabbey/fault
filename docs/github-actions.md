# GitHub Actions Integration

## Quick Start

Add this to `.github/workflows/fault.yml`:

```yaml
name: Fault Check
on:
  pull_request:

permissions:
  security-events: write
  pull-requests: write

jobs:
  fault:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
        with:
          fetch-depth: 0  # Need full history for branch diff
      - uses: joeabbey/fault/.github/actions/fault-action@main
```

This will post inline review comments on your PR diffs and upload SARIF results to GitHub Code Scanning.

## PR Review Comments

By default, Fault posts inline review comments directly on your PR diff. Each finding appears as a comment on the exact line where the issue was detected.

Review comments are enabled by default. To disable them and use SARIF-only output:

```yaml
      - uses: joeabbey/fault/.github/actions/fault-action@main
        with:
          review-comments: 'false'
```

### Required Permissions

PR review comments require the `pull-requests: write` permission:

```yaml
permissions:
  security-events: write    # For SARIF upload to Code Scanning
  pull-requests: write      # For inline PR review comments
```

### How It Works

When `review-comments` is enabled (default) and the workflow runs on a pull request:

1. Fault runs with `--format github`, which posts inline comments via the GitHub API
2. Each finding with a file and line number becomes an inline comment on the PR diff
3. Findings without line information appear in the review summary
4. SARIF is also written to `fault-results.sarif` for Code Scanning upload
5. Both inline comments and Code Scanning annotations are visible on the PR

When the event is not a pull request (e.g., push to main), Fault falls back to SARIF-only output.

### Custom GitHub Token

The action uses the default `GITHUB_TOKEN` for posting review comments. To use a custom token (e.g., a PAT with broader permissions):

```yaml
      - uses: joeabbey/fault/.github/actions/fault-action@main
        with:
          github-token: ${{ secrets.MY_GITHUB_TOKEN }}
```

## With Pro Features

```yaml
      - uses: joeabbey/fault/.github/actions/fault-action@main
        with:
          fault-api-key: ${{ secrets.FAULT_API_KEY }}
```

## With Baseline

To adopt Fault in an existing project without fixing every pre-existing issue:

```bash
# Generate a baseline of current issues
fault baseline
```

This creates `.fault-baseline.json`. Commit it to your repo, then use it in CI:

```yaml
      - uses: joeabbey/fault/.github/actions/fault-action@main
        with:
          args: '--baseline'
```

Only new issues (not in the baseline) will be reported.

## Inline Suppression

You can suppress individual issues with comments:

```typescript
// fault:ignore imports
import { thing } from './legacy-module';

// fault:ignore *
const x = eval('code');  // suppresses all categories on this line
```

## Options

| Input | Description | Default |
|-------|-------------|---------|
| `version` | Fault version to install | `latest` |
| `args` | Additional arguments | `` |
| `fault-api-key` | API key for Pro features | `` |
| `github-token` | GitHub token for PR review comments | `${{ github.token }}` |
| `review-comments` | Post inline review comments on PRs | `true` |

## How It Works

1. Installs the Fault CLI binary
2. Runs `fault check --branch <base> --format github` (or `sarif` if review comments are disabled)
3. Posts inline review comments on the PR diff (when in a PR context)
4. Writes SARIF results to `fault-results.sarif`
5. Uploads SARIF results to GitHub Code Scanning
6. Issues appear as both inline comments and Code Scanning annotations
