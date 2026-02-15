# GitHub Actions Integration

## Quick Start

Add this to `.github/workflows/fault.yml`:

```yaml
name: Fault Check
on:
  pull_request:

permissions:
  security-events: write

jobs:
  fault:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
        with:
          fetch-depth: 0  # Need full history for branch diff
      - uses: joeabbey/fault/.github/actions/fault-action@main
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

## How It Works

1. Installs the Fault CLI binary
2. Runs `fault check --branch <base> --format sarif` comparing PR changes against the base branch
3. Uploads SARIF results to GitHub Code Scanning
4. Issues appear as annotations on the PR diff
