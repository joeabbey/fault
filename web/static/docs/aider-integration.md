# Using Fault with aider

aider is an AI pair programming tool that edits code in your local git repo. Fault validates those edits before they enter your commit history, catching broken imports, stale references, and cross-file inconsistencies.

## Install Fault

```bash
# From source
go install github.com/joeabbey/fault/cmd/fault@latest

# Or download binary
curl -sSf https://fault.jabbey.io/install.sh | sh
```

## Set up your project

```bash
cd your-project
fault init
fault hook install
```

This creates a `.fault.yaml` config and installs a git pre-commit hook.

## How aider and Fault interact

By default, aider auto-commits every change it makes. The Fault pre-commit hook runs `fault check --staged` before each of those commits. If Fault finds errors, the commit is blocked and aider's auto-commit fails.

There are two ways to handle this:

### Option A: Let the pre-commit hook block aider's auto-commits

With the hook installed, aider's auto-commits are validated by Fault. When a commit is blocked:

1. aider reports the commit failed
2. You see the Fault output in your terminal
3. You can ask aider to fix the issues: "The commit failed because fault found broken imports in handlers.go -- fix them"
4. aider makes corrections and tries to commit again

This works well but can be noisy if aider makes many small commits.

### Option B: Disable auto-commits, validate manually

Run aider with `--no-auto-commits` to disable automatic committing. Then validate and commit manually:

```bash
# Start aider without auto-commits
aider --no-auto-commits

# Inside aider, make your changes...
# When done, exit aider and validate:

fault check
# or, after staging:
git add -A
fault check --staged

# If no errors, commit:
git commit -m "Add user preferences endpoint"
```

This gives you more control. You can batch multiple aider edits into a single commit after validating them together.

### Option C: Use aider's lint integration

aider supports a `--lint-cmd` flag that runs a linter after every edit. You can point this at Fault:

```bash
aider --lint-cmd "fault check"
```

When aider modifies a file, it runs `fault check` and feeds the output back to the model if there are issues. The model then attempts to fix the problems automatically before committing.

## Example session

```bash
$ aider --no-auto-commits

aider> Add a REST endpoint for user preferences with CRUD operations

# aider edits 4 files:
#   src/routes/preferences.ts
#   src/models/preferences.ts
#   src/index.ts
#   src/routes/preferences.test.ts

aider> /exit

$ fault check

  src/routes/preferences.ts
    3:1   error  [imports]       Import { PreferenceStore } from '../stores/preferences'
                                 -- module '../stores/preferences' does not exist
                                 (did you mean '../models/preferences'?)
    28:5  error  [consistency]   Function 'updatePreference(id, data)' called but
                                 definition is 'updatePreference(userId, id, data)'

  src/index.ts
    15:1  warning  [patterns]   console.log('preferences route loaded') --
                                debug statement

  2 errors, 1 warning

$ aider

aider> Fix the import path in preferences.ts (should be ../models not ../stores),
       add the missing userId parameter to the updatePreference call on line 28,
       and remove the console.log on line 15 of index.ts

# aider fixes the issues...

aider> /exit

$ fault check

  No issues found.
  All checks passed.

$ git add -A
$ git commit -m "Add user preferences CRUD endpoint"
```

## Recommended configuration for aider

```yaml
# .fault.yaml
version: 1
languages: [go, typescript, python]
block_on: error

analyzers:
  imports: true        # Critical -- agents frequently reference wrong module paths
  consistency: true    # Important -- signature mismatches across files
  references: true     # Important -- stale references after renames
  tests: true          # Helpful reminder to update tests
  patterns: true       # Catch debug artifacts left by the agent

ignore:
  - "vendor/"
  - "node_modules/"
  - "dist/"
  - "*.generated.*"
  - "*.min.js"
```

## LLM-powered analysis (Pro)

Enable LLM-powered analysis for confidence scoring and spec comparison:

```bash
export FAULT_API_KEY=fk_your_key_here
```

```yaml
# .fault.yaml
llm:
  enabled: true
  spec_file: "SPEC.md"  # optional
```

Get an API key at [fault.jabbey.io](https://fault.jabbey.io).

## Tips

**Use `--no-auto-commits` for larger tasks.** When aider is making changes across many files, disabling auto-commits lets you validate the full set of changes together rather than file-by-file.

**Use `--lint-cmd` for hands-free validation.** If you want aider to self-correct without manual intervention, the lint integration is the most seamless option.

**Check the whole branch.** After a long aider session with multiple commits, run `fault check --branch main` to validate the entire branch diff. Individual commits may pass but the combined changes could have cross-file issues.
