# Using Fault with Claude Code

Claude Code is an agentic coding tool that makes multi-file changes directly in your terminal. Fault validates those changes before they enter your commit history, catching broken imports, cross-file inconsistencies, and stale references that agents commonly introduce.

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

This creates a `.fault.yaml` config and installs a git pre-commit hook that runs `fault check --staged` before every commit.

## How it works with Claude Code

The typical Claude Code workflow is:

1. You give Claude a task: "Add a user preferences API endpoint"
2. Claude edits multiple files -- handlers, routes, models, tests
3. You review the changes in your terminal
4. You stage and commit the changes

Fault fits between steps 3 and 4. When you run `git commit`, the pre-commit hook triggers `fault check --staged` automatically. If Fault finds structural errors, the commit is blocked and you see exactly what needs fixing.

### Example session

```
$ claude "Add a new API endpoint for user preferences"

Claude edits 5 files:
  pkg/models/preferences.go      (new file)
  pkg/api/handlers_preferences.go (new file)
  pkg/api/routes.go              (modified)
  pkg/storage/preferences.go     (new file)
  pkg/api/handlers_preferences_test.go (new file)

$ git add -A
$ git commit -m "Add user preferences endpoint"

Running fault check --staged...

  pkg/api/handlers_preferences.go
    8:2   error  [imports]       Import 'github.com/example/app/pkg/models' references
                                 unexported type 'Preferences' (should be exported)
    45:12 error  [consistency]   Method 'store.GetPreferences(ctx, userID)' called but
                                 'pkg/storage/preferences.go' defines
                                 'GetPreferences(ctx, id, includeDefaults)'

  pkg/api/routes.go
    67:3  warning  [references]  Route '/api/prefs' registered but handler references
                                 '/api/preferences' in documentation comment

  2 errors, 1 warning
  Commit blocked (block_on: error)
```

At this point you can either fix the issues yourself or ask Claude to fix them:

```
$ claude "Fix the issues fault reported: the Preferences type needs to be exported,
          GetPreferences needs the includeDefaults parameter, and the route
          path doesn't match the doc comment"
```

Then commit again:

```
$ git add -A
$ git commit -m "Add user preferences endpoint"

Running fault check --staged...

  No issues found.
  All checks passed.

[main abc1234] Add user preferences endpoint
 5 files changed, 287 insertions(+)
```

## Running Fault manually

You do not need the pre-commit hook to use Fault. You can run it directly at any point:

```bash
# Check staged changes
fault check --staged

# Check all uncommitted changes (staged + unstaged)
fault check

# Check changes on a feature branch vs main
fault check --branch main
```

This is useful when you want to validate changes mid-session, before you are ready to commit.

## Using with Claude Code hooks

Claude Code supports [hooks](https://docs.anthropic.com/en/docs/claude-code/hooks) -- shell commands that run in response to agent actions. You can configure Fault to run after Claude Code modifies files, giving you immediate feedback without waiting until commit time.

Add this to your `.claude/settings.json` (project-level) or `~/.claude/settings.json` (global):

```json
{
  "hooks": {
    "PostToolUse": [
      {
        "matcher": "Edit|Write",
        "command": "fault check 2>&1 | tail -20"
      }
    ]
  }
}
```

This runs `fault check` after every file edit or write operation. The output is shown to Claude Code, which can then fix issues proactively before you even review the changes.

A lighter alternative is to run Fault only after Claude finishes a full task, using the `Stop` event:

```json
{
  "hooks": {
    "Stop": [
      {
        "command": "fault check 2>&1"
      }
    ]
  }
}
```

## Recommended configuration for Claude Code

AI agents are particularly prone to import errors and cross-file inconsistencies. This configuration focuses on the checks that matter most:

```yaml
# .fault.yaml
version: 1
languages: [go, typescript, python]
block_on: error

analyzers:
  imports: true        # Critical -- agents often add imports for symbols that
                       # don't exist or forget to export new types
  consistency: true    # Important -- agents change function signatures without
                       # updating all callers
  references: true     # Important -- agents rename/delete files but leave
                       # stale references elsewhere
  tests: true          # Useful reminder, but warning-only
  patterns: true       # Catches leftover TODOs, console.logs, debug artifacts

ignore:
  - "vendor/"
  - "node_modules/"
  - "dist/"
  - "*.generated.*"
  - "*.min.js"
```

Set `block_on: error` (the default) so that only definite structural problems block commits. Warnings are still reported but won't prevent you from committing.

## LLM-powered analysis (Pro)

For deeper validation, enable LLM-powered analysis. This sends the diff to the Fault API for confidence scoring and optional spec comparison:

```bash
export FAULT_API_KEY=fk_your_key_here
```

```yaml
# .fault.yaml
llm:
  enabled: true
  spec_file: "SPEC.md"  # optional -- compare changes against a requirements doc
```

With LLM analysis enabled, Fault provides per-file confidence scores and flags deviations from your spec. This is particularly valuable for larger changes where static analysis alone may miss semantic issues.

Get an API key at [fault.jabbey.io](https://fault.jabbey.io).

## Tips

**Let Claude fix Fault errors.** When Fault blocks a commit, paste the error output into Claude Code. It can usually fix structural issues in one pass.

**Use `--branch` for large features.** If Claude is working on a feature branch with many commits, run `fault check --branch main` to validate the entire branch diff, not just the latest staged changes.

**Bypass when needed.** If Fault reports a false positive and you are confident the code is correct, bypass the hook with `git commit --no-verify`. Consider adding the file pattern to the `ignore` list if it recurs.

**SARIF output for GitHub.** If your project uses GitHub Code Scanning, configure CI to run `fault check --format=sarif` and upload the results. This surfaces Fault issues as code annotations on pull requests.
