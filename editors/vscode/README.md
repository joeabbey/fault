# Fault - AI Code Validator (VS Code Extension)

Integrates [Fault](https://github.com/joeabbey/fault) into VS Code, providing real-time AI-powered code validation directly in your editor.

## Features

- **Automatic checking on save** -- runs `fault check` when you save a file, showing issues as VS Code diagnostics
- **Inline diagnostics** -- errors, warnings, and info-level issues appear directly in the editor with squiggly underlines
- **Quick fixes** -- code actions offer to apply Fault suggestions via `fault fix`
- **Status bar** -- shows the current issue count at a glance
- **Watch mode** -- toggle continuous checking with the `Fault: Toggle Watch Mode` command

## Supported Languages

JavaScript, TypeScript, Python, Go, Java, Rust

## Requirements

- The `fault` CLI must be installed and available on your PATH (or configure the path in settings)
- A git repository (Fault analyzes diffs)

## Extension Settings

| Setting | Default | Description |
|---------|---------|-------------|
| `fault.executablePath` | `"fault"` | Path to the fault executable |
| `fault.autoCheckOnSave` | `true` | Automatically run fault check on save |
| `fault.severity` | `"info"` | Minimum severity to display: `error`, `warning`, or `info` |

## Commands

| Command | Description |
|---------|-------------|
| `Fault: Run Check` | Manually run fault check on the workspace |
| `Fault: Run Auto-Fix` | Run fault fix to automatically resolve issues |
| `Fault: Toggle Watch Mode` | Enable/disable continuous checking |

## How It Works

The extension shells out to the `fault` CLI binary, running `fault check --format json --unstaged` and parsing the JSON output into VS Code diagnostics. It does not bundle or import the Fault source code.

## Installation

### From source

```bash
cd editors/vscode
npm install
npm run build
npm run package
code --install-extension fault-vscode-0.1.0.vsix
```

### From the CLI

```bash
fault --install-vscode
```
