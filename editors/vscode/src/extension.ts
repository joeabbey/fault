import * as vscode from "vscode";
import * as cp from "child_process";
import * as path from "path";
import { parseFaultOutput, filterBySeverity, mapToDiagnostics } from "./diagnostics";
import { FaultCodeActionProvider } from "./actions";
import { FaultConfig, Severity } from "./types";

let diagnosticCollection: vscode.DiagnosticCollection;
let statusBarItem: vscode.StatusBarItem;
let outputChannel: vscode.OutputChannel;
let checkTimeout: ReturnType<typeof setTimeout> | undefined;
let watchEnabled = false;

function getConfig(): FaultConfig {
  const config = vscode.workspace.getConfiguration("fault");
  return {
    executablePath: config.get<string>("executablePath", "fault"),
    autoCheckOnSave: config.get<boolean>("autoCheckOnSave", true),
    severity: config.get<Severity>("severity", "info"),
  };
}

function getWorkspaceRoot(): string | undefined {
  const folders = vscode.workspace.workspaceFolders;
  if (!folders || folders.length === 0) {
    return undefined;
  }
  return folders[0].uri.fsPath;
}

function updateStatusBar(errorCount: number, warningCount: number, infoCount: number): void {
  const total = errorCount + warningCount + infoCount;
  if (total === 0) {
    statusBarItem.text = "$(check) Fault: 0 issues";
    statusBarItem.backgroundColor = undefined;
  } else {
    const parts: string[] = [];
    if (errorCount > 0) parts.push(`${errorCount} error${errorCount !== 1 ? "s" : ""}`);
    if (warningCount > 0) parts.push(`${warningCount} warning${warningCount !== 1 ? "s" : ""}`);
    if (infoCount > 0) parts.push(`${infoCount} info`);
    statusBarItem.text = `$(warning) Fault: ${parts.join(", ")}`;
    if (errorCount > 0) {
      statusBarItem.backgroundColor = new vscode.ThemeColor("statusBarItem.errorBackground");
    } else if (warningCount > 0) {
      statusBarItem.backgroundColor = new vscode.ThemeColor("statusBarItem.warningBackground");
    } else {
      statusBarItem.backgroundColor = undefined;
    }
  }
  statusBarItem.show();
}

function runFaultCheck(): void {
  const workspaceRoot = getWorkspaceRoot();
  if (!workspaceRoot) {
    vscode.window.showWarningMessage("Fault: No workspace folder open.");
    return;
  }

  const config = getConfig();
  const executable = config.executablePath;
  const args = ["check", "--format", "json", "--unstaged"];

  outputChannel.appendLine(`Running: ${executable} ${args.join(" ")}`);
  outputChannel.appendLine(`Working directory: ${workspaceRoot}`);

  statusBarItem.text = "$(sync~spin) Fault: checking...";
  statusBarItem.show();

  cp.execFile(
    executable,
    args,
    {
      cwd: workspaceRoot,
      maxBuffer: 10 * 1024 * 1024, // 10MB
      timeout: 120_000, // 2 minutes
    },
    (error, stdout, stderr) => {
      if (stderr) {
        outputChannel.appendLine(`stderr: ${stderr}`);
      }

      // fault exits non-zero when issues are found — that's normal
      if (error && !stdout) {
        outputChannel.appendLine(`Error: ${error.message}`);
        statusBarItem.text = "$(error) Fault: failed";
        statusBarItem.backgroundColor = new vscode.ThemeColor("statusBarItem.errorBackground");
        statusBarItem.show();
        return;
      }

      const result = parseFaultOutput(stdout);
      if (!result) {
        outputChannel.appendLine("Failed to parse fault output.");
        outputChannel.appendLine(`Raw output: ${stdout.substring(0, 500)}`);
        statusBarItem.text = "$(error) Fault: parse error";
        statusBarItem.show();
        return;
      }

      outputChannel.appendLine(
        `Found ${result.issues.length} issues (${result.files_changed} files changed)`,
      );

      const filtered = filterBySeverity(result.issues, config.severity);
      const diagnosticsMap = mapToDiagnostics(filtered, workspaceRoot);

      // Clear old diagnostics and set new ones
      diagnosticCollection.clear();
      for (const [uri, diagnostics] of diagnosticsMap) {
        diagnosticCollection.set(uri, diagnostics);
      }

      // Count by severity for status bar
      let errors = 0, warnings = 0, infos = 0;
      for (const issue of filtered) {
        switch (issue.severity) {
          case "error": errors++; break;
          case "warning": warnings++; break;
          case "info": infos++; break;
        }
      }
      updateStatusBar(errors, warnings, infos);
    },
  );
}

function runFaultFix(): void {
  const workspaceRoot = getWorkspaceRoot();
  if (!workspaceRoot) {
    vscode.window.showWarningMessage("Fault: No workspace folder open.");
    return;
  }

  const config = getConfig();
  const executable = config.executablePath;
  const args = ["fix", "--unstaged"];

  outputChannel.appendLine(`Running: ${executable} ${args.join(" ")}`);

  vscode.window.withProgress(
    {
      location: vscode.ProgressLocation.Notification,
      title: "Fault: Running auto-fix...",
      cancellable: false,
    },
    () =>
      new Promise<void>((resolve) => {
        cp.execFile(
          executable,
          args,
          {
            cwd: workspaceRoot,
            maxBuffer: 10 * 1024 * 1024,
            timeout: 300_000, // 5 minutes for fixes
          },
          (error, stdout, stderr) => {
            if (stderr) {
              outputChannel.appendLine(`stderr: ${stderr}`);
            }
            if (error) {
              outputChannel.appendLine(`Error: ${error.message}`);
              vscode.window.showErrorMessage(`Fault fix failed: ${error.message}`);
            } else {
              outputChannel.appendLine(`Fix output: ${stdout}`);
              vscode.window.showInformationMessage("Fault: Auto-fix complete.");
            }
            // Re-run check to update diagnostics
            runFaultCheck();
            resolve();
          },
        );
      }),
  );
}

function debouncedCheck(): void {
  if (checkTimeout) {
    clearTimeout(checkTimeout);
  }
  checkTimeout = setTimeout(runFaultCheck, 300);
}

export function activate(context: vscode.ExtensionContext): void {
  outputChannel = vscode.window.createOutputChannel("Fault");
  diagnosticCollection = vscode.languages.createDiagnosticCollection("fault");
  statusBarItem = vscode.window.createStatusBarItem(vscode.StatusBarAlignment.Left, 100);
  statusBarItem.command = "fault.check";
  statusBarItem.tooltip = "Click to run Fault check";

  // Register commands
  context.subscriptions.push(
    vscode.commands.registerCommand("fault.check", runFaultCheck),
  );
  context.subscriptions.push(
    vscode.commands.registerCommand("fault.fix", runFaultFix),
  );
  context.subscriptions.push(
    vscode.commands.registerCommand("fault.watch", () => {
      watchEnabled = !watchEnabled;
      if (watchEnabled) {
        vscode.window.showInformationMessage("Fault: Watch mode enabled.");
        runFaultCheck();
      } else {
        vscode.window.showInformationMessage("Fault: Watch mode disabled.");
      }
    }),
  );

  // Auto-check on save
  context.subscriptions.push(
    vscode.workspace.onDidSaveTextDocument((document) => {
      const config = getConfig();
      if (!config.autoCheckOnSave && !watchEnabled) {
        return;
      }
      // Only trigger for supported languages
      const supported = [
        "javascript", "typescript", "python", "go", "java", "rust",
        "javascriptreact", "typescriptreact",
      ];
      if (supported.includes(document.languageId)) {
        debouncedCheck();
      }
    }),
  );

  // Register code action provider for all supported languages
  const supportedSelector: vscode.DocumentSelector = [
    { language: "javascript" },
    { language: "typescript" },
    { language: "javascriptreact" },
    { language: "typescriptreact" },
    { language: "python" },
    { language: "go" },
    { language: "java" },
    { language: "rust" },
  ];

  context.subscriptions.push(
    vscode.languages.registerCodeActionsProvider(
      supportedSelector,
      new FaultCodeActionProvider(),
      { providedCodeActionKinds: FaultCodeActionProvider.providedCodeActionKinds },
    ),
  );

  context.subscriptions.push(diagnosticCollection);
  context.subscriptions.push(statusBarItem);
  context.subscriptions.push(outputChannel);

  outputChannel.appendLine("Fault extension activated.");

  // Run initial check
  runFaultCheck();
}

export function deactivate(): void {
  if (checkTimeout) {
    clearTimeout(checkTimeout);
  }
}
