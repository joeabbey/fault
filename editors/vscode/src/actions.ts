import * as vscode from "vscode";

/**
 * Provides code actions for Fault diagnostics.
 * - "Apply Fault suggestion" when a diagnostic includes a suggestion
 * - "Run fault fix" to auto-fix via CLI
 */
export class FaultCodeActionProvider implements vscode.CodeActionProvider {
  public static readonly providedCodeActionKinds = [
    vscode.CodeActionKind.QuickFix,
  ];

  provideCodeActions(
    document: vscode.TextDocument,
    range: vscode.Range,
    context: vscode.CodeActionContext,
  ): vscode.CodeAction[] {
    const actions: vscode.CodeAction[] = [];

    for (const diagnostic of context.diagnostics) {
      if (diagnostic.source !== "Fault") {
        continue;
      }

      // If the diagnostic message contains a suggestion, offer to apply it
      const suggestionMatch = diagnostic.message.match(
        /\n\nSuggestion: (.+)$/s,
      );
      if (suggestionMatch) {
        const action = new vscode.CodeAction(
          "Apply Fault suggestion",
          vscode.CodeActionKind.QuickFix,
        );
        action.diagnostics = [diagnostic];
        action.isPreferred = true;
        // The suggestion is informational — run fault fix to actually apply
        action.command = {
          command: "fault.fix",
          title: "Run Fault Auto-Fix",
        };
        actions.push(action);
      }

      // Always offer "Run fault fix"
      const fixAction = new vscode.CodeAction(
        "Run fault fix",
        vscode.CodeActionKind.QuickFix,
      );
      fixAction.diagnostics = [diagnostic];
      fixAction.command = {
        command: "fault.fix",
        title: "Run Fault Auto-Fix",
      };
      // Avoid duplicating if we already added the preferred suggestion action
      if (!suggestionMatch) {
        actions.push(fixAction);
      }
    }

    return actions;
  }
}
