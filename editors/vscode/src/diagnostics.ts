import * as vscode from "vscode";
import * as path from "path";
import { FaultAnalysisResult, FaultIssue, Severity } from "./types";

const severityOrder: Record<Severity, number> = {
  error: 0,
  warning: 1,
  info: 2,
};

function toVSCodeSeverity(severity: Severity): vscode.DiagnosticSeverity {
  switch (severity) {
    case "error":
      return vscode.DiagnosticSeverity.Error;
    case "warning":
      return vscode.DiagnosticSeverity.Warning;
    case "info":
      return vscode.DiagnosticSeverity.Information;
    default:
      return vscode.DiagnosticSeverity.Warning;
  }
}

/**
 * Parse raw JSON output from `fault check --format json` into a typed result.
 * Returns null if the output is not valid JSON.
 */
export function parseFaultOutput(
  jsonOutput: string,
): FaultAnalysisResult | null {
  try {
    const result: FaultAnalysisResult = JSON.parse(jsonOutput);
    if (!result.issues) {
      result.issues = [];
    }
    return result;
  } catch {
    return null;
  }
}

/**
 * Filter issues by minimum severity level.
 */
export function filterBySeverity(
  issues: FaultIssue[],
  minSeverity: Severity,
): FaultIssue[] {
  const minOrder = severityOrder[minSeverity];
  return issues.filter((issue) => severityOrder[issue.severity] <= minOrder);
}

/**
 * Convert Fault issues into VS Code diagnostics, grouped by file URI.
 */
export function mapToDiagnostics(
  issues: FaultIssue[],
  workspaceRoot: string,
): Map<vscode.Uri, vscode.Diagnostic[]> {
  const diagnosticMap = new Map<vscode.Uri, vscode.Diagnostic[]>();

  for (const issue of issues) {
    const filePath = path.isAbsolute(issue.file)
      ? issue.file
      : path.join(workspaceRoot, issue.file);
    const uri = vscode.Uri.file(filePath);

    const line = Math.max(0, (issue.line ?? 1) - 1);
    const endLine = issue.end_line ? issue.end_line - 1 : line;

    const range = new vscode.Range(line, 0, endLine, Number.MAX_SAFE_INTEGER);

    let message = issue.message;
    if (issue.suggestion) {
      message += `\n\nSuggestion: ${issue.suggestion}`;
    }

    const diagnostic = new vscode.Diagnostic(
      range,
      message,
      toVSCodeSeverity(issue.severity),
    );
    diagnostic.source = "Fault";
    diagnostic.code = issue.category;

    if (issue.related_files && issue.related_files.length > 0) {
      diagnostic.relatedInformation = issue.related_files.map((relFile) => {
        const relPath = path.isAbsolute(relFile)
          ? relFile
          : path.join(workspaceRoot, relFile);
        return new vscode.DiagnosticRelatedInformation(
          new vscode.Location(vscode.Uri.file(relPath), new vscode.Position(0, 0)),
          `Related file: ${relFile}`,
        );
      });
    }

    const existing = diagnosticMap.get(uri) ?? [];
    existing.push(diagnostic);
    diagnosticMap.set(uri, existing);
  }

  return diagnosticMap;
}
