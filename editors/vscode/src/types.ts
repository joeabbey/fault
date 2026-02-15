/** Matches the Severity type from pkg/analyzer/analyzer.go */
export type Severity = "error" | "warning" | "info";

/** Matches the Issue struct from pkg/analyzer/analyzer.go */
export interface FaultIssue {
  id: string;
  fix_id?: string;
  severity: Severity;
  category: string;
  file: string;
  line?: number;
  end_line?: number;
  message: string;
  suggestion?: string;
  related_files?: string[];
}

/** Matches the Confidence struct from pkg/analyzer/analyzer.go */
export interface FaultConfidence {
  score: number;
  factors?: string[];
}

/** Matches the AnalysisResult struct from pkg/analyzer/analyzer.go */
export interface FaultAnalysisResult {
  repo_path: string;
  branch: string;
  commit_range: string;
  timestamp: string;
  duration: number;
  files_changed: number;
  issues: FaultIssue[];
  confidence?: FaultConfidence;
  summary?: string;
}

/** Extension configuration from package.json contributes.configuration */
export interface FaultConfig {
  executablePath: string;
  autoCheckOnSave: boolean;
  severity: Severity;
}
