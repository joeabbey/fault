export interface UsageResponse {
	user_id: string;
	email: string;
	plan: string;
	month: string;
	llm_calls: number;
	tokens_input: number;
	tokens_output: number;
	analyses: number;
	llm_limit: number;
	llm_remaining: number;
}

export interface RotateKeyResponse {
	api_key: string;
	email: string;
}

export interface CheckoutResponse {
	checkout_url: string;
}

export interface PortalResponse {
	portal_url: string;
}

export interface SubscriptionResponse {
	plan: string;
	status: string;
	llm_calls: number;
	llm_limit: number;
	llm_remaining: number;
}

export interface ErrorResponse {
	error: string;
}

export interface Run {
	id: string;
	user_id: string;
	repo_url: string;
	branch: string;
	commit_sha: string;
	commit_range: string;
	mode: string;
	timestamp: string;
	duration_ms: number;
	files_changed: number;
	errors: number;
	warnings: number;
	infos: number;
	total_issues: number;
	issues: RunIssue[];
	confidence_score: number | null;
	summary: string;
	metadata: Record<string, unknown>;
	created_at: string;
}

export interface RunIssue {
	id: string;
	severity: string;
	category: string;
	file: string;
	line: number;
	message: string;
	suggestion?: string;
}

export interface ListRunsResponse {
	runs: Run[];
	total: number;
	limit: number;
	offset: number;
}

export interface RunStats {
	total_runs: number;
	total_issues: number;
	avg_errors: number;
	avg_warnings: number;
	avg_duration_ms: number;
}

export interface SpecResultEntry {
	id: string;
	user_id: string;
	spec_hash: string;
	spec_title: string;
	total_requirements: number;
	anchored_count: number;
	implemented_count: number;
	partial_count: number;
	missing_count: number;
	overall_score: number;
	result_json: RequirementResultEntry[];
	created_at: string;
}

export interface RequirementResultEntry {
	id: string;
	status: string;
	evidence: string;
	confidence: number;
}

export interface ListSpecResultsResponse {
	results: SpecResultEntry[];
	total: number;
	limit: number;
	offset: number;
}

export interface Organization {
	id: string;
	name: string;
	slug: string;
	owner_id: string;
	plan: string;
	created_at: string;
	updated_at: string;
}

export interface OrgMember {
	id: string;
	org_id: string;
	user_id: string;
	role: string;
	created_at: string;
	email?: string;
	name?: string;
}

export interface AuditEntry {
	id: string;
	org_id: string;
	user_id: string;
	action: string;
	resource_type: string;
	resource_id: string;
	details: Record<string, unknown>;
	created_at: string;
	user_email?: string;
}

export interface ListOrgsResponse {
	organizations: Organization[];
}

export interface OrgDetailResponse extends Organization {
	member_count: number;
}

export interface ListAuditResponse {
	entries: AuditEntry[];
	total: number;
	limit: number;
	offset: number;
}
