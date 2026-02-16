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
