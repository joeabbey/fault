import type {
	UsageResponse,
	RotateKeyResponse,
	CheckoutResponse,
	PortalResponse,
	SubscriptionResponse,
	ErrorResponse,
	ListRunsResponse,
	ListSpecResultsResponse,
	ListAuditResponse,
	Organization,
	OrgDetailResponse,
	OrgMember,
	Run,
	RunStats
} from './types';

const API_BASE = '/api';

async function request<T>(endpoint: string, options: RequestInit = {}): Promise<T> {
	const headers: Record<string, string> = {
		'Content-Type': 'application/json',
		...(options.headers as Record<string, string>)
	};

	const response = await fetch(`${API_BASE}${endpoint}`, {
		...options,
		headers,
		credentials: 'include'
	});

	if (response.status === 401 || response.status === 403) {
		throw new AuthError('Unauthorized');
	}

	if (!response.ok) {
		const body = (await response.json().catch(() => ({ error: 'Request failed' }))) as ErrorResponse;
		throw new Error(body.error || `HTTP ${response.status}`);
	}

	return response.json() as Promise<T>;
}

export class AuthError extends Error {
	constructor(message: string) {
		super(message);
		this.name = 'AuthError';
	}
}

export const api = {
	usage(): Promise<UsageResponse> {
		return request('/v1/usage');
	},

	rotateKey(): Promise<RotateKeyResponse> {
		return request('/v1/api-keys/rotate', { method: 'POST' });
	},

	runs: {
		list(limit = 20, offset = 0): Promise<ListRunsResponse> {
			return request(`/v1/runs?limit=${limit}&offset=${offset}`);
		},

		get(id: string): Promise<Run> {
			return request(`/v1/runs/${id}`);
		},

		stats(): Promise<RunStats> {
			return request('/v1/runs/stats');
		}
	},

	specs: {
		results(limit = 20, offset = 0): Promise<ListSpecResultsResponse> {
			return request(`/v1/spec/results?limit=${limit}&offset=${offset}`);
		}
	},

	orgs: {
		list(): Promise<Organization[]> {
			return request('/v1/orgs');
		},

		get(slug: string): Promise<OrgDetailResponse> {
			return request(`/v1/orgs/${slug}`);
		},

		members(slug: string): Promise<OrgMember[]> {
			return request(`/v1/orgs/${slug}/members`);
		},

		runs(slug: string, limit = 20, offset = 0): Promise<ListRunsResponse> {
			return request(`/v1/orgs/${slug}/runs?limit=${limit}&offset=${offset}`);
		},

		stats(slug: string): Promise<RunStats> {
			return request(`/v1/orgs/${slug}/runs/stats`);
		},

		audit(slug: string, limit = 50, offset = 0): Promise<ListAuditResponse> {
			return request(`/v1/orgs/${slug}/audit?limit=${limit}&offset=${offset}`);
		}
	},

	billing: {
		subscription(): Promise<SubscriptionResponse> {
			return request('/v1/billing/subscription');
		},

		checkout(plan: string): Promise<CheckoutResponse> {
			return request('/v1/billing/checkout', {
				method: 'POST',
				body: JSON.stringify({ plan })
			});
		},

		portal(): Promise<PortalResponse> {
			return request('/v1/billing/portal', { method: 'POST' });
		}
	}
};
