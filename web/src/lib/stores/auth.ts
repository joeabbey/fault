import { writable, derived } from 'svelte/store';

export interface AuthUser {
	id: string;
	email: string;
	name: string;
	picture_url: string;
	plan: string;
	has_api_key: boolean;
}

interface AuthState {
	user: AuthUser | null;
	loading: boolean;
}

function createAuthStore() {
	const { subscribe, set, update } = writable<AuthState>({
		user: null,
		loading: true
	});

	return {
		subscribe,
		async init() {
			try {
				const response = await fetch('/api/auth/me', { credentials: 'include' });
				const data = await response.json();
				set({ user: data.user || null, loading: false });
			} catch {
				set({ user: null, loading: false });
			}
		},
		setUser(user: AuthUser | null) {
			update((s) => ({ ...s, user, loading: false }));
		},
		async logout() {
			try {
				await fetch('/api/auth/logout', { method: 'POST', credentials: 'include' });
			} catch {
				// ignore
			}
			set({ user: null, loading: false });
		}
	};
}

export const auth = createAuthStore();
export const isAuthenticated = derived(auth, ($auth) => !!$auth.user);
export const isLoading = derived(auth, ($auth) => $auth.loading);
export const currentUser = derived(auth, ($auth) => $auth.user);
export const currentEmail = derived(auth, ($auth) => $auth.user?.email ?? null);
