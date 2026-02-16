import { writable, derived } from 'svelte/store';

interface AuthState {
	apiKey: string | null;
	email: string | null;
	loading: boolean;
}

function createAuthStore() {
	const { subscribe, set, update } = writable<AuthState>({
		apiKey: null,
		email: null,
		loading: true
	});

	return {
		subscribe,
		init() {
			if (typeof localStorage === 'undefined') return;
			const key = localStorage.getItem('fault_api_key');
			const email = localStorage.getItem('fault_email');
			set({ apiKey: key, email, loading: false });
		},
		login(apiKey: string, email: string) {
			localStorage.setItem('fault_api_key', apiKey);
			localStorage.setItem('fault_email', email);
			set({ apiKey, email, loading: false });
		},
		logout() {
			localStorage.removeItem('fault_api_key');
			localStorage.removeItem('fault_email');
			set({ apiKey: null, email: null, loading: false });
		},
		setLoading(loading: boolean) {
			update((s) => ({ ...s, loading }));
		},
		updateKey(apiKey: string) {
			localStorage.setItem('fault_api_key', apiKey);
			update((s) => ({ ...s, apiKey }));
		}
	};
}

export const auth = createAuthStore();
export const isAuthenticated = derived(auth, ($auth) => !!$auth.apiKey);
export const isLoading = derived(auth, ($auth) => $auth.loading);
export const currentEmail = derived(auth, ($auth) => $auth.email);
