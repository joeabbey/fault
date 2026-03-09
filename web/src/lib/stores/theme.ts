import { writable } from 'svelte/store';
import { browser } from '$app/environment';

function getInitialTheme(): 'light' | 'dark' {
	if (!browser) return 'dark';
	const stored = localStorage.getItem('theme');
	if (stored === 'light' || stored === 'dark') return stored;
	return window.matchMedia('(prefers-color-scheme: dark)').matches ? 'dark' : 'dark';
}

function createThemeStore() {
	const { subscribe, set, update } = writable<'light' | 'dark'>(getInitialTheme());

	return {
		subscribe,
		toggle() {
			update((current) => {
				const next = current === 'dark' ? 'light' : 'dark';
				if (browser) {
					localStorage.setItem('theme', next);
					// Sync with dashboard (which uses documentElement class)
					document.documentElement.classList.toggle('dark', next === 'dark');
				}
				return next;
			});
		},
		init() {
			if (!browser) return;
			const theme = getInitialTheme();
			set(theme);
			document.documentElement.classList.toggle('dark', theme === 'dark');
		}
	};
}

export const theme = createThemeStore();
