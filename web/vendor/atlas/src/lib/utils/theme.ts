import { writable } from 'svelte/store';

export type Theme = 'light' | 'dark' | 'system';

const STORAGE_KEY = 'theme';

function createThemeStore() {
	const { subscribe, set } = writable<Theme>('system');

	let mediaQuery: MediaQueryList | null = null;
	let mediaHandler: ((e: MediaQueryListEvent) => void) | null = null;
	let initialized = false;

	function applyTheme(theme: Theme) {
		if (typeof document === 'undefined') return;

		const isDark =
			theme === 'dark' ||
			(theme === 'system' && window.matchMedia('(prefers-color-scheme: dark)').matches);

		document.documentElement.classList.toggle('dark', isDark);

		// Listen/unlisten to system preference changes
		if (theme === 'system') {
			if (!mediaQuery) {
				mediaQuery = window.matchMedia('(prefers-color-scheme: dark)');
				mediaHandler = (e: MediaQueryListEvent) => {
					document.documentElement.classList.toggle('dark', e.matches);
				};
				mediaQuery.addEventListener('change', mediaHandler);
			}
		} else if (mediaQuery && mediaHandler) {
			mediaQuery.removeEventListener('change', mediaHandler);
			mediaQuery = null;
			mediaHandler = null;
		}
	}

	function setTheme(value: Theme) {
		set(value);
		applyTheme(value);
		if (typeof localStorage !== 'undefined') {
			localStorage.setItem(STORAGE_KEY, value);
		}
	}

	function init() {
		if (initialized) return;
		initialized = true;

		if (typeof localStorage === 'undefined') return;

		const stored = localStorage.getItem(STORAGE_KEY) as Theme | null;
		const initial = stored && ['light', 'dark', 'system'].includes(stored) ? stored : 'system';
		set(initial);
		applyTheme(initial);
	}

	return {
		subscribe,
		set: setTheme,
		init
	};
}

export const theme = createThemeStore();

export function initTheme() {
	theme.init();
}
