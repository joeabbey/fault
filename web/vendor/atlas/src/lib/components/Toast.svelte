<script lang="ts" module>
	import { writable } from 'svelte/store';

	export interface ToastItem {
		id: string;
		type: 'info' | 'success' | 'warning' | 'error';
		title?: string;
		message: string;
		duration?: number;
		dismissible?: boolean;
	}

	function createToastStore() {
		const { subscribe, update } = writable<ToastItem[]>([]);

		function add(toast: Omit<ToastItem, 'id'>) {
			const id = Math.random().toString(36).slice(2, 9);
			const newToast: ToastItem = {
				id,
				dismissible: true,
				duration: 5000,
				...toast
			};

			update((toasts) => [...toasts, newToast]);

			if (newToast.duration && newToast.duration > 0) {
				setTimeout(() => {
					remove(id);
				}, newToast.duration);
			}

			return id;
		}

		function remove(id: string) {
			update((toasts) => toasts.filter((t) => t.id !== id));
		}

		function clear() {
			update(() => []);
		}

		return {
			subscribe,
			add,
			remove,
			clear,
			info: (message: string, options?: Partial<ToastItem>) =>
				add({ type: 'info', message, ...options }),
			success: (message: string, options?: Partial<ToastItem>) =>
				add({ type: 'success', message, ...options }),
			warning: (message: string, options?: Partial<ToastItem>) =>
				add({ type: 'warning', message, ...options }),
			error: (message: string, options?: Partial<ToastItem>) =>
				add({ type: 'error', message, ...options })
		};
	}

	export const toast = createToastStore();
</script>

<script lang="ts">
	import { cn } from '$lib/utils/cn';
	import { flip } from 'svelte/animate';

	interface Props {
		position?:
			| 'top-left'
			| 'top-center'
			| 'top-right'
			| 'bottom-left'
			| 'bottom-center'
			| 'bottom-right';
		class?: string;
	}

	let { position = 'bottom-right', class: className }: Props = $props();

	const positions = {
		'top-left': 'top-4 left-4',
		'top-center': 'top-4 left-1/2 -translate-x-1/2',
		'top-right': 'top-4 right-4',
		'bottom-left': 'bottom-4 left-4',
		'bottom-center': 'bottom-4 left-1/2 -translate-x-1/2',
		'bottom-right': 'bottom-4 right-4'
	};

	const typeStyles = {
		info: 'bg-primary-50 border-primary-200 text-primary-900 dark:bg-primary-900 dark:border-primary-700 dark:text-primary-300',
		success:
			'bg-success-50 border-success-200 text-success-900 dark:bg-success-900 dark:border-success-700 dark:text-success-300',
		warning:
			'bg-warning-50 border-warning-200 text-warning-900 dark:bg-warning-900 dark:border-warning-700 dark:text-warning-300',
		error:
			'bg-error-50 border-error-200 text-error-900 dark:bg-error-900 dark:border-error-700 dark:text-error-300'
	};

	const icons = {
		info: `<svg class="h-5 w-5" viewBox="0 0 20 20" fill="currentColor"><path fill-rule="evenodd" d="M18 10a8 8 0 11-16 0 8 8 0 0116 0zm-7-4a1 1 0 11-2 0 1 1 0 012 0zM9 9a1 1 0 000 2v3a1 1 0 001 1h1a1 1 0 100-2v-3a1 1 0 00-1-1H9z" clip-rule="evenodd" /></svg>`,
		success: `<svg class="h-5 w-5" viewBox="0 0 20 20" fill="currentColor"><path fill-rule="evenodd" d="M10 18a8 8 0 100-16 8 8 0 000 16zm3.707-9.293a1 1 0 00-1.414-1.414L9 10.586 7.707 9.293a1 1 0 00-1.414 1.414l2 2a1 1 0 001.414 0l4-4z" clip-rule="evenodd" /></svg>`,
		warning: `<svg class="h-5 w-5" viewBox="0 0 20 20" fill="currentColor"><path fill-rule="evenodd" d="M8.257 3.099c.765-1.36 2.722-1.36 3.486 0l5.58 9.92c.75 1.334-.213 2.98-1.742 2.98H4.42c-1.53 0-2.493-1.646-1.743-2.98l5.58-9.92zM11 13a1 1 0 11-2 0 1 1 0 012 0zm-1-8a1 1 0 00-1 1v3a1 1 0 002 0V6a1 1 0 00-1-1z" clip-rule="evenodd" /></svg>`,
		error: `<svg class="h-5 w-5" viewBox="0 0 20 20" fill="currentColor"><path fill-rule="evenodd" d="M10 18a8 8 0 100-16 8 8 0 000 16zM8.707 7.293a1 1 0 00-1.414 1.414L8.586 10l-1.293 1.293a1 1 0 101.414 1.414L10 11.414l1.293 1.293a1 1 0 001.414-1.414L11.414 10l1.293-1.293a1 1 0 00-1.414-1.414L10 8.586 8.707 7.293z" clip-rule="evenodd" /></svg>`
	};
</script>

<div
	class={cn('fixed z-50 flex flex-col gap-2 pointer-events-none', positions[position], className)}
	aria-live="polite"
	aria-label="Notifications"
>
	{#each $toast as item (item.id)}
		<div
			animate:flip={{ duration: 200 }}
			class={cn(
				'pointer-events-auto w-80 rounded-lg border p-4 shadow-lg',
				'animate-in',
				typeStyles[item.type]
			)}
			role="alert"
		>
			<div class="flex items-start gap-3">
				<div class="flex-shrink-0">
					{@html icons[item.type]}
				</div>
				<div class="flex-1 min-w-0">
					{#if item.title}
						<p class="text-sm font-medium">{item.title}</p>
						<p class="mt-1 text-sm opacity-90">{item.message}</p>
					{:else}
						<p class="text-sm">{item.message}</p>
					{/if}
				</div>
				{#if item.dismissible}
					<button
						type="button"
						onclick={() => toast.remove(item.id)}
						class="flex-shrink-0 rounded-md p-1 opacity-50 hover:opacity-100 focus:outline-none focus:ring-2 focus:ring-offset-2 transition-opacity"
					>
						<span class="sr-only">Dismiss</span>
						<svg class="h-4 w-4" viewBox="0 0 20 20" fill="currentColor">
							<path
								d="M6.28 5.22a.75.75 0 00-1.06 1.06L8.94 10l-3.72 3.72a.75.75 0 101.06 1.06L10 11.06l3.72 3.72a.75.75 0 101.06-1.06L11.06 10l3.72-3.72a.75.75 0 00-1.06-1.06L10 8.94 6.28 5.22z"
							/>
						</svg>
					</button>
				{/if}
			</div>
		</div>
	{/each}
</div>
