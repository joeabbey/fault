<script lang="ts">
	import { cn } from '$lib/utils/cn';

	interface Props {
		variant?: 'info' | 'success' | 'warning' | 'error';
		dismissible?: boolean;
		class?: string;
		children: import('svelte').Snippet;
		title?: import('svelte').Snippet;
		icon?: import('svelte').Snippet;
		ondismiss?: () => void;
	}

	let {
		variant = 'info',
		dismissible = false,
		class: className,
		children,
		title,
		icon,
		ondismiss
	}: Props = $props();

	let visible = $state(true);

	const baseClasses = 'relative rounded-lg border p-4';

	const variants = {
		info: 'bg-primary-50 border-primary-200 text-primary-900 dark:bg-primary-900 dark:border-primary-700 dark:text-primary-300',
		success:
			'bg-success-50 border-success-200 text-success-900 dark:bg-success-900 dark:border-success-700 dark:text-success-300',
		warning:
			'bg-warning-50 border-warning-200 text-warning-900 dark:bg-warning-900 dark:border-warning-700 dark:text-warning-300',
		error:
			'bg-error-50 border-error-200 text-error-900 dark:bg-error-900 dark:border-error-700 dark:text-error-300'
	};

	const iconColors = {
		info: 'text-primary-500',
		success: 'text-success-500',
		warning: 'text-warning-500',
		error: 'text-error-500'
	};

	function handleDismiss() {
		visible = false;
		ondismiss?.();
	}

	// Default icons
	const defaultIcons = {
		info: `<svg class="h-5 w-5" viewBox="0 0 20 20" fill="currentColor"><path fill-rule="evenodd" d="M18 10a8 8 0 11-16 0 8 8 0 0116 0zm-7-4a1 1 0 11-2 0 1 1 0 012 0zM9 9a1 1 0 000 2v3a1 1 0 001 1h1a1 1 0 100-2v-3a1 1 0 00-1-1H9z" clip-rule="evenodd" /></svg>`,
		success: `<svg class="h-5 w-5" viewBox="0 0 20 20" fill="currentColor"><path fill-rule="evenodd" d="M10 18a8 8 0 100-16 8 8 0 000 16zm3.707-9.293a1 1 0 00-1.414-1.414L9 10.586 7.707 9.293a1 1 0 00-1.414 1.414l2 2a1 1 0 001.414 0l4-4z" clip-rule="evenodd" /></svg>`,
		warning: `<svg class="h-5 w-5" viewBox="0 0 20 20" fill="currentColor"><path fill-rule="evenodd" d="M8.257 3.099c.765-1.36 2.722-1.36 3.486 0l5.58 9.92c.75 1.334-.213 2.98-1.742 2.98H4.42c-1.53 0-2.493-1.646-1.743-2.98l5.58-9.92zM11 13a1 1 0 11-2 0 1 1 0 012 0zm-1-8a1 1 0 00-1 1v3a1 1 0 002 0V6a1 1 0 00-1-1z" clip-rule="evenodd" /></svg>`,
		error: `<svg class="h-5 w-5" viewBox="0 0 20 20" fill="currentColor"><path fill-rule="evenodd" d="M10 18a8 8 0 100-16 8 8 0 000 16zM8.707 7.293a1 1 0 00-1.414 1.414L8.586 10l-1.293 1.293a1 1 0 101.414 1.414L10 11.414l1.293 1.293a1 1 0 001.414-1.414L11.414 10l1.293-1.293a1 1 0 00-1.414-1.414L10 8.586 8.707 7.293z" clip-rule="evenodd" /></svg>`
	};
</script>

{#if visible}
	<div role="alert" class={cn(baseClasses, variants[variant], className)}>
		<div class="flex">
			<div class={cn('flex-shrink-0', iconColors[variant])}>
				{#if icon}
					{@render icon()}
				{:else}
					{@html defaultIcons[variant]}
				{/if}
			</div>
			<div class="ml-3 flex-1">
				{#if title}
					<h3 class="text-sm font-medium">
						{@render title()}
					</h3>
					<div class="mt-1 text-sm opacity-90">
						{@render children()}
					</div>
				{:else}
					<p class="text-sm">
						{@render children()}
					</p>
				{/if}
			</div>
			{#if dismissible}
				<div class="ml-auto pl-3">
					<button
						type="button"
						onclick={handleDismiss}
						class="-m-1.5 inline-flex h-8 w-8 items-center justify-center rounded-md p-1.5 opacity-50 hover:opacity-100 focus:outline-none focus:ring-2 focus:ring-offset-2 transition-opacity"
					>
						<span class="sr-only">Dismiss</span>
						<svg class="h-5 w-5" viewBox="0 0 20 20" fill="currentColor">
							<path
								d="M6.28 5.22a.75.75 0 00-1.06 1.06L8.94 10l-3.72 3.72a.75.75 0 101.06 1.06L10 11.06l3.72 3.72a.75.75 0 101.06-1.06L11.06 10l3.72-3.72a.75.75 0 00-1.06-1.06L10 8.94 6.28 5.22z"
							/>
						</svg>
					</button>
				</div>
			{/if}
		</div>
	</div>
{/if}
