<script lang="ts">
	import { cn } from '$lib/utils/cn';
	import { theme, type Theme } from '$lib/utils/theme';

	interface Props {
		size?: 'sm' | 'md';
		class?: string;
	}

	let { size = 'sm', class: className }: Props = $props();

	let current: Theme = $state('system');
	theme.subscribe((v) => (current = v));

	const options: { value: Theme; label: string }[] = [
		{ value: 'light', label: 'Light' },
		{ value: 'system', label: 'System' },
		{ value: 'dark', label: 'Dark' }
	];

	const sizes = {
		sm: 'h-7 px-0.5 gap-0.5',
		md: 'h-8 px-1 gap-1'
	};

	const buttonSizes = {
		sm: 'h-6 w-6',
		md: 'h-6 w-7'
	};

	const iconSizes = {
		sm: 'h-3.5 w-3.5',
		md: 'h-4 w-4'
	};
</script>

<div
	class={cn(
		'inline-flex items-center rounded-full bg-secondary-100 dark:bg-secondary-800',
		sizes[size],
		className
	)}
	role="radiogroup"
	aria-label="Theme preference"
>
	{#each options as opt}
		<button
			type="button"
			role="radio"
			aria-checked={current === opt.value}
			aria-label="{opt.label} theme"
			onclick={() => theme.set(opt.value)}
			class={cn(
				'inline-flex items-center justify-center rounded-full transition-colors',
				buttonSizes[size],
				current === opt.value
					? 'bg-white dark:bg-secondary-600 text-foreground shadow-sm'
					: 'text-muted hover:text-foreground'
			)}
		>
			{#if opt.value === 'light'}
				<!-- Sun -->
				<svg class={iconSizes[size]} fill="none" stroke="currentColor" viewBox="0 0 24 24" stroke-width="2">
					<circle cx="12" cy="12" r="5" />
					<path d="M12 1v2M12 21v2M4.22 4.22l1.42 1.42M18.36 18.36l1.42 1.42M1 12h2M21 12h2M4.22 19.78l1.42-1.42M18.36 5.64l1.42-1.42" />
				</svg>
			{:else if opt.value === 'system'}
				<!-- Monitor -->
				<svg class={iconSizes[size]} fill="none" stroke="currentColor" viewBox="0 0 24 24" stroke-width="2">
					<rect x="2" y="3" width="20" height="14" rx="2" />
					<path d="M8 21h8M12 17v4" />
				</svg>
			{:else}
				<!-- Moon -->
				<svg class={iconSizes[size]} fill="none" stroke="currentColor" viewBox="0 0 24 24" stroke-width="2">
					<path d="M21 12.79A9 9 0 1111.21 3 7 7 0 0021 12.79z" />
				</svg>
			{/if}
		</button>
	{/each}
</div>
