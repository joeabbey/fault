<script lang="ts">
	import { cn } from '$lib/utils/cn';

	interface Props {
		variant?: 'default' | 'interactive' | 'elevated';
		padding?: 'none' | 'sm' | 'md' | 'lg';
		class?: string;
		children: import('svelte').Snippet;
		header?: import('svelte').Snippet;
		footer?: import('svelte').Snippet;
		onclick?: (e: MouseEvent) => void;
	}

	let {
		variant = 'default',
		padding = 'md',
		class: className,
		children,
		header,
		footer,
		onclick
	}: Props = $props();

	const baseClasses = 'rounded-lg border border-border bg-card text-card-foreground';

	const variants = {
		default: '',
		interactive: 'cursor-pointer hover:border-primary-300 hover:shadow-md transition-all',
		elevated: 'shadow-lg border-0'
	};

	const paddings = {
		none: '',
		sm: 'p-3',
		md: 'p-4',
		lg: 'p-6'
	};

	const isInteractive = $derived(variant === 'interactive' || onclick);
</script>

{#if isInteractive}
	<div
		role="button"
		tabindex="0"
		{onclick}
		onkeydown={(e) => {
			if (e.key === 'Enter' || e.key === ' ') {
				e.preventDefault();
				onclick?.(e as unknown as MouseEvent);
			}
		}}
		class={cn(baseClasses, variants[variant], className)}
	>
		{#if header}
			<div class="border-b border-border px-4 py-3">
				{@render header()}
			</div>
		{/if}
		{#if padding === 'none'}
			{@render children()}
		{:else}
			<div class={paddings[padding]}>
				{@render children()}
			</div>
		{/if}
		{#if footer}
			<div class="border-t border-border px-4 py-3 bg-secondary-50 dark:bg-secondary-900/50">
				{@render footer()}
			</div>
		{/if}
	</div>
{:else}
	<div class={cn(baseClasses, variants[variant], className)}>
		{#if header}
			<div class="border-b border-border px-4 py-3">
				{@render header()}
			</div>
		{/if}
		{#if padding === 'none'}
			{@render children()}
		{:else}
			<div class={paddings[padding]}>
				{@render children()}
			</div>
		{/if}
		{#if footer}
			<div class="border-t border-border px-4 py-3 bg-secondary-50 dark:bg-secondary-900/50">
				{@render footer()}
			</div>
		{/if}
	</div>
{/if}
