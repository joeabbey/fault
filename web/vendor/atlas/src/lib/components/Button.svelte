<script lang="ts">
	import { cn } from '$lib/utils/cn';
	import Spinner from './Spinner.svelte';

	interface Props {
		variant?: 'primary' | 'secondary' | 'ghost' | 'destructive' | 'outline' | 'link';
		size?: 'sm' | 'md' | 'lg' | 'icon';
		disabled?: boolean;
		loading?: boolean;
		type?: 'button' | 'submit' | 'reset';
		href?: string;
		target?: '_blank' | '_self' | '_parent' | '_top';
		class?: string;
		onclick?: (e: MouseEvent) => void;
		children?: import('svelte').Snippet;
	}

	let {
		variant = 'primary',
		size = 'md',
		disabled = false,
		loading = false,
		type = 'button',
		href,
		target,
		class: className,
		onclick,
		children
	}: Props = $props();

	const baseClasses =
		'inline-flex items-center justify-center font-medium transition-colors focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-ring focus-visible:ring-offset-2 disabled:pointer-events-none disabled:opacity-50';

	const variants = {
		primary: 'bg-primary-500 text-white hover:bg-primary-600 active:bg-primary-700',
		secondary:
			'bg-secondary-200 text-secondary-900 hover:bg-secondary-300 dark:bg-secondary-700 dark:text-secondary-100 dark:hover:bg-secondary-600',
		ghost: 'hover:bg-accent hover:text-accent-foreground',
		destructive: 'bg-error-500 text-white hover:bg-error-600 active:bg-error-700',
		outline: 'border border-border bg-transparent hover:bg-accent hover:text-accent-foreground',
		link: 'text-primary-500 underline-offset-4 hover:underline'
	};

	const sizes = {
		sm: 'h-8 px-3 text-sm rounded-md gap-1.5',
		md: 'h-10 px-4 text-sm rounded-md gap-2',
		lg: 'h-12 px-6 text-base rounded-lg gap-2',
		icon: 'h-10 w-10 rounded-md'
	};
</script>

{#if href && !disabled}
	<a
		{href}
		{target}
		{onclick}
		class={cn(baseClasses, variants[variant], sizes[size], className)}
		rel={target === '_blank' ? 'noopener noreferrer' : undefined}
	>
		{#if loading}
			<Spinner size="sm" class={size === 'icon' ? '' : '-ml-1'} />
		{/if}
		{#if !(loading && size === 'icon') && children}
			{@render children()}
		{/if}
	</a>
{:else}
	<button
		{type}
		{onclick}
		disabled={disabled || loading}
		class={cn(baseClasses, variants[variant], sizes[size], className)}
	>
		{#if loading}
			<Spinner size="sm" class={size === 'icon' ? '' : '-ml-1'} />
		{/if}
		{#if !(loading && size === 'icon') && children}
			{@render children()}
		{/if}
	</button>
{/if}
