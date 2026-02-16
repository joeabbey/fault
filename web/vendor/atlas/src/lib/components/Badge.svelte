<script lang="ts">
	import { cn } from '$lib/utils/cn';

	interface Props {
		variant?: 'default' | 'primary' | 'secondary' | 'success' | 'warning' | 'error' | 'info' | 'outline';
		size?: 'sm' | 'md' | 'lg';
		dot?: boolean;
		class?: string;
		children: import('svelte').Snippet;
	}

	let { variant = 'default', size = 'md', dot = false, class: className, children }: Props = $props();

	const baseClasses = 'inline-flex items-center font-medium rounded-full';

	const variants = {
		default: 'bg-secondary-100 text-secondary-800 dark:bg-secondary-700 dark:text-secondary-200',
		primary: 'bg-primary-100 text-primary-800 dark:bg-primary-700 dark:text-primary-200',
		secondary: 'bg-secondary-100 text-secondary-800 dark:bg-secondary-700 dark:text-secondary-200',
		success: 'bg-success-100 text-success-800 dark:bg-success-700 dark:text-success-200',
		warning: 'bg-warning-100 text-warning-800 dark:bg-warning-700 dark:text-warning-200',
		error: 'bg-error-100 text-error-800 dark:bg-error-700 dark:text-error-200',
		info: 'bg-primary-100 text-primary-800 dark:bg-primary-700 dark:text-primary-200',
		outline: 'border border-border bg-transparent text-foreground'
	};

	const sizes = {
		sm: 'px-2 py-0.5 text-xs',
		md: 'px-2.5 py-0.5 text-sm',
		lg: 'px-3 py-1 text-sm'
	};

	const dotColors = {
		default: 'bg-secondary-500',
		primary: 'bg-primary-500',
		secondary: 'bg-secondary-500',
		success: 'bg-success-500',
		warning: 'bg-warning-500',
		error: 'bg-error-500',
		info: 'bg-primary-500',
		outline: 'bg-foreground'
	};
</script>

<span class={cn(baseClasses, variants[variant], sizes[size], className)}>
	{#if dot}
		<span class={cn('w-1.5 h-1.5 rounded-full mr-1.5', dotColors[variant])}></span>
	{/if}
	{@render children()}
</span>
