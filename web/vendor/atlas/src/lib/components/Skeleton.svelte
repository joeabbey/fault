<script lang="ts">
	import { cn } from '$lib/utils/cn';

	interface Props {
		variant?: 'text' | 'circle' | 'rect';
		width?: string;
		height?: string;
		lines?: number;
		class?: string;
	}

	let {
		variant = 'text',
		width,
		height,
		lines = 1,
		class: className
	}: Props = $props();

	const baseClasses = 'animate-pulse bg-secondary-200 dark:bg-secondary-700';

	const variants = {
		text: 'h-4 rounded',
		circle: 'rounded-full',
		rect: 'rounded-md'
	};

	function getStyle(): string {
		const styles: string[] = [];
		if (width) styles.push(`width: ${width}`);
		if (height) styles.push(`height: ${height}`);
		return styles.join('; ');
	}
</script>

{#if variant === 'text' && lines > 1}
	<div class="space-y-2">
		{#each Array(lines) as _, i}
			<div
				class={cn(
					baseClasses,
					variants[variant],
					i === lines - 1 ? 'w-3/4' : 'w-full',
					className
				)}
				style={getStyle()}
				aria-hidden="true"
			></div>
		{/each}
	</div>
{:else}
	<div
		class={cn(
			baseClasses,
			variants[variant],
			variant === 'circle' && !width && !height && 'h-10 w-10',
			variant === 'rect' && !width && !height && 'h-24 w-full',
			className
		)}
		style={getStyle()}
		aria-hidden="true"
	></div>
{/if}
