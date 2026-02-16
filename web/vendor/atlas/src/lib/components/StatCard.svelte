<script lang="ts">
	import { cn } from '$lib/utils/cn';

	interface Props {
		label: string;
		value: string | number;
		change?: number;
		changeLabel?: string;
		class?: string;
		icon?: import('svelte').Snippet;
	}

	let { label, value, change, changeLabel, class: className, icon }: Props = $props();

	const isPositive = $derived(change !== undefined && change >= 0);
</script>

<div
	class={cn(
		'rounded-lg border border-border bg-card p-6',
		className
	)}
>
	<div class="flex items-center justify-between">
		<p class="text-sm font-medium text-muted">
			{label}
		</p>
		{#if icon}
			<div class="text-muted">
				{@render icon()}
			</div>
		{/if}
	</div>

	<div class="mt-2 flex items-baseline gap-2">
		<p class="text-3xl font-semibold text-foreground tabular-nums">
			{value}
		</p>

		{#if change !== undefined}
			<span
				class={cn(
					'inline-flex items-center text-sm font-medium',
					isPositive ? 'text-success-600 dark:text-success-400' : 'text-error-600 dark:text-error-400'
				)}
			>
				{#if isPositive}
					<svg class="h-4 w-4 mr-0.5" viewBox="0 0 20 20" fill="currentColor">
						<path
							fill-rule="evenodd"
							d="M5.293 9.707a1 1 0 010-1.414l4-4a1 1 0 011.414 0l4 4a1 1 0 01-1.414 1.414L11 7.414V15a1 1 0 11-2 0V7.414L6.707 9.707a1 1 0 01-1.414 0z"
							clip-rule="evenodd"
						/>
					</svg>
				{:else}
					<svg class="h-4 w-4 mr-0.5" viewBox="0 0 20 20" fill="currentColor">
						<path
							fill-rule="evenodd"
							d="M14.707 10.293a1 1 0 010 1.414l-4 4a1 1 0 01-1.414 0l-4-4a1 1 0 111.414-1.414L9 12.586V5a1 1 0 012 0v7.586l2.293-2.293a1 1 0 011.414 0z"
							clip-rule="evenodd"
						/>
					</svg>
				{/if}
				{Math.abs(change)}%
			</span>
		{/if}
	</div>

	{#if changeLabel}
		<p class="mt-1 text-xs text-muted">
			{changeLabel}
		</p>
	{/if}
</div>
