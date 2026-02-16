<script lang="ts">
	import { cn } from '$lib/utils/cn';

	interface Props {
		title: string;
		description?: string;
		class?: string;
		icon?: import('svelte').Snippet;
		action?: import('svelte').Snippet;
	}

	let { title, description, class: className, icon, action }: Props = $props();
</script>

<div class={cn('flex flex-col items-center justify-center py-12 px-4 text-center', className)}>
	{#if icon}
		<div class="mb-4 text-muted">
			{@render icon()}
		</div>
	{:else}
		<div class="mb-4">
			<svg
				class="h-12 w-12 text-secondary-300 dark:text-secondary-600"
				fill="none"
				viewBox="0 0 24 24"
				stroke="currentColor"
			>
				<path
					stroke-linecap="round"
					stroke-linejoin="round"
					stroke-width="1.5"
					d="M20 13V6a2 2 0 00-2-2H6a2 2 0 00-2 2v7m16 0v5a2 2 0 01-2 2H6a2 2 0 01-2-2v-5m16 0h-2.586a1 1 0 00-.707.293l-2.414 2.414a1 1 0 01-.707.293h-3.172a1 1 0 01-.707-.293l-2.414-2.414A1 1 0 006.586 13H4"
				/>
			</svg>
		</div>
	{/if}

	<h3 class="text-lg font-medium text-foreground mb-1">
		{title}
	</h3>

	{#if description}
		<p class="text-sm text-muted max-w-sm mb-6">
			{description}
		</p>
	{/if}

	{#if action}
		<div>
			{@render action()}
		</div>
	{/if}
</div>
