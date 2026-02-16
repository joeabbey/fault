<script lang="ts">
	import { cn } from '$lib/utils/cn';

	interface Props {
		title: string;
		subtitle?: string;
		class?: string;
		actions?: import('svelte').Snippet;
		breadcrumbs?: import('svelte').Snippet;
	}

	let { title, subtitle, class: className, actions, breadcrumbs }: Props = $props();
</script>

<div class={cn('mb-8', className)}>
	{#if breadcrumbs}
		<div class="mb-4">
			{@render breadcrumbs()}
		</div>
	{/if}

	<div class="flex flex-col sm:flex-row sm:items-center sm:justify-between gap-4">
		<div>
			<h1 class="text-2xl font-bold text-foreground sm:text-3xl">
				{title}
			</h1>
			{#if subtitle}
				<p class="mt-1 text-muted">
					{subtitle}
				</p>
			{/if}
		</div>

		{#if actions}
			<div class="flex flex-shrink-0 gap-3">
				{@render actions()}
			</div>
		{/if}
	</div>
</div>
