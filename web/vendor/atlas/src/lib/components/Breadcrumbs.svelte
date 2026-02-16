<script lang="ts">
	import { cn } from '$lib/utils/cn';

	interface Crumb {
		label: string;
		href?: string;
	}

	interface Props {
		items: Crumb[];
		separator?: 'slash' | 'chevron' | 'dot';
		class?: string;
	}

	let { items, separator = 'chevron', class: className }: Props = $props();

	const separators = {
		slash: '/',
		chevron: `<svg class="h-4 w-4" viewBox="0 0 20 20" fill="currentColor"><path fill-rule="evenodd" d="M7.293 14.707a1 1 0 010-1.414L10.586 10 7.293 6.707a1 1 0 011.414-1.414l4 4a1 1 0 010 1.414l-4 4a1 1 0 01-1.414 0z" clip-rule="evenodd" /></svg>`,
		dot: '·'
	};
</script>

<nav aria-label="Breadcrumb" class={className}>
	<ol class="flex items-center space-x-2">
		{#each items as item, index}
			<li class="flex items-center">
				{#if index > 0}
					<span class="mx-2 text-muted" aria-hidden="true">
						{#if separator === 'chevron'}
							{@html separators[separator]}
						{:else}
							{separators[separator]}
						{/if}
					</span>
				{/if}

				{#if index === items.length - 1}
					<span class="text-sm font-medium text-foreground" aria-current="page">
						{item.label}
					</span>
				{:else if item.href}
					<a
						href={item.href}
						class={cn(
							'text-sm text-muted hover:text-foreground transition-colors',
							'focus:outline-none focus-visible:ring-2 focus-visible:ring-ring rounded'
						)}
					>
						{item.label}
					</a>
				{:else}
					<span class="text-sm text-muted">
						{item.label}
					</span>
				{/if}
			</li>
		{/each}
	</ol>
</nav>
