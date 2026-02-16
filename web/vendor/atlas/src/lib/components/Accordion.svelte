<script lang="ts">
	import { cn } from '$lib/utils/cn';

	interface Item {
		id: string;
		title: string;
		disabled?: boolean;
	}

	interface Props {
		items: Item[];
		expanded?: string[];
		multiple?: boolean;
		class?: string;
		onchange?: (expanded: string[]) => void;
		children: import('svelte').Snippet<[string]>;
	}

	let {
		items,
		expanded = $bindable([]),
		multiple = false,
		class: className,
		onchange,
		children
	}: Props = $props();

	function toggle(itemId: string) {
		if (expanded.includes(itemId)) {
			expanded = expanded.filter((id) => id !== itemId);
		} else {
			expanded = multiple ? [...expanded, itemId] : [itemId];
		}
		onchange?.(expanded);
	}

	function isExpanded(itemId: string): boolean {
		return expanded.includes(itemId);
	}
</script>

<div class={cn('divide-y divide-border rounded-lg border border-border', className)}>
	{#each items as item}
		{@const open = isExpanded(item.id)}
		<div class="group">
			<button
				type="button"
				id="accordion-header-{item.id}"
				aria-expanded={open}
				aria-controls="accordion-panel-{item.id}"
				disabled={item.disabled}
				onclick={() => toggle(item.id)}
				class={cn(
					'flex w-full items-center justify-between px-4 py-4 text-left',
					'text-sm font-medium text-foreground',
					'hover:bg-secondary-50 dark:hover:bg-secondary-800/50',
					'focus:outline-none focus-visible:ring-2 focus-visible:ring-ring focus-visible:ring-inset',
					'disabled:opacity-50 disabled:cursor-not-allowed disabled:hover:bg-transparent',
					'transition-colors'
				)}
			>
				<span>{item.title}</span>
				<svg
					class={cn(
						'h-5 w-5 text-muted transition-transform duration-200',
						open && 'rotate-180'
					)}
					fill="none"
					viewBox="0 0 24 24"
					stroke="currentColor"
				>
					<path
						stroke-linecap="round"
						stroke-linejoin="round"
						stroke-width="2"
						d="M19 9l-7 7-7-7"
					/>
				</svg>
			</button>

			{#if open}
				<div
					id="accordion-panel-{item.id}"
					role="region"
					aria-labelledby="accordion-header-{item.id}"
					class="px-4 pb-4"
				>
					<div class="text-sm text-muted">
						{@render children(item.id)}
					</div>
				</div>
			{/if}
		</div>
	{/each}
</div>
