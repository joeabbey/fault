<script lang="ts">
	import { cn } from '$lib/utils/cn';

	interface SidebarItem {
		id: string;
		label: string;
		href?: string;
		icon?: string;
		active?: boolean;
		badge?: string | number;
		children?: SidebarItem[];
	}

	interface Props {
		items: SidebarItem[];
		collapsed?: boolean;
		class?: string;
		header?: import('svelte').Snippet;
		footer?: import('svelte').Snippet;
		onItemClick?: (item: SidebarItem) => void;
	}

	let {
		items,
		collapsed = $bindable(false),
		class: className,
		header,
		footer,
		onItemClick
	}: Props = $props();

	let expandedGroups = $state<Set<string>>(new Set());

	function toggleGroup(id: string) {
		const newExpanded = new Set(expandedGroups);
		if (newExpanded.has(id)) {
			newExpanded.delete(id);
		} else {
			newExpanded.add(id);
		}
		expandedGroups = newExpanded;
	}

	function handleItemClick(item: SidebarItem, e: MouseEvent) {
		if (item.children && item.children.length > 0) {
			e.preventDefault();
			toggleGroup(item.id);
		} else {
			onItemClick?.(item);
		}
	}
</script>

<aside
	class={cn(
		'flex flex-col bg-card border-r border-border transition-all duration-300',
		collapsed ? 'w-16' : 'w-64',
		className
	)}
>
	{#if header}
		<div class="p-4 border-b border-border">
			{@render header()}
		</div>
	{/if}

	<nav class="flex-1 overflow-y-auto p-2">
		<ul class="space-y-1">
			{#each items as item}
				{@const hasChildren = item.children && item.children.length > 0}
				{@const isExpanded = expandedGroups.has(item.id)}

				<li>
					<a
						href={item.href || '#'}
						onclick={(e) => handleItemClick(item, e)}
						class={cn(
							'flex items-center gap-3 px-3 py-2 rounded-md text-sm font-medium transition-colors',
							'focus:outline-none focus-visible:ring-2 focus-visible:ring-ring',
							item.active
								? 'bg-primary-50 text-primary-600 dark:bg-primary-800 dark:text-primary-200'
								: 'text-muted hover:text-foreground hover:bg-accent',
							collapsed && 'justify-center'
						)}
						aria-current={item.active ? 'page' : undefined}
						title={collapsed ? item.label : undefined}
					>
						{#if item.icon}
							<span class="flex-shrink-0" aria-hidden="true">
								{@html item.icon}
							</span>
						{/if}

						{#if !collapsed}
							<span class="flex-1 truncate">{item.label}</span>

							{#if item.badge}
								<span
									class="inline-flex items-center px-2 py-0.5 rounded-full text-xs font-medium bg-primary-100 text-primary-800 dark:bg-primary-700 dark:text-primary-200"
								>
									{item.badge}
								</span>
							{/if}

							{#if hasChildren}
								<svg
									class={cn(
										'h-4 w-4 transition-transform',
										isExpanded && 'rotate-90'
									)}
									viewBox="0 0 20 20"
									fill="currentColor"
								>
									<path
										fill-rule="evenodd"
										d="M7.293 14.707a1 1 0 010-1.414L10.586 10 7.293 6.707a1 1 0 011.414-1.414l4 4a1 1 0 010 1.414l-4 4a1 1 0 01-1.414 0z"
										clip-rule="evenodd"
									/>
								</svg>
							{/if}
						{/if}
					</a>

					{#if hasChildren && isExpanded && !collapsed}
						<ul class="mt-1 ml-4 pl-4 border-l border-border space-y-1">
							{#each item.children as child}
								<li>
									<a
										href={child.href || '#'}
										onclick={(e) => handleItemClick(child, e)}
										class={cn(
											'flex items-center gap-3 px-3 py-2 rounded-md text-sm transition-colors',
											'focus:outline-none focus-visible:ring-2 focus-visible:ring-ring',
											child.active
												? 'text-primary-600 dark:text-primary-400 font-medium'
												: 'text-muted hover:text-foreground'
										)}
										aria-current={child.active ? 'page' : undefined}
									>
										{#if child.icon}
											<span class="flex-shrink-0" aria-hidden="true">
												{@html child.icon}
											</span>
										{/if}
										<span class="truncate">{child.label}</span>
										{#if child.badge}
											<span
												class="ml-auto inline-flex items-center px-2 py-0.5 rounded-full text-xs font-medium bg-secondary-100 text-secondary-800 dark:bg-secondary-800 dark:text-secondary-200"
											>
												{child.badge}
											</span>
										{/if}
									</a>
								</li>
							{/each}
						</ul>
					{/if}
				</li>
			{/each}
		</ul>
	</nav>

	{#if footer}
		<div class="p-4 border-t border-border">
			{@render footer()}
		</div>
	{/if}
</aside>
