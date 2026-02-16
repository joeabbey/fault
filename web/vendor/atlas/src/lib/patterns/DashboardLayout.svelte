<script lang="ts">
	import { cn } from '$lib/utils/cn';

	interface NavItem {
		id: string;
		label: string;
		href: string;
		icon?: import('svelte').Snippet;
		badge?: string | number;
		children?: NavItem[];
	}

	interface Props {
		/** Navigation items for sidebar */
		navItems?: NavItem[];
		/** Current active nav item id */
		activeNavId?: string;
		/** Whether sidebar is collapsed */
		collapsed?: boolean;
		/** Logo/brand snippet */
		logo?: import('svelte').Snippet;
		/** User menu snippet (bottom of sidebar) */
		userMenu?: import('svelte').Snippet;
		/** Header content (breadcrumbs, actions) */
		header?: import('svelte').Snippet;
		/** Main content */
		children: import('svelte').Snippet;
		/** Optional command palette trigger */
		onCommandPalette?: () => void;
		class?: string;
	}

	let {
		navItems = [],
		activeNavId = '',
		collapsed = $bindable(false),
		logo,
		userMenu,
		header,
		children,
		onCommandPalette,
		class: className
	}: Props = $props();

	function handleKeydown(e: KeyboardEvent) {
		if ((e.metaKey || e.ctrlKey) && e.key === 'k') {
			e.preventDefault();
			onCommandPalette?.();
		}
	}
</script>

<svelte:window onkeydown={handleKeydown} />

<div class={cn('flex h-screen bg-background', className)}>
	<!-- Sidebar -->
	<aside
		class={cn(
			'flex flex-col border-r border-border bg-card transition-all duration-300',
			collapsed ? 'w-16' : 'w-56'
		)}
	>
		<!-- Logo -->
		<div class="h-14 flex items-center px-4 border-b border-border">
			{#if logo}
				{@render logo()}
			{:else}
				<div class={cn('font-semibold text-lg', collapsed && 'hidden')}>Logo</div>
			{/if}
		</div>

		<!-- Search / Command Palette Trigger -->
		{#if onCommandPalette && !collapsed}
			<div class="p-2">
				<button
					onclick={onCommandPalette}
					class="w-full flex items-center gap-2 px-3 py-2 text-sm text-muted rounded-md border border-border hover:bg-secondary-50 dark:hover:bg-secondary-800 transition-colors"
				>
					<svg class="h-4 w-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
						<path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M21 21l-6-6m2-5a7 7 0 11-14 0 7 7 0 0114 0z" />
					</svg>
					<span class="flex-1 text-left">Search...</span>
					<kbd class="text-xs bg-secondary-100 dark:bg-secondary-800 px-1.5 py-0.5 rounded">⌘K</kbd>
				</button>
			</div>
		{/if}

		<!-- Navigation -->
		<nav class="flex-1 overflow-y-auto p-2">
			<ul class="space-y-1">
				{#each navItems as item}
					<li>
						<a
							href={item.href}
							class={cn(
								'flex items-center gap-3 px-3 py-2 rounded-md text-sm font-medium transition-colors',
								activeNavId === item.id
									? 'bg-primary-50 dark:bg-primary-800 text-primary-600 dark:text-primary-200'
									: 'text-foreground hover:bg-secondary-50 dark:hover:bg-secondary-800'
							)}
							title={collapsed ? item.label : undefined}
						>
							{#if item.icon}
								<span class="flex-shrink-0">
									{@render item.icon()}
								</span>
							{/if}
							{#if !collapsed}
								<span class="flex-1">{item.label}</span>
								{#if item.badge}
									<span class="px-2 py-0.5 text-xs bg-primary-100 dark:bg-primary-800 text-primary-700 dark:text-primary-200 rounded-full">
										{item.badge}
									</span>
								{/if}
							{/if}
						</a>

						{#if item.children && !collapsed}
							<ul class="ml-6 mt-1 space-y-1">
								{#each item.children as child}
									<li>
										<a
											href={child.href}
											class={cn(
												'flex items-center gap-2 px-3 py-1.5 rounded-md text-sm transition-colors',
												activeNavId === child.id
													? 'text-primary-600 dark:text-primary-400'
													: 'text-muted hover:text-foreground'
											)}
										>
											{child.label}
										</a>
									</li>
								{/each}
							</ul>
						{/if}
					</li>
				{/each}
			</ul>
		</nav>

		<!-- Collapse Toggle -->
		<div class="p-2 border-t border-border">
			<button
				onclick={() => (collapsed = !collapsed)}
				class="w-full flex items-center justify-center p-2 rounded-md text-muted hover:bg-secondary-50 dark:hover:bg-secondary-800 transition-colors"
				aria-label={collapsed ? 'Expand sidebar' : 'Collapse sidebar'}
			>
				<svg
					class={cn('h-5 w-5 transition-transform', collapsed && 'rotate-180')}
					fill="none"
					stroke="currentColor"
					viewBox="0 0 24 24"
				>
					<path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M11 19l-7-7 7-7m8 14l-7-7 7-7" />
				</svg>
			</button>
		</div>

		<!-- User Menu -->
		{#if userMenu}
			<div class="p-2 border-t border-border">
				{@render userMenu()}
			</div>
		{/if}
	</aside>

	<!-- Main Content Area -->
	<main class="flex-1 flex flex-col overflow-hidden">
		<!-- Header -->
		{#if header}
			<header class="h-14 flex items-center px-6 border-b border-border bg-card">
				{@render header()}
			</header>
		{/if}

		<!-- Content -->
		<div class="flex-1 overflow-y-auto">
			{@render children()}
		</div>
	</main>
</div>
