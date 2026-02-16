<script lang="ts">
	import { cn } from '$lib/utils/cn';

	interface NavItem {
		label: string;
		href: string;
		active?: boolean;
	}

	interface Props {
		brand?: string;
		brandHref?: string;
		items?: NavItem[];
		sticky?: boolean;
		class?: string;
		logo?: import('svelte').Snippet;
		actions?: import('svelte').Snippet;
	}

	let {
		brand,
		brandHref = '/',
		items = [],
		sticky = false,
		class: className,
		logo,
		actions
	}: Props = $props();

	let mobileMenuOpen = $state(false);
</script>

<nav
	class={cn(
		'bg-card border-b border-border',
		sticky && 'sticky top-0 z-40',
		className
	)}
>
	<div class="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8">
		<div class="flex justify-between h-16">
			<!-- Brand / Logo -->
			<div class="flex items-center">
				<a href={brandHref} class="flex items-center gap-2">
					{#if logo}
						{@render logo()}
					{/if}
					{#if brand}
						<span class="text-lg font-semibold text-foreground">{brand}</span>
					{/if}
				</a>

				<!-- Desktop Navigation -->
				{#if items.length > 0}
					<div class="hidden sm:ml-8 sm:flex sm:space-x-4">
						{#each items as item}
							<a
								href={item.href}
								class={cn(
									'px-3 py-2 rounded-md text-sm font-medium transition-colors',
									'focus:outline-none focus-visible:ring-2 focus-visible:ring-ring',
									item.active
										? 'bg-primary-50 text-primary-600 dark:bg-primary-800 dark:text-primary-200'
										: 'text-muted hover:text-foreground hover:bg-accent'
								)}
								aria-current={item.active ? 'page' : undefined}
							>
								{item.label}
							</a>
						{/each}
					</div>
				{/if}
			</div>

			<!-- Actions -->
			<div class="flex items-center gap-4">
				{#if actions}
					<div class="hidden sm:flex sm:items-center sm:gap-3">
						{@render actions()}
					</div>
				{/if}

				<!-- Mobile menu button -->
				{#if items.length > 0 || actions}
					<button
						type="button"
						onclick={() => (mobileMenuOpen = !mobileMenuOpen)}
						class="sm:hidden p-2 rounded-md text-muted hover:text-foreground hover:bg-accent focus:outline-none focus-visible:ring-2 focus-visible:ring-ring"
						aria-expanded={mobileMenuOpen}
					>
						<span class="sr-only">Toggle menu</span>
						{#if mobileMenuOpen}
							<svg class="h-6 w-6" fill="none" viewBox="0 0 24 24" stroke="currentColor">
								<path
									stroke-linecap="round"
									stroke-linejoin="round"
									stroke-width="2"
									d="M6 18L18 6M6 6l12 12"
								/>
							</svg>
						{:else}
							<svg class="h-6 w-6" fill="none" viewBox="0 0 24 24" stroke="currentColor">
								<path
									stroke-linecap="round"
									stroke-linejoin="round"
									stroke-width="2"
									d="M4 6h16M4 12h16M4 18h16"
								/>
							</svg>
						{/if}
					</button>
				{/if}
			</div>
		</div>
	</div>

	<!-- Mobile menu -->
	{#if mobileMenuOpen}
		<div class="sm:hidden border-t border-border">
			{#if items.length > 0}
				<div class="px-2 pt-2 pb-3 space-y-1">
					{#each items as item}
						<a
							href={item.href}
							class={cn(
								'block px-3 py-2 rounded-md text-base font-medium',
								item.active
									? 'bg-primary-50 text-primary-600 dark:bg-primary-800 dark:text-primary-200'
									: 'text-muted hover:text-foreground hover:bg-accent'
							)}
							aria-current={item.active ? 'page' : undefined}
						>
							{item.label}
						</a>
					{/each}
				</div>
			{/if}
			{#if actions}
				<div class="px-4 py-3 border-t border-border">
					{@render actions()}
				</div>
			{/if}
		</div>
	{/if}
</nav>
