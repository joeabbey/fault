<script lang="ts">
	import { cn } from '$lib/utils/cn';

	interface SettingsSection {
		id: string;
		label: string;
		icon?: import('svelte').Snippet;
		href?: string;
	}

	interface Props {
		/** Settings sections for navigation */
		sections: SettingsSection[];
		/** Currently active section id */
		activeSection?: string;
		/** Page title */
		title?: string;
		/** Page description */
		description?: string;
		/** Navigation mode */
		navMode?: 'links' | 'tabs';
		/** Main content */
		children: import('svelte').Snippet;
		/** Callback when section changes (for tabs mode) */
		onSectionChange?: (sectionId: string) => void;
		class?: string;
	}

	let {
		sections,
		activeSection = $bindable(sections[0]?.id ?? ''),
		title = 'Settings',
		description,
		navMode = 'links',
		children,
		onSectionChange,
		class: className
	}: Props = $props();

	function handleSectionClick(sectionId: string) {
		if (navMode === 'tabs') {
			activeSection = sectionId;
			onSectionChange?.(sectionId);
		}
	}
</script>

<div class={cn('min-h-screen bg-background', className)}>
	<!-- Header -->
	<div class="border-b border-border bg-card">
		<div class="max-w-6xl mx-auto px-6 py-8">
			<h1 class="text-2xl font-bold text-foreground">{title}</h1>
			{#if description}
				<p class="mt-1 text-muted">{description}</p>
			{/if}
		</div>
	</div>

	<!-- Content -->
	<div class="max-w-6xl mx-auto px-6 py-8">
		<div class="flex flex-col lg:flex-row gap-8">
			<!-- Sidebar Navigation -->
			<nav class="lg:w-60 flex-shrink-0">
				<ul class="space-y-1 lg:sticky lg:top-8">
					{#each sections as section}
						{#if navMode === 'links' && section.href}
							<li>
								<a
									href={section.href}
									class={cn(
										'flex items-center gap-3 px-4 py-2.5 rounded-md text-sm font-medium transition-colors',
										activeSection === section.id
											? 'bg-primary-50 dark:bg-primary-800 text-primary-600 dark:text-primary-200'
											: 'text-foreground hover:bg-secondary-50 dark:hover:bg-secondary-800'
									)}
								>
									{#if section.icon}
										<span class="flex-shrink-0 text-muted">
											{@render section.icon()}
										</span>
									{/if}
									{section.label}
								</a>
							</li>
						{:else}
							<li>
								<button
									onclick={() => handleSectionClick(section.id)}
									class={cn(
										'w-full flex items-center gap-3 px-4 py-2.5 rounded-md text-sm font-medium transition-colors text-left',
										activeSection === section.id
											? 'bg-primary-50 dark:bg-primary-800 text-primary-600 dark:text-primary-200'
											: 'text-foreground hover:bg-secondary-50 dark:hover:bg-secondary-800'
									)}
								>
									{#if section.icon}
										<span class="flex-shrink-0 text-muted">
											{@render section.icon()}
										</span>
									{/if}
									{section.label}
								</button>
							</li>
						{/if}
					{/each}
				</ul>
			</nav>

			<!-- Main Content -->
			<div class="flex-1 min-w-0">
				{@render children()}
			</div>
		</div>
	</div>
</div>
