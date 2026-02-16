<script lang="ts">
	import { cn } from '$lib/utils/cn';

	interface Tab {
		id: string;
		label: string;
		disabled?: boolean;
	}

	interface Props {
		tabs: Tab[];
		activeTab?: string;
		variant?: 'underline' | 'pills' | 'enclosed';
		class?: string;
		onchange?: (tabId: string) => void;
		children: import('svelte').Snippet<[string]>;
	}

	let {
		tabs,
		activeTab = $bindable(tabs[0]?.id || ''),
		variant = 'underline',
		class: className,
		onchange,
		children
	}: Props = $props();

	function selectTab(tabId: string) {
		activeTab = tabId;
		onchange?.(tabId);
	}

	function handleKeydown(e: KeyboardEvent, index: number) {
		const enabledTabs = tabs.filter((t) => !t.disabled);
		const currentIndex = enabledTabs.findIndex((t) => t.id === activeTab);

		let newIndex = currentIndex;

		if (e.key === 'ArrowRight' || e.key === 'ArrowDown') {
			e.preventDefault();
			newIndex = (currentIndex + 1) % enabledTabs.length;
		} else if (e.key === 'ArrowLeft' || e.key === 'ArrowUp') {
			e.preventDefault();
			newIndex = (currentIndex - 1 + enabledTabs.length) % enabledTabs.length;
		} else if (e.key === 'Home') {
			e.preventDefault();
			newIndex = 0;
		} else if (e.key === 'End') {
			e.preventDefault();
			newIndex = enabledTabs.length - 1;
		}

		if (newIndex !== currentIndex) {
			selectTab(enabledTabs[newIndex].id);
		}
	}

	const variantStyles = {
		underline: {
			list: 'border-b border-border',
			tab: 'border-b-2 border-transparent py-3 px-4 -mb-px',
			active: 'border-primary-500 text-primary-600 dark:text-primary-400',
			inactive: 'text-muted hover:text-foreground hover:border-secondary-300'
		},
		pills: {
			list: 'gap-2',
			tab: 'rounded-md py-2 px-4',
			active: 'bg-primary-500 text-white',
			inactive: 'text-muted hover:bg-secondary-100 dark:hover:bg-secondary-800'
		},
		enclosed: {
			list: 'border-b border-border',
			tab: 'border border-transparent rounded-t-md py-2 px-4 -mb-px',
			active: 'border-border border-b-background bg-background',
			inactive: 'text-muted hover:text-foreground'
		}
	};
</script>

<div class={className}>
	<div
		role="tablist"
		class={cn('flex', variantStyles[variant].list)}
	>
		{#each tabs as tab, index}
			<button
				role="tab"
				id="tab-{tab.id}"
				aria-selected={activeTab === tab.id}
				aria-controls="panel-{tab.id}"
				tabindex={activeTab === tab.id ? 0 : -1}
				disabled={tab.disabled}
				onclick={() => selectTab(tab.id)}
				onkeydown={(e) => handleKeydown(e, index)}
				class={cn(
					'text-sm font-medium transition-colors whitespace-nowrap',
					'focus:outline-none focus-visible:ring-2 focus-visible:ring-ring focus-visible:ring-offset-2',
					'disabled:opacity-50 disabled:cursor-not-allowed',
					variantStyles[variant].tab,
					activeTab === tab.id
						? variantStyles[variant].active
						: variantStyles[variant].inactive
				)}
			>
				{tab.label}
			</button>
		{/each}
	</div>

	{#each tabs as tab}
		<div
			role="tabpanel"
			id="panel-{tab.id}"
			aria-labelledby="tab-{tab.id}"
			hidden={activeTab !== tab.id}
			tabindex={0}
			class="pt-4 focus:outline-none"
		>
			{#if activeTab === tab.id}
				{@render children(tab.id)}
			{/if}
		</div>
	{/each}
</div>
