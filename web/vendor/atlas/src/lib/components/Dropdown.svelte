<script lang="ts">
	import { cn } from '$lib/utils/cn';

	interface MenuItem {
		id: string;
		label: string;
		icon?: string;
		disabled?: boolean;
		destructive?: boolean;
		divider?: boolean;
	}

	interface Props {
		items: MenuItem[];
		open?: boolean;
		align?: 'start' | 'center' | 'end';
		side?: 'top' | 'bottom';
		class?: string;
		trigger: import('svelte').Snippet;
		onselect?: (itemId: string) => void;
	}

	let {
		items,
		open = $bindable(false),
		align = 'start',
		side = 'bottom',
		class: className,
		trigger,
		onselect
	}: Props = $props();

	let triggerEl: HTMLDivElement;
	let menuEl: HTMLDivElement;

	function toggle() {
		open = !open;
	}

	function close() {
		open = false;
	}

	function handleSelect(item: MenuItem) {
		if (!item.disabled && !item.divider) {
			onselect?.(item.id);
			close();
		}
	}

	function handleKeydown(e: KeyboardEvent) {
		if (e.key === 'Escape') {
			close();
		}
	}

	function handleClickOutside(e: MouseEvent) {
		if (open && triggerEl && menuEl) {
			if (!triggerEl.contains(e.target as Node) && !menuEl.contains(e.target as Node)) {
				close();
			}
		}
	}

	const alignments = {
		start: 'left-0',
		center: 'left-1/2 -translate-x-1/2',
		end: 'right-0'
	};

	const sides = {
		top: 'bottom-full mb-1',
		bottom: 'top-full mt-1'
	};
</script>

<svelte:window onclick={handleClickOutside} onkeydown={handleKeydown} />

<!-- svelte-ignore a11y_no_static_element_interactions a11y_click_events_have_key_events -->
<div class={cn('relative inline-block', className)}>
	<div bind:this={triggerEl} onclick={toggle} role="button" tabindex="0" onkeydown={(e) => (e.key === 'Enter' || e.key === ' ') && toggle()}>
		{@render trigger()}
	</div>

	{#if open}
		<div
			bind:this={menuEl}
			role="menu"
			class={cn(
				'absolute z-50 min-w-[12rem] rounded-md border border-border bg-card py-1 shadow-lg',
				'animate-in',
				alignments[align],
				sides[side]
			)}
		>
			{#each items as item}
				{#if item.divider}
					<div class="my-1 h-px bg-border" role="separator"></div>
				{:else}
					<button
						type="button"
						role="menuitem"
						disabled={item.disabled}
						onclick={() => handleSelect(item)}
						class={cn(
							'flex w-full items-center px-3 py-2 text-sm',
							'transition-colors focus:outline-none focus:bg-accent',
							item.disabled
								? 'text-muted cursor-not-allowed'
								: item.destructive
									? 'text-error-600 dark:text-error-400 hover:bg-error-50 dark:hover:bg-error-950'
									: 'text-foreground hover:bg-accent'
						)}
					>
						{item.label}
					</button>
				{/if}
			{/each}
		</div>
	{/if}
</div>
