<script lang="ts">
	import { cn } from '$lib/utils/cn';

	interface Props {
		open?: boolean;
		align?: 'start' | 'center' | 'end';
		side?: 'top' | 'right' | 'bottom' | 'left';
		class?: string;
		trigger: import('svelte').Snippet;
		children: import('svelte').Snippet;
		onOpenChange?: (open: boolean) => void;
	}

	let {
		open = $bindable(false),
		align = 'center',
		side = 'bottom',
		class: className,
		trigger,
		children,
		onOpenChange
	}: Props = $props();

	let triggerEl: HTMLDivElement;
	let contentEl: HTMLDivElement;

	function toggle() {
		open = !open;
		onOpenChange?.(open);
	}

	function close() {
		open = false;
		onOpenChange?.(false);
	}

	function handleKeydown(e: KeyboardEvent) {
		if (e.key === 'Escape' && open) {
			close();
		}
	}

	function handleClickOutside(e: MouseEvent) {
		if (open && triggerEl && contentEl) {
			if (!triggerEl.contains(e.target as Node) && !contentEl.contains(e.target as Node)) {
				close();
			}
		}
	}

	const positions = {
		top: {
			start: 'bottom-full left-0 mb-2',
			center: 'bottom-full left-1/2 -translate-x-1/2 mb-2',
			end: 'bottom-full right-0 mb-2'
		},
		right: {
			start: 'left-full top-0 ml-2',
			center: 'left-full top-1/2 -translate-y-1/2 ml-2',
			end: 'left-full bottom-0 ml-2'
		},
		bottom: {
			start: 'top-full left-0 mt-2',
			center: 'top-full left-1/2 -translate-x-1/2 mt-2',
			end: 'top-full right-0 mt-2'
		},
		left: {
			start: 'right-full top-0 mr-2',
			center: 'right-full top-1/2 -translate-y-1/2 mr-2',
			end: 'right-full bottom-0 mr-2'
		}
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
			bind:this={contentEl}
			role="dialog"
			class={cn(
				'absolute z-50 min-w-[12rem] rounded-md border border-border bg-card p-4 shadow-lg',
				'animate-in',
				positions[side][align]
			)}
		>
			{@render children()}
		</div>
	{/if}
</div>
