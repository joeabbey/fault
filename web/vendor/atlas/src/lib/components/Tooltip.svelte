<script lang="ts">
	import { cn } from '$lib/utils/cn';

	interface Props {
		content: string;
		position?: 'top' | 'right' | 'bottom' | 'left';
		delay?: number;
		class?: string;
		children: import('svelte').Snippet;
	}

	let {
		content,
		position = 'top',
		delay = 200,
		class: className,
		children
	}: Props = $props();

	let visible = $state(false);
	let timeoutId: ReturnType<typeof setTimeout> | null = null;

	function showTooltip() {
		timeoutId = setTimeout(() => {
			visible = true;
		}, delay);
	}

	function hideTooltip() {
		if (timeoutId) {
			clearTimeout(timeoutId);
			timeoutId = null;
		}
		visible = false;
	}

	const positions = {
		top: 'bottom-full left-1/2 -translate-x-1/2 mb-2',
		right: 'left-full top-1/2 -translate-y-1/2 ml-2',
		bottom: 'top-full left-1/2 -translate-x-1/2 mt-2',
		left: 'right-full top-1/2 -translate-y-1/2 mr-2'
	};

	const arrows = {
		top: 'top-full left-1/2 -translate-x-1/2 border-t-secondary-900 border-x-transparent border-b-transparent dark:border-t-secondary-100',
		right:
			'right-full top-1/2 -translate-y-1/2 border-r-secondary-900 border-y-transparent border-l-transparent dark:border-r-secondary-100',
		bottom:
			'bottom-full left-1/2 -translate-x-1/2 border-b-secondary-900 border-x-transparent border-t-transparent dark:border-b-secondary-100',
		left: 'left-full top-1/2 -translate-y-1/2 border-l-secondary-900 border-y-transparent border-r-transparent dark:border-l-secondary-100'
	};
</script>

<div
	role="group"
	class={cn('relative inline-flex', className)}
	onmouseenter={showTooltip}
	onmouseleave={hideTooltip}
	onfocus={showTooltip}
	onblur={hideTooltip}
>
	{@render children()}

	{#if visible}
		<div
			role="tooltip"
			class={cn(
				'absolute z-50 px-2 py-1 text-xs font-medium text-white bg-secondary-900 dark:bg-secondary-100 dark:text-secondary-900 rounded shadow-lg whitespace-nowrap pointer-events-none',
				positions[position]
			)}
		>
			{content}
			<span
				class={cn('absolute w-0 h-0 border-4', arrows[position])}
				aria-hidden="true"
			></span>
		</div>
	{/if}
</div>
