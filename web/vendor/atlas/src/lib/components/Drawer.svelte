<script lang="ts">
	import { cn } from '$lib/utils/cn';
	import { onMount } from 'svelte';

	interface Props {
		open?: boolean;
		title?: string;
		description?: string;
		position?: 'left' | 'right' | 'top' | 'bottom';
		size?: 'sm' | 'md' | 'lg' | 'xl';
		closeOnOverlay?: boolean;
		closeOnEscape?: boolean;
		showClose?: boolean;
		class?: string;
		onclose?: () => void;
		children: import('svelte').Snippet;
		footer?: import('svelte').Snippet;
	}

	let {
		open = $bindable(false),
		title,
		description,
		position = 'right',
		size = 'md',
		closeOnOverlay = true,
		closeOnEscape = true,
		showClose = true,
		class: className,
		onclose,
		children,
		footer
	}: Props = $props();

	// Track visibility (DOM presence) separately from animation state
	let visible = $state(false);
	let animating = $state(false);

	const sizes = {
		sm: { horizontal: 'max-w-xs', vertical: 'max-h-64' },
		md: { horizontal: 'max-w-md', vertical: 'max-h-96' },
		lg: { horizontal: 'max-w-lg', vertical: 'max-h-[32rem]' },
		xl: { horizontal: 'max-w-xl', vertical: 'max-h-[40rem]' }
	};

	const positions = {
		left: 'inset-y-0 left-0',
		right: 'inset-y-0 right-0',
		top: 'inset-x-0 top-0',
		bottom: 'inset-x-0 bottom-0'
	};

	const closedTransforms = {
		left: '-translate-x-full',
		right: 'translate-x-full',
		top: '-translate-y-full',
		bottom: 'translate-y-full'
	};

	const isHorizontal = $derived(position === 'left' || position === 'right');

	// Handle open state changes with animation timing
	$effect(() => {
		if (open) {
			// Opening: show immediately, animate in after a tick
			visible = true;
			requestAnimationFrame(() => {
				requestAnimationFrame(() => {
					animating = true;
				});
			});
			document.body.style.overflow = 'hidden';
		} else if (visible) {
			// Closing: animate out, then hide after transition
			animating = false;
			document.body.style.overflow = '';
			const timeout = setTimeout(() => {
				visible = false;
			}, 300); // Match transition duration
			return () => clearTimeout(timeout);
		}
	});

	function close() {
		open = false;
		onclose?.();
	}

	function handleKeydown(e: KeyboardEvent) {
		if (e.key === 'Escape' && closeOnEscape && open) {
			e.preventDefault();
			close();
		}
	}

	function handleOverlayClick(e: MouseEvent) {
		if (closeOnOverlay && e.target === e.currentTarget) {
			close();
		}
	}

	onMount(() => {
		return () => {
			document.body.style.overflow = '';
		};
	});
</script>

<svelte:window onkeydown={handleKeydown} />

{#if visible}
	<div class="fixed inset-0 z-50 overflow-hidden" role="dialog" aria-modal="true">
		<!-- Overlay -->
		<div
			class={cn(
				'absolute inset-0 bg-black/50 backdrop-blur-sm transition-opacity duration-300',
				animating ? 'opacity-100' : 'opacity-0'
			)}
			onclick={handleOverlayClick}
			aria-hidden="true"
		></div>

		<!-- Drawer panel -->
		<div
			class={cn(
				'fixed bg-card shadow-xl transition-transform duration-300 ease-in-out',
				positions[position],
				animating ? 'translate-x-0 translate-y-0' : closedTransforms[position],
				isHorizontal ? ['w-full', sizes[size].horizontal, 'h-full'] : ['h-auto', sizes[size].vertical, 'w-full'],
				className
			)}
		>
			<div class="flex h-full flex-col">
				{#if title || showClose}
					<div class="flex items-start justify-between p-4 border-b border-border">
						<div>
							{#if title}
								<h2 class="text-lg font-semibold text-foreground">
									{title}
								</h2>
							{/if}
							{#if description}
								<p class="mt-1 text-sm text-muted">
									{description}
								</p>
							{/if}
						</div>
						{#if showClose}
							<button
								type="button"
								onclick={close}
								class="rounded-md p-1.5 text-muted hover:text-foreground hover:bg-secondary-100 dark:hover:bg-secondary-800 transition-colors focus:outline-none focus:ring-2 focus:ring-ring"
							>
								<span class="sr-only">Close</span>
								<svg class="h-5 w-5" viewBox="0 0 20 20" fill="currentColor">
									<path
										d="M6.28 5.22a.75.75 0 00-1.06 1.06L8.94 10l-3.72 3.72a.75.75 0 101.06 1.06L10 11.06l3.72 3.72a.75.75 0 101.06-1.06L11.06 10l3.72-3.72a.75.75 0 00-1.06-1.06L10 8.94 6.28 5.22z"
									/>
								</svg>
							</button>
						{/if}
					</div>
				{/if}

				<div class="flex-1 overflow-y-auto p-4">
					{@render children()}
				</div>

				{#if footer}
					<div class="flex items-center justify-end gap-3 p-4 border-t border-border">
						{@render footer()}
					</div>
				{/if}
			</div>
		</div>
	</div>
{/if}
