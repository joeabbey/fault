<script lang="ts">
	import { cn } from '$lib/utils/cn';

	interface Props {
		open?: boolean;
		title?: string;
		description?: string;
		size?: 'sm' | 'md' | 'lg' | 'xl' | 'full';
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
		size = 'md',
		closeOnOverlay = true,
		closeOnEscape = true,
		showClose = true,
		class: className,
		onclose,
		children,
		footer
	}: Props = $props();

	const sizes = {
		sm: 'max-w-sm',
		md: 'max-w-lg',
		lg: 'max-w-2xl',
		xl: 'max-w-4xl',
		full: 'max-w-[calc(100vw-2rem)] max-h-[calc(100vh-2rem)]'
	};

	function close() {
		open = false;
		onclose?.();
	}

	function handleKeydown(e: KeyboardEvent) {
		if (e.key === 'Escape' && closeOnEscape) {
			e.preventDefault();
			close();
		}
	}

	function handleOverlayClick(e: MouseEvent) {
		if (closeOnOverlay && e.target === e.currentTarget) {
			close();
		}
	}

	function lockScroll(_node: HTMLElement) {
		document.body.style.overflow = 'hidden';
		return {
			destroy() {
				document.body.style.overflow = '';
			}
		};
	}
</script>

{#if open}
	<!-- svelte-ignore a11y_no_noninteractive_element_interactions -->
	<div
		use:lockScroll
		role="dialog"
		aria-modal="true"
		class="fixed inset-0 z-50 flex items-center justify-center p-4"
		onkeydown={handleKeydown}
		onclick={handleOverlayClick}
	>
		<!-- Backdrop with blur -->
		<div class="absolute inset-0 bg-black/30 backdrop-blur-md" aria-hidden="true"></div>

		<!-- Modal content -->
		<div
			role="document"
			class={cn(
				'relative w-full rounded-lg bg-card shadow-xl',
				'animate-in',
				sizes[size],
				className
			)}
			onclick={(e) => e.stopPropagation()}
			onkeydown={(e) => e.stopPropagation()}
		>
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

			<div class="p-4 max-h-[60vh] overflow-y-auto">
				{@render children()}
			</div>

			{#if footer}
				<div class="flex items-center justify-end gap-3 p-4 border-t border-border">
					{@render footer()}
				</div>
			{/if}
		</div>
	</div>
{/if}
