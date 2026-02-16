<script lang="ts">
	import { cn } from '$lib/utils/cn';

	interface Props {
		currentPage: number;
		totalPages: number;
		siblingCount?: number;
		showFirstLast?: boolean;
		class?: string;
		onPageChange?: (page: number) => void;
	}

	let {
		currentPage = $bindable(1),
		totalPages,
		siblingCount = 1,
		showFirstLast = true,
		class: className,
		onPageChange
	}: Props = $props();

	function goToPage(page: number) {
		if (page >= 1 && page <= totalPages && page !== currentPage) {
			currentPage = page;
			onPageChange?.(page);
		}
	}

	function getPageNumbers(): (number | 'ellipsis')[] {
		const pages: (number | 'ellipsis')[] = [];

		// Always show first page
		pages.push(1);

		// Calculate range around current page
		const leftSibling = Math.max(2, currentPage - siblingCount);
		const rightSibling = Math.min(totalPages - 1, currentPage + siblingCount);

		// Add ellipsis after first page if needed
		if (leftSibling > 2) {
			pages.push('ellipsis');
		}

		// Add pages around current
		for (let i = leftSibling; i <= rightSibling; i++) {
			if (i !== 1 && i !== totalPages) {
				pages.push(i);
			}
		}

		// Add ellipsis before last page if needed
		if (rightSibling < totalPages - 1) {
			pages.push('ellipsis');
		}

		// Always show last page (if more than 1 page)
		if (totalPages > 1) {
			pages.push(totalPages);
		}

		return pages;
	}

	const pageNumbers = $derived(getPageNumbers());
</script>

<nav aria-label="Pagination" class={cn('flex items-center gap-1', className)}>
	{#if showFirstLast}
		<button
			type="button"
			onclick={() => goToPage(1)}
			disabled={currentPage === 1}
			aria-label="Go to first page"
			class={cn(
				'p-2 rounded-md text-muted hover:text-foreground hover:bg-accent',
				'disabled:opacity-50 disabled:cursor-not-allowed disabled:hover:bg-transparent',
				'focus:outline-none focus-visible:ring-2 focus-visible:ring-ring'
			)}
		>
			<svg class="h-4 w-4" viewBox="0 0 20 20" fill="currentColor">
				<path
					fill-rule="evenodd"
					d="M15.707 15.707a1 1 0 01-1.414 0l-5-5a1 1 0 010-1.414l5-5a1 1 0 111.414 1.414L11.414 10l4.293 4.293a1 1 0 010 1.414zm-6 0a1 1 0 01-1.414 0l-5-5a1 1 0 010-1.414l5-5a1 1 0 011.414 1.414L5.414 10l4.293 4.293a1 1 0 010 1.414z"
					clip-rule="evenodd"
				/>
			</svg>
		</button>
	{/if}

	<button
		type="button"
		onclick={() => goToPage(currentPage - 1)}
		disabled={currentPage === 1}
		aria-label="Go to previous page"
		class={cn(
			'p-2 rounded-md text-muted hover:text-foreground hover:bg-accent',
			'disabled:opacity-50 disabled:cursor-not-allowed disabled:hover:bg-transparent',
			'focus:outline-none focus-visible:ring-2 focus-visible:ring-ring'
		)}
	>
		<svg class="h-4 w-4" viewBox="0 0 20 20" fill="currentColor">
			<path
				fill-rule="evenodd"
				d="M12.707 5.293a1 1 0 010 1.414L9.414 10l3.293 3.293a1 1 0 01-1.414 1.414l-4-4a1 1 0 010-1.414l4-4a1 1 0 011.414 0z"
				clip-rule="evenodd"
			/>
		</svg>
	</button>

	{#each pageNumbers as page, i}
		{#if page === 'ellipsis'}
			<span class="px-2 text-muted">...</span>
		{:else}
			<button
				type="button"
				onclick={() => goToPage(page)}
				aria-label="Go to page {page}"
				aria-current={currentPage === page ? 'page' : undefined}
				class={cn(
					'min-w-[2.25rem] h-9 px-3 rounded-md text-sm font-medium',
					'focus:outline-none focus-visible:ring-2 focus-visible:ring-ring',
					currentPage === page
						? 'bg-primary-500 text-white'
						: 'text-muted hover:text-foreground hover:bg-accent'
				)}
			>
				{page}
			</button>
		{/if}
	{/each}

	<button
		type="button"
		onclick={() => goToPage(currentPage + 1)}
		disabled={currentPage === totalPages}
		aria-label="Go to next page"
		class={cn(
			'p-2 rounded-md text-muted hover:text-foreground hover:bg-accent',
			'disabled:opacity-50 disabled:cursor-not-allowed disabled:hover:bg-transparent',
			'focus:outline-none focus-visible:ring-2 focus-visible:ring-ring'
		)}
	>
		<svg class="h-4 w-4" viewBox="0 0 20 20" fill="currentColor">
			<path
				fill-rule="evenodd"
				d="M7.293 14.707a1 1 0 010-1.414L10.586 10 7.293 6.707a1 1 0 011.414-1.414l4 4a1 1 0 010 1.414l-4 4a1 1 0 01-1.414 0z"
				clip-rule="evenodd"
			/>
		</svg>
	</button>

	{#if showFirstLast}
		<button
			type="button"
			onclick={() => goToPage(totalPages)}
			disabled={currentPage === totalPages}
			aria-label="Go to last page"
			class={cn(
				'p-2 rounded-md text-muted hover:text-foreground hover:bg-accent',
				'disabled:opacity-50 disabled:cursor-not-allowed disabled:hover:bg-transparent',
				'focus:outline-none focus-visible:ring-2 focus-visible:ring-ring'
			)}
		>
			<svg class="h-4 w-4" viewBox="0 0 20 20" fill="currentColor">
				<path
					fill-rule="evenodd"
					d="M4.293 15.707a1 1 0 010-1.414L8.586 10 4.293 5.707a1 1 0 111.414-1.414l5 5a1 1 0 010 1.414l-5 5a1 1 0 01-1.414 0zm6 0a1 1 0 010-1.414L14.586 10l-4.293-4.293a1 1 0 111.414-1.414l5 5a1 1 0 010 1.414l-5 5a1 1 0 01-1.414 0z"
					clip-rule="evenodd"
				/>
			</svg>
		</button>
	{/if}
</nav>
