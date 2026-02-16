<script lang="ts" generics="T">
	import { cn } from '$lib/utils/cn';

	interface Column<T> {
		key: string;
		label: string;
		sortable?: boolean;
		width?: string;
		align?: 'left' | 'center' | 'right';
		render?: (row: T) => string;
	}

	interface Props {
		columns: Column<T>[];
		data: T[];
		sortKey?: string;
		sortDirection?: 'asc' | 'desc';
		selectable?: boolean;
		selectedRows?: Set<number>;
		loading?: boolean;
		emptyMessage?: string;
		class?: string;
		onSort?: (key: string, direction: 'asc' | 'desc') => void;
		onSelectionChange?: (selected: Set<number>) => void;
		onRowClick?: (row: T, index: number) => void;
	}

	let {
		columns,
		data,
		sortKey = $bindable(''),
		sortDirection = $bindable<'asc' | 'desc'>('asc'),
		selectable = false,
		selectedRows = $bindable(new Set<number>()),
		loading = false,
		emptyMessage = 'No data available',
		class: className,
		onSort,
		onSelectionChange,
		onRowClick
	}: Props = $props();

	function handleSort(key: string) {
		if (sortKey === key) {
			sortDirection = sortDirection === 'asc' ? 'desc' : 'asc';
		} else {
			sortKey = key;
			sortDirection = 'asc';
		}
		onSort?.(sortKey, sortDirection);
	}

	function toggleSelectAll() {
		if (selectedRows.size === data.length) {
			selectedRows = new Set();
		} else {
			selectedRows = new Set(data.map((_, i) => i));
		}
		onSelectionChange?.(selectedRows);
	}

	function toggleSelectRow(index: number) {
		const newSelected = new Set(selectedRows);
		if (newSelected.has(index)) {
			newSelected.delete(index);
		} else {
			newSelected.add(index);
		}
		selectedRows = newSelected;
		onSelectionChange?.(selectedRows);
	}

	function getCellValue(row: T, column: Column<T>): string {
		if (column.render) {
			return column.render(row);
		}
		const value = (row as Record<string, unknown>)[column.key];
		return value?.toString() ?? '';
	}

	const allSelected = $derived(data.length > 0 && selectedRows.size === data.length);
	const someSelected = $derived(selectedRows.size > 0 && selectedRows.size < data.length);

	const alignments = {
		left: 'text-left',
		center: 'text-center',
		right: 'text-right'
	};
</script>

<div class={cn('overflow-x-auto rounded-lg border border-border', className)}>
	<table class="w-full text-sm">
		<thead class="bg-secondary-100 dark:bg-secondary-800">
			<tr>
				{#if selectable}
					<th class="w-10 px-4 py-3">
						<input
							type="checkbox"
							checked={allSelected}
							indeterminate={someSelected}
							onchange={toggleSelectAll}
							class="h-4 w-4 rounded border-input bg-background text-primary-500 focus:ring-2 focus:ring-ring cursor-pointer"
						/>
					</th>
				{/if}
				{#each columns as column}
					<th
						class={cn(
							'px-4 py-3 text-xs font-medium text-secondary-600 dark:text-secondary-300 uppercase tracking-wider',
							alignments[column.align || 'left'],
							column.sortable && 'cursor-pointer hover:text-foreground select-none'
						)}
						style={column.width ? `width: ${column.width}` : undefined}
						onclick={() => column.sortable && handleSort(column.key)}
					>
						<div class="flex items-center gap-1">
							<span>{column.label}</span>
							{#if column.sortable}
								<span class="inline-flex flex-col">
									<svg
										class={cn(
											'h-3 w-3 -mb-1',
											sortKey === column.key && sortDirection === 'asc'
												? 'text-primary-500'
												: 'text-secondary-300'
										)}
										viewBox="0 0 20 20"
										fill="currentColor"
									>
										<path d="M5 10l5-5 5 5H5z" />
									</svg>
									<svg
										class={cn(
											'h-3 w-3',
											sortKey === column.key && sortDirection === 'desc'
												? 'text-primary-500'
												: 'text-secondary-300'
										)}
										viewBox="0 0 20 20"
										fill="currentColor"
									>
										<path d="M5 10l5 5 5-5H5z" />
									</svg>
								</span>
							{/if}
						</div>
					</th>
				{/each}
			</tr>
		</thead>
		<tbody class="divide-y divide-border bg-card">
			{#if loading}
				<tr>
					<td
						colspan={columns.length + (selectable ? 1 : 0)}
						class="px-4 py-12 text-center text-muted"
					>
						<div class="flex items-center justify-center gap-2">
							<svg
								class="h-5 w-5 animate-spin"
								xmlns="http://www.w3.org/2000/svg"
								fill="none"
								viewBox="0 0 24 24"
							>
								<circle
									class="opacity-25"
									cx="12"
									cy="12"
									r="10"
									stroke="currentColor"
									stroke-width="4"
								></circle>
								<path
									class="opacity-75"
									fill="currentColor"
									d="M4 12a8 8 0 018-8V0C5.373 0 0 5.373 0 12h4zm2 5.291A7.962 7.962 0 014 12H0c0 3.042 1.135 5.824 3 7.938l3-2.647z"
								></path>
							</svg>
							<span>Loading...</span>
						</div>
					</td>
				</tr>
			{:else if data.length === 0}
				<tr>
					<td
						colspan={columns.length + (selectable ? 1 : 0)}
						class="px-4 py-12 text-center text-muted"
					>
						{emptyMessage}
					</td>
				</tr>
			{:else}
				{#each data as row, index}
					<tr
						class={cn(
							'transition-colors',
							onRowClick && 'cursor-pointer hover:bg-secondary-50 dark:hover:bg-secondary-800/50',
							selectedRows.has(index) && 'bg-primary-50 dark:bg-primary-900'
						)}
						onclick={() => onRowClick?.(row, index)}
					>
						{#if selectable}
							<td class="px-4 py-3" onclick={(e) => e.stopPropagation()}>
								<input
									type="checkbox"
									checked={selectedRows.has(index)}
									onchange={() => toggleSelectRow(index)}
									class="h-4 w-4 rounded border-input bg-background text-primary-500 focus:ring-2 focus:ring-ring cursor-pointer"
								/>
							</td>
						{/if}
						{#each columns as column}
							<td
								class={cn(
									'px-4 py-3 text-foreground',
									alignments[column.align || 'left']
								)}
							>
								{getCellValue(row, column)}
							</td>
						{/each}
					</tr>
				{/each}
			{/if}
		</tbody>
	</table>
</div>
