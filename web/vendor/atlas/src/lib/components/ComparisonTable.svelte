<script lang="ts">
	import { cn } from '$lib/utils/cn';

	interface Props {
		label?: string;
		headline: string;
		description?: string;
		columns: string[];
		rows: { check: string; values: string[] }[];
		class?: string;
	}

	let { label, headline, description, columns, rows, class: className }: Props = $props();

	function valueClass(value: string): string {
		const v = value.toLowerCase();
		if (v === 'yes' || v.includes('yes')) return 'text-success-500 font-semibold';
		if (v === 'no') return 'text-secondary-600 dark:text-secondary-700';
		if (v === 'partial') return 'text-warning-500';
		return 'text-muted-foreground';
	}
</script>

<section class={cn('py-[72px] max-[768px]:py-12', className)}>
	<div class="mx-auto max-w-[1140px] px-7">
		{#if label}
			<div
				class="mb-3 font-mono text-xs font-semibold uppercase tracking-[2px] text-primary-500"
			>
				{label}
			</div>
		{/if}

		<h2
			class="mb-4 text-[34px] font-bold leading-[1.15] tracking-[-0.5px] max-[768px]:text-[26px]"
		>
			{headline}
		</h2>

		{#if description}
			<p class="mb-9 max-w-[620px] text-base leading-[1.7] text-muted-foreground">
				{description}
			</p>
		{/if}

		<div class="max-w-[740px] overflow-x-auto rounded-[10px] border border-border">
			<table class="w-full border-collapse text-sm max-[768px]:text-[13px]">
				<thead>
					<tr>
						{#each columns as col}
							<th
								class="border-b border-border bg-card px-[18px] py-3 text-left font-mono text-[11px] font-semibold uppercase tracking-[0.5px] text-muted-foreground max-[768px]:px-3 max-[768px]:py-2.5"
							>
								{col}
							</th>
						{/each}
					</tr>
				</thead>
				<tbody>
					{#each rows as row, rowIdx}
						<tr class="transition-colors hover:bg-primary-500/[0.02]">
							<td
								class={cn(
									'border-b border-border px-[18px] py-3 text-muted-foreground max-[768px]:px-3 max-[768px]:py-2.5',
									rowIdx === rows.length - 1 && 'border-b-0'
								)}
							>
								{row.check}
							</td>
							{#each row.values as value, valIdx}
								<td
									class={cn(
										'border-b border-border px-[18px] py-3 max-[768px]:px-3 max-[768px]:py-2.5',
										rowIdx === rows.length - 1 && 'border-b-0',
										valueClass(value)
									)}
								>
									{value}
								</td>
							{/each}
						</tr>
					{/each}
				</tbody>
			</table>
		</div>
	</div>
</section>
