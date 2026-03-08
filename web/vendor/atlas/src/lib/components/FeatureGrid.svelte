<script lang="ts">
	import { cn } from '$lib/utils/cn';
	import Badge from './Badge.svelte';

	interface Props {
		label?: string;
		headline: string;
		description?: string;
		features: {
			icon?: string;
			title: string;
			description: string;
			badge?: string;
			badgeVariant?: 'default' | 'warning' | 'success';
		}[];
		columns?: 2 | 3;
		class?: string;
	}

	let {
		label,
		headline,
		description,
		features,
		columns,
		class: className
	}: Props = $props();

	const gridClass = $derived(
		columns === 2
			? 'grid-cols-2 max-[768px]:grid-cols-1'
			: columns === 3
				? 'grid-cols-3 max-[768px]:grid-cols-1'
				: 'max-[768px]:grid-cols-1'
	);

	const gridStyle = $derived(
		columns ? undefined : 'grid-template-columns: repeat(auto-fill, minmax(280px, 1fr))'
	);
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

		<div class={cn('grid gap-3', gridClass)} style={gridStyle}>
			{#each features as feature}
				<div
					class="relative rounded-[10px] border border-border bg-card p-6 transition-all duration-200 hover:border-primary-500/20"
				>
					<div class="flex items-start justify-between">
						{#if feature.icon}
							<span class="mb-2 block text-base">{feature.icon}</span>
						{/if}
						{#if feature.badge}
							<Badge
								variant={feature.badgeVariant === 'warning'
									? 'warning'
									: feature.badgeVariant === 'success'
										? 'success'
										: 'primary'}
								size="sm"
							>
								{feature.badge}
							</Badge>
						{/if}
					</div>
					<h3 class="mb-2 text-base font-bold tracking-[-0.3px]">
						{feature.title}
					</h3>
					<p class="text-sm leading-relaxed text-muted-foreground">
						{feature.description}
					</p>
				</div>
			{/each}
		</div>
	</div>
</section>
