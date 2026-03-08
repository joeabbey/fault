<script lang="ts">
	import { cn } from '$lib/utils/cn';

	interface Props {
		label?: string;
		headline: string;
		steps: { title: string; description: string }[];
		class?: string;
	}

	let { label, headline, steps, class: className }: Props = $props();
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
			class="mb-9 text-[34px] font-bold leading-[1.15] tracking-[-0.5px] max-[768px]:text-[26px]"
		>
			{headline}
		</h2>

		<div class="flex max-w-[640px] flex-col">
			{#each steps as step, i}
				<div class="flex gap-5 max-[768px]:gap-3.5">
					<!-- Number circle + connector -->
					<div class="flex flex-shrink-0 flex-col items-center">
						<div
							class="flex h-10 w-10 items-center justify-center rounded-full border border-primary-500/10 bg-primary-500/5 font-mono text-[15px] font-bold text-primary-500"
						>
							{i + 1}
						</div>
						{#if i < steps.length - 1}
							<div class="w-0.5 flex-1 bg-primary-500/10" style="min-height: 20px"></div>
						{/if}
					</div>

					<!-- Content -->
					<div class={cn('pb-7', i === steps.length - 1 && 'pb-0')}>
						<h3 class="mb-1.5 text-[17px] font-bold">
							{step.title}
						</h3>
						<p class="text-sm leading-relaxed text-muted-foreground">
							{step.description}
						</p>
					</div>
				</div>
			{/each}
		</div>
	</div>
</section>
