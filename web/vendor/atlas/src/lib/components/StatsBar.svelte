<script lang="ts">
	import { cn } from '$lib/utils/cn';

	interface Props {
		stats: { value: string; label: string }[];
		class?: string;
	}

	let { stats, class: className }: Props = $props();
</script>

<section class={cn('border-y border-border py-11', className)}>
	<div class="mx-auto max-w-[1140px] px-7">
		<div
			class="grid gap-8 text-center max-[768px]:grid-cols-2 max-[768px]:gap-6"
			style="grid-template-columns: repeat({stats.length}, 1fr)"
		>
			{#each stats as stat}
				<div>
					<div
						class="mb-1 bg-gradient-to-r from-primary-500 to-warning-500 bg-clip-text text-[44px] font-extrabold leading-none tracking-[-2px] text-transparent max-[768px]:text-[34px]"
					>
						{stat.value}
					</div>
					<div class="font-mono text-[13px] font-medium text-muted-foreground">
						{stat.label}
					</div>
				</div>
			{/each}
		</div>
	</div>
</section>
