<script lang="ts">
	import { cn } from '$lib/utils/cn';

	interface Props {
		orientation?: 'horizontal' | 'vertical';
		label?: string;
		class?: string;
	}

	let { orientation = 'horizontal', label, class: className }: Props = $props();
</script>

{#if orientation === 'horizontal'}
	{#if label}
		<div class={cn('relative', className)}>
			<div class="absolute inset-0 flex items-center" aria-hidden="true">
				<div class="w-full border-t border-border"></div>
			</div>
			<div class="relative flex justify-center">
				<span class="bg-background px-3 text-sm text-muted">
					{label}
				</span>
			</div>
		</div>
	{:else}
		<hr class={cn('border-t border-border', className)} />
	{/if}
{:else}
	<div
		class={cn('inline-block h-full min-h-[1em] w-px bg-border self-stretch', className)}
		role="separator"
		aria-orientation="vertical"
	></div>
{/if}
