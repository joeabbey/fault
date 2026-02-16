<script lang="ts">
	import { cn } from '$lib/utils/cn';

	interface Props {
		checked?: boolean;
		label?: string;
		description?: string;
		disabled?: boolean;
		id?: string;
		name?: string;
		value?: string;
		class?: string;
		onchange?: (e: Event) => void;
	}

	let {
		checked = $bindable(false),
		label,
		description,
		disabled = false,
		id,
		name,
		value,
		class: className,
		onchange
	}: Props = $props();

	const radioId = $derived(id || `radio-${Math.random().toString(36).slice(2, 9)}`);
</script>

<div class={cn('flex items-start', className)}>
	<div class="flex items-center h-5">
		<input
			type="radio"
			id={radioId}
			{name}
			{value}
			checked={checked}
			{disabled}
			{onchange}
			class={cn(
				'h-4 w-4 border-input bg-background text-primary-500',
				'focus:ring-2 focus:ring-ring focus:ring-offset-2 focus:ring-offset-background',
				'disabled:cursor-not-allowed disabled:opacity-50',
				'cursor-pointer'
			)}
		/>
	</div>
	{#if label || description}
		<div class="ml-3">
			{#if label}
				<label
					for={radioId}
					class={cn(
						'text-sm font-medium text-foreground cursor-pointer',
						disabled && 'opacity-50 cursor-not-allowed'
					)}
				>
					{label}
				</label>
			{/if}
			{#if description}
				<p class={cn('text-sm text-muted', disabled && 'opacity-50')}>
					{description}
				</p>
			{/if}
		</div>
	{/if}
</div>
