<script lang="ts">
	import { cn } from '$lib/utils/cn';

	interface Props {
		value?: number;
		min?: number;
		max?: number;
		step?: number;
		label?: string;
		showValue?: boolean;
		showMinMax?: boolean;
		disabled?: boolean;
		id?: string;
		name?: string;
		class?: string;
		onchange?: (value: number) => void;
		oninput?: (value: number) => void;
	}

	let {
		value = $bindable(50),
		min = 0,
		max = 100,
		step = 1,
		label,
		showValue = false,
		showMinMax = false,
		disabled = false,
		id,
		name,
		class: className,
		onchange,
		oninput
	}: Props = $props();

	const sliderId = $derived(id || `slider-${Math.random().toString(36).slice(2, 9)}`);

	function handleInput(e: Event) {
		const target = e.target as HTMLInputElement;
		value = Number(target.value);
		oninput?.(value);
	}

	function handleChange(e: Event) {
		const target = e.target as HTMLInputElement;
		value = Number(target.value);
		onchange?.(value);
	}

	const percentage = $derived(((value - min) / (max - min)) * 100);
</script>

<div class={cn('w-full', className)}>
	{#if label || showValue}
		<div class="flex justify-between items-center mb-2">
			{#if label}
				<label for={sliderId} class="text-sm font-medium text-foreground">
					{label}
				</label>
			{/if}
			{#if showValue}
				<span class="text-sm text-muted tabular-nums">{value}</span>
			{/if}
		</div>
	{/if}

	<div class="relative">
		<input
			type="range"
			id={sliderId}
			{name}
			{value}
			{min}
			{max}
			{step}
			{disabled}
			oninput={handleInput}
			onchange={handleChange}
			class={cn(
				'w-full h-2 rounded-full appearance-none cursor-pointer',
				'bg-secondary-200 dark:bg-secondary-700',
				'focus:outline-none focus:ring-2 focus:ring-ring focus:ring-offset-2 focus:ring-offset-background',
				'disabled:cursor-not-allowed disabled:opacity-50',
				'[&::-webkit-slider-thumb]:appearance-none',
				'[&::-webkit-slider-thumb]:h-5 [&::-webkit-slider-thumb]:w-5',
				'[&::-webkit-slider-thumb]:rounded-full [&::-webkit-slider-thumb]:bg-primary-500',
				'[&::-webkit-slider-thumb]:shadow-md [&::-webkit-slider-thumb]:cursor-pointer',
				'[&::-webkit-slider-thumb]:transition-transform [&::-webkit-slider-thumb]:hover:scale-110',
				'[&::-moz-range-thumb]:h-5 [&::-moz-range-thumb]:w-5',
				'[&::-moz-range-thumb]:rounded-full [&::-moz-range-thumb]:bg-primary-500',
				'[&::-moz-range-thumb]:border-0 [&::-moz-range-thumb]:shadow-md',
				'[&::-moz-range-thumb]:cursor-pointer'
			)}
			style="background: linear-gradient(to right, var(--color-primary-500) 0%, var(--color-primary-500) {percentage}%, var(--color-secondary-200) {percentage}%, var(--color-secondary-200) 100%)"
		/>
	</div>

	{#if showMinMax}
		<div class="flex justify-between mt-1">
			<span class="text-xs text-muted">{min}</span>
			<span class="text-xs text-muted">{max}</span>
		</div>
	{/if}
</div>
