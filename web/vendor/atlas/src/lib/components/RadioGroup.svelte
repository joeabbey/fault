<script lang="ts">
	import { cn } from '$lib/utils/cn';

	interface Option {
		value: string;
		label: string;
		description?: string;
		disabled?: boolean;
	}

	interface Props {
		options: Option[];
		value?: string;
		name?: string;
		label?: string;
		orientation?: 'horizontal' | 'vertical';
		disabled?: boolean;
		class?: string;
		onchange?: (value: string) => void;
	}

	let {
		options,
		value = $bindable(''),
		name,
		label,
		orientation = 'vertical',
		disabled = false,
		class: className,
		onchange
	}: Props = $props();

	const groupName = $derived(name || `radio-group-${Math.random().toString(36).slice(2, 9)}`);

	function handleChange(optionValue: string) {
		value = optionValue;
		onchange?.(optionValue);
	}
</script>

<fieldset class={cn('w-full', className)} {disabled}>
	{#if label}
		<legend class="text-sm font-medium text-foreground mb-3">
			{label}
		</legend>
	{/if}

	<div
		class={cn(
			'flex',
			orientation === 'vertical' ? 'flex-col space-y-3' : 'flex-row flex-wrap gap-4'
		)}
		role="radiogroup"
	>
		{#each options as option}
			<div class="flex items-start">
				<div class="flex items-center h-5">
					<input
						type="radio"
						id="{groupName}-{option.value}"
						name={groupName}
						value={option.value}
						checked={value === option.value}
						disabled={disabled || option.disabled}
						onchange={() => handleChange(option.value)}
						class={cn(
							'h-4 w-4 border-input bg-background text-primary-500',
							'focus:ring-2 focus:ring-ring focus:ring-offset-2 focus:ring-offset-background',
							'disabled:cursor-not-allowed disabled:opacity-50',
							'cursor-pointer'
						)}
					/>
				</div>
				<div class="ml-3">
					<label
						for="{groupName}-{option.value}"
						class={cn(
							'text-sm font-medium text-foreground cursor-pointer',
							(disabled || option.disabled) && 'opacity-50 cursor-not-allowed'
						)}
					>
						{option.label}
					</label>
					{#if option.description}
						<p
							class={cn(
								'text-sm text-muted',
								(disabled || option.disabled) && 'opacity-50'
							)}
						>
							{option.description}
						</p>
					{/if}
				</div>
			</div>
		{/each}
	</div>
</fieldset>
