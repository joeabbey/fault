<script lang="ts">
	import { cn } from '$lib/utils/cn';

	interface Option {
		value: string;
		label: string;
		disabled?: boolean;
	}

	interface OptionGroup {
		label: string;
		options: Option[];
	}

	interface Props {
		options: (Option | OptionGroup)[];
		value?: string;
		placeholder?: string;
		label?: string;
		error?: string;
		helperText?: string;
		disabled?: boolean;
		required?: boolean;
		id?: string;
		name?: string;
		class?: string;
		selectClass?: string;
		onchange?: (value: string) => void;
	}

	let {
		options,
		value = $bindable(''),
		placeholder = 'Select an option',
		label,
		error,
		helperText,
		disabled = false,
		required = false,
		id,
		name,
		class: className,
		selectClass,
		onchange
	}: Props = $props();

	const selectId = $derived(id || `select-${Math.random().toString(36).slice(2, 9)}`);

	function isOptionGroup(item: Option | OptionGroup): item is OptionGroup {
		return 'options' in item;
	}

	function handleChange(e: Event) {
		const target = e.target as HTMLSelectElement;
		value = target.value;
		onchange?.(value);
	}
</script>

<div class={cn('w-full', className)}>
	{#if label}
		<label for={selectId} class="block text-sm font-medium text-foreground mb-1.5">
			{label}
			{#if required}
				<span class="text-error-500">*</span>
			{/if}
		</label>
	{/if}

	<div class="relative">
		<select
			id={selectId}
			{name}
			{value}
			{disabled}
			{required}
			onchange={handleChange}
			aria-invalid={error ? 'true' : undefined}
			aria-describedby={error ? `${selectId}-error` : helperText ? `${selectId}-helper` : undefined}
			class={cn(
				'block w-full rounded-md border bg-background px-3 py-2 pr-10 text-sm text-foreground',
				'appearance-none cursor-pointer transition-colors',
				'focus:outline-none focus:ring-2 focus:ring-ring focus:border-transparent',
				'disabled:cursor-not-allowed disabled:opacity-50 disabled:bg-secondary-100 dark:disabled:bg-secondary-800',
				error
					? 'border-error-500 focus:ring-error-500'
					: 'border-input hover:border-secondary-400 dark:hover:border-secondary-500',
				!value && 'text-muted',
				selectClass
			)}
		>
			{#if placeholder}
				<option value="" disabled selected={!value}>{placeholder}</option>
			{/if}

			{#each options as item}
				{#if isOptionGroup(item)}
					<optgroup label={item.label}>
						{#each item.options as option}
							<option value={option.value} disabled={option.disabled}>
								{option.label}
							</option>
						{/each}
					</optgroup>
				{:else}
					<option value={item.value} disabled={item.disabled}>
						{item.label}
					</option>
				{/if}
			{/each}
		</select>

		<div
			class="absolute inset-y-0 right-0 flex items-center pr-2 pointer-events-none text-muted"
		>
			<svg class="h-5 w-5" viewBox="0 0 20 20" fill="currentColor">
				<path
					fill-rule="evenodd"
					d="M5.293 7.293a1 1 0 011.414 0L10 10.586l3.293-3.293a1 1 0 111.414 1.414l-4 4a1 1 0 01-1.414 0l-4-4a1 1 0 010-1.414z"
					clip-rule="evenodd"
				/>
			</svg>
		</div>
	</div>

	{#if error}
		<p id="{selectId}-error" class="mt-1.5 text-sm text-error-500">
			{error}
		</p>
	{:else if helperText}
		<p id="{selectId}-helper" class="mt-1.5 text-sm text-muted">
			{helperText}
		</p>
	{/if}
</div>
