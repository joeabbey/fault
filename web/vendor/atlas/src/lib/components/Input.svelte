<script lang="ts">
	import { cn } from '$lib/utils/cn';

	interface Props {
		type?: 'text' | 'email' | 'password' | 'number' | 'tel' | 'url' | 'search';
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
		inputClass?: string;
		prefix?: import('svelte').Snippet;
		suffix?: import('svelte').Snippet;
		oninput?: (e: Event) => void;
		onchange?: (e: Event) => void;
		onblur?: (e: FocusEvent) => void;
		onfocus?: (e: FocusEvent) => void;
	}

	let {
		type = 'text',
		value = $bindable(''),
		placeholder,
		label,
		error,
		helperText,
		disabled = false,
		required = false,
		id,
		name,
		class: className,
		inputClass,
		prefix,
		suffix,
		oninput,
		onchange,
		onblur,
		onfocus
	}: Props = $props();

	const inputId = $derived(id || `input-${Math.random().toString(36).slice(2, 9)}`);
</script>

<div class={cn('w-full', className)}>
	{#if label}
		<label for={inputId} class="block text-sm font-medium text-foreground mb-1.5">
			{label}
			{#if required}
				<span class="text-error-500">*</span>
			{/if}
		</label>
	{/if}

	<div class="relative">
		{#if prefix}
			<div
				class="absolute inset-y-0 left-0 pl-3 flex items-center pointer-events-none text-muted"
			>
				{@render prefix()}
			</div>
		{/if}

		<input
			{type}
			{name}
			id={inputId}
			bind:value
			{placeholder}
			{disabled}
			{required}
			{oninput}
			{onchange}
			{onblur}
			{onfocus}
			aria-invalid={error ? 'true' : undefined}
			aria-describedby={error ? `${inputId}-error` : helperText ? `${inputId}-helper` : undefined}
			class={cn(
				'block w-full rounded-md border bg-background px-3 py-2 text-sm text-foreground',
				'placeholder:text-muted transition-colors',
				'focus:outline-none focus:ring-2 focus:ring-ring focus:border-transparent',
				'disabled:cursor-not-allowed disabled:opacity-50 disabled:bg-secondary-100 dark:disabled:bg-secondary-800',
				error
					? 'border-error-500 focus:ring-error-500'
					: 'border-input hover:border-secondary-400 dark:hover:border-secondary-500',
				prefix && 'pl-10',
				suffix && 'pr-10',
				inputClass
			)}
		/>

		{#if suffix}
			<div
				class="absolute inset-y-0 right-0 pr-3 flex items-center pointer-events-none text-muted"
			>
				{@render suffix()}
			</div>
		{/if}
	</div>

	{#if error}
		<p id="{inputId}-error" class="mt-1.5 text-sm text-error-500">
			{error}
		</p>
	{:else if helperText}
		<p id="{inputId}-helper" class="mt-1.5 text-sm text-muted">
			{helperText}
		</p>
	{/if}
</div>
