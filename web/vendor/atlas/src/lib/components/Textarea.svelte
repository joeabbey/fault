<script lang="ts">
	import { cn } from '$lib/utils/cn';

	interface Props {
		value?: string;
		placeholder?: string;
		label?: string;
		error?: string;
		helperText?: string;
		disabled?: boolean;
		required?: boolean;
		rows?: number;
		maxLength?: number;
		showCount?: boolean;
		autoResize?: boolean;
		id?: string;
		name?: string;
		class?: string;
		textareaClass?: string;
		oninput?: (e: Event) => void;
		onchange?: (e: Event) => void;
		onblur?: (e: FocusEvent) => void;
		onfocus?: (e: FocusEvent) => void;
	}

	let {
		value = $bindable(''),
		placeholder,
		label,
		error,
		helperText,
		disabled = false,
		required = false,
		rows = 3,
		maxLength,
		showCount = false,
		autoResize = false,
		id,
		name,
		class: className,
		textareaClass,
		oninput,
		onchange,
		onblur,
		onfocus
	}: Props = $props();

	const textareaId = $derived(id || `textarea-${Math.random().toString(36).slice(2, 9)}`);
	let textareaEl: HTMLTextAreaElement;

	function handleInput(e: Event) {
		if (autoResize && textareaEl) {
			textareaEl.style.height = 'auto';
			textareaEl.style.height = `${textareaEl.scrollHeight}px`;
		}
		oninput?.(e);
	}

	const charCount = $derived(value?.length || 0);
</script>

<div class={cn('w-full', className)}>
	{#if label}
		<label for={textareaId} class="block text-sm font-medium text-foreground mb-1.5">
			{label}
			{#if required}
				<span class="text-error-500">*</span>
			{/if}
		</label>
	{/if}

	<textarea
		bind:this={textareaEl}
		{name}
		id={textareaId}
		bind:value
		{placeholder}
		{disabled}
		{required}
		{rows}
		maxlength={maxLength}
		oninput={handleInput}
		{onchange}
		{onblur}
		{onfocus}
		aria-invalid={error ? 'true' : undefined}
		aria-describedby={error ? `${textareaId}-error` : helperText ? `${textareaId}-helper` : undefined}
		class={cn(
			'block w-full rounded-md border bg-background px-3 py-2 text-sm text-foreground',
			'placeholder:text-muted transition-colors resize-y',
			'focus:outline-none focus:ring-2 focus:ring-ring focus:border-transparent',
			'disabled:cursor-not-allowed disabled:opacity-50 disabled:bg-secondary-100 dark:disabled:bg-secondary-800',
			error
				? 'border-error-500 focus:ring-error-500'
				: 'border-input hover:border-secondary-400 dark:hover:border-secondary-500',
			autoResize && 'resize-none overflow-hidden',
			textareaClass
		)}
	></textarea>

	<div class="flex justify-between mt-1.5">
		<div>
			{#if error}
				<p id="{textareaId}-error" class="text-sm text-error-500">
					{error}
				</p>
			{:else if helperText}
				<p id="{textareaId}-helper" class="text-sm text-muted">
					{helperText}
				</p>
			{/if}
		</div>
		{#if showCount || maxLength}
			<p class="text-sm text-muted">
				{charCount}{#if maxLength}/{maxLength}{/if}
			</p>
		{/if}
	</div>
</div>
