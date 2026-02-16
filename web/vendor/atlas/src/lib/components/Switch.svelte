<script lang="ts">
	import { cn } from '$lib/utils/cn';

	interface Props {
		checked?: boolean;
		label?: string;
		description?: string;
		disabled?: boolean;
		size?: 'sm' | 'md' | 'lg';
		id?: string;
		name?: string;
		class?: string;
		onchange?: (checked: boolean) => void;
	}

	let {
		checked = $bindable(false),
		label,
		description,
		disabled = false,
		size = 'md',
		id,
		name,
		class: className,
		onchange
	}: Props = $props();

	const switchId = $derived(id || `switch-${Math.random().toString(36).slice(2, 9)}`);

	const sizes = {
		sm: { track: 'h-5 w-9', thumb: 'h-4 w-4', translate: 'translate-x-4' },
		md: { track: 'h-6 w-11', thumb: 'h-5 w-5', translate: 'translate-x-5' },
		lg: { track: 'h-7 w-14', thumb: 'h-6 w-6', translate: 'translate-x-7' }
	};

	function handleClick() {
		if (!disabled) {
			checked = !checked;
			onchange?.(checked);
		}
	}

	function handleKeydown(e: KeyboardEvent) {
		if (e.key === 'Enter' || e.key === ' ') {
			e.preventDefault();
			handleClick();
		}
	}
</script>

<div class={cn('flex items-start', className)}>
	<button
		type="button"
		role="switch"
		aria-checked={checked}
		aria-labelledby={label ? `${switchId}-label` : undefined}
		aria-describedby={description ? `${switchId}-description` : undefined}
		{disabled}
		onclick={handleClick}
		onkeydown={handleKeydown}
		class={cn(
			'relative inline-flex flex-shrink-0 cursor-pointer rounded-full border-2 border-transparent',
			'transition-colors duration-200 ease-in-out',
			'focus:outline-none focus:ring-2 focus:ring-ring focus:ring-offset-2 focus:ring-offset-background',
			'disabled:cursor-not-allowed disabled:opacity-50',
			sizes[size].track,
			checked ? 'bg-primary-500' : 'bg-secondary-300 dark:bg-secondary-600'
		)}
	>
		<span
			aria-hidden="true"
			class={cn(
				'pointer-events-none inline-block rounded-full bg-white shadow-lg ring-0',
				'transform transition duration-200 ease-in-out',
				sizes[size].thumb,
				checked ? sizes[size].translate : 'translate-x-0'
			)}
		></span>
	</button>

	{#if label || description}
		<div class="ml-3">
			{#if label}
				<span
					id="{switchId}-label"
					class={cn(
						'text-sm font-medium text-foreground cursor-pointer select-none',
						disabled && 'opacity-50 cursor-not-allowed'
					)}
					onclick={handleClick}
					onkeydown={handleKeydown}
					role="presentation"
				>
					{label}
				</span>
			{/if}
			{#if description}
				<p
					id="{switchId}-description"
					class={cn('text-sm text-muted', disabled && 'opacity-50')}
				>
					{description}
				</p>
			{/if}
		</div>
	{/if}

	{#if name}
		<input type="hidden" {name} value={checked ? 'on' : 'off'} />
	{/if}
</div>
