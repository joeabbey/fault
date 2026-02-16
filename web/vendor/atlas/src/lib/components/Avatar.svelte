<script lang="ts">
	import { cn } from '$lib/utils/cn';

	interface Props {
		src?: string | null;
		alt?: string;
		initials?: string;
		size?: 'xs' | 'sm' | 'md' | 'lg' | 'xl';
		class?: string;
		fallback?: import('svelte').Snippet;
	}

	let { src, alt = '', initials, size = 'md', class: className, fallback }: Props = $props();

	let imageError = $state(false);

	const sizes = {
		xs: 'h-6 w-6 text-xs',
		sm: 'h-8 w-8 text-sm',
		md: 'h-10 w-10 text-base',
		lg: 'h-12 w-12 text-lg',
		xl: 'h-16 w-16 text-xl'
	};

	const baseClasses =
		'relative inline-flex items-center justify-center rounded-full bg-secondary-200 dark:bg-secondary-700 text-secondary-700 dark:text-secondary-200 font-medium overflow-hidden';

	function getInitials(name: string): string {
		return name
			.split(' ')
			.map((part) => part.charAt(0))
			.slice(0, 2)
			.join('')
			.toUpperCase();
	}

	const displayInitials = $derived(initials || (alt ? getInitials(alt) : '?'));
</script>

<span class={cn(baseClasses, sizes[size], className)}>
	{#if src && !imageError}
		<img
			{src}
			{alt}
			class="h-full w-full object-cover"
			onerror={() => {
				imageError = true;
			}}
		/>
	{:else if fallback}
		{@render fallback()}
	{:else}
		<span aria-hidden="true">{displayInitials}</span>
	{/if}
</span>
