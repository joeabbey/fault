<script lang="ts">
	import { cn } from '$lib/utils/cn';

	interface Props {
		/** Logo snippet */
		logo?: import('svelte').Snippet;
		/** Title text */
		title?: string;
		/** Subtitle/description */
		subtitle?: string;
		/** Main content (form) */
		children: import('svelte').Snippet;
		/** Footer content (links, terms) */
		footer?: import('svelte').Snippet;
		/** Background variant */
		variant?: 'simple' | 'split' | 'gradient';
		/** Side panel content for split variant */
		sideContent?: import('svelte').Snippet;
		class?: string;
	}

	let {
		logo,
		title,
		subtitle,
		children,
		footer,
		variant = 'simple',
		sideContent,
		class: className
	}: Props = $props();
</script>

{#if variant === 'split'}
	<div class={cn('flex min-h-screen', className)}>
		<!-- Left side - Branding/Marketing -->
		<div class="hidden lg:flex lg:w-1/2 bg-primary-600 dark:bg-primary-900 text-white p-12 flex-col justify-between">
			{#if sideContent}
				{@render sideContent()}
			{:else}
				<div>
					{#if logo}
						{@render logo()}
					{/if}
				</div>
				<div>
					<h1 class="text-4xl font-bold mb-4">Welcome back</h1>
					<p class="text-primary-100 text-lg">Sign in to continue to your dashboard.</p>
				</div>
				<div class="text-primary-200 text-sm">
					&copy; {new Date().getFullYear()} Your Company
				</div>
			{/if}
		</div>

		<!-- Right side - Form -->
		<div class="flex-1 flex items-center justify-center p-8 bg-background">
			<div class="w-full max-w-md">
				<div class="lg:hidden mb-8">
					{#if logo}
						{@render logo()}
					{/if}
				</div>

				{#if title}
					<h2 class="text-2xl font-bold text-foreground mb-2">{title}</h2>
				{/if}
				{#if subtitle}
					<p class="text-muted mb-8">{subtitle}</p>
				{/if}

				{@render children()}

				{#if footer}
					<div class="mt-8 text-center text-sm text-muted">
						{@render footer()}
					</div>
				{/if}
			</div>
		</div>
	</div>
{:else if variant === 'gradient'}
	<div class={cn('min-h-screen flex items-center justify-center p-4 bg-gradient-to-br from-primary-500 to-primary-700', className)}>
		<div class="w-full max-w-md">
			<div class="bg-card rounded-xl shadow-2xl p-8">
				{#if logo}
					<div class="flex justify-center mb-6">
						{@render logo()}
					</div>
				{/if}

				{#if title}
					<h2 class="text-2xl font-bold text-foreground text-center mb-2">{title}</h2>
				{/if}
				{#if subtitle}
					<p class="text-muted text-center mb-8">{subtitle}</p>
				{/if}

				{@render children()}

				{#if footer}
					<div class="mt-8 text-center text-sm text-muted">
						{@render footer()}
					</div>
				{/if}
			</div>
		</div>
	</div>
{:else}
	<!-- Simple centered layout -->
	<div class={cn('min-h-screen flex flex-col items-center justify-center p-4 bg-background', className)}>
		<div class="w-full max-w-md">
			{#if logo}
				<div class="flex justify-center mb-8">
					{@render logo()}
				</div>
			{/if}

			<div class="bg-card rounded-lg border border-border shadow-sm p-8">
				{#if title}
					<h2 class="text-2xl font-bold text-foreground text-center mb-2">{title}</h2>
				{/if}
				{#if subtitle}
					<p class="text-muted text-center mb-8">{subtitle}</p>
				{/if}

				{@render children()}
			</div>

			{#if footer}
				<div class="mt-8 text-center text-sm text-muted">
					{@render footer()}
				</div>
			{/if}
		</div>
	</div>
{/if}
