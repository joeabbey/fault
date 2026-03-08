<script lang="ts">
	import { cn } from '$lib/utils/cn';

	interface Props {
		title: string;
		content: string;
		sidebar?: { label: string; href: string; active?: boolean }[];
		class?: string;
	}

	let { title, content, sidebar, class: className }: Props = $props();
</script>

<div class={cn('mx-auto max-w-[1140px] px-7 py-12', className)}>
	<div class={cn('gap-12', sidebar && sidebar.length > 0 ? 'flex max-[768px]:flex-col' : '')}>
		<!-- Sidebar -->
		{#if sidebar && sidebar.length > 0}
			<aside class="w-60 flex-shrink-0 max-[768px]:w-full">
				<nav class="sticky top-24">
					<ul class="flex flex-col gap-1">
						{#each sidebar as item}
							<li>
								<a
									href={item.href}
									class={cn(
										'block rounded-md px-3 py-2 text-sm font-medium transition-colors',
										item.active
											? 'bg-primary-500/10 text-primary-500'
											: 'text-muted-foreground hover:bg-card hover:text-foreground'
									)}
								>
									{item.label}
								</a>
							</li>
						{/each}
					</ul>
				</nav>
			</aside>
		{/if}

		<!-- Main content -->
		<main class="min-w-0 flex-1">
			<h1 class="mb-8 text-3xl font-bold tracking-tight">{title}</h1>
			<div class="docs-prose">
				{@html content}
			</div>
		</main>
	</div>
</div>

<style>
	/* Prose styling for rendered markdown/HTML content */
	.docs-prose :global(h1) {
		font-size: 2rem;
		font-weight: 700;
		letter-spacing: -0.025em;
		margin-top: 2.5rem;
		margin-bottom: 1rem;
		line-height: 1.2;
	}
	.docs-prose :global(h2) {
		font-size: 1.5rem;
		font-weight: 700;
		letter-spacing: -0.025em;
		margin-top: 2rem;
		margin-bottom: 0.75rem;
		line-height: 1.3;
		padding-bottom: 0.5rem;
		border-bottom: 1px solid var(--color-border, oklch(0.91 0.01 260));
	}
	.docs-prose :global(h3) {
		font-size: 1.25rem;
		font-weight: 600;
		margin-top: 1.5rem;
		margin-bottom: 0.5rem;
		line-height: 1.4;
	}
	.docs-prose :global(h4) {
		font-size: 1.1rem;
		font-weight: 600;
		margin-top: 1.25rem;
		margin-bottom: 0.5rem;
	}
	.docs-prose :global(p) {
		margin-bottom: 1rem;
		line-height: 1.7;
		color: var(--color-muted-foreground, oklch(0.70 0.02 260));
	}
	.docs-prose :global(a) {
		color: var(--color-primary-500, oklch(0.55 0.20 250));
		text-decoration: none;
		font-weight: 500;
	}
	.docs-prose :global(a:hover) {
		text-decoration: underline;
	}
	.docs-prose :global(strong) {
		font-weight: 600;
		color: var(--color-foreground, oklch(0.13 0.02 260));
	}
	.docs-prose :global(code) {
		font-family: ui-monospace, SFMono-Regular, 'SF Mono', Menlo, Consolas, monospace;
		font-size: 0.875em;
		padding: 0.2em 0.4em;
		border-radius: 0.375rem;
		background: var(--color-card, oklch(1 0 0));
		border: 1px solid var(--color-border, oklch(0.91 0.01 260));
	}
	.docs-prose :global(pre) {
		margin-bottom: 1.5rem;
		padding: 1rem 1.25rem;
		border-radius: 0.5rem;
		background: var(--color-card, oklch(1 0 0));
		border: 1px solid var(--color-border, oklch(0.91 0.01 260));
		overflow-x: auto;
		font-size: 0.875rem;
		line-height: 1.7;
	}
	.docs-prose :global(pre code) {
		padding: 0;
		border: none;
		background: transparent;
		font-size: inherit;
	}
	.docs-prose :global(ul),
	.docs-prose :global(ol) {
		margin-bottom: 1rem;
		padding-left: 1.5rem;
		color: var(--color-muted-foreground, oklch(0.70 0.02 260));
	}
	.docs-prose :global(ul) {
		list-style-type: disc;
	}
	.docs-prose :global(ol) {
		list-style-type: decimal;
	}
	.docs-prose :global(li) {
		margin-bottom: 0.5rem;
		line-height: 1.6;
	}
	.docs-prose :global(blockquote) {
		margin-bottom: 1rem;
		padding: 0.75rem 1rem;
		border-left: 3px solid var(--color-primary-500, oklch(0.55 0.20 250));
		background: var(--color-card, oklch(1 0 0));
		border-radius: 0 0.375rem 0.375rem 0;
		color: var(--color-muted-foreground, oklch(0.70 0.02 260));
		font-style: italic;
	}
	.docs-prose :global(table) {
		width: 100%;
		margin-bottom: 1.5rem;
		border-collapse: collapse;
		font-size: 0.875rem;
	}
	.docs-prose :global(th) {
		text-align: left;
		font-weight: 600;
		padding: 0.625rem 0.75rem;
		border-bottom: 2px solid var(--color-border, oklch(0.91 0.01 260));
	}
	.docs-prose :global(td) {
		padding: 0.625rem 0.75rem;
		border-bottom: 1px solid var(--color-border, oklch(0.91 0.01 260));
		color: var(--color-muted-foreground, oklch(0.70 0.02 260));
	}
	.docs-prose :global(hr) {
		margin: 2rem 0;
		border: none;
		border-top: 1px solid var(--color-border, oklch(0.91 0.01 260));
	}
	.docs-prose :global(img) {
		max-width: 100%;
		height: auto;
		border-radius: 0.5rem;
		margin-bottom: 1rem;
	}
</style>
