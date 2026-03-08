<script lang="ts">
	import { Navbar, DocsRenderer } from '@jabbey/atlas';
	import { marked } from 'marked';
	import { docs } from '$lib/data/docs';

	let { data } = $props();

	const currentDoc = $derived(docs.find((d) => d.slug === data.slug));
	const title = $derived(currentDoc?.title ?? data.slug);

	const sidebar = $derived(
		docs.map((d) => ({
			label: d.title,
			href: `/docs/${d.slug}`,
			active: d.slug === data.slug
		}))
	);

	const html = $derived(marked.parse(data.markdown) as string);

	const navItems = [
		{ label: 'Docs', href: '/docs' },
		{ label: 'GitHub', href: 'https://github.com/joeabbey/fault' }
	];
</script>

<svelte:head>
	<title>{title} - Fault</title>
</svelte:head>

<div class="dark min-h-screen bg-background text-foreground">
	<!-- Navbar -->
	<Navbar brandHref="/" items={navItems} sticky>
		{#snippet logo()}
			<span
				class="inline-flex items-center justify-center w-7 h-7 rounded-md text-xs font-bold tracking-tighter font-mono"
				style="background: #f43f5e; color: #07080c; letter-spacing: -1px;"
			>
				//
			</span>
			<span class="text-lg font-bold font-display text-foreground" style="letter-spacing: -0.5px;">
				Fault
			</span>
		{/snippet}
		{#snippet actions()}
			<a
				href="/login"
				class="rounded-lg bg-primary-500 px-4 py-2 text-sm font-semibold text-white transition-colors hover:bg-primary-600"
			>
				Dashboard
			</a>
		{/snippet}
	</Navbar>

	<!-- Docs Content -->
	<DocsRenderer {title} content={html} {sidebar} />

	<!-- Footer -->
	<footer class="border-t border-border py-10">
		<div class="mx-auto max-w-[1140px] px-7">
			<div class="flex items-center justify-between max-[768px]:flex-col max-[768px]:gap-4">
				<div class="flex items-center gap-2">
					<span
						class="inline-flex items-center justify-center w-6 h-6 rounded-md text-[10px] font-bold tracking-tighter font-mono"
						style="background: #f43f5e; color: #07080c; letter-spacing: -1px;"
					>
						//
					</span>
					<span class="text-sm font-semibold text-foreground">Fault</span>
				</div>
				<div class="flex items-center gap-6 text-sm text-muted-foreground">
					<a href="/docs" class="transition-colors hover:text-foreground">Docs</a>
					<a href="https://github.com/joeabbey/fault" class="transition-colors hover:text-foreground">GitHub</a>
					<a href="mailto:joe@jabbey.io" class="transition-colors hover:text-foreground">Contact</a>
				</div>
			</div>
		</div>
	</footer>
</div>
