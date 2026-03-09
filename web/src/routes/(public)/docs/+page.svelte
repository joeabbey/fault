<script lang="ts">
	import { Navbar } from '@jabbey/atlas';
	import ThemeToggle from '$lib/components/ThemeToggle.svelte';
	import { theme } from '$lib/stores/theme';
	import { docs } from '$lib/data/docs';

	const navItems = [
		{ label: 'Docs', href: '/docs' },
		{ label: 'GitHub', href: 'https://github.com/joeabbey/fault' }
	];
</script>

<svelte:head>
	<title>Documentation - Fault</title>
	<meta name="description" content="Fault documentation. Installation, integrations, language support, and CI configuration." />
	<meta name="theme-color" content={$theme === 'dark' ? '#07080c' : '#faf8f7'} />
</svelte:head>

<div class:dark={$theme === 'dark'} class="min-h-screen bg-background text-foreground">
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
			<ThemeToggle />
			<a
				href="/login"
				class="rounded-lg bg-primary-500 px-4 py-2 text-sm font-semibold text-white transition-colors hover:bg-primary-600"
			>
				Dashboard
			</a>
		{/snippet}
	</Navbar>

	<!-- Content -->
	<section class="py-[72px] max-[768px]:py-12">
		<div class="mx-auto max-w-[1140px] px-7">
			<div
				class="mb-3 font-mono text-xs font-semibold uppercase tracking-[2px] text-primary-500"
			>
				Resources
			</div>
			<h1
				class="mb-4 text-[34px] font-bold leading-[1.15] tracking-[-0.5px] max-[768px]:text-[26px]"
			>
				Documentation
			</h1>
			<p class="mb-10 max-w-[620px] text-base leading-[1.7] text-muted-foreground">
				Everything you need to install, configure, and integrate Fault into your workflow.
			</p>

			<div class="grid gap-4 sm:grid-cols-2 lg:grid-cols-3">
				{#each docs as doc}
					<a
						href="/docs/{doc.slug}"
						class="group rounded-xl border border-border bg-card p-6 transition-colors hover:border-primary-500/30"
					>
						<h2 class="mb-2 text-lg font-semibold text-foreground group-hover:text-primary-500 transition-colors">
							{doc.title}
						</h2>
						<p class="text-sm leading-relaxed text-muted-foreground">
							{doc.description}
						</p>
					</a>
				{/each}
			</div>
		</div>
	</section>

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
					<a href="/terms" class="transition-colors hover:text-foreground">Terms</a>
					<a href="/privacy" class="transition-colors hover:text-foreground">Privacy</a>
				</div>
			</div>
		</div>
	</footer>
</div>
