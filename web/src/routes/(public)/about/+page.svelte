<script lang="ts">
	import { Navbar } from '@jabbey/atlas';
	import ThemeToggle from '$lib/components/ThemeToggle.svelte';
	import { theme } from '$lib/stores/theme';

	const navItems = [
		{ label: 'Docs', href: '/docs' },
		{ label: 'GitHub', href: 'https://github.com/joeabbey/fault' }
	];
</script>

<svelte:head>
	<title>About - Fault</title>
	<meta name="description" content="About Fault — a pre-commit validation tool for AI-generated code, built by Joe Abbey." />
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

	<!-- Hero -->
	<section class="py-[72px] max-[768px]:py-12">
		<div class="mx-auto max-w-[720px] px-7">
			<div
				class="mb-3 font-mono text-xs font-semibold uppercase tracking-[2px] text-primary-500"
			>
				About
			</div>
			<h1
				class="mb-6 text-[40px] font-bold leading-[1.1] tracking-[-0.5px] max-[768px]:text-[28px]"
			>
				AI agents are fast.<br />
				<span class="text-primary-500">Fault keeps them honest.</span>
			</h1>
			<p class="text-lg leading-[1.8] text-muted-foreground">
				AI coding agents write code at superhuman speed. But speed without validation means
				broken imports, hallucinated APIs, stale references, and swallowed errors silently
				entering your commit history. Fault is the safety net &mdash; a pre-commit validation
				tool that catches structural errors before they ship.
			</p>
		</div>
	</section>

	<!-- Mission -->
	<section class="py-[72px] max-[768px]:py-12 border-t border-border">
		<div class="mx-auto max-w-[720px] px-7">
			<h2
				class="mb-6 text-[28px] font-bold leading-[1.15] tracking-[-0.5px] max-[768px]:text-[22px]"
			>
				The problem
			</h2>
			<div class="space-y-5 text-[15px] leading-[1.8] text-muted-foreground">
				<p>
					Every team using AI agents for code generation has the same experience: the first
					draft looks great, but dig deeper and you find functions that call APIs that don't
					exist, imports pointing at deleted files, tests that never got updated, and error
					handling that silently swallows failures.
				</p>
				<p>
					Traditional linters analyze one file at a time. They catch syntax errors and style
					violations. But AI-generated bugs are <em>structural</em> &mdash; they span files,
					break relationships, and violate assumptions that only cross-file analysis can detect.
				</p>
				<p>
					Fault was built to close this gap. It parses your git diff, builds a cross-file
					dependency graph, and runs 16 analyzers in parallel &mdash; catching broken imports,
					stale references, hallucinated APIs, dead code, spec drift, and more. It works
					offline, supports 42 languages, and installs in 10 seconds.
				</p>
			</div>
		</div>
	</section>

	<!-- How it works -->
	<section class="py-[72px] max-[768px]:py-12 border-t border-border">
		<div class="mx-auto max-w-[720px] px-7">
			<h2
				class="mb-6 text-[28px] font-bold leading-[1.15] tracking-[-0.5px] max-[768px]:text-[22px]"
			>
				How it works
			</h2>
			<div class="space-y-5 text-[15px] leading-[1.8] text-muted-foreground">
				<p>
					Fault reads your git diff and parses every changed file into a language-specific AST.
					It builds a symbol index across your entire project &mdash; every function, class,
					import, and export &mdash; then validates that every reference in the diff resolves
					to something real.
				</p>
				<p>
					All 16 analyzers run in parallel against this index. The core engine is pure Go with
					zero external dependencies, so it starts instantly and works completely offline. No
					account, no API key, no network required.
				</p>
				<p>
					For teams that want deeper analysis, optional Pro features add LLM-powered confidence
					scoring and spec comparison &mdash; verifying not just that the code is structurally
					sound, but that it does what it was supposed to do.
				</p>
			</div>
		</div>
	</section>

	<!-- Builder -->
	<section class="py-[72px] max-[768px]:py-12 border-t border-border">
		<div class="mx-auto max-w-[720px] px-7">
			<h2
				class="mb-6 text-[28px] font-bold leading-[1.15] tracking-[-0.5px] max-[768px]:text-[22px]"
			>
				Built by Joe Abbey
			</h2>
			<div class="space-y-5 text-[15px] leading-[1.8] text-muted-foreground">
				<p>
					Fault is built and maintained by
					<a href="https://jabbey.io" class="text-primary-500 hover:underline">Joe Abbey</a>,
					a systems engineer with 20+ years building infrastructure, security tooling, and
					developer platforms. The entire stack &mdash; Go backend, SvelteKit frontend,
					PostgreSQL &mdash; runs on bare-metal infrastructure.
				</p>
				<p>
					Joe's career spans Engineering Lead at Docker (cluster lifecycle, release pipelines),
					VP of Engineering at Arxan Technologies (binary protection, LLVM, scaling a team
					from 4 to 40), HSM firmware at Anchorage Digital, and IBC testing frameworks at
					Strangelove. Before that, he operated as a Cosmos/Osmosis validator across 15+
					blockchain networks &mdash; building distributed systems that had to be reliable
					around the clock.
				</p>
				<p>
					That operational rigor &mdash; key security, zero-downtime deploys, systems that
					can't go down &mdash; carries through to Fault. When your pre-commit hook runs,
					it needs to be fast, correct, and offline-capable. No excuses.
				</p>
				<p>
					Fault is one of three products in active development:
				</p>
				<ul class="space-y-3 pl-1">
					<li class="flex items-start gap-3">
						<span
							class="mt-1.5 inline-flex items-center justify-center w-5 h-5 rounded text-[9px] font-bold tracking-tighter font-mono flex-shrink-0"
							style="background: #f43f5e; color: #07080c; letter-spacing: -1px;"
						>
							//
						</span>
						<span>
							<strong class="text-foreground">Fault</strong> &mdash; Pre-commit validation for AI-generated code
						</span>
					</li>
					<li class="flex items-start gap-3">
						<span
							class="mt-1.5 inline-flex items-center justify-center w-5 h-5 rounded text-[9px] font-bold tracking-tighter font-mono flex-shrink-0"
							style="background: #3b82f6; color: #fff; letter-spacing: -1px;"
						>
							M
						</span>
						<span>
							<strong class="text-foreground">Mantle</strong> &mdash; Opportunity discovery for indie hackers, aggregating signals from HN, Reddit, and GitHub
						</span>
					</li>
					<li class="flex items-start gap-3">
						<span
							class="mt-1.5 inline-flex items-center justify-center w-5 h-5 rounded text-[9px] font-bold tracking-tighter font-mono flex-shrink-0"
							style="background: #8b5cf6; color: #fff; letter-spacing: -1px;"
						>
							C
						</span>
						<span>
							<strong class="text-foreground">Conduit</strong> &mdash; Meeting-to-action automation, processing recordings into executable tasks
						</span>
					</li>
				</ul>
				<p>
					All three share a common foundation:
					<a href="https://github.com/joeabbey/atlas" class="text-primary-500 hover:underline">Atlas</a>
					(a 33-component Svelte 5 design system) and
					<a href="https://github.com/joeabbey/magma" class="text-primary-500 hover:underline">Magma</a>
					(a Go billing library for Stripe).
				</p>
			</div>
		</div>
	</section>

	<!-- Contact -->
	<section class="py-[72px] max-[768px]:py-12 border-t border-border">
		<div class="mx-auto max-w-[720px] px-7">
			<h2
				class="mb-6 text-[28px] font-bold leading-[1.15] tracking-[-0.5px] max-[768px]:text-[22px]"
			>
				Get in touch
			</h2>
			<div class="flex flex-wrap gap-4">
				<a
					href="mailto:joe@jabbey.io"
					class="inline-flex items-center gap-2 rounded-lg border border-border bg-card px-5 py-3 text-sm font-medium text-foreground transition-colors hover:border-primary-500/30"
				>
					<svg class="h-4 w-4 text-muted-foreground" fill="none" stroke="currentColor" viewBox="0 0 24 24">
						<path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M3 8l7.89 5.26a2 2 0 002.22 0L21 8M5 19h14a2 2 0 002-2V7a2 2 0 00-2-2H5a2 2 0 00-2 2v10a2 2 0 002 2z" />
					</svg>
					joe@jabbey.io
				</a>
				<a
					href="https://github.com/joeabbey"
					class="inline-flex items-center gap-2 rounded-lg border border-border bg-card px-5 py-3 text-sm font-medium text-foreground transition-colors hover:border-primary-500/30"
				>
					<svg class="h-4 w-4 text-muted-foreground" fill="currentColor" viewBox="0 0 24 24">
						<path d="M12 0c-6.626 0-12 5.373-12 12 0 5.302 3.438 9.8 8.207 11.387.599.111.793-.261.793-.577v-2.234c-3.338.726-4.033-1.416-4.033-1.416-.546-1.387-1.333-1.756-1.333-1.756-1.089-.745.083-.729.083-.729 1.205.084 1.839 1.237 1.839 1.237 1.07 1.834 2.807 1.304 3.492.997.107-.775.418-1.305.762-1.604-2.665-.305-5.467-1.334-5.467-5.931 0-1.311.469-2.381 1.236-3.221-.124-.303-.535-1.524.117-3.176 0 0 1.008-.322 3.301 1.23.957-.266 1.983-.399 3.003-.404 1.02.005 2.047.138 3.006.404 2.291-1.552 3.297-1.23 3.297-1.23.653 1.653.242 2.874.118 3.176.77.84 1.235 1.911 1.235 3.221 0 4.609-2.807 5.624-5.479 5.921.43.372.823 1.102.823 2.222v3.293c0 .319.192.694.801.576 4.765-1.589 8.199-6.086 8.199-11.386 0-6.627-5.373-12-12-12z"/>
					</svg>
					GitHub
				</a>
				<a
					href="https://x.com/JoeAbbey"
					class="inline-flex items-center gap-2 rounded-lg border border-border bg-card px-5 py-3 text-sm font-medium text-foreground transition-colors hover:border-primary-500/30"
				>
					<svg class="h-4 w-4 text-muted-foreground" fill="currentColor" viewBox="0 0 24 24">
						<path d="M18.244 2.25h3.308l-7.227 8.26 8.502 11.24H16.17l-5.214-6.817L4.99 21.75H1.68l7.73-8.835L1.254 2.25H8.08l4.713 6.231zm-1.161 17.52h1.833L7.084 4.126H5.117z"/>
					</svg>
					@JoeAbbey
				</a>
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
