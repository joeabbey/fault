<svelte:head>
	<title>Fault - Catch AI agent mistakes before they ship</title>
	<meta name="description" content="Fault validates AI-generated code before and after it ships. Broken imports, swallowed errors, hallucinated APIs, spec drift. Pre-commit, post-merge, and CI. 42 languages, 16 analyzers, works offline." />
	<meta property="og:title" content="Fault - Catch AI agent mistakes before they ship" />
	<meta property="og:description" content="Pre-commit validation for AI-generated code. 42 languages, 16 analyzers, works completely offline." />
	<meta property="og:type" content="website" />
	<meta property="og:url" content="https://fault.jabbey.io" />
	<meta name="twitter:card" content="summary_large_image" />
	<meta name="twitter:title" content="Fault - Catch AI agent mistakes before they ship" />
	<meta name="twitter:description" content="Pre-commit validation for AI-generated code. 42 languages, 16 analyzers, works completely offline." />
</svelte:head>

<script lang="ts">
	import { Navbar, Hero, StatsBar, FeatureGrid, ComparisonTable, StepsList, PricingSection } from '@jabbey/atlas';
	import { hero } from '$lib/data/hero';
	import { terminalLines } from '$lib/data/terminal';
	import { stats } from '$lib/data/stats';
	import { problem } from '$lib/data/problem';
	import { languages } from '$lib/data/languages';
	import { analyzers } from '$lib/data/analyzers';
	import { features } from '$lib/data/features';
	import { steps } from '$lib/data/steps';
	import { comparisonColumns, comparisonRows } from '$lib/data/comparison';
	import { integrations } from '$lib/data/integrations';
	import { pricing } from '$lib/data/pricing';

	const analyzerFeatures = analyzers.map((a) => ({
		icon: a.emoji,
		title: a.name,
		description: a.description,
		badge: a.version,
		badgeVariant: 'default' as const
	}));

	const comparisonData = comparisonRows.map((row) => ({
		check: row.check,
		values: [row.linter, row.fault]
	}));

	const pricingTiers = pricing.map((tier) => ({
		id: tier.name.toLowerCase(),
		name: tier.name,
		description: tier.description,
		priceMonthly: tier.price === '$0' ? 0 : parseInt(tier.price.replace('$', ''), 10),
		features: tier.features.map((f) => ({ text: f, included: true })),
		cta: tier.cta.label,
		ctaHref: tier.cta.href,
		popular: tier.highlighted ?? false
	}));

	const navItems = [
		{ label: 'Docs', href: '/docs' },
		{ label: 'GitHub', href: 'https://github.com/joeabbey/fault' }
	];

	let copied = $state(false);

	async function copyInstall() {
		try {
			await navigator.clipboard.writeText(hero.installCommand);
			copied = true;
			setTimeout(() => (copied = false), 2000);
		} catch {
			// Clipboard API not available
		}
	}
</script>

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

	<!-- Hero -->
	<Hero
		badge={hero.badge}
		headline={hero.headline}
		highlightWord={hero.highlightWord}
		subheadline={hero.subheadline}
		installCommand={hero.installCommand}
		ctaSubtext={hero.ctaSubtext}
		terminalTitle={hero.terminalTitle}
		terminalLines={terminalLines}
	/>

	<!-- Stats Bar -->
	<StatsBar {stats} />

	<!-- Problem Section -->
	<section class="py-[72px] max-[768px]:py-12">
		<div class="mx-auto max-w-[1140px] px-7">
			<div
				class="mb-3 font-mono text-xs font-semibold uppercase tracking-[2px] text-primary-500"
			>
				{problem.label}
			</div>
			<h2
				class="mb-4 text-[34px] font-bold leading-[1.15] tracking-[-0.5px] max-[768px]:text-[26px]"
			>
				{problem.headline}
			</h2>
			<p class="mb-8 max-w-[620px] text-base leading-[1.7] text-muted-foreground">
				{problem.description}
			</p>
			<ul class="max-w-[620px] space-y-3">
				{#each problem.bullets as bullet}
					<li class="flex items-start gap-3 text-sm leading-relaxed text-muted-foreground">
						<svg class="mt-1 h-4 w-4 flex-shrink-0 text-error-400" fill="none" stroke="currentColor" viewBox="0 0 24 24">
							<path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M6 18L18 6M6 6l12 12" />
						</svg>
						{bullet}
					</li>
				{/each}
			</ul>
		</div>
	</section>

	<!-- Languages Section -->
	<section class="py-[72px] max-[768px]:py-12 border-t border-border">
		<div class="mx-auto max-w-[1140px] px-7">
			<div
				class="mb-3 font-mono text-xs font-semibold uppercase tracking-[2px] text-primary-500"
			>
				Language support
			</div>
			<h2
				class="mb-4 text-[34px] font-bold leading-[1.15] tracking-[-0.5px] max-[768px]:text-[26px]"
			>
				42 languages. One tool.
			</h2>
			<p class="mb-9 max-w-[620px] text-base leading-[1.7] text-muted-foreground">
				Full cross-file analysis for every language. Import resolution, reference tracking, and error handling detection work the same everywhere.
			</p>
			<div class="flex flex-wrap gap-2">
				{#each languages as lang}
					<span
						class="inline-flex items-center gap-1.5 rounded-full border border-border bg-card px-3 py-1.5 text-xs font-medium text-muted-foreground transition-colors hover:border-primary-500/20"
					>
						<span
							class="h-2 w-2 rounded-full"
							style="background-color: {lang.color}"
						></span>
						{lang.name}
					</span>
				{/each}
			</div>
		</div>
	</section>

	<!-- Analyzers Section -->
	<FeatureGrid
		label="Analyzers"
		headline="16 analyzers. Zero configuration."
		description="Every analyzer runs in parallel against your git diff. No config files needed -- Fault detects your project structure automatically."
		features={analyzerFeatures}
		class="border-t border-border"
	/>

	<!-- Features Section -->
	<FeatureGrid
		label="Features"
		headline="Everything you need to trust AI output"
		description="From pre-commit hooks to post-merge audits, Fault covers the full lifecycle of AI-generated code."
		features={features}
		class="border-t border-border"
	/>

	<!-- How It Works -->
	<StepsList
		label="How it works"
		headline="From AI agent to clean commit"
		steps={steps}
		class="border-t border-border"
	/>

	<!-- Comparison Table -->
	<ComparisonTable
		label="Comparison"
		headline="Beyond single-file linting"
		description="Traditional linters analyze one file at a time. Fault analyzes relationships across your entire codebase."
		columns={comparisonColumns}
		rows={comparisonData}
		class="border-t border-border"
	/>

	<!-- Integrations Section -->
	<section class="py-[72px] max-[768px]:py-12 border-t border-border">
		<div class="mx-auto max-w-[1140px] px-7">
			<div
				class="mb-3 font-mono text-xs font-semibold uppercase tracking-[2px] text-primary-500"
			>
				Integrations
			</div>
			<h2
				class="mb-4 text-[34px] font-bold leading-[1.15] tracking-[-0.5px] max-[768px]:text-[26px]"
			>
				Works with your stack
			</h2>
			<p class="mb-9 max-w-[620px] text-base leading-[1.7] text-muted-foreground">
				Fault plugs into any git-based workflow. Use it with your favorite AI coding tool, CI pipeline, or editor.
			</p>
			<div class="flex flex-wrap gap-3">
				{#each integrations as integration}
					<span
						class="inline-flex items-center rounded-lg border border-border bg-card px-5 py-3 text-sm font-medium text-foreground transition-colors hover:border-primary-500/20"
					>
						{integration}
					</span>
				{/each}
			</div>
		</div>
	</section>

	<!-- Pricing Section -->
	<PricingSection
		tiers={pricingTiers}
		headline="Simple, transparent pricing"
		subheadline="All static analyzers are free, forever. Pay only for LLM-powered features."
		class="border-t border-border"
	/>

	<!-- Final CTA -->
	<section class="relative py-[72px] max-[768px]:py-12 border-t border-border">
		<!-- Ambient glow -->
		<div
			class="pointer-events-none absolute left-1/2 top-0 h-[400px] w-[600px] -translate-x-1/2"
			style="background: radial-gradient(ellipse at center, rgba(244,63,94,0.04) 0%, transparent 70%)"
		></div>
		<div class="relative z-[1] mx-auto max-w-[1140px] px-7 text-center">
			<h2
				class="mb-4 text-[34px] font-bold leading-[1.15] tracking-[-0.5px] max-[768px]:text-[26px]"
			>
				Stop shipping broken AI code
			</h2>
			<p class="mx-auto mb-8 max-w-[480px] text-base leading-[1.7] text-muted-foreground">
				Install in 10 seconds. No account required. Works offline forever.
			</p>
			<div class="flex items-center justify-center gap-3 max-[768px]:flex-col max-[768px]:items-stretch max-[768px]:px-4">
				<div
					class="inline-flex items-center gap-3 rounded-lg border border-border bg-card px-4 py-3"
				>
					<span class="font-mono text-sm text-secondary-700 dark:text-secondary-500">$</span>
					<code class="whitespace-nowrap font-mono text-sm text-primary-400"
						>{hero.installCommand}</code
					>
				</div>
				<button
					onclick={copyInstall}
					class="cursor-pointer rounded-md border border-primary-500/15 bg-primary-500/5 px-3.5 py-1.5 font-sans text-xs font-semibold text-primary-400 transition-all hover:bg-primary-500/15"
				>
					{copied ? 'Copied!' : 'Copy'}
				</button>
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
				</div>
			</div>
		</div>
	</footer>
</div>
