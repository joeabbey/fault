<script lang="ts">
	import { Navbar } from '@jabbey/atlas';
	import ThemeToggle from '$lib/components/ThemeToggle.svelte';
	import { theme } from '$lib/stores/theme';

	const lastUpdated = 'March 8, 2026';

	const navItems = [
		{ label: 'Docs', href: '/docs' },
		{ label: 'GitHub', href: 'https://github.com/joeabbey/fault' }
	];
</script>

<svelte:head>
	<title>Terms of Use - Fault</title>
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

	<div class="mx-auto max-w-3xl px-6 py-16">
		<h1 class="text-3xl font-bold font-display text-foreground" style="letter-spacing: -0.5px;">
			Terms of Use
		</h1>
		<p class="text-sm text-muted mt-2 font-mono">Last updated: {lastUpdated}</p>

		<div class="mt-10 space-y-8 text-[15px] leading-relaxed text-secondary-300">
			<section>
				<h2 class="text-lg font-semibold font-display text-foreground mb-3">1. Acceptance of Terms</h2>
				<p>
					By accessing or using Fault ("the Service"), you agree to be bound by these Terms of Use.
					If you do not agree to these terms, please do not use the Service.
				</p>
			</section>

			<section>
				<h2 class="text-lg font-semibold font-display text-foreground mb-3">2. Description of Service</h2>
				<p>Fault is a pre-commit validation tool for AI-generated code that:</p>
				<ul class="list-disc pl-6 mt-2 space-y-1.5">
					<li>Runs static analysis to catch broken imports, stale references, hallucinated APIs, and other structural errors</li>
					<li>Supports 42 programming languages with 16 analyzers</li>
					<li>Works as a local CLI tool, git pre-commit hook, or CI step</li>
					<li>Optionally provides LLM-powered analysis via the cloud API (Pro and Team plans)</li>
				</ul>
			</section>

			<section>
				<h2 class="text-lg font-semibold font-display text-foreground mb-3">3. User Accounts</h2>
				<p>The Free tier does not require an account. For Pro and Team features, you must create an account via the CLI (<code class="text-[#f43f5e]">fault signup</code>) or Google OAuth on the web dashboard. You are responsible for:</p>
				<ul class="list-disc pl-6 mt-2 space-y-1.5">
					<li>Maintaining the security of your API key and account credentials</li>
					<li>All activities that occur under your account</li>
					<li>Notifying us immediately of any unauthorized use</li>
				</ul>
			</section>

			<section>
				<h2 class="text-lg font-semibold font-display text-foreground mb-3">4. Acceptable Use</h2>
				<p>You agree not to:</p>
				<ul class="list-disc pl-6 mt-2 space-y-1.5">
					<li>Use the Service for any unlawful purpose or in violation of any laws</li>
					<li>Attempt to gain unauthorized access to the Service or its systems</li>
					<li>Reverse engineer, decompile, or disassemble any part of the cloud Service</li>
					<li>Use the cloud API in a way that could damage, disable, or impair its functionality</li>
					<li>Share or redistribute your API key</li>
					<li>Exceed your plan's usage limits through circumvention</li>
				</ul>
				<p class="mt-3">The open-source CLI tool is distributed under the MIT License and governed by its own terms.</p>
			</section>

			<section>
				<h2 class="text-lg font-semibold font-display text-foreground mb-3">5. AI-Powered Analysis</h2>
				<p>Pro and Team plans use AI (Anthropic Claude) to analyze code. You acknowledge that:</p>
				<ul class="list-disc pl-6 mt-2 space-y-1.5">
					<li>AI analysis results (confidence scores, spec comparisons) may not be 100% accurate and should be reviewed</li>
					<li>Code diffs you submit are sent to the Anthropic API for processing</li>
					<li>Code submitted via the API is subject to Anthropic's <a href="https://www.anthropic.com/policies/usage-policy" class="text-[#f43f5e] hover:underline">commercial API usage policy</a>, which prohibits using API inputs for model training</li>
					<li>You are responsible for ensuring you have the right to submit the code you analyze</li>
				</ul>
			</section>

			<section>
				<h2 class="text-lg font-semibold font-display text-foreground mb-3">6. Payment Terms</h2>
				<p>
					Fault offers Free, Pro ($15/mo), and Team ($30/user/mo) plans. Paid subscriptions are billed monthly via Stripe.
					Fees are non-refundable except as required by law. You may cancel at any time through the billing portal;
					cancellation takes effect at the end of the current billing period.
				</p>
				<p class="mt-3">
					We may change pricing with 30 days' notice. Continued use after the change constitutes acceptance.
				</p>
			</section>

			<section>
				<h2 class="text-lg font-semibold font-display text-foreground mb-3">7. Intellectual Property</h2>
				<p>
					The Service and its original content, features, and functionality are owned by Fault and
					protected by international copyright, trademark, and other intellectual property laws.
					Your code and analysis results remain your property.
				</p>
			</section>

			<section>
				<h2 class="text-lg font-semibold font-display text-foreground mb-3">8. Disclaimer of Warranties</h2>
				<p class="uppercase text-xs tracking-wide leading-relaxed">
					The Service is provided "as is" and "as available" without warranties of any kind, either
					express or implied, including but not limited to implied warranties of merchantability,
					fitness for a particular purpose, and non-infringement. We do not warrant that the Service
					will be uninterrupted, secure, or error-free, or that analysis results will be accurate or complete.
				</p>
			</section>

			<section>
				<h2 class="text-lg font-semibold font-display text-foreground mb-3">9. Limitation of Liability</h2>
				<p class="uppercase text-xs tracking-wide leading-relaxed">
					In no event shall Fault, its directors, employees, or agents be liable for any indirect,
					incidental, special, consequential, or punitive damages, including loss of profits, data,
					or other intangible losses, resulting from your use of the Service. Our total liability
					shall not exceed the fees paid by you in the 12 months preceding the claim.
				</p>
			</section>

			<section>
				<h2 class="text-lg font-semibold font-display text-foreground mb-3">10. Termination</h2>
				<p>
					We may terminate or suspend your access to the Service immediately, without prior notice,
					for any reason, including breach of these Terms. Upon termination, your right to use the
					cloud Service will cease immediately. The CLI tool may continue to be used under the MIT License.
				</p>
			</section>

			<section>
				<h2 class="text-lg font-semibold font-display text-foreground mb-3">11. Changes to Terms</h2>
				<p>
					We reserve the right to modify these Terms at any time. We will provide notice of significant
					changes by posting the new Terms on this page. Your continued use of the Service after such
					changes constitutes acceptance of the new Terms.
				</p>
			</section>

			<section>
				<h2 class="text-lg font-semibold font-display text-foreground mb-3">12. Governing Law</h2>
				<p>
					These Terms shall be governed by and construed in accordance with the laws of the United States,
					without regard to its conflict of law provisions.
				</p>
			</section>

			<section>
				<h2 class="text-lg font-semibold font-display text-foreground mb-3">13. Contact Us</h2>
				<p>
					If you have any questions about these Terms, please contact us at
					<a href="mailto:legal@jabbey.io" class="text-[#f43f5e] hover:underline">legal@jabbey.io</a>.
				</p>
			</section>
		</div>
	</div>

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
