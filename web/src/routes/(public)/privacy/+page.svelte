<script lang="ts">
	import { theme } from '$lib/stores/theme';

	const lastUpdated = 'March 8, 2026';
</script>

<svelte:head>
	<title>Privacy Policy - Fault</title>
	<meta name="theme-color" content={$theme === 'dark' ? '#07080c' : '#faf8f7'} />
</svelte:head>

<div class:dark={$theme === 'dark'} class="min-h-screen bg-background text-foreground">
	<div class="mx-auto max-w-3xl px-6 py-16">
		<a
			href="/"
			class="inline-flex items-center gap-2 text-sm text-muted hover:text-foreground transition-colors mb-10"
		>
			<svg class="h-4 w-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
				<path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M15 19l-7-7 7-7" />
			</svg>
			Back to Fault
		</a>

		<h1 class="text-3xl font-bold font-display text-foreground" style="letter-spacing: -0.5px;">
			Privacy Policy
		</h1>
		<p class="text-sm text-muted mt-2 font-mono">Last updated: {lastUpdated}</p>

		<div class="mt-10 space-y-8 text-[15px] leading-relaxed text-secondary-300">
			<section>
				<h2 class="text-lg font-semibold font-display text-foreground mb-3">1. Introduction</h2>
				<p>
					Fault ("we", "our", or "us") is a pre-commit validation tool for AI-generated code.
					This Privacy Policy explains how we collect, use, and protect your information when you
					use our CLI tool and cloud service.
				</p>
				<p class="mt-3">
					<strong class="text-foreground">The CLI tool collects no data.</strong> It runs entirely on your
					machine and never contacts our servers unless you explicitly use Pro features with an API key.
					This policy covers data collected by the optional cloud service.
				</p>
			</section>

			<section>
				<h2 class="text-lg font-semibold font-display text-foreground mb-3">2. Information We Collect</h2>
				<p>We collect the following types of information:</p>
				<ul class="list-disc pl-6 mt-2 space-y-1.5">
					<li><strong class="text-foreground">Account Information:</strong> When you sign up, we collect your email address. If you sign in with Google, we also receive your name and profile picture.</li>
					<li><strong class="text-foreground">Code Diffs:</strong> When you use Pro features (confidence scoring, spec comparison), your code diffs are sent to our API for processing. These are forwarded to the Anthropic Claude API and are not stored after analysis.</li>
					<li><strong class="text-foreground">Usage Data:</strong> We track API call counts, token usage, and analysis counts per billing period.</li>
					<li><strong class="text-foreground">Audit Run Data:</strong> When you use <code class="text-[#f43f5e]">fault audit --upload</code>, we store repository URL, branch name, commit SHA, issue summaries, confidence scores, and analysis metadata on your dashboard.</li>
					<li><strong class="text-foreground">Payment Information:</strong> Billing details are processed and stored by Stripe. We do not store your credit card information.</li>
				</ul>
			</section>

			<section>
				<h2 class="text-lg font-semibold font-display text-foreground mb-3">3. How We Use Your Information</h2>
				<p>We use your information to:</p>
				<ul class="list-disc pl-6 mt-2 space-y-1.5">
					<li>Provide the cloud analysis service (confidence scoring, spec comparison)</li>
					<li>Enforce plan limits and track usage for billing</li>
					<li>Display analysis history and trends on your dashboard</li>
					<li>Provide customer support and respond to inquiries</li>
					<li>Ensure the security and integrity of our service</li>
				</ul>
				<p class="mt-3">We do <strong class="text-foreground">not</strong>:</p>
				<ul class="list-disc pl-6 mt-2 space-y-1.5">
					<li>Sell your personal information</li>
					<li>Use your code to train AI models — Anthropic's <a href="https://www.anthropic.com/policies/usage-policy" class="text-[#f43f5e] hover:underline">commercial API usage policy</a> prohibits using API inputs for model training</li>
					<li>Share your data with third parties for marketing purposes</li>
					<li>Store code diffs after analysis is complete (spec results are stored as structured summaries only)</li>
				</ul>
			</section>

			<section>
				<h2 class="text-lg font-semibold font-display text-foreground mb-3">4. Data Storage and Security</h2>
				<p>We take the security of your data seriously:</p>
				<ul class="list-disc pl-6 mt-2 space-y-1.5">
					<li>API keys are hashed (SHA-256) before storage; the raw key is shown only once at creation</li>
					<li>All data is transmitted over HTTPS/TLS</li>
					<li>Database is hosted on a private network with restricted access</li>
					<li>Code diffs are processed in memory and not persisted to disk</li>
				</ul>
			</section>

			<section>
				<h2 class="text-lg font-semibold font-display text-foreground mb-3">5. Third-Party Services</h2>
				<p>We use the following third-party services:</p>
				<ul class="list-disc pl-6 mt-2 space-y-1.5">
					<li><strong class="text-foreground">Anthropic (Claude API):</strong> Processes code diffs for AI-powered analysis. Subject to <a href="https://www.anthropic.com/privacy" class="text-[#f43f5e] hover:underline">Anthropic's Privacy Policy</a>.</li>
					<li><strong class="text-foreground">Stripe:</strong> Processes payments. Subject to <a href="https://stripe.com/privacy" class="text-[#f43f5e] hover:underline">Stripe's Privacy Policy</a>.</li>
					<li><strong class="text-foreground">Google:</strong> Provides OAuth authentication. Subject to <a href="https://policies.google.com/privacy" class="text-[#f43f5e] hover:underline">Google's Privacy Policy</a>.</li>
					<li><strong class="text-foreground">GitHub:</strong> When using <code class="text-[#f43f5e]">--format github</code> in CI, Fault posts PR review comments via the GitHub API using your CI-provided token. Subject to <a href="https://docs.github.com/en/site-policy/privacy-policies/github-general-privacy-statement" class="text-[#f43f5e] hover:underline">GitHub's Privacy Statement</a>.</li>
				</ul>
				<p class="mt-3">Team plan organizations may configure custom OIDC identity providers for SSO. When configured, authentication requests are sent to the organization's chosen identity provider.</p>
			</section>

			<section>
				<h2 class="text-lg font-semibold font-display text-foreground mb-3">6. Your Rights</h2>
				<p>You have the right to:</p>
				<ul class="list-disc pl-6 mt-2 space-y-1.5">
					<li>Access your personal data stored in Fault</li>
					<li>Request deletion of your account and associated data</li>
					<li>Rotate or revoke your API key at any time</li>
					<li>Export your usage and analysis data</li>
					<li>Opt out of the cloud service entirely and use only the free CLI</li>
				</ul>
				<p class="mt-3">
					EU/EEA users have additional rights under GDPR, including the right to restrict processing,
					object to processing, and lodge a complaint with a supervisory authority.
					California users have rights under CCPA, including the right to know what data we collect and
					request its deletion. We do not sell personal information.
				</p>
			</section>

			<section>
				<h2 class="text-lg font-semibold font-display text-foreground mb-3">7. Data Retention</h2>
				<p>
					We retain your data for as long as your account is active or as needed to provide you with our
					services. After account deletion, account data is removed within 30 days and backups are purged
					within 90 days. Anonymized usage statistics may be retained indefinitely.
				</p>
			</section>

			<section>
				<h2 class="text-lg font-semibold font-display text-foreground mb-3">8. Children's Privacy</h2>
				<p>
					The Service is not intended for children under 16. We do not knowingly collect data from children.
					If you believe a child has provided us data, contact us to have it removed.
				</p>
			</section>

			<section>
				<h2 class="text-lg font-semibold font-display text-foreground mb-3">9. Changes to This Policy</h2>
				<p>
					We may update this Privacy Policy from time to time. We will notify you of any changes by
					posting the new Privacy Policy on this page and updating the "Last updated" date.
				</p>
			</section>

			<section>
				<h2 class="text-lg font-semibold font-display text-foreground mb-3">10. Contact Us</h2>
				<p>
					If you have any questions about this Privacy Policy, please contact us at
					<a href="mailto:privacy@jabbey.io" class="text-[#f43f5e] hover:underline">privacy@jabbey.io</a>.
				</p>
			</section>
		</div>
	</div>
</div>
