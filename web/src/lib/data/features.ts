export interface Feature {
	title: string;
	description: string;
	badge?: string;
	badgeVariant?: 'default' | 'warning' | 'success';
}

export const features: Feature[] = [
	{
		title: 'Works completely offline',
		description:
			'All 16 analyzers run locally against your git diff. No API calls, no network access, no data leaves your machine. Works on planes and in air-gapped environments.'
	},
	{
		title: 'Manifest-aware validation',
		description:
			'Validates imports against your actual project dependencies \u2014 package.json, go.mod, Cargo.toml, pom.xml, build.gradle, .csproj, composer.json, Gemfile, Package.swift, and 21 more.'
	},
	{
		title: 'Git native',
		description:
			'Installs as a pre-commit hook with fault hook install. Reads staged, unstaged, or branch diffs directly from git. Outputs terminal, JSON, or SARIF.'
	},
	{
		title: 'Auto-fix engine',
		description:
			'Run fault fix to automatically repair broken imports, replace hardcoded secrets with env vars, and remove debug statements.'
	},
	{
		title: 'Watch mode',
		description:
			'Run fault watch for continuous validation as you edit. Detects file changes in real-time and re-runs analysis within milliseconds.'
	},
	{
		title: 'GitHub PR review',
		description:
			'Post inline review comments directly on pull requests via GitHub Actions. Findings appear right in the diff where developers already review code.'
	},
	{
		title: 'Error handling detection',
		description:
			'Catches discarded Go errors, unhandled TS promises, bare Python except:, empty Java catch blocks, and Rust .unwrap() outside tests.',
		badge: 'New in v4',
		badgeVariant: 'default'
	},
	{
		title: 'Dead code & dep graph',
		description:
			'Detects exported symbols nothing imports, circular dependencies between modules, and unused entries in your manifest files.',
		badge: 'New in v4',
		badgeVariant: 'default'
	},
	{
		title: 'Post-merge audit',
		description:
			'Run fault audit --commits 5 to scan code that already shipped. Catches regressions after merge. Upload results to the cloud dashboard with trend analytics (Pro).',
		badge: 'New in v6',
		badgeVariant: 'default'
	},
	{
		title: 'Spec validation',
		description:
			'Define requirements in .fault-spec.yaml, anchor them in code with // spec:REQ-001. The spec analyzer tracks coverage across all 42 languages.',
		badge: 'New in v6',
		badgeVariant: 'default'
	},
	{
		title: 'VS Code extension',
		description:
			'See Fault findings inline as you code. The extension runs analysis on save and shows diagnostics, quick-fixes, and code actions in the editor.',
		badge: 'Beta',
		badgeVariant: 'default'
	},
	{
		title: 'AI-enhanced validation',
		description:
			'LLM-powered confidence scoring rates how likely each changed file is to be correct. Structured spec comparison analyzes each requirement individually with evidence and confidence scores.',
		badge: 'Pro',
		badgeVariant: 'warning'
	},
	{
		title: 'Compliance packs',
		description:
			'Enable owasp-top-10-2021 or cwe-top-25-2023 in your config. Fault maps findings to CWE IDs and generates a compliance summary with pass/fail per weakness.',
		badge: 'New in v7',
		badgeVariant: 'default'
	},
	{
		title: 'Custom analyzer rules',
		description:
			'Define regex-based rules in .fault.yaml with file glob filters and severity levels. Enforce team conventions without writing a plugin.',
		badge: 'New in v7',
		badgeVariant: 'default'
	},
	{
		title: 'Team organizations',
		description:
			'Create organizations with shared baselines, aggregated run history, member management, and audit trails. Pull team config with fault config pull to keep everyone aligned.',
		badge: 'New in v7',
		badgeVariant: 'default'
	},
	{
		title: 'SSO & webhooks',
		description:
			'Connect your identity provider via OIDC for single sign-on. Set up webhooks to get notified in Slack, Discord, or any HTTP endpoint when runs complete or specs drift.',
		badge: 'New in v7',
		badgeVariant: 'default'
	}
];
