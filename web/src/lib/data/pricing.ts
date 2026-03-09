export interface PricingTier {
	name: string;
	price: string;
	period?: string;
	description: string;
	features: string[];
	highlighted?: boolean;
	cta: { label: string; href: string };
}

export const pricing: PricingTier[] = [
	{
		name: 'Free',
		price: '$0',
		description: 'All static analyzers. Unlimited local use. No account required.',
		features: [
			'16 analyzers including spec validation, concurrency, resource leaks',
			'42 languages with full cross-file analysis',
			'30+ manifest formats for dependency validation',
			'OWASP Top 10 & CWE Top 25 compliance packs',
			'Custom analyzer rules via .fault.yaml',
			'Terminal, JSON, SARIF output',
			'Auto-fix engine (fault fix)',
			'Watch mode (fault watch)',
			'Pre-commit hook & GitHub PR comments',
			'Works offline \u2014 forever'
		],
		cta: { label: 'Install free', href: '#install' }
	},
	{
		name: 'Pro',
		price: '$15',
		period: '/mo',
		description: 'LLM-powered analysis and compliance reporting.',
		features: [
			'Everything in Free',
			'Confidence scoring per file',
			'Structured spec comparison (per-requirement LLM analysis)',
			'Post-merge audit with fault audit --upload',
			'Historical trend dashboard with issues-over-time charts',
			'Spec compliance dashboard with coverage tracking',
			'CWE-mapped SARIF output for security compliance',
			'Priority support'
		],
		highlighted: true,
		cta: { label: 'Start free trial', href: '/login' }
	},
	{
		name: 'Team',
		price: '$30',
		period: '/user/mo',
		description: 'Shared rules, compliance, and visibility across your team.',
		features: [
			'Everything in Pro',
			'Organization-wide shared baselines & config (fault config pull)',
			'Organization compliance tracking & reports',
			'Webhook notifications (Slack, Discord, HTTP)',
			'Team dashboard with change audit trail',
			'Member management & audit logs',
			'SSO integration (OIDC)'
		],
		cta: { label: 'Contact us', href: 'mailto:joe@jabbey.io' }
	}
];
