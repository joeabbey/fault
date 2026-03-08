export interface DocEntry {
	slug: string;
	title: string;
	description: string;
}

export const docs: DocEntry[] = [
	{
		slug: 'getting-started',
		title: 'Getting Started',
		description: 'Install and configure Fault'
	},
	{
		slug: 'claude-code-integration',
		title: 'Claude Code Integration',
		description: 'Use Fault with Claude Code hooks'
	},
	{
		slug: 'aider-integration',
		title: 'aider Integration',
		description: 'Use Fault with aider --lint-cmd'
	},
	{
		slug: 'github-actions',
		title: 'GitHub Actions',
		description: 'SARIF output for GitHub Code Scanning'
	},
	{
		slug: 'languages',
		title: 'Language Support',
		description: 'Supported languages and parser coverage'
	}
];
