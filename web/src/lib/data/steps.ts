export interface Step {
	title: string;
	description: string;
}

export const steps: Step[] = [
	{
		title: 'AI agent makes changes',
		description:
			'Claude Code, aider, or Cursor edits files across your project \u2014 new endpoints, refactored modules, updated types.'
	},
	{
		title: 'Pre-commit: Fault validates the diff',
		description:
			'When you commit, fault check --staged runs via the pre-commit hook. It parses every changed file, resolves imports and references, and runs 17 analyzers in parallel.'
	},
	{
		title: 'Auto-fix or commit with confidence',
		description:
			'If Fault finds errors, the commit is blocked. Run fault fix to auto-repair what it can, or fix manually. Clean checks, clean commits.'
	},
	{
		title: 'Post-merge: Audit what shipped',
		description:
			'After merging to main, fault audit --commits 1 --upload runs in CI. Results flow to the cloud dashboard with historical trends so regressions never go unnoticed.'
	}
];
