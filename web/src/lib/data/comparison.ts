export interface ComparisonRow {
	check: string;
	linter: string;
	fault: string;
}

export const comparisonColumns = ['Check', 'Single-file linter', 'Fault'];

export const comparisonRows: ComparisonRow[] = [
	{ check: 'Broken imports across files', linter: 'No', fault: 'Yes' },
	{ check: 'Phantom packages not in manifest', linter: 'No', fault: 'Yes (32 formats)' },
	{ check: 'Signature changes with stale callers', linter: 'No', fault: 'Yes' },
	{ check: 'Swallowed errors & missing catch', linter: 'Partial', fault: 'Yes (42 languages)' },
	{ check: 'Dead exported code', linter: 'No', fault: 'Yes' },
	{ check: 'Circular dependencies', linter: 'No', fault: 'Yes' },
	{ check: 'Missing test updates', linter: 'No', fault: 'Yes' },
	{ check: 'Agent artifacts (TODOs, debug logs)', linter: 'Partial', fault: 'Yes + auto-fix' },
	{ check: 'Hardcoded secrets & insecure crypto', linter: 'Partial', fault: 'Yes + auto-fix' },
	{ check: 'Post-merge regression detection', linter: 'No', fault: 'Yes (fault audit)' },
	{ check: 'Requirements/spec coverage tracking', linter: 'No', fault: 'Yes (.fault-spec.yaml)' },
	{ check: 'OWASP/CWE compliance reporting', linter: 'No', fault: 'Yes (compliance packs)' },
	{ check: 'Custom team-defined rules', linter: 'Plugin system', fault: 'Yes (YAML regex rules)' }
];
