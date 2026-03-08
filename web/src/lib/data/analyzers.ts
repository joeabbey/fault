export interface Analyzer {
	emoji: string;
	name: string;
	description: string;
	version?: string;
}

export const analyzers: Analyzer[] = [
	{ emoji: '\u{1F517}', name: 'imports', description: 'Broken imports, missing modules' },
	{ emoji: '\u{1F50D}', name: 'references', description: 'Stale callers, renamed symbols' },
	{ emoji: '\u{1F4CB}', name: 'consistency', description: 'Signature mismatches across files' },
	{ emoji: '\u{1F9EA}', name: 'tests', description: 'Missing test updates for changed code' },
	{ emoji: '\u{26A0}', name: 'patterns', description: 'Debug logs, TODO placeholders' },
	{ emoji: '\u{1F512}', name: 'security', description: 'Secrets, injection, insecure crypto' },
	{ emoji: '\u{1F47B}', name: 'hallucination', description: 'Phantom packages, invented APIs' },
	{ emoji: '\u{1F6A8}', name: 'errorhandling', description: 'Swallowed errors, missing catch blocks', version: 'v4' },
	{ emoji: '\u{1FAA6}', name: 'deadcode', description: 'Exported symbols nothing imports', version: 'v4' },
	{ emoji: '\u{1F504}', name: 'depgraph', description: 'Circular deps, unused manifest entries', version: 'v4' },
	{ emoji: '\u{1F3CE}', name: 'concurrency', description: 'Race conditions, goroutine leaks, missing locks', version: 'v5' },
	{ emoji: '\u{1F6B0}', name: 'resource', description: 'Unclosed files, connections, missing defer', version: 'v5' },
	{ emoji: '\u{1F4BE}', name: 'migration', description: 'Destructive DB ops: DROP TABLE, column removal', version: 'v5' },
	{ emoji: '\u{1F4CF}', name: 'complexity', description: 'Deep nesting, oversized functions, high branching', version: 'v5' },
	{ emoji: '\u{1F4DD}', name: 'docdrift', description: 'Stale comments after signature changes', version: 'v5' },
	{ emoji: '\u{1F4CB}', name: 'spec', description: 'Orphaned anchors, unanchored requirements, spec drift', version: 'v6' },
	{ emoji: '\u{1F527}', name: 'custom', description: 'Your own regex rules via .fault.yaml', version: 'v7' }
];
