export const problem = {
	label: 'The problem',
	headline: 'AI agents are fast. But they make structural mistakes.',
	description:
		'AI coding agents scaffold features across dozens of files in seconds. They also introduce failures that slip through code review:',
	bullets: [
		"An import references a package that doesn't exist in your manifest",
		'A function signature changed but not all callers were updated',
		'Error handling was silently swallowed \u2014 _ = err in Go, empty catch {} in Java',
		'Exported code is now dead \u2014 nothing imports it anymore',
		'Circular dependencies were introduced between modules',
		'TODO placeholders and debug statements were left behind'
	]
};
