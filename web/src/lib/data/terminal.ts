export interface TerminalLine {
	text: string;
	type: 'command' | 'info' | 'error' | 'warning' | 'success' | 'summary';
	delay: number;
}

export const terminalLines: TerminalLine[] = [
	{ text: '$ fault check --staged', type: 'command', delay: 0 },
	{ text: '', type: 'info', delay: 0.6 },
	{
		text: '  <span class="text-error-400 font-semibold">error</span>  <span class="text-secondary-600">[imports]</span> api/routes.ts:3',
		type: 'info',
		delay: 0.9
	},
	{
		text: '  Import "fastify-cors" could not be resolved',
		type: 'info',
		delay: 1.1
	},
	{ text: '', type: 'info', delay: 1.6 },
	{
		text: '    <span class="text-secondary-700">3 |</span>  import cors from "fastify-cors"   <span class="text-primary-400 font-semibold">&lt;-- here</span>',
		type: 'info',
		delay: 1.8
	},
	{ text: '', type: 'info', delay: 2.3 },
	{
		text: '  <span class="text-error-400 font-semibold">error</span>  <span class="text-secondary-600">[error-handling]</span> pkg/db.go:42',
		type: 'info',
		delay: 2.5
	},
	{
		text: '  Error from db.Query() assigned to blank identifier',
		type: 'info',
		delay: 2.7
	},
	{ text: '', type: 'info', delay: 3.2 },
	{
		text: '  <span class="text-warning-400 font-semibold">warning</span>  <span class="text-secondary-600">[tests]</span> src/auth.py:15',
		type: 'info',
		delay: 3.4
	},
	{
		text: '  verify_token() modified but tests not updated',
		type: 'info',
		delay: 3.6
	},
	{ text: '', type: 'info', delay: 4.1 },
	{
		text: '  3 issues (<span class="text-error-400">2 errors</span>, <span class="text-warning-400">1 warning</span>) in 3 files \u2014 89ms',
		type: 'summary',
		delay: 4.3
	}
];
