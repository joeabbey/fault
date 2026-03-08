export interface TerminalLine {
	text: string;
	type: 'command' | 'info' | 'error' | 'warning' | 'success' | 'summary';
	delay: number;
}

export const terminalLines: TerminalLine[] = [
	{ text: '$ fault check --staged', type: 'command', delay: 0 },
	{ text: '', type: 'info', delay: 0.5 },
	{ text: '  scanning 47 files across 3 languages...', type: 'info', delay: 0.7 },
	{ text: '', type: 'info', delay: 1.4 },
	{ text: '  \u2717 phantom-import  api/routes.ts:3', type: 'error', delay: 1.6 },
	{
		text: '    imports "fastify-cors" \u2014 not in package.json',
		type: 'info',
		delay: 1.8
	},
	{ text: '', type: 'info', delay: 2.4 },
	{ text: '  \u2717 swallowed-error  pkg/db.go:42', type: 'error', delay: 2.6 },
	{
		text: '    error from db.Query() assigned to blank identifier',
		type: 'info',
		delay: 2.8
	},
	{ text: '', type: 'info', delay: 3.4 },
	{ text: '  ! missing-test  src/auth.py:15', type: 'warning', delay: 3.6 },
	{
		text: '    verify_token() modified but tests not updated',
		type: 'info',
		delay: 3.8
	},
	{ text: '', type: 'info', delay: 4.4 },
	{ text: '  \u2713 44 files passed', type: 'success', delay: 4.6 },
	{ text: '', type: 'info', delay: 5.0 },
	{ text: '  2 errors, 1 warning \u2014 commit blocked.', type: 'summary', delay: 5.2 }
];
