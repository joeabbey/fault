export interface TerminalLine {
	text: string;
	type: 'command' | 'info' | 'error' | 'warning' | 'success' | 'summary';
	delay: number;
}

export const terminalLines: TerminalLine[] = [
	{ text: '$ fault check --staged', type: 'command', delay: 0 },
	{ text: '', type: 'info', delay: 500 },
	{ text: '  scanning 47 files across 3 languages...', type: 'info', delay: 700 },
	{ text: '', type: 'info', delay: 1400 },
	{ text: '  \u2717 phantom-import  api/routes.ts:3', type: 'error', delay: 1600 },
	{
		text: '    imports "fastify-cors" \u2014 not in package.json',
		type: 'info',
		delay: 1800
	},
	{ text: '', type: 'info', delay: 2400 },
	{ text: '  \u2717 swallowed-error  pkg/db.go:42', type: 'error', delay: 2600 },
	{
		text: '    error from db.Query() assigned to blank identifier',
		type: 'info',
		delay: 2800
	},
	{ text: '', type: 'info', delay: 3400 },
	{ text: '  ! missing-test  src/auth.py:15', type: 'warning', delay: 3600 },
	{
		text: '    verify_token() modified but tests not updated',
		type: 'info',
		delay: 3800
	},
	{ text: '', type: 'info', delay: 4400 },
	{ text: '  \u2713 44 files passed', type: 'success', delay: 4600 },
	{ text: '', type: 'info', delay: 5000 },
	{ text: '  2 errors, 1 warning \u2014 commit blocked.', type: 'summary', delay: 5200 }
];
