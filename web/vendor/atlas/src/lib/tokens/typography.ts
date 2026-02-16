/**
 * Atlas Design System - Typography Tokens
 */

export const fontFamily = {
	sans: 'ui-sans-serif, system-ui, sans-serif, "Apple Color Emoji", "Segoe UI Emoji", "Segoe UI Symbol", "Noto Color Emoji"',
	mono: 'ui-monospace, SFMono-Regular, "SF Mono", Menlo, Consolas, "Liberation Mono", monospace'
} as const;

export const fontSize = {
	xs: ['0.75rem', { lineHeight: '1rem' }],
	sm: ['0.875rem', { lineHeight: '1.25rem' }],
	base: ['1rem', { lineHeight: '1.5rem' }],
	lg: ['1.125rem', { lineHeight: '1.75rem' }],
	xl: ['1.25rem', { lineHeight: '1.75rem' }],
	'2xl': ['1.5rem', { lineHeight: '2rem' }],
	'3xl': ['1.875rem', { lineHeight: '2.25rem' }],
	'4xl': ['2.25rem', { lineHeight: '2.5rem' }],
	'5xl': ['3rem', { lineHeight: '1' }]
} as const;

export const fontWeight = {
	thin: '100',
	extralight: '200',
	light: '300',
	normal: '400',
	medium: '500',
	semibold: '600',
	bold: '700',
	extrabold: '800',
	black: '900'
} as const;

export const lineHeight = {
	none: '1',
	tight: '1.25',
	snug: '1.375',
	normal: '1.5',
	relaxed: '1.625',
	loose: '2'
} as const;

export const letterSpacing = {
	tighter: '-0.05em',
	tight: '-0.025em',
	normal: '0',
	wide: '0.025em',
	wider: '0.05em',
	widest: '0.1em'
} as const;

export type FontSizeKey = keyof typeof fontSize;
export type FontWeightKey = keyof typeof fontWeight;
