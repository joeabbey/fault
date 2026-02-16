/**
 * Atlas Design System - Color Tokens
 *
 * Colors are defined in OKLCH for better perceptual uniformity.
 * These match the CSS custom properties defined in base.css
 */

export const colors = {
	primary: {
		50: 'oklch(0.97 0.01 250)',
		100: 'oklch(0.93 0.03 250)',
		200: 'oklch(0.86 0.06 250)',
		300: 'oklch(0.76 0.10 250)',
		400: 'oklch(0.65 0.15 250)',
		500: 'oklch(0.55 0.20 250)',
		600: 'oklch(0.48 0.22 250)',
		700: 'oklch(0.42 0.19 250)',
		800: 'oklch(0.36 0.15 250)',
		900: 'oklch(0.30 0.12 250)',
		950: 'oklch(0.22 0.08 250)',
		DEFAULT: 'oklch(0.55 0.20 250)'
	},
	secondary: {
		50: 'oklch(0.98 0.00 260)',
		100: 'oklch(0.96 0.01 260)',
		200: 'oklch(0.91 0.01 260)',
		300: 'oklch(0.85 0.02 260)',
		400: 'oklch(0.70 0.02 260)',
		500: 'oklch(0.55 0.02 260)',
		600: 'oklch(0.45 0.02 260)',
		700: 'oklch(0.37 0.02 260)',
		800: 'oklch(0.28 0.02 260)',
		900: 'oklch(0.21 0.02 260)',
		950: 'oklch(0.13 0.02 260)',
		DEFAULT: 'oklch(0.55 0.02 260)'
	},
	success: {
		50: 'oklch(0.97 0.02 145)',
		100: 'oklch(0.93 0.05 145)',
		200: 'oklch(0.87 0.10 145)',
		300: 'oklch(0.78 0.15 145)',
		400: 'oklch(0.68 0.18 145)',
		500: 'oklch(0.58 0.18 145)',
		600: 'oklch(0.50 0.16 145)',
		700: 'oklch(0.43 0.14 145)',
		800: 'oklch(0.37 0.11 145)',
		900: 'oklch(0.31 0.09 145)',
		950: 'oklch(0.20 0.06 145)',
		DEFAULT: 'oklch(0.58 0.18 145)'
	},
	warning: {
		50: 'oklch(0.98 0.03 85)',
		100: 'oklch(0.95 0.07 85)',
		200: 'oklch(0.90 0.13 85)',
		300: 'oklch(0.84 0.17 85)',
		400: 'oklch(0.78 0.18 85)',
		500: 'oklch(0.72 0.17 80)',
		600: 'oklch(0.62 0.16 55)',
		700: 'oklch(0.52 0.14 50)',
		800: 'oklch(0.44 0.11 50)',
		900: 'oklch(0.38 0.09 50)',
		950: 'oklch(0.25 0.06 50)',
		DEFAULT: 'oklch(0.72 0.17 80)'
	},
	error: {
		50: 'oklch(0.97 0.02 25)',
		100: 'oklch(0.94 0.05 25)',
		200: 'oklch(0.88 0.10 25)',
		300: 'oklch(0.79 0.15 25)',
		400: 'oklch(0.68 0.19 25)',
		500: 'oklch(0.58 0.22 25)',
		600: 'oklch(0.52 0.22 25)',
		700: 'oklch(0.45 0.19 25)',
		800: 'oklch(0.39 0.15 25)',
		900: 'oklch(0.33 0.12 25)',
		950: 'oklch(0.22 0.08 25)',
		DEFAULT: 'oklch(0.58 0.22 25)'
	}
} as const;

export type ColorScale = keyof typeof colors;
export type ColorShade = keyof (typeof colors)['primary'];

/**
 * Chart colors for Stratum integration
 */
export const chartColors = {
	light: {
		background: '#ffffff',
		foreground: '#0f172a',
		grid: '#e2e8f0',
		axis: '#64748b',
		series: ['#3b82f6', '#22c55e', '#f59e0b', '#ef4444', '#8b5cf6', '#06b6d4']
	},
	dark: {
		background: '#0f172a',
		foreground: '#f1f5f9',
		grid: '#334155',
		axis: '#94a3b8',
		series: ['#60a5fa', '#4ade80', '#fbbf24', '#f87171', '#a78bfa', '#22d3ee']
	}
} as const;
