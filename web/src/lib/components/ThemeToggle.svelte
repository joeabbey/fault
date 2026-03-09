<script lang="ts">
	import { theme } from '$lib/stores/theme';

	const isDark = $derived($theme === 'dark');
</script>

<button
	onclick={() => theme.toggle()}
	class="theme-toggle"
	class:is-light={!isDark}
	aria-label={isDark ? 'Switch to light mode' : 'Switch to dark mode'}
	title={isDark ? 'Light mode' : 'Dark mode'}
>
	<svg class="theme-icon" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="1.5">
		{#if isDark}
			<!-- Sun: visible in dark mode -->
			<circle cx="12" cy="12" r="4" />
			<path d="M12 2v2m0 16v2M4.93 4.93l1.41 1.41m11.32 11.32l1.41 1.41M2 12h2m16 0h2M4.93 19.07l1.41-1.41m11.32-11.32l1.41-1.41" />
		{:else}
			<!-- Moon: visible in light mode -->
			<path d="M21 12.79A9 9 0 1111.21 3 7 7 0 0021 12.79z" />
		{/if}
	</svg>
</button>

<style>
	.theme-toggle {
		position: relative;
		display: flex;
		align-items: center;
		justify-content: center;
		width: 36px;
		height: 36px;
		border-radius: 8px;
		border: 1px solid transparent;
		background: transparent;
		color: var(--color-muted);
		cursor: pointer;
		transition: all 0.2s ease;
	}

	.theme-toggle:hover {
		color: #f43f5e;
		background: rgba(244, 63, 94, 0.06);
		border-color: rgba(244, 63, 94, 0.1);
	}

	.theme-toggle:active {
		transform: scale(0.92);
	}

	.theme-icon {
		width: 18px;
		height: 18px;
		transition: transform 0.35s cubic-bezier(0.34, 1.56, 0.64, 1);
	}

	.theme-toggle:hover .theme-icon {
		transform: rotate(15deg);
	}

	.is-light .theme-icon {
		transform: rotate(-15deg);
	}

	.is-light:hover .theme-icon {
		transform: rotate(0deg);
	}
</style>
