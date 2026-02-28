<script lang="ts">
	import { onMount } from 'svelte';
	import { goto } from '$app/navigation';
	import { api } from '$lib/api/client';
	import type { Run, RunStats } from '$lib/api/types';
	import { Spinner } from '@jabbey/atlas';

	let runs = $state<Run[]>([]);
	let stats = $state<RunStats | null>(null);
	let loading = $state(true);
	let error = $state<string | null>(null);

	onMount(async () => {
		try {
			const [runsResp, statsResp] = await Promise.all([
				api.runs.list(50),
				api.runs.stats()
			]);
			runs = runsResp.runs;
			stats = statsResp;
		} catch (e) {
			error = e instanceof Error ? e.message : 'Failed to load history';
		} finally {
			loading = false;
		}
	});

	function formatDate(iso: string): string {
		return new Date(iso).toLocaleDateString('en-US', {
			month: 'short',
			day: 'numeric',
			hour: '2-digit',
			minute: '2-digit'
		});
	}

	function formatDuration(ms: number): string {
		if (ms < 1000) return `${ms}ms`;
		return `${(ms / 1000).toFixed(1)}s`;
	}

	function shortSHA(sha: string): string {
		return sha.slice(0, 7);
	}

	// SVG chart data: issues over time (last 20 runs, oldest first)
	const chartData = $derived.by(() => {
		if (runs.length === 0) return [];
		const recent = runs.slice(0, 20).reverse();
		const maxIssues = Math.max(1, ...recent.map((r) => r.total_issues));
		return recent.map((r, i) => ({
			x: (i / Math.max(1, recent.length - 1)) * 280 + 10,
			y: 80 - (r.total_issues / maxIssues) * 70,
			issues: r.total_issues,
			errors: r.errors
		}));
	});

	const chartPath = $derived.by(() => {
		if (chartData.length < 2) return '';
		return chartData.map((p, i) => `${i === 0 ? 'M' : 'L'}${p.x},${p.y}`).join(' ');
	});
</script>

<svelte:head>
	<title>History - Fault</title>
</svelte:head>

<div class="space-y-6">
	<!-- Header -->
	<div class="flex items-center justify-between">
		<h1 class="text-2xl font-bold font-display text-foreground" style="letter-spacing: -0.5px;">
			Audit History
		</h1>
	</div>

	{#if loading}
		<div class="flex items-center justify-center py-12">
			<Spinner size="lg" />
		</div>
	{:else if error}
		<div
			class="px-4 py-3 rounded-lg text-sm"
			style="background: rgba(244,63,94,0.08); border: 1px solid rgba(244,63,94,0.15); color: #fb7185;"
		>
			{error}
		</div>
	{:else}
		<!-- Stat Cards -->
		{#if stats}
			<div class="grid grid-cols-1 gap-4 sm:grid-cols-2 lg:grid-cols-4">
				{#each [
					{ label: 'Total Runs', value: stats.total_runs, accent: true },
					{ label: 'Total Issues', value: stats.total_issues, accent: false },
					{ label: 'Avg Errors', value: stats.avg_errors.toFixed(1), accent: false },
					{ label: 'Avg Duration', value: formatDuration(stats.avg_duration_ms), accent: false }
				] as stat}
					<div class="rounded-xl p-5 bg-card border border-border transition-[border-color] duration-200">
						<p class="text-xs font-medium uppercase tracking-wider mb-2 text-muted">
							{stat.label}
						</p>
						<p
							class="text-2xl font-bold font-mono"
							class:text-foreground={!stat.accent}
							style="letter-spacing: -1px; {stat.accent ? 'color: #fb7185;' : ''}"
						>
							{stat.value}
						</p>
					</div>
				{/each}
			</div>
		{/if}

		<!-- Issues Over Time Chart -->
		{#if chartData.length >= 2}
			<div class="rounded-xl p-6 bg-card border border-border">
				<h2 class="text-sm font-medium mb-4 text-foreground">Issues Over Time</h2>
				<svg viewBox="0 0 300 90" class="w-full" style="max-height: 120px;">
					<path d={chartPath} fill="none" stroke="#fb7185" stroke-width="1.5" />
					{#each chartData as point}
						<circle
							cx={point.x}
							cy={point.y}
							r="3"
							fill={point.errors > 0 ? '#f43f5e' : '#34d399'}
							opacity="0.8"
						/>
					{/each}
				</svg>
			</div>
		{/if}

		<!-- Runs Table -->
		<div class="rounded-xl overflow-hidden bg-card border border-border">
			<table class="w-full text-sm">
				<thead>
					<tr class="border-b border-border">
						<th class="px-4 py-3 text-left text-xs font-medium uppercase tracking-wider text-muted">
							Date
						</th>
						<th class="px-4 py-3 text-left text-xs font-medium uppercase tracking-wider text-muted">
							Branch
						</th>
						<th class="px-4 py-3 text-left text-xs font-medium uppercase tracking-wider text-muted">
							Commit
						</th>
						<th class="px-4 py-3 text-right text-xs font-medium uppercase tracking-wider text-muted">
							Files
						</th>
						<th class="px-4 py-3 text-right text-xs font-medium uppercase tracking-wider" style="color: #f43f5e;">
							Errors
						</th>
						<th class="px-4 py-3 text-right text-xs font-medium uppercase tracking-wider" style="color: #fbbf24;">
							Warnings
						</th>
						<th class="px-4 py-3 text-right text-xs font-medium uppercase tracking-wider text-muted">
							Duration
						</th>
					</tr>
				</thead>
				<tbody>
					{#each runs as run}
						<tr
							class="cursor-pointer transition-colors border-b border-border/50 hover:bg-[rgba(244,63,94,0.02)]"
							onclick={() => goto(`/history/${run.id}`)}
						>
							<td class="px-4 py-3 font-mono text-xs text-muted">
								{formatDate(run.timestamp)}
							</td>
							<td class="px-4 py-3 text-foreground">
								<span
									class="px-2 py-0.5 rounded text-xs font-mono"
									style="background: rgba(244,63,94,0.06); color: #fb7185;"
								>
									{run.branch || '-'}
								</span>
							</td>
							<td class="px-4 py-3 font-mono text-xs text-muted">
								{run.commit_sha ? shortSHA(run.commit_sha) : '-'}
							</td>
							<td class="px-4 py-3 text-right font-mono text-foreground">
								{run.files_changed}
							</td>
							<td class="px-4 py-3 text-right font-mono font-medium" style="color: {run.errors > 0 ? '#f43f5e' : 'var(--color-muted-foreground)'};">
								{run.errors}
							</td>
							<td class="px-4 py-3 text-right font-mono font-medium" style="color: {run.warnings > 0 ? '#fbbf24' : 'var(--color-muted-foreground)'};">
								{run.warnings}
							</td>
							<td class="px-4 py-3 text-right font-mono text-xs text-muted">
								{formatDuration(run.duration_ms)}
							</td>
						</tr>
					{/each}
					{#if runs.length === 0}
						<tr>
							<td colspan="7" class="px-4 py-8 text-center text-sm text-muted">
								No audit runs yet. Run <code class="font-mono px-1.5 py-0.5 rounded bg-accent" style="color: #fb7185;">fault audit --upload</code> to get started.
							</td>
						</tr>
					{/if}
				</tbody>
			</table>
		</div>
	{/if}
</div>
