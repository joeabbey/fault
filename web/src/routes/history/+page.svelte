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
		<h1 class="text-2xl font-bold font-display" style="color: #e2e8f4; letter-spacing: -0.5px;">
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
					<div
						class="rounded-xl p-5"
						style="background: #0e1017; border: 1px solid rgba(244,63,94,0.06); transition: border-color 0.2s;"
						onmouseenter={(e) => (e.currentTarget.style.borderColor = 'rgba(244,63,94,0.15)')}
						onmouseleave={(e) => (e.currentTarget.style.borderColor = 'rgba(244,63,94,0.06)')}
					>
						<p class="text-xs font-medium uppercase tracking-wider mb-2" style="color: #64748b;">
							{stat.label}
						</p>
						<p
							class="text-2xl font-bold font-mono"
							style="color: {stat.accent ? '#fb7185' : '#e2e8f4'}; letter-spacing: -1px;"
						>
							{stat.value}
						</p>
					</div>
				{/each}
			</div>
		{/if}

		<!-- Issues Over Time Chart -->
		{#if chartData.length >= 2}
			<div class="rounded-xl p-6" style="background: #0e1017; border: 1px solid rgba(244,63,94,0.06);">
				<h2 class="text-sm font-medium mb-4" style="color: #e2e8f4;">Issues Over Time</h2>
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
		<div class="rounded-xl overflow-hidden" style="background: #0e1017; border: 1px solid rgba(244,63,94,0.06);">
			<table class="w-full text-sm">
				<thead>
					<tr style="border-bottom: 1px solid rgba(244,63,94,0.06);">
						<th class="px-4 py-3 text-left text-xs font-medium uppercase tracking-wider" style="color: #64748b;">
							Date
						</th>
						<th class="px-4 py-3 text-left text-xs font-medium uppercase tracking-wider" style="color: #64748b;">
							Branch
						</th>
						<th class="px-4 py-3 text-left text-xs font-medium uppercase tracking-wider" style="color: #64748b;">
							Commit
						</th>
						<th class="px-4 py-3 text-right text-xs font-medium uppercase tracking-wider" style="color: #64748b;">
							Files
						</th>
						<th class="px-4 py-3 text-right text-xs font-medium uppercase tracking-wider" style="color: #f43f5e;">
							Errors
						</th>
						<th class="px-4 py-3 text-right text-xs font-medium uppercase tracking-wider" style="color: #fbbf24;">
							Warnings
						</th>
						<th class="px-4 py-3 text-right text-xs font-medium uppercase tracking-wider" style="color: #64748b;">
							Duration
						</th>
					</tr>
				</thead>
				<tbody>
					{#each runs as run}
						<tr
							class="cursor-pointer transition-colors"
							style="border-bottom: 1px solid rgba(244,63,94,0.04);"
							onmouseenter={(e) => (e.currentTarget.style.background = 'rgba(244,63,94,0.02)')}
							onmouseleave={(e) => (e.currentTarget.style.background = 'transparent')}
							onclick={() => goto(`/history/${run.id}`)}
						>
							<td class="px-4 py-3 font-mono text-xs" style="color: #94a3b8;">
								{formatDate(run.timestamp)}
							</td>
							<td class="px-4 py-3" style="color: #e2e8f4;">
								<span
									class="px-2 py-0.5 rounded text-xs font-mono"
									style="background: rgba(244,63,94,0.06); color: #fb7185;"
								>
									{run.branch || '-'}
								</span>
							</td>
							<td class="px-4 py-3 font-mono text-xs" style="color: #94a3b8;">
								{run.commit_sha ? shortSHA(run.commit_sha) : '-'}
							</td>
							<td class="px-4 py-3 text-right font-mono" style="color: #e2e8f4;">
								{run.files_changed}
							</td>
							<td class="px-4 py-3 text-right font-mono font-medium" style="color: {run.errors > 0 ? '#f43f5e' : '#334155'};">
								{run.errors}
							</td>
							<td class="px-4 py-3 text-right font-mono font-medium" style="color: {run.warnings > 0 ? '#fbbf24' : '#334155'};">
								{run.warnings}
							</td>
							<td class="px-4 py-3 text-right font-mono text-xs" style="color: #64748b;">
								{formatDuration(run.duration_ms)}
							</td>
						</tr>
					{/each}
					{#if runs.length === 0}
						<tr>
							<td colspan="7" class="px-4 py-8 text-center text-sm" style="color: #64748b;">
								No audit runs yet. Run <code class="font-mono px-1.5 py-0.5 rounded" style="background: #151821; color: #fb7185;">fault audit --upload</code> to get started.
							</td>
						</tr>
					{/if}
				</tbody>
			</table>
		</div>
	{/if}
</div>
