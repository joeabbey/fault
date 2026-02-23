<script lang="ts">
	import { onMount } from 'svelte';
	import { api } from '$lib/api/client';
	import type { SpecResultEntry } from '$lib/api/types';
	import { Spinner } from '@jabbey/atlas';

	let results = $state<SpecResultEntry[]>([]);
	let loading = $state(true);
	let error = $state<string | null>(null);

	onMount(async () => {
		try {
			const resp = await api.specs.results(50);
			results = resp.results;
		} catch (e) {
			error = e instanceof Error ? e.message : 'Failed to load spec results';
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

	function scoreColor(score: number): string {
		if (score >= 0.8) return '#34d399';
		if (score >= 0.5) return '#fbbf24';
		return '#f43f5e';
	}

	function statusBadge(status: string): { bg: string; color: string } {
		switch (status) {
			case 'implemented':
				return { bg: 'rgba(52,211,153,0.1)', color: '#34d399' };
			case 'partial':
				return { bg: 'rgba(251,191,36,0.1)', color: '#fbbf24' };
			case 'missing':
				return { bg: 'rgba(244,63,94,0.1)', color: '#fb7185' };
			default:
				return { bg: 'rgba(100,116,139,0.1)', color: '#64748b' };
		}
	}

	let expandedId = $state<string | null>(null);

	function toggleExpand(id: string) {
		expandedId = expandedId === id ? null : id;
	}
</script>

<svelte:head>
	<title>Specs - Fault</title>
</svelte:head>

<div class="space-y-6">
	<!-- Header -->
	<div class="flex items-center justify-between">
		<h1 class="text-2xl font-bold font-display" style="color: #e2e8f4; letter-spacing: -0.5px;">
			Spec Compliance
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
	{:else if results.length === 0}
		<div
			class="rounded-xl p-8 text-center"
			style="background: #0e1017; border: 1px solid rgba(244,63,94,0.06);"
		>
			<p class="text-sm mb-2" style="color: #64748b;">No spec validation results yet.</p>
			<p class="text-xs" style="color: #475569;">
				Add a <code class="font-mono px-1.5 py-0.5 rounded" style="background: #151821; color: #fb7185;">.fault-spec.yaml</code>
				to your repo and run <code class="font-mono px-1.5 py-0.5 rounded" style="background: #151821; color: #fb7185;">fault check --spec .fault-spec.yaml</code>
			</p>
		</div>
	{:else}
		<!-- Results list -->
		{#each results as result}
			<div
				class="rounded-xl overflow-hidden"
				style="background: #0e1017; border: 1px solid rgba(244,63,94,0.06);"
			>
				<!-- Summary header -->
				<button
					class="w-full px-6 py-4 flex items-center gap-4 cursor-pointer transition-colors"
					style="background: transparent; border: none; text-align: left;"
					onmouseenter={(e) => (e.currentTarget.style.background = 'rgba(244,63,94,0.02)')}
					onmouseleave={(e) => (e.currentTarget.style.background = 'transparent')}
					onclick={() => toggleExpand(result.id)}
				>
					<!-- Score gauge -->
					<div class="flex-shrink-0 relative" style="width: 48px; height: 48px;">
						<svg viewBox="0 0 36 36" class="w-full h-full" style="transform: rotate(-90deg);">
							<circle
								cx="18" cy="18" r="16"
								fill="none"
								stroke="#151821"
								stroke-width="3"
							/>
							<circle
								cx="18" cy="18" r="16"
								fill="none"
								stroke={scoreColor(result.overall_score)}
								stroke-width="3"
								stroke-dasharray="{result.overall_score * 100.53} 100.53"
								stroke-linecap="round"
							/>
						</svg>
						<span
							class="absolute inset-0 flex items-center justify-center text-xs font-bold font-mono"
							style="color: {scoreColor(result.overall_score)};"
						>
							{Math.round(result.overall_score * 100)}
						</span>
					</div>

					<!-- Title and date -->
					<div class="flex-1 min-w-0">
						<p class="text-sm font-medium truncate" style="color: #e2e8f4;">
							{result.spec_title || 'Untitled Spec'}
						</p>
						<p class="text-xs font-mono mt-0.5" style="color: #64748b;">
							{formatDate(result.created_at)}
						</p>
					</div>

					<!-- Coverage bar -->
					<div class="hidden sm:flex items-center gap-3 flex-shrink-0" style="width: 200px;">
						<div class="flex-1">
							<div class="w-full rounded-full h-2 flex overflow-hidden" style="background: #151821;">
								{#if result.total_requirements > 0}
									<div
										class="h-2"
										style="width: {(result.implemented_count / result.total_requirements) * 100}%; background: #34d399;"
									></div>
									<div
										class="h-2"
										style="width: {(result.partial_count / result.total_requirements) * 100}%; background: #fbbf24;"
									></div>
									<div
										class="h-2"
										style="width: {(result.missing_count / result.total_requirements) * 100}%; background: #f43f5e;"
									></div>
								{/if}
							</div>
						</div>
						<span class="text-xs font-mono whitespace-nowrap" style="color: #94a3b8;">
							{result.implemented_count}/{result.total_requirements}
						</span>
					</div>

					<!-- Expand icon -->
					<svg
						class="h-4 w-4 flex-shrink-0 transition-transform"
						style="color: #64748b; transform: {expandedId === result.id ? 'rotate(180deg)' : 'rotate(0)'};"
						fill="none" stroke="currentColor" viewBox="0 0 24 24"
					>
						<path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M19 9l-7 7-7-7" />
					</svg>
				</button>

				<!-- Expanded: requirements table -->
				{#if expandedId === result.id && result.result_json}
					<div style="border-top: 1px solid rgba(244,63,94,0.06);">
						<table class="w-full text-sm">
							<thead>
								<tr style="border-bottom: 1px solid rgba(244,63,94,0.04);">
									<th class="px-6 py-2 text-left text-xs font-medium uppercase tracking-wider" style="color: #64748b;">
										ID
									</th>
									<th class="px-4 py-2 text-left text-xs font-medium uppercase tracking-wider" style="color: #64748b;">
										Status
									</th>
									<th class="px-4 py-2 text-left text-xs font-medium uppercase tracking-wider" style="color: #64748b;">
										Evidence
									</th>
									<th class="px-4 py-2 text-right text-xs font-medium uppercase tracking-wider" style="color: #64748b;">
										Confidence
									</th>
								</tr>
							</thead>
							<tbody>
								{#each result.result_json as req}
									{@const badge = statusBadge(req.status)}
									<tr style="border-bottom: 1px solid rgba(244,63,94,0.03);">
										<td class="px-6 py-2 font-mono text-xs" style="color: #e2e8f4;">
											{req.id}
										</td>
										<td class="px-4 py-2">
											<span
												class="px-2 py-0.5 rounded text-xs font-medium"
												style="background: {badge.bg}; color: {badge.color};"
											>
												{req.status}
											</span>
										</td>
										<td class="px-4 py-2 text-xs max-w-xs truncate" style="color: #94a3b8;">
											{req.evidence}
										</td>
										<td class="px-4 py-2 text-right font-mono text-xs" style="color: {scoreColor(req.confidence)};">
											{Math.round(req.confidence * 100)}%
										</td>
									</tr>
								{/each}
							</tbody>
						</table>
					</div>
				{/if}
			</div>
		{/each}
	{/if}
</div>
