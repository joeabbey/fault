<script lang="ts">
	import { onMount } from 'svelte';
	import { page } from '$app/stores';
	import { goto } from '$app/navigation';
	import { api } from '$lib/api/client';
	import type { Run, RunIssue } from '$lib/api/types';
	import { Spinner } from '@jabbey/atlas';

	let run = $state<Run | null>(null);
	let loading = $state(true);
	let error = $state<string | null>(null);

	const runId = $derived($page.params.id);

	onMount(async () => {
		try {
			run = await api.runs.get(runId!);
		} catch (e) {
			error = e instanceof Error ? e.message : 'Failed to load run';
		} finally {
			loading = false;
		}
	});

	function formatDate(iso: string): string {
		return new Date(iso).toLocaleString('en-US', {
			year: 'numeric',
			month: 'short',
			day: 'numeric',
			hour: '2-digit',
			minute: '2-digit',
			second: '2-digit'
		});
	}

	function formatDuration(ms: number): string {
		if (ms < 1000) return `${ms}ms`;
		return `${(ms / 1000).toFixed(1)}s`;
	}

	function shortSHA(sha: string): string {
		return sha.slice(0, 7);
	}

	function severityColor(severity: string): string {
		switch (severity) {
			case 'error':
				return '#f43f5e';
			case 'warning':
				return '#fbbf24';
			default:
				return 'var(--color-muted)';
		}
	}

	// Group issues by category
	const groupedIssues = $derived.by(() => {
		if (!run || !run.issues) return new Map<string, RunIssue[]>();
		const map = new Map<string, RunIssue[]>();
		for (const issue of run.issues) {
			const cat = issue.category || 'other';
			if (!map.has(cat)) map.set(cat, []);
			map.get(cat)!.push(issue);
		}
		return map;
	});
</script>

<svelte:head>
	<title>Run Detail - Fault</title>
</svelte:head>

<div class="space-y-6">
	<!-- Back button + Header -->
	<div class="flex items-center gap-4">
		<button
			class="px-3 py-1.5 rounded-lg text-sm font-medium transition-all cursor-pointer bg-accent text-muted border border-border hover:border-[rgba(244,63,94,0.18)] hover:text-foreground"
			onclick={() => goto('/history')}
		>
			Back
		</button>
		<h1 class="text-2xl font-bold font-display text-foreground" style="letter-spacing: -0.5px;">
			Run Detail
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
	{:else if run}
		<!-- Run Metadata -->
		<div class="grid grid-cols-1 gap-4 sm:grid-cols-2 lg:grid-cols-4">
			{#each [
				{ label: 'Errors', value: run.errors, color: run.errors > 0 ? '#f43f5e' : 'var(--color-muted-foreground)' },
				{ label: 'Warnings', value: run.warnings, color: run.warnings > 0 ? '#fbbf24' : 'var(--color-muted-foreground)' },
				{ label: 'Files Changed', value: run.files_changed, color: 'var(--color-foreground)' },
				{ label: 'Duration', value: formatDuration(run.duration_ms), color: 'var(--color-foreground)' }
			] as stat}
				<div class="rounded-xl p-5 bg-card border border-border">
					<p class="text-xs font-medium uppercase tracking-wider mb-2 text-muted">
						{stat.label}
					</p>
					<p class="text-2xl font-bold font-mono" style="color: {stat.color}; letter-spacing: -1px;">
						{stat.value}
					</p>
				</div>
			{/each}
		</div>

		<!-- Run Info -->
		<div class="rounded-xl p-6 bg-card border border-border">
			<div class="grid grid-cols-2 gap-4 text-sm">
				<div>
					<span class="text-xs uppercase tracking-wider text-muted">Branch</span>
					<p class="font-mono mt-1 text-foreground">{run.branch || '-'}</p>
				</div>
				<div>
					<span class="text-xs uppercase tracking-wider text-muted">Commit</span>
					<p class="font-mono mt-1 text-foreground">
						{run.commit_sha ? shortSHA(run.commit_sha) : '-'}
					</p>
				</div>
				<div>
					<span class="text-xs uppercase tracking-wider text-muted">Timestamp</span>
					<p class="font-mono mt-1 text-foreground">{formatDate(run.timestamp)}</p>
				</div>
				<div>
					<span class="text-xs uppercase tracking-wider text-muted">Commit Range</span>
					<p class="font-mono mt-1 text-foreground">{run.commit_range || '-'}</p>
				</div>
			</div>
		</div>

		<!-- Confidence Score -->
		{#if run.confidence_score != null}
			<div class="rounded-xl p-6 bg-card border border-border">
				<h3 class="text-sm font-medium uppercase tracking-wider mb-4 text-muted">
					Confidence Score
				</h3>
				<div class="flex items-center gap-4 mb-4">
					<div class="flex-1 h-2 rounded-full overflow-hidden bg-accent">
						<div
							class="h-full rounded-full transition-all"
							style="width: {(run.confidence_score * 100).toFixed(0)}%; background: {run.confidence_score >= 0.8 ? '#34d399' : run.confidence_score >= 0.5 ? '#fbbf24' : '#f43f5e'};"
						></div>
					</div>
					<span class="text-lg font-bold font-mono" style="color: {run.confidence_score >= 0.8 ? '#34d399' : run.confidence_score >= 0.5 ? '#fbbf24' : '#f43f5e'};">
						{(run.confidence_score * 100).toFixed(0)}%
					</span>
				</div>

				{#if run.metadata?.confidence_per_file && Object.keys(run.metadata.confidence_per_file).length > 0}
					<div class="space-y-2">
						<p class="text-xs font-medium uppercase tracking-wider text-muted">Per-File Confidence</p>
						{#each Object.entries(run.metadata.confidence_per_file) as [file, score]}
							<div class="flex items-center gap-3">
								<span class="text-xs font-mono truncate flex-1 text-muted" title={file}>
									{file}
								</span>
								<div class="w-24 h-1.5 rounded-full overflow-hidden bg-accent">
									<div
										class="h-full rounded-full"
										style="width: {((score as number) * 100).toFixed(0)}%; background: {(score as number) >= 0.8 ? '#34d399' : (score as number) >= 0.5 ? '#fbbf24' : '#f43f5e'};"
									></div>
								</div>
								<span class="text-xs font-mono w-10 text-right" style="color: {(score as number) >= 0.8 ? '#34d399' : (score as number) >= 0.5 ? '#fbbf24' : '#f43f5e'};">
									{((score as number) * 100).toFixed(0)}%
								</span>
							</div>
						{/each}
					</div>
				{/if}
			</div>
		{/if}

		<!-- Issues by Category -->
		{#if run.issues && run.issues.length > 0}
			{#each [...groupedIssues] as [category, issues]}
				<div class="rounded-xl overflow-hidden bg-card border border-border">
					<div class="px-4 py-3 flex items-center justify-between border-b border-border">
						<h3 class="text-sm font-medium capitalize text-foreground">{category}</h3>
						<span class="text-xs font-mono px-2 py-0.5 rounded" style="background: rgba(244,63,94,0.06); color: #fb7185;">
							{issues.length}
						</span>
					</div>
					<div class="divide-y divide-border/50">
						{#each issues as issue}
							<div class="px-4 py-3">
								<div class="flex items-start gap-3">
									<span
										class="mt-0.5 px-1.5 py-0.5 rounded text-xs font-bold uppercase"
										style="background: {severityColor(issue.severity)}15; color: {severityColor(issue.severity)};"
									>
										{issue.severity}
									</span>
									<div class="flex-1 min-w-0">
										<p class="text-sm text-foreground">{issue.message}</p>
										{#if issue.file}
											<p class="text-xs font-mono mt-1 text-muted">
												{issue.file}{issue.line ? `:${issue.line}` : ''}
											</p>
										{/if}
										{#if issue.suggestion}
											<p class="text-xs mt-1 text-muted">
												Suggestion: {issue.suggestion}
											</p>
										{/if}
									</div>
								</div>
							</div>
						{/each}
					</div>
				</div>
			{/each}
		{:else}
			<div class="rounded-xl p-8 text-center bg-card border border-border">
				<p class="text-sm" style="color: #34d399;">No issues found in this run.</p>
			</div>
		{/if}
	{/if}
</div>
