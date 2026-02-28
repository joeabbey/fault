<script lang="ts">
	import { onMount } from 'svelte';
	import { goto } from '$app/navigation';
	import { page } from '$app/stores';
	import { api } from '$lib/api/client';
	import type { OrgDetailResponse, OrgMember, Run, RunStats, AuditEntry } from '$lib/api/types';
	import { Spinner } from '@jabbey/atlas';

	let org = $state<OrgDetailResponse | null>(null);
	let members = $state<OrgMember[]>([]);
	let runs = $state<Run[]>([]);
	let stats = $state<RunStats | null>(null);
	let auditEntries = $state<AuditEntry[]>([]);
	let loading = $state(true);
	let error = $state<string | null>(null);

	const slug = $derived($page.url.pathname.split('/')[2]);

	onMount(async () => {
		try {
			const [orgResp, membersResp, runsResp, statsResp, auditResp] = await Promise.all([
				api.orgs.get(slug),
				api.orgs.members(slug),
				api.orgs.runs(slug, 10),
				api.orgs.stats(slug),
				api.orgs.audit(slug, 20)
			]);
			org = orgResp;
			members = membersResp;
			runs = runsResp.runs;
			stats = statsResp;
			auditEntries = auditResp.entries;
		} catch (e) {
			error = e instanceof Error ? e.message : 'Failed to load organization';
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

	function actionLabel(action: string): string {
		switch (action) {
			case 'member.added':
				return 'Added member';
			case 'member.removed':
				return 'Removed member';
			case 'webhook.created':
				return 'Created webhook';
			case 'webhook.deleted':
				return 'Deleted webhook';
			default:
				return action;
		}
	}
</script>

<svelte:head>
	<title>{org ? org.name : 'Organization'} - Fault</title>
</svelte:head>

<div class="space-y-6">
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
	{:else if org}
		<!-- Org Header -->
		<div class="flex items-center justify-between">
			<div class="flex items-center gap-4">
				<button
					class="text-sm cursor-pointer text-muted bg-transparent border-none"
					onclick={() => goto('/org')}
				>
					Teams
				</button>
				<span class="text-muted-foreground">/</span>
				<h1
					class="text-2xl font-bold font-display text-foreground"
					style="letter-spacing: -0.5px;"
				>
					{org.name}
				</h1>
			</div>
			<span
				class="px-3 py-1 rounded-full text-xs font-semibold font-mono uppercase tracking-wide"
				style="background: rgba(244,63,94,0.08); color: #fb7185; border: 1px solid rgba(244,63,94,0.12);"
			>
				{org.member_count} {org.member_count === 1 ? 'member' : 'members'}
			</span>
		</div>

		<!-- Stats Cards -->
		{#if stats}
			<div class="grid grid-cols-1 gap-4 sm:grid-cols-2 lg:grid-cols-4">
				{#each [
					{ label: 'Total Runs', value: stats.total_runs, accent: true },
					{ label: 'Total Issues', value: stats.total_issues, accent: false },
					{ label: 'Avg Errors', value: stats.avg_errors.toFixed(1), accent: false },
					{
						label: 'Avg Duration',
						value: formatDuration(stats.avg_duration_ms),
						accent: false
					}
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

		<!-- Recent Runs -->
		<div class="rounded-xl overflow-hidden bg-card border border-border">
			<div class="px-4 py-3 border-b border-border">
				<h2 class="text-sm font-medium text-foreground">Recent Runs</h2>
			</div>
			<table class="w-full text-sm">
				<thead>
					<tr class="border-b border-border">
						<th class="px-4 py-3 text-left text-xs font-medium uppercase tracking-wider text-muted">Date</th>
						<th class="px-4 py-3 text-left text-xs font-medium uppercase tracking-wider text-muted">Branch</th>
						<th class="px-4 py-3 text-left text-xs font-medium uppercase tracking-wider text-muted">Commit</th>
						<th class="px-4 py-3 text-right text-xs font-medium uppercase tracking-wider" style="color: #f43f5e;">Errors</th>
						<th class="px-4 py-3 text-right text-xs font-medium uppercase tracking-wider" style="color: #fbbf24;">Warnings</th>
						<th class="px-4 py-3 text-right text-xs font-medium uppercase tracking-wider text-muted">Duration</th>
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
							<td
								class="px-4 py-3 text-right font-mono font-medium"
								style="color: {run.errors > 0 ? '#f43f5e' : 'var(--color-muted-foreground)'};"
							>
								{run.errors}
							</td>
							<td
								class="px-4 py-3 text-right font-mono font-medium"
								style="color: {run.warnings > 0 ? '#fbbf24' : 'var(--color-muted-foreground)'};"
							>
								{run.warnings}
							</td>
							<td class="px-4 py-3 text-right font-mono text-xs text-muted">
								{formatDuration(run.duration_ms)}
							</td>
						</tr>
					{/each}
					{#if runs.length === 0}
						<tr>
							<td colspan="6" class="px-4 py-8 text-center text-sm text-muted">
								No runs yet for this organization.
							</td>
						</tr>
					{/if}
				</tbody>
			</table>
		</div>

		<!-- Members + Audit side by side -->
		<div class="grid grid-cols-1 gap-6 lg:grid-cols-2">
			<!-- Members -->
			<div class="rounded-xl overflow-hidden bg-card border border-border">
				<div class="px-4 py-3 border-b border-border">
					<h2 class="text-sm font-medium text-foreground">Members</h2>
				</div>
				<div class="divide-y divide-border/50">
					{#each members as member}
						<div class="px-4 py-3 flex items-center justify-between">
							<div class="flex items-center gap-3">
								<div
									class="h-8 w-8 rounded-full flex items-center justify-center text-xs font-medium flex-shrink-0"
									style="background: rgba(244,63,94,0.12); color: #fb7185;"
								>
									{(member.email || member.name || '?').charAt(0).toUpperCase()}
								</div>
								<div>
									<p class="text-sm text-foreground">
										{member.name || member.email || member.user_id}
									</p>
									{#if member.email && member.name}
										<p class="text-xs text-muted">{member.email}</p>
									{/if}
								</div>
							</div>
							<span
								class="px-2 py-0.5 rounded text-xs font-mono uppercase"
								style="background: {member.role === 'owner'
									? 'rgba(244,63,94,0.08)'
									: 'rgba(100,116,139,0.08)'}; color: {member.role === 'owner'
									? '#fb7185'
									: 'var(--color-muted)'};"
							>
								{member.role}
							</span>
						</div>
					{/each}
				</div>
			</div>

			<!-- Audit Trail -->
			<div class="rounded-xl overflow-hidden bg-card border border-border">
				<div class="px-4 py-3 border-b border-border">
					<h2 class="text-sm font-medium text-foreground">Audit Trail</h2>
				</div>
				<div class="divide-y divide-border/50">
					{#each auditEntries as entry}
						<div class="px-4 py-3">
							<div class="flex items-center justify-between mb-1">
								<span class="text-sm text-foreground">
									{actionLabel(entry.action)}
								</span>
								<span class="text-xs font-mono text-muted-foreground">
									{formatDate(entry.created_at)}
								</span>
							</div>
							{#if entry.user_email}
								<p class="text-xs text-muted">
									by {entry.user_email}
								</p>
							{/if}
						</div>
					{/each}
					{#if auditEntries.length === 0}
						<div class="px-4 py-8 text-center text-sm text-muted">
							No audit events yet.
						</div>
					{/if}
				</div>
			</div>
		</div>
	{/if}
</div>
