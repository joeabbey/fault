<script lang="ts">
	import { onMount } from 'svelte';
	import { goto } from '$app/navigation';
	import { api } from '$lib/api/client';
	import type { Organization } from '$lib/api/types';
	import { Spinner } from '@jabbey/atlas';

	let orgs = $state<Organization[]>([]);
	let loading = $state(true);
	let error = $state<string | null>(null);

	onMount(async () => {
		try {
			orgs = await api.orgs.list();
		} catch (e) {
			error = e instanceof Error ? e.message : 'Failed to load organizations';
		} finally {
			loading = false;
		}
	});

	function formatDate(iso: string): string {
		return new Date(iso).toLocaleDateString('en-US', {
			month: 'short',
			day: 'numeric',
			year: 'numeric'
		});
	}
</script>

<svelte:head>
	<title>Team - Fault</title>
</svelte:head>

<div class="space-y-6">
	<div class="flex items-center justify-between">
		<h1 class="text-2xl font-bold font-display" style="color: #e2e8f4; letter-spacing: -0.5px;">
			Team
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
	{:else if orgs.length === 0}
		<div
			class="rounded-xl p-8 text-center"
			style="background: #0e1017; border: 1px solid rgba(244,63,94,0.06);"
		>
			<p class="text-sm mb-2" style="color: #64748b;">No organizations yet.</p>
			<p class="text-xs" style="color: #475569;">
				Organizations are available on the Team plan. Create one from the API or upgrade your plan.
			</p>
		</div>
	{:else}
		<div class="grid grid-cols-1 gap-4 sm:grid-cols-2 lg:grid-cols-3">
			{#each orgs as org}
				<button
					class="rounded-xl p-5 text-left cursor-pointer transition-colors w-full"
					style="background: #0e1017; border: 1px solid rgba(244,63,94,0.06);"
					onmouseenter={(e) => (e.currentTarget.style.borderColor = 'rgba(244,63,94,0.18)')}
					onmouseleave={(e) => (e.currentTarget.style.borderColor = 'rgba(244,63,94,0.06)')}
					onclick={() => goto(`/org/${org.slug}`)}
				>
					<div class="flex items-center gap-3 mb-3">
						<div
							class="h-10 w-10 rounded-lg flex items-center justify-center text-sm font-bold"
							style="background: rgba(244,63,94,0.12); color: #fb7185;"
						>
							{org.name.charAt(0).toUpperCase()}
						</div>
						<div class="flex-1 min-w-0">
							<p class="text-sm font-semibold truncate" style="color: #e2e8f4;">
								{org.name}
							</p>
							<p class="text-xs font-mono" style="color: #64748b;">
								{org.slug}
							</p>
						</div>
					</div>
					<div class="flex items-center justify-between">
						<span
							class="px-2 py-0.5 rounded text-xs font-mono uppercase"
							style="background: rgba(244,63,94,0.06); color: #fb7185;"
						>
							{org.plan}
						</span>
						<span class="text-xs" style="color: #475569;">
							Created {formatDate(org.created_at)}
						</span>
					</div>
				</button>
			{/each}
		</div>
	{/if}
</div>
