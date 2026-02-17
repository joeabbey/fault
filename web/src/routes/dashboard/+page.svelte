<script lang="ts">
	import { onMount } from 'svelte';
	import { goto } from '$app/navigation';
	import { api } from '$lib/api/client';
	import type { UsageResponse } from '$lib/api/types';
	import { Spinner } from '@jabbey/atlas';

	let usage = $state<UsageResponse | null>(null);
	let loading = $state(true);
	let error = $state<string | null>(null);

	onMount(async () => {
		try {
			usage = await api.usage();
		} catch (e) {
			error = e instanceof Error ? e.message : 'Failed to load usage data';
		} finally {
			loading = false;
		}
	});

	function formatNumber(n: number): string {
		if (n >= 1_000_000) return `${(n / 1_000_000).toFixed(1)}M`;
		if (n >= 1_000) return `${(n / 1_000).toFixed(1)}K`;
		return n.toString();
	}

	const usagePercent = $derived(
		usage ? Math.min(100, Math.round((usage.llm_calls / Math.max(usage.llm_limit, 1)) * 100)) : 0
	);

	const meterColor = $derived(
		usagePercent >= 90 ? '#f43f5e' : usagePercent >= 70 ? '#fbbf24' : '#34d399'
	);
</script>

<svelte:head>
	<title>Dashboard - Fault</title>
</svelte:head>

<div class="space-y-6">
	<!-- Header -->
	<div class="flex items-center justify-between">
		<h1 class="text-2xl font-bold font-display" style="color: #e2e8f4; letter-spacing: -0.5px;">
			Dashboard
		</h1>
		{#if usage}
			<span
				class="px-3 py-1 rounded-full text-xs font-semibold font-mono uppercase tracking-wide"
				style="background: rgba(244,63,94,0.08); color: #fb7185; border: 1px solid rgba(244,63,94,0.12);"
			>
				{usage.plan} plan
			</span>
		{/if}
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
	{:else if usage}
		<!-- Usage Stats Grid -->
		<div class="grid grid-cols-1 gap-4 sm:grid-cols-2 lg:grid-cols-4">
			{#each [
				{ label: 'LLM Calls', value: usage.llm_calls, accent: true },
				{ label: 'Remaining', value: usage.llm_remaining, accent: false },
				{ label: 'Input Tokens', value: formatNumber(usage.tokens_input), accent: false },
				{ label: 'Output Tokens', value: formatNumber(usage.tokens_output), accent: false }
			] as stat}
				<div
					class="rounded-xl p-5"
					style="background: #0e1017; border: 1px solid rgba(244,63,94,0.06); transition: border-color 0.2s;"
					onmouseenter={(e) => e.currentTarget.style.borderColor = 'rgba(244,63,94,0.15)'}
					onmouseleave={(e) => e.currentTarget.style.borderColor = 'rgba(244,63,94,0.06)'}
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

		<!-- Usage Meter -->
		<div class="rounded-xl p-6" style="background: #0e1017; border: 1px solid rgba(244,63,94,0.06);">
			<div class="flex items-center justify-between mb-4">
				<h2 class="text-sm font-medium" style="color: #e2e8f4;">Usage This Month</h2>
				<span class="text-xs font-mono" style="color: #334155;">{usage.month}</span>
			</div>
			<div class="flex items-center gap-4">
				<div class="flex-1">
					<div class="w-full rounded-full h-2" style="background: #151821;">
						<div
							class="h-2 rounded-full transition-all duration-700"
							style="width: {usagePercent}%; background: {meterColor}; box-shadow: 0 0 12px {meterColor}40;"
						></div>
					</div>
				</div>
				<span class="text-sm font-medium font-mono whitespace-nowrap" style="color: #e2e8f4;">
					{usage.llm_calls} / {usage.llm_limit === 0 ? '&infin;' : usage.llm_limit}
				</span>
			</div>
			{#if usagePercent >= 90 && usage.plan === 'free'}
				<p class="mt-3 text-sm" style="color: #fbbf24;">
					Running low on calls.
					<button
						class="underline hover:no-underline cursor-pointer"
						style="color: #fb7185; background: none; border: none;"
						onclick={() => goto('/billing')}
					>
						Upgrade to Pro
					</button>
					for 1,000 calls/month.
				</p>
			{/if}
		</div>

		<!-- Quick Actions -->
		<div class="rounded-xl p-6" style="background: #0e1017; border: 1px solid rgba(244,63,94,0.06);">
			<h2 class="text-sm font-medium mb-4" style="color: #e2e8f4;">Quick Actions</h2>
			<div class="flex flex-wrap gap-3">
				<button
					class="px-4 py-2 rounded-lg text-sm font-medium transition-all cursor-pointer"
					style="background: #151821; color: #e2e8f4; border: 1px solid rgba(244,63,94,0.06);"
					onmouseenter={(e) => { e.currentTarget.style.borderColor = 'rgba(244,63,94,0.18)'; e.currentTarget.style.background = 'rgba(244,63,94,0.04)'; }}
					onmouseleave={(e) => { e.currentTarget.style.borderColor = 'rgba(244,63,94,0.06)'; e.currentTarget.style.background = '#151821'; }}
					onclick={() => goto('/api-keys')}
				>
					Rotate API Key
				</button>
				{#if usage.plan === 'free'}
					<button
						class="px-4 py-2 rounded-lg text-sm font-semibold transition-all cursor-pointer"
						style="background: #f43f5e; color: #fff; border: none;"
						onmouseenter={(e) => e.currentTarget.style.background = '#e11d48'}
						onmouseleave={(e) => e.currentTarget.style.background = '#f43f5e'}
						onclick={() => goto('/billing')}
					>
						Upgrade Plan
					</button>
				{:else}
					<button
						class="px-4 py-2 rounded-lg text-sm font-medium transition-all cursor-pointer"
						style="background: #151821; color: #e2e8f4; border: 1px solid rgba(244,63,94,0.06);"
						onmouseenter={(e) => { e.currentTarget.style.borderColor = 'rgba(244,63,94,0.18)'; e.currentTarget.style.background = 'rgba(244,63,94,0.04)'; }}
						onmouseleave={(e) => { e.currentTarget.style.borderColor = 'rgba(244,63,94,0.06)'; e.currentTarget.style.background = '#151821'; }}
						onclick={() => goto('/billing')}
					>
						Manage Billing
					</button>
				{/if}
			</div>
		</div>
	{/if}
</div>
