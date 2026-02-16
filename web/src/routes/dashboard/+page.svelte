<script lang="ts">
	import { onMount } from 'svelte';
	import { goto } from '$app/navigation';
	import { api } from '$lib/api/client';
	import type { UsageResponse } from '$lib/api/types';
	import { Card, StatCard, Badge, Button, Spinner, Alert } from '@jabbey/atlas';

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

	function getPlanVariant(plan: string): 'default' | 'primary' | 'success' {
		switch (plan) {
			case 'pro':
				return 'primary';
			case 'team':
				return 'success';
			default:
				return 'default';
		}
	}

	function formatNumber(n: number): string {
		if (n >= 1_000_000) return `${(n / 1_000_000).toFixed(1)}M`;
		if (n >= 1_000) return `${(n / 1_000).toFixed(1)}K`;
		return n.toString();
	}

	const usagePercent = $derived(
		usage ? Math.min(100, Math.round((usage.llm_calls / Math.max(usage.llm_limit, 1)) * 100)) : 0
	);
</script>

<svelte:head>
	<title>Dashboard - Fault</title>
</svelte:head>

<div class="space-y-6">
	<div class="flex items-center justify-between">
		<h1 class="text-2xl font-bold text-foreground font-display">Dashboard</h1>
		{#if usage}
			<Badge variant={getPlanVariant(usage.plan)} size="lg">
				{usage.plan.charAt(0).toUpperCase() + usage.plan.slice(1)} Plan
			</Badge>
		{/if}
	</div>

	{#if loading}
		<div class="flex items-center justify-center py-12">
			<Spinner size="lg" />
		</div>
	{:else if error}
		<Alert variant="error">{error}</Alert>
	{:else if usage}
		<!-- Usage Stats -->
		<div class="grid grid-cols-1 gap-5 sm:grid-cols-2 lg:grid-cols-4">
			<StatCard label="LLM Calls" value={usage.llm_calls} />
			<StatCard label="Remaining" value={usage.llm_remaining} />
			<StatCard label="Input Tokens" value={formatNumber(usage.tokens_input)} />
			<StatCard label="Output Tokens" value={formatNumber(usage.tokens_output)} />
		</div>

		<!-- Usage Meter -->
		<Card class="p-6">
			<div class="flex items-center justify-between mb-3">
				<h2 class="text-sm font-medium text-foreground">Usage This Month</h2>
				<span class="text-xs text-muted">{usage.month}</span>
			</div>
			<div class="flex items-center gap-4">
				<div class="flex-1">
					<div class="w-full bg-secondary-200 dark:bg-secondary-800 rounded-full h-3">
						<div
							class="h-3 rounded-full transition-all duration-500 {usagePercent >= 90
								? 'bg-error-500'
								: usagePercent >= 70
									? 'bg-warning-500'
									: 'bg-primary-500'}"
							style="width: {usagePercent}%"
						></div>
					</div>
				</div>
				<span class="text-sm font-medium text-foreground whitespace-nowrap">
					{usage.llm_calls} / {usage.llm_limit === 0 ? 'unlimited' : usage.llm_limit}
				</span>
			</div>
			{#if usagePercent >= 90 && usage.plan === 'free'}
				<p class="mt-3 text-sm text-warning-600 dark:text-warning-400">
					Running low on calls.
					<button class="underline hover:no-underline" onclick={() => goto('/billing')}>
						Upgrade to Pro
					</button>
					for 1,000 calls/month.
				</p>
			{/if}
		</Card>

		<!-- Quick Actions -->
		<Card class="p-6">
			<h2 class="text-sm font-medium text-foreground mb-4">Quick Actions</h2>
			<div class="flex flex-wrap gap-3">
				<Button variant="secondary" onclick={() => goto('/api-keys')}>
					Rotate API Key
				</Button>
				{#if usage.plan === 'free'}
					<Button variant="primary" onclick={() => goto('/billing')}>
						Upgrade Plan
					</Button>
				{:else}
					<Button variant="secondary" onclick={() => goto('/billing')}>
						Manage Billing
					</Button>
				{/if}
			</div>
		</Card>
	{/if}
</div>
