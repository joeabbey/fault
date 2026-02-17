<script lang="ts">
	import { onMount } from 'svelte';
	import { api } from '$lib/api/client';
	import type { SubscriptionResponse } from '$lib/api/types';
	import { Spinner } from '@jabbey/atlas';

	let subscription = $state<SubscriptionResponse | null>(null);
	let loading = $state(true);
	let error = $state<string | null>(null);
	let checkingOut = $state<string | null>(null);
	let openingPortal = $state(false);

	const plans = [
		{
			id: 'free',
			name: 'Free',
			price: '$0',
			period: '',
			description: 'All static analyzers. Unlimited local use. No account required.',
			features: [
				'15 analyzers including concurrency, resource leaks, migration safety',
				'42 languages with full cross-file analysis',
				'32 manifest formats for dependency validation',
				'Terminal, JSON, SARIF output',
				'Auto-fix engine',
				'Watch mode',
				'Pre-commit hook & GitHub PR comments',
				'Works offline'
			]
		},
		{
			id: 'pro',
			name: 'Pro',
			price: '$15',
			period: '/mo',
			description: 'LLM-powered analysis and compliance reporting.',
			features: [
				'Everything in Free',
				'Confidence scoring per file',
				'Spec comparison (changes vs requirements)',
				'CWE-mapped SARIF output for security compliance',
				'Custom suppression rules with expiry dates',
				'Historical trend analysis dashboard',
				'Priority support'
			]
		},
		{
			id: 'team',
			name: 'Team',
			price: '$30',
			period: '/user/mo',
			description: 'Shared rules, compliance, and visibility across your team.',
			features: [
				'Everything in Pro',
				'Organization-wide shared baselines',
				'OWASP Top 10 & CWE Top 25 compliance packs',
				'Webhook notifications (Slack, Discord, HTTP)',
				'Team dashboard with change audit trail',
				'Custom analyzer rules',
				'SSO integration'
			]
		}
	];

	onMount(async () => {
		try {
			subscription = await api.billing.subscription();
		} catch (e) {
			subscription = { plan: 'free', status: 'active', llm_calls: 0, llm_limit: 50, llm_remaining: 50 };
		} finally {
			loading = false;
		}
	});

	async function handleCheckout(plan: string) {
		checkingOut = plan;
		error = null;
		try {
			const response = await api.billing.checkout(plan);
			window.location.href = response.checkout_url;
		} catch (e) {
			error = e instanceof Error ? e.message : 'Failed to start checkout';
			checkingOut = null;
		}
	}

	const planOrder = ['free', 'pro', 'team'];
	const recommendedPlan = $derived.by(() => {
		if (!subscription) return 'pro';
		const currentIdx = planOrder.indexOf(subscription.plan);
		const nextIdx = currentIdx + 1;
		return nextIdx < planOrder.length ? planOrder[nextIdx] : null;
	});

	async function handlePortal() {
		openingPortal = true;
		error = null;
		try {
			const response = await api.billing.portal();
			window.location.href = response.portal_url;
		} catch (e) {
			error = e instanceof Error ? e.message : 'Failed to open billing portal';
			openingPortal = false;
		}
	}
</script>

<svelte:head>
	<title>Billing - Fault</title>
</svelte:head>

<div class="space-y-6">
	<h1 class="text-2xl font-bold font-display" style="color: #e2e8f4; letter-spacing: -0.5px;">
		Billing
	</h1>

	{#if loading}
		<div class="flex items-center justify-center py-12">
			<Spinner size="lg" />
		</div>
	{:else}
		{#if error}
			<div
				class="px-4 py-3 rounded-lg text-sm"
				style="background: rgba(244,63,94,0.08); border: 1px solid rgba(244,63,94,0.15); color: #fb7185;"
			>
				{error}
			</div>
		{/if}

		<!-- Current plan -->
		{#if subscription}
			<div class="rounded-xl p-6" style="background: #0e1017; border: 1px solid rgba(244,63,94,0.06);">
				<div class="flex items-center justify-between">
					<div>
						<h2 class="text-sm font-medium" style="color: #64748b;">Current Plan</h2>
						<div class="flex items-center gap-3 mt-1">
							<span class="text-xl font-bold capitalize font-display" style="color: #e2e8f4;">
								{subscription.plan}
							</span>
							<span
								class="px-2 py-0.5 rounded-full text-xs font-semibold font-mono"
								style="background: {subscription.status === 'active' ? 'rgba(52,211,153,0.08)' : 'rgba(251,191,36,0.08)'}; color: {subscription.status === 'active' ? '#34d399' : '#fbbf24'}; border: 1px solid {subscription.status === 'active' ? 'rgba(52,211,153,0.15)' : 'rgba(251,191,36,0.15)'};"
							>
								{subscription.status}
							</span>
						</div>
					</div>
					{#if subscription.plan !== 'free'}
						<button
							class="px-4 py-2 rounded-lg text-sm font-medium transition-all cursor-pointer"
							style="background: #151821; color: #e2e8f4; border: 1px solid rgba(244,63,94,0.06);"
							onmouseenter={(e) => { e.currentTarget.style.borderColor = 'rgba(244,63,94,0.18)'; }}
							onmouseleave={(e) => { e.currentTarget.style.borderColor = 'rgba(244,63,94,0.06)'; }}
							onclick={handlePortal}
							disabled={openingPortal}
						>
							{#if openingPortal}
								<Spinner size="sm" />
							{/if}
							Manage Subscription
						</button>
					{/if}
				</div>
			</div>
		{/if}

		<!-- Plans -->
		<div class="grid grid-cols-1 md:grid-cols-3 gap-5">
			{#each plans as plan}
				<div
					class="rounded-xl p-6 relative"
					style="background: #0e1017; border: {recommendedPlan === plan.id ? '2px solid #f43f5e' : '1px solid rgba(244,63,94,0.06)'}; transition: border-color 0.2s;"
					onmouseenter={(e) => { if (recommendedPlan !== plan.id) e.currentTarget.style.borderColor = 'rgba(244,63,94,0.15)'; }}
					onmouseleave={(e) => { if (recommendedPlan !== plan.id) e.currentTarget.style.borderColor = 'rgba(244,63,94,0.06)'; }}
				>
					{#if recommendedPlan === plan.id}
						<div class="absolute -top-3 left-1/2 -translate-x-1/2">
							<span
								class="px-3 py-1 rounded-full text-xs font-semibold font-mono"
								style="background: #f43f5e; color: #fff;"
							>
								Recommended
							</span>
						</div>
					{/if}

					<h3 class="text-lg font-bold font-display" style="color: #e2e8f4;">{plan.name}</h3>
					<div class="mt-2">
						<span class="text-3xl font-bold font-display" style="color: #e2e8f4;">{plan.price}</span>
						{#if plan.period}
							<span style="color: #64748b;">{plan.period}</span>
						{/if}
					</div>
					<p class="mt-2 text-sm" style="color: #64748b;">{plan.description}</p>

					<ul class="mt-6 space-y-2">
						{#each plan.features as feature}
							<li class="flex items-start gap-2 text-sm" style="color: #e2e8f4;">
								<svg class="h-4 w-4 mt-0.5 flex-shrink-0" style="color: #fb7185;" fill="none" stroke="currentColor" viewBox="0 0 24 24">
									<path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M5 13l4 4L19 7" />
								</svg>
								{feature}
							</li>
						{/each}
					</ul>

					<div class="mt-6">
						{#if subscription?.plan === plan.id}
							<button
								class="w-full px-4 py-2.5 rounded-lg text-sm font-medium cursor-default"
								style="background: #151821; color: #64748b; border: 1px solid rgba(244,63,94,0.06);"
								disabled
							>
								Current Plan
							</button>
						{:else if plan.id === 'free'}
							<button
								class="w-full px-4 py-2.5 rounded-lg text-sm font-medium cursor-default"
								style="background: #151821; color: #64748b; border: 1px solid rgba(244,63,94,0.06);"
								disabled
							>
								Included
							</button>
						{:else}
							<button
								class="w-full px-4 py-2.5 rounded-lg text-sm font-semibold transition-all cursor-pointer"
								style="background: {recommendedPlan === plan.id ? '#f43f5e' : '#151821'}; color: {recommendedPlan === plan.id ? '#fff' : '#e2e8f4'}; border: {recommendedPlan === plan.id ? 'none' : '1px solid rgba(244,63,94,0.06)'};"
								onmouseenter={(e) => e.currentTarget.style.background = recommendedPlan === plan.id ? '#e11d48' : 'rgba(244,63,94,0.04)'}
								onmouseleave={(e) => e.currentTarget.style.background = recommendedPlan === plan.id ? '#f43f5e' : '#151821'}
								onclick={() => handleCheckout(plan.id)}
								disabled={checkingOut === plan.id}
							>
								{#if checkingOut === plan.id}
									<Spinner size="sm" />
								{/if}
								Upgrade to {plan.name}
							</button>
						{/if}
					</div>
				</div>
			{/each}
		</div>
	{/if}
</div>
