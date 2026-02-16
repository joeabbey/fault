<script lang="ts">
	import { onMount } from 'svelte';
	import { api } from '$lib/api/client';
	import type { SubscriptionResponse } from '$lib/api/types';
	import { Card, Badge, Button, Spinner, Alert } from '@jabbey/atlas';

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
			description: 'All static analyzers. Unlimited local use.',
			features: [
				'7 analyzers (imports, consistency, references, tests, patterns, security, hallucination)',
				'Go, TypeScript, Python, Java, Rust',
				'Terminal, JSON, SARIF output',
				'Auto-fix engine',
				'Watch mode',
				'Pre-commit hook',
				'Works offline'
			]
		},
		{
			id: 'pro',
			name: 'Pro',
			price: '$15',
			period: '/mo',
			description: 'LLM-powered analysis for deeper validation.',
			recommended: true,
			features: [
				'Everything in Free',
				'1,000 LLM calls/month',
				'Confidence scoring per file',
				'Spec comparison',
				'Priority support'
			]
		},
		{
			id: 'team',
			name: 'Team',
			price: '$30',
			period: '/user/mo',
			description: 'Shared rules and visibility across your team.',
			features: [
				'Everything in Pro',
				'Unlimited LLM calls',
				'Shared analyzer configurations',
				'Team dashboard',
				'Custom analyzer rules',
				'SSO integration'
			]
		}
	];

	onMount(async () => {
		try {
			subscription = await api.billing.subscription();
		} catch (e) {
			// billing might not be enabled — show plans anyway
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
	<h1 class="text-2xl font-bold text-foreground font-display">Billing</h1>

	{#if loading}
		<div class="flex items-center justify-center py-12">
			<Spinner size="lg" />
		</div>
	{:else}
		{#if error}
			<Alert variant="error">{error}</Alert>
		{/if}

		<!-- Current plan -->
		{#if subscription}
			<Card class="p-6">
				<div class="flex items-center justify-between">
					<div>
						<h2 class="text-sm font-medium text-muted">Current Plan</h2>
						<div class="flex items-center gap-3 mt-1">
							<span class="text-xl font-bold text-foreground capitalize">{subscription.plan}</span>
							<Badge variant={subscription.status === 'active' ? 'success' : 'warning'}>
								{subscription.status}
							</Badge>
						</div>
					</div>
					{#if subscription.plan !== 'free'}
						<Button variant="secondary" onclick={handlePortal} disabled={openingPortal}>
							{#if openingPortal}
								<Spinner size="sm" class="mr-2" />
							{/if}
							Manage Subscription
						</Button>
					{/if}
				</div>
			</Card>
		{/if}

		<!-- Plans -->
		<div class="grid grid-cols-1 md:grid-cols-3 gap-6">
			{#each plans as plan}
				<Card class="p-6 relative {plan.recommended ? 'ring-2 ring-primary-500' : ''}">
					{#if plan.recommended}
						<div class="absolute -top-3 left-1/2 -translate-x-1/2">
							<Badge variant="primary">Recommended</Badge>
						</div>
					{/if}

					<h3 class="text-lg font-bold text-foreground font-display">{plan.name}</h3>
					<div class="mt-2">
						<span class="text-3xl font-bold text-foreground font-display">{plan.price}</span>
						{#if plan.period}
							<span class="text-muted">{plan.period}</span>
						{/if}
					</div>
					<p class="mt-2 text-sm text-muted">{plan.description}</p>

					<ul class="mt-6 space-y-2">
						{#each plan.features as feature}
							<li class="flex items-start gap-2 text-sm text-foreground">
								<svg class="h-4 w-4 text-primary-500 mt-0.5 flex-shrink-0" fill="none" stroke="currentColor" viewBox="0 0 24 24">
									<path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M5 13l4 4L19 7" />
								</svg>
								{feature}
							</li>
						{/each}
					</ul>

					<div class="mt-6">
						{#if subscription?.plan === plan.id}
							<Button variant="secondary" disabled class="w-full">Current Plan</Button>
						{:else if plan.id === 'free'}
							<Button variant="secondary" disabled class="w-full">Included</Button>
						{:else}
							<Button
								variant={plan.recommended ? 'primary' : 'secondary'}
								class="w-full"
								onclick={() => handleCheckout(plan.id)}
								disabled={checkingOut === plan.id}
							>
								{#if checkingOut === plan.id}
									<Spinner size="sm" class="mr-2" />
								{/if}
								Upgrade to {plan.name}
							</Button>
						{/if}
					</div>
				</Card>
			{/each}
		</div>
	{/if}
</div>
