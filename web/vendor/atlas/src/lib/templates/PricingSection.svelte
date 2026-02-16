<script lang="ts">
	import { cn } from '$lib/utils/cn';
	import Button from '$lib/components/Button.svelte';
	import Badge from '$lib/components/Badge.svelte';

	interface PricingFeature {
		text: string;
		included: boolean;
		tooltip?: string;
	}

	interface PricingTier {
		id: string;
		name: string;
		description?: string;
		priceMonthly: number | null;
		priceAnnual?: number | null;
		features: PricingFeature[];
		cta: string;
		ctaHref?: string;
		popular?: boolean;
		enterprise?: boolean;
	}

	interface Props {
		/** Pricing tiers */
		tiers: PricingTier[];
		/** Whether to show annual pricing */
		annual?: boolean;
		/** Headline */
		headline?: string;
		/** Subheadline */
		subheadline?: string;
		/** Currency symbol */
		currency?: string;
		/** Annual savings text */
		savingsText?: string;
		/** Callback when CTA is clicked */
		onCtaClick?: (tierId: string) => void;
		class?: string;
	}

	let {
		tiers,
		annual = $bindable(false),
		headline = 'Simple, transparent pricing',
		subheadline = 'Choose the plan that fits your needs',
		currency = '$',
		savingsText = 'Save 20%',
		onCtaClick,
		class: className
	}: Props = $props();

	function getPrice(tier: PricingTier): number | null {
		if (tier.enterprise) return null;
		return annual && tier.priceAnnual !== undefined ? tier.priceAnnual : tier.priceMonthly;
	}

	function formatPrice(price: number | null): string {
		if (price === null) return 'Custom';
		if (price === 0) return 'Free';
		return `${currency}${price}`;
	}
</script>

<section class={cn('py-16', className)}>
	<div class="max-w-7xl mx-auto px-6">
		<!-- Header -->
		<div class="text-center mb-12">
			<h2 class="text-3xl font-bold text-foreground">{headline}</h2>
			{#if subheadline}
				<p class="mt-4 text-lg text-muted">{subheadline}</p>
			{/if}

			<!-- Billing Toggle -->
			{#if tiers.some(t => t.priceAnnual !== undefined)}
				<div class="mt-8 flex items-center justify-center gap-4">
					<span class={cn('text-sm', !annual ? 'text-foreground font-medium' : 'text-muted')}>
						Monthly
					</span>
					<button
						onclick={() => (annual = !annual)}
						class={cn(
							'relative w-14 h-7 rounded-full transition-colors',
							annual ? 'bg-primary-500' : 'bg-secondary-300 dark:bg-secondary-600'
						)}
						aria-label="Toggle annual billing"
					>
						<span
							class={cn(
								'absolute top-1 w-5 h-5 bg-white rounded-full shadow transition-transform',
								annual ? 'translate-x-8' : 'translate-x-1'
							)}
						></span>
					</button>
					<span class={cn('text-sm', annual ? 'text-foreground font-medium' : 'text-muted')}>
						Annual
						{#if savingsText}
							<Badge variant="success" class="ml-2">{savingsText}</Badge>
						{/if}
					</span>
				</div>
			{/if}
		</div>

		<!-- Pricing Cards -->
		<div class="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-{Math.min(tiers.length, 4)} gap-8">
			{#each tiers as tier}
				<div
					class={cn(
						'relative flex flex-col rounded-xl border p-8',
						tier.popular
							? 'border-primary-500 shadow-lg ring-1 ring-primary-500'
							: 'border-border bg-card'
					)}
				>
					{#if tier.popular}
						<div class="absolute -top-4 left-1/2 -translate-x-1/2">
							<Badge variant="primary">Most Popular</Badge>
						</div>
					{/if}

					<!-- Tier Header -->
					<div class="mb-6">
						<h3 class="text-lg font-semibold text-foreground">{tier.name}</h3>
						{#if tier.description}
							<p class="mt-1 text-sm text-muted">{tier.description}</p>
						{/if}
					</div>

					<!-- Price -->
					<div class="mb-6">
						<div class="flex items-baseline gap-1">
							<span class="text-4xl font-bold text-foreground">
								{formatPrice(getPrice(tier))}
							</span>
							{#if getPrice(tier) !== null && getPrice(tier) !== 0}
								<span class="text-muted">/{annual ? 'year' : 'month'}</span>
							{/if}
						</div>
						{#if annual && tier.priceMonthly && tier.priceAnnual}
							<p class="mt-1 text-sm text-muted">
								{currency}{Math.round(tier.priceAnnual / 12)}/month billed annually
							</p>
						{/if}
					</div>

					<!-- CTA -->
					<div class="mb-8">
						{#if tier.ctaHref}
							<Button
								variant={tier.popular ? 'primary' : 'outline'}
								class="w-full"
								href={tier.ctaHref}
							>
								{tier.cta}
							</Button>
						{:else}
							<Button
								variant={tier.popular ? 'primary' : 'outline'}
								class="w-full"
								onclick={() => onCtaClick?.(tier.id)}
							>
								{tier.cta}
							</Button>
						{/if}
					</div>

					<!-- Features -->
					<ul class="space-y-3 flex-1">
						{#each tier.features as feature}
							<li class="flex items-start gap-3">
								{#if feature.included}
									<svg class="w-5 h-5 text-success-500 flex-shrink-0 mt-0.5" fill="none" stroke="currentColor" viewBox="0 0 24 24">
										<path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M5 13l4 4L19 7" />
									</svg>
								{:else}
									<svg class="w-5 h-5 text-secondary-300 flex-shrink-0 mt-0.5" fill="none" stroke="currentColor" viewBox="0 0 24 24">
										<path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M6 18L18 6M6 6l12 12" />
									</svg>
								{/if}
								<span class={cn('text-sm', feature.included ? 'text-foreground' : 'text-muted line-through')}>
									{feature.text}
								</span>
							</li>
						{/each}
					</ul>
				</div>
			{/each}
		</div>
	</div>
</section>
