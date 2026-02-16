<script lang="ts">
	import { cn } from '$lib/utils/cn';
	import Button from '$lib/components/Button.svelte';
	import Card from '$lib/components/Card.svelte';

	interface QuickAction {
		id: string;
		title: string;
		description: string;
		icon?: import('svelte').Snippet;
		href?: string;
		onClick?: () => void;
	}

	interface Props {
		/** Welcome headline */
		headline?: string;
		/** Welcome message */
		message?: string;
		/** User's name for personalization */
		userName?: string;
		/** Quick start actions */
		actions?: QuickAction[];
		/** Primary CTA */
		primaryAction?: {
			label: string;
			href?: string;
			onClick?: () => void;
		};
		/** Whether to show sample/demo data preview */
		showPreview?: boolean;
		/** Sample data preview content */
		preview?: import('svelte').Snippet;
		/** Help resources */
		helpResources?: Array<{
			title: string;
			description: string;
			href: string;
		}>;
		class?: string;
	}

	let {
		headline,
		message = "Let's get you set up. Here are some things you can do to get started.",
		userName,
		actions = [],
		primaryAction,
		showPreview = false,
		preview,
		helpResources = [],
		class: className
	}: Props = $props();

	const displayHeadline = $derived(
		headline ?? (userName ? `Welcome, ${userName}!` : 'Welcome!')
	);
</script>

<div class={cn('max-w-4xl mx-auto py-12 px-6', className)}>
	<!-- Welcome Header -->
	<div class="text-center mb-12">
		<div class="inline-flex items-center justify-center w-16 h-16 rounded-full bg-primary-100 dark:bg-primary-900 mb-6">
			<svg class="w-8 h-8 text-primary-600 dark:text-primary-400" fill="none" stroke="currentColor" viewBox="0 0 24 24">
				<path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M5 3v4M3 5h4M6 17v4m-2-2h4m5-16l2.286 6.857L21 12l-5.714 2.143L13 21l-2.286-6.857L5 12l5.714-2.143L13 3z" />
			</svg>
		</div>
		<h1 class="text-3xl font-bold text-foreground">{displayHeadline}</h1>
		<p class="mt-4 text-lg text-muted max-w-2xl mx-auto">{message}</p>

		{#if primaryAction}
			<div class="mt-8">
				{#if primaryAction.href}
					<Button size="lg" href={primaryAction.href}>
						{primaryAction.label}
					</Button>
				{:else}
					<Button size="lg" onclick={primaryAction.onClick}>
						{primaryAction.label}
					</Button>
				{/if}
			</div>
		{/if}
	</div>

	<!-- Quick Actions -->
	{#if actions.length > 0}
		<div class="mb-12">
			<h2 class="text-lg font-semibold text-foreground mb-4">Quick Start</h2>
			<div class="grid grid-cols-1 md:grid-cols-2 gap-4">
				{#each actions as action}
					{#if action.href}
						<a href={action.href} class="block">
							<Card variant="interactive" class="h-full">
								<div class="flex items-start gap-4">
									{#if action.icon}
										<div class="flex-shrink-0 w-10 h-10 rounded-lg bg-primary-100 dark:bg-primary-900 flex items-center justify-center text-primary-600 dark:text-primary-400">
											{@render action.icon()}
										</div>
									{/if}
									<div>
										<h3 class="font-medium text-foreground">{action.title}</h3>
										<p class="text-sm text-muted mt-1">{action.description}</p>
									</div>
								</div>
							</Card>
						</a>
					{:else}
						<button onclick={action.onClick} class="block w-full text-left">
							<Card variant="interactive" class="h-full">
								<div class="flex items-start gap-4">
									{#if action.icon}
										<div class="flex-shrink-0 w-10 h-10 rounded-lg bg-primary-100 dark:bg-primary-900 flex items-center justify-center text-primary-600 dark:text-primary-400">
											{@render action.icon()}
										</div>
									{/if}
									<div>
										<h3 class="font-medium text-foreground">{action.title}</h3>
										<p class="text-sm text-muted mt-1">{action.description}</p>
									</div>
								</div>
							</Card>
						</button>
					{/if}
				{/each}
			</div>
		</div>
	{/if}

	<!-- Sample Data Preview -->
	{#if showPreview && preview}
		<div class="mb-12">
			<h2 class="text-lg font-semibold text-foreground mb-4">Here's what your dashboard will look like</h2>
			<div class="relative rounded-lg border border-border overflow-hidden">
				<div class="absolute inset-0 bg-gradient-to-t from-background to-transparent pointer-events-none z-10"></div>
				<div class="opacity-60 blur-[1px]">
					{@render preview()}
				</div>
			</div>
		</div>
	{/if}

	<!-- Help Resources -->
	{#if helpResources.length > 0}
		<div>
			<h2 class="text-lg font-semibold text-foreground mb-4">Learn more</h2>
			<div class="grid grid-cols-1 md:grid-cols-3 gap-4">
				{#each helpResources as resource}
					<a
						href={resource.href}
						class="block p-4 rounded-lg border border-border hover:border-primary-500 hover:bg-primary-50 dark:hover:bg-primary-950 transition-colors"
					>
						<h3 class="font-medium text-foreground">{resource.title}</h3>
						<p class="text-sm text-muted mt-1">{resource.description}</p>
					</a>
				{/each}
			</div>
		</div>
	{/if}

	<!-- Progress Indicator -->
	<div class="mt-12 p-6 bg-secondary-50 dark:bg-secondary-900 rounded-lg">
		<div class="flex items-center justify-between mb-2">
			<span class="text-sm font-medium text-foreground">Getting started progress</span>
			<span class="text-sm text-muted">0% complete</span>
		</div>
		<div class="h-2 bg-secondary-200 dark:bg-secondary-700 rounded-full overflow-hidden">
			<div class="h-full w-0 bg-primary-500 rounded-full transition-all duration-500"></div>
		</div>
		<p class="mt-2 text-sm text-muted">
			Complete the quick start actions above to get the most out of your account.
		</p>
	</div>
</div>
