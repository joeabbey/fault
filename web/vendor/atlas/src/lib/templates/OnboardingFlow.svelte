<script lang="ts">
	import { cn } from '$lib/utils/cn';
	import Button from '$lib/components/Button.svelte';

	interface OnboardingStep {
		id: string;
		title: string;
		description?: string;
	}

	interface Props {
		/** Onboarding steps */
		steps: OnboardingStep[];
		/** Current step index */
		currentStep?: number;
		/** Logo snippet */
		logo?: import('svelte').Snippet;
		/** Content for each step */
		children: import('svelte').Snippet<[string]>;
		/** Whether current step is valid */
		canProceed?: boolean;
		/** Whether submitting */
		submitting?: boolean;
		/** Whether to allow skipping */
		allowSkip?: boolean;
		/** Custom skip text */
		skipText?: string;
		/** Callback on step change */
		onStepChange?: (step: number) => void;
		/** Callback on complete */
		onComplete?: () => void;
		/** Callback on skip */
		onSkip?: () => void;
		class?: string;
	}

	let {
		steps,
		currentStep = $bindable(0),
		logo,
		children,
		canProceed = true,
		submitting = false,
		allowSkip = true,
		skipText = 'Skip for now',
		onStepChange,
		onComplete,
		onSkip,
		class: className
	}: Props = $props();

	const isLastStep = $derived(currentStep === steps.length - 1);
	const progress = $derived(((currentStep + 1) / steps.length) * 100);

	function goBack() {
		if (currentStep > 0) {
			currentStep--;
			onStepChange?.(currentStep);
		}
	}

	function goNext() {
		if (isLastStep) {
			onComplete?.();
		} else if (canProceed) {
			currentStep++;
			onStepChange?.(currentStep);
		}
	}

	function handleKeydown(e: KeyboardEvent) {
		if (e.key === 'Enter' && canProceed && !submitting) {
			goNext();
		}
	}
</script>

<svelte:window onkeydown={handleKeydown} />

<div class={cn('min-h-screen flex flex-col bg-background', className)}>
	<!-- Header -->
	<header class="flex items-center justify-between px-6 py-4 border-b border-border">
		<div class="flex items-center gap-4">
			{#if logo}
				{@render logo()}
			{/if}
		</div>
		{#if allowSkip && onSkip}
			<Button variant="ghost" onclick={onSkip}>
				{skipText}
			</Button>
		{/if}
	</header>

	<!-- Progress -->
	<div class="h-1 bg-secondary-200 dark:bg-secondary-800">
		<div
			class="h-full bg-primary-500 transition-all duration-300 ease-out"
			style="width: {progress}%"
		></div>
	</div>

	<!-- Main Content -->
	<main class="flex-1 flex items-center justify-center p-6">
		<div class="w-full max-w-lg">
			<!-- Step indicator -->
			<div class="text-sm text-muted mb-2">
				Step {currentStep + 1} of {steps.length}
			</div>

			<!-- Step header -->
			<h1 class="text-2xl font-bold text-foreground mb-2">
				{steps[currentStep].title}
			</h1>
			{#if steps[currentStep].description}
				<p class="text-muted mb-8">
					{steps[currentStep].description}
				</p>
			{/if}

			<!-- Step content -->
			<div class="mb-8">
				{@render children(steps[currentStep].id)}
			</div>

			<!-- Navigation -->
			<div class="flex items-center justify-between">
				<div>
					{#if currentStep > 0}
						<Button variant="ghost" onclick={goBack}>
							<svg class="w-4 h-4 mr-2" fill="none" stroke="currentColor" viewBox="0 0 24 24">
								<path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M15 19l-7-7 7-7" />
							</svg>
							Back
						</Button>
					{/if}
				</div>
				<Button
					onclick={goNext}
					disabled={!canProceed}
					loading={submitting && isLastStep}
				>
					{isLastStep ? 'Get Started' : 'Continue'}
					{#if !isLastStep}
						<svg class="w-4 h-4 ml-2" fill="none" stroke="currentColor" viewBox="0 0 24 24">
							<path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M9 5l7 7-7 7" />
						</svg>
					{/if}
				</Button>
			</div>
		</div>
	</main>

	<!-- Footer -->
	<footer class="px-6 py-4 border-t border-border">
		<div class="flex justify-center gap-2">
			{#each steps as _, index}
				<div
					class={cn(
						'w-2 h-2 rounded-full transition-colors',
						index === currentStep
							? 'bg-primary-500'
							: index < currentStep
								? 'bg-primary-300'
								: 'bg-secondary-300 dark:bg-secondary-600'
					)}
				></div>
			{/each}
		</div>
	</footer>
</div>
