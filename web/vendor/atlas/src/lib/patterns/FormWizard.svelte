<script lang="ts">
	import { cn } from '$lib/utils/cn';
	import Button from '$lib/components/Button.svelte';

	interface Step {
		id: string;
		title: string;
		description?: string;
	}

	interface Props {
		/** Wizard steps */
		steps: Step[];
		/** Current step index (0-based) */
		currentStep?: number;
		/** Whether to show step numbers */
		showNumbers?: boolean;
		/** Orientation of step indicator */
		orientation?: 'horizontal' | 'vertical';
		/** Content for each step (receives stepId) */
		children: import('svelte').Snippet<[string]>;
		/** Whether the current step is valid and can proceed */
		canProceed?: boolean;
		/** Whether the wizard is submitting */
		submitting?: boolean;
		/** Custom back button text */
		backText?: string;
		/** Custom next button text */
		nextText?: string;
		/** Custom submit button text */
		submitText?: string;
		/** Callback when step changes */
		onStepChange?: (step: number) => void;
		/** Callback when wizard completes */
		onComplete?: () => void;
		class?: string;
	}

	let {
		steps,
		currentStep = $bindable(0),
		showNumbers = true,
		orientation = 'horizontal',
		children,
		canProceed = true,
		submitting = false,
		backText = 'Back',
		nextText = 'Continue',
		submitText = 'Complete',
		onStepChange,
		onComplete,
		class: className
	}: Props = $props();

	const isFirstStep = $derived(currentStep === 0);
	const isLastStep = $derived(currentStep === steps.length - 1);
	const progress = $derived(((currentStep + 1) / steps.length) * 100);

	function goBack() {
		if (!isFirstStep) {
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
</script>

<div class={cn('w-full', className)}>
	<!-- Step Indicator -->
	{#if orientation === 'horizontal'}
		<div class="mb-8">
			<!-- Progress bar -->
			<div class="h-1 bg-secondary-200 dark:bg-secondary-700 rounded-full mb-4 overflow-hidden">
				<div
					class="h-full bg-primary-500 transition-all duration-300"
					style="width: {progress}%"
				></div>
			</div>

			<!-- Steps -->
			<div class="flex justify-between">
				{#each steps as step, index}
					<div class="flex flex-col items-center">
						<div
							class={cn(
								'w-8 h-8 rounded-full flex items-center justify-center text-sm font-medium transition-colors',
								index < currentStep
									? 'bg-primary-500 text-white'
									: index === currentStep
										? 'bg-primary-500 text-white ring-4 ring-primary-100 dark:ring-primary-900'
										: 'bg-secondary-200 dark:bg-secondary-700 text-muted'
							)}
						>
							{#if index < currentStep}
								<svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
									<path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M5 13l4 4L19 7" />
								</svg>
							{:else if showNumbers}
								{index + 1}
							{/if}
						</div>
						<div class="mt-2 text-center">
							<div class={cn(
								'text-sm font-medium',
								index <= currentStep ? 'text-foreground' : 'text-muted'
							)}>
								{step.title}
							</div>
							{#if step.description}
								<div class="text-xs text-muted hidden sm:block">{step.description}</div>
							{/if}
						</div>
					</div>
				{/each}
			</div>
		</div>
	{:else}
		<!-- Vertical orientation -->
		<div class="flex gap-8 mb-8">
			<div class="flex flex-col">
				{#each steps as step, index}
					<div class="flex items-start">
						<div class="flex flex-col items-center">
							<div
								class={cn(
									'w-8 h-8 rounded-full flex items-center justify-center text-sm font-medium transition-colors',
									index < currentStep
										? 'bg-primary-500 text-white'
										: index === currentStep
											? 'bg-primary-500 text-white ring-4 ring-primary-100 dark:ring-primary-900'
											: 'bg-secondary-200 dark:bg-secondary-700 text-muted'
								)}
							>
								{#if index < currentStep}
									<svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
										<path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M5 13l4 4L19 7" />
									</svg>
								{:else if showNumbers}
									{index + 1}
								{/if}
							</div>
							{#if index < steps.length - 1}
								<div class={cn(
									'w-0.5 h-12 mt-2',
									index < currentStep ? 'bg-primary-500' : 'bg-secondary-200 dark:bg-secondary-700'
								)}></div>
							{/if}
						</div>
						<div class="ml-4 pb-8">
							<div class={cn(
								'text-sm font-medium',
								index <= currentStep ? 'text-foreground' : 'text-muted'
							)}>
								{step.title}
							</div>
							{#if step.description}
								<div class="text-xs text-muted mt-0.5">{step.description}</div>
							{/if}
						</div>
					</div>
				{/each}
			</div>
		</div>
	{/if}

	<!-- Step Content -->
	<div class="bg-card border border-border rounded-lg p-6">
		<div class="mb-6">
			<h2 class="text-xl font-semibold text-foreground">{steps[currentStep].title}</h2>
			{#if steps[currentStep].description}
				<p class="mt-1 text-muted">{steps[currentStep].description}</p>
			{/if}
		</div>

		{@render children(steps[currentStep].id)}

		<!-- Navigation -->
		<div class="flex justify-between mt-8 pt-6 border-t border-border">
			<Button
				variant="ghost"
				onclick={goBack}
				disabled={isFirstStep}
			>
				{backText}
			</Button>
			<Button
				onclick={goNext}
				disabled={!canProceed}
				loading={submitting && isLastStep}
			>
				{isLastStep ? submitText : nextText}
			</Button>
		</div>
	</div>
</div>
