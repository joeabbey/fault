<script lang="ts">
	import { cn } from '$lib/utils/cn';

	interface Props {
		badge?: string;
		headline: string;
		highlightWord?: string;
		subheadline?: string;
		installCommand?: string;
		ctaSubtext?: string;
		terminalTitle?: string;
		terminalLines?: {
			text: string;
			type?: 'command' | 'info' | 'error' | 'warning' | 'success' | 'summary';
			delay?: number;
		}[];
		class?: string;
	}

	let {
		badge,
		headline,
		highlightWord,
		subheadline,
		installCommand,
		ctaSubtext,
		terminalTitle,
		terminalLines = [],
		class: className
	}: Props = $props();

	let copied = $state(false);

	function getHighlightedHeadline(): { before: string; word: string; after: string } | null {
		if (!highlightWord) return null;
		const idx = headline.indexOf(highlightWord);
		if (idx === -1) return null;
		return {
			before: headline.slice(0, idx),
			word: highlightWord,
			after: headline.slice(idx + highlightWord.length)
		};
	}

	async function copyCommand() {
		if (!installCommand) return;
		try {
			await navigator.clipboard.writeText(installCommand);
			copied = true;
			setTimeout(() => (copied = false), 2000);
		} catch {
			// Clipboard API not available
		}
	}

	const highlighted = $derived(getHighlightedHeadline());

	function lineTypeClass(type?: string): string {
		switch (type) {
			case 'error':
				return 'text-error-400';
			case 'warning':
				return 'text-warning-400';
			case 'success':
				return 'text-success-400';
			case 'command':
				return 'text-secondary-400';
			case 'summary':
				return 'font-semibold text-foreground';
			case 'info':
			default:
				return 'text-muted-foreground';
		}
	}
</script>

<section
	class={cn('relative py-20 pb-8', className)}
>
	<!-- Radial glow -->
	<div
		class="pointer-events-none absolute -top-20 left-[20%] h-[500px] w-[600px]"
		style="background: radial-gradient(ellipse at center, oklch(0.58 0.22 25 / 0.06) 0%, transparent 70%)"
	></div>

	<div class="relative z-[1] mx-auto max-w-[1140px] px-7">
		<div class="grid items-center gap-14 max-[900px]:grid-cols-1" style="grid-template-columns: 1.2fr 1fr;">
			<!-- Left: text content -->
			<div>
				{#if badge}
					<div
						class="mb-6 inline-flex items-center gap-2 rounded-full border border-primary-500/10 bg-primary-500/5 px-3.5 py-1.5 font-mono text-xs font-medium text-primary-400"
					>
						<span class="h-1.5 w-1.5 animate-pulse rounded-full bg-primary-500"></span>
						{badge}
					</div>
				{/if}

				<h1
					class="mb-5 text-[50px] font-extrabold leading-[1.06] tracking-[-2px] max-[900px]:text-[38px] max-[900px]:tracking-[-1.5px] max-[768px]:text-[34px] max-[768px]:tracking-[-1px]"
				>
					{#if highlighted}
						{highlighted.before}<span
							class="bg-gradient-to-r from-primary-500 to-warning-500 bg-clip-text text-transparent"
							>{highlighted.word}</span
						>{highlighted.after}
					{:else}
						{headline}
					{/if}
				</h1>

				{#if subheadline}
					<p
						class="mb-8 max-w-[460px] text-[17px] leading-[1.7] text-muted-foreground max-[900px]:max-w-full max-[900px]:text-base"
					>
						{subheadline}
					</p>
				{/if}

				{#if installCommand}
					<div class="mb-3 flex items-center gap-3 max-[768px]:flex-col max-[768px]:items-stretch">
						<div
							class="inline-flex items-center gap-3 rounded-lg border border-border bg-card px-4 py-3"
						>
							<span class="font-mono text-sm text-secondary-700 dark:text-secondary-500">$</span>
							<code class="whitespace-nowrap font-mono text-sm text-primary-400"
								>{installCommand}</code
							>
						</div>
						<button
							onclick={copyCommand}
							class="cursor-pointer rounded-md border border-primary-500/15 bg-primary-500/5 px-3.5 py-1.5 font-sans text-xs font-semibold text-primary-400 transition-all hover:bg-primary-500/15"
						>
							{copied ? 'Copied!' : 'Copy'}
						</button>
					</div>
				{/if}

				{#if ctaSubtext}
					<div>
						<span class="text-[13px] text-secondary-700 dark:text-secondary-600">{ctaSubtext}</span>
					</div>
				{/if}
			</div>

			<!-- Right: terminal preview -->
			{#if terminalLines.length > 0}
				<div
					class="overflow-hidden rounded-xl border border-white/5 shadow-[0_24px_80px_rgba(0,0,0,0.5),0_0_0_1px_rgba(255,255,255,0.03)]"
					style="background: oklch(0.15 0.01 260)"
				>
					<!-- Terminal bar -->
					<div
						class="flex items-center gap-1.5 border-b border-white/5 px-4 py-3"
						style="background: rgba(255,255,255,0.02)"
					>
						<span class="h-2.5 w-2.5 rounded-full bg-error-500"></span>
						<span class="h-2.5 w-2.5 rounded-full bg-warning-400"></span>
						<span class="h-2.5 w-2.5 rounded-full bg-success-400"></span>
						{#if terminalTitle}
							<span class="ml-2 font-mono text-[11px] text-secondary-700 dark:text-secondary-600"
								>{terminalTitle}</span
							>
						{/if}
					</div>

					<!-- Terminal body -->
					<div
						class="relative min-h-[320px] p-5 font-mono text-[12.5px] leading-[1.8] max-[480px]:p-3.5 max-[480px]:text-[11px]"
					>
						{#each terminalLines as line, i}
							<div
								class={cn('animate-[lineIn_0.3s_ease_forwards] opacity-0', lineTypeClass(line.type))}
								style="animation-delay: {line.delay ?? i * 0.5}s; transform: translateY(6px)"
							>
								{@html line.text}
							</div>
						{/each}
					</div>
				</div>
			{/if}
		</div>
	</div>
</section>

<style>
	@keyframes lineIn {
		to {
			opacity: 1;
			transform: translateY(0);
		}
	}
</style>
