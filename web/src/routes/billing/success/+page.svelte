<script lang="ts">
	import { onMount } from 'svelte';
	import { goto } from '$app/navigation';

	let mounted = $state(false);
	let countdown = $state(4);
	let progress = $state(0);

	onMount(() => {
		// Trigger entrance animations
		requestAnimationFrame(() => { mounted = true; });

		// Smooth progress ring
		const startTime = Date.now();
		const duration = 4000;
		const frame = () => {
			const elapsed = Date.now() - startTime;
			progress = Math.min(1, elapsed / duration);
			countdown = Math.max(0, Math.ceil((duration - elapsed) / 1000));
			if (elapsed < duration) {
				requestAnimationFrame(frame);
			} else {
				goto('/billing');
			}
		};
		requestAnimationFrame(frame);
	});

	const circumference = 2 * Math.PI * 18;
	const dashOffset = $derived(circumference * (1 - progress));
</script>

<svelte:head>
	<title>Subscription Active - Fault</title>
</svelte:head>

<div class="flex items-center justify-center" style="min-height: calc(100vh - 8rem);">
	<div
		class="w-full max-w-md text-center"
		style="opacity: {mounted ? 1 : 0}; transform: translateY({mounted ? 0 : 12}px); transition: all 0.6s cubic-bezier(0.16, 1, 0.3, 1);"
	>
		<!-- Animated checkmark -->
		<div class="relative inline-flex items-center justify-center mb-8">
			<!-- Outer glow -->
			<div
				class="absolute inset-0 rounded-full"
				style="
					width: 80px; height: 80px;
					background: radial-gradient(circle, rgba(52,211,153,0.12) 0%, transparent 70%);
					filter: blur(8px);
					opacity: {mounted ? 1 : 0};
					transition: opacity 1s ease 0.3s;
				"
			></div>

			<!-- Ring background -->
			<svg width="80" height="80" class="relative" style="transform: rotate(-90deg);">
				<circle
					cx="40" cy="40" r="18"
					fill="none"
					stroke="rgba(52,211,153,0.08)"
					stroke-width="2"
				/>
				<circle
					cx="40" cy="40" r="18"
					fill="none"
					stroke="#34d399"
					stroke-width="2"
					stroke-linecap="round"
					stroke-dasharray={circumference}
					stroke-dashoffset={dashOffset}
					style="transition: stroke-dashoffset 0.1s linear;"
				/>
			</svg>

			<!-- Check icon -->
			<svg
				class="absolute"
				width="24" height="24"
				viewBox="0 0 24 24"
				fill="none"
				stroke="#34d399"
				stroke-width="2.5"
				stroke-linecap="round"
				stroke-linejoin="round"
				style="
					opacity: {mounted ? 1 : 0};
					transform: scale({mounted ? 1 : 0.5});
					transition: all 0.5s cubic-bezier(0.34, 1.56, 0.64, 1) 0.2s;
				"
			>
				<path d="M6 13l4 4L18 7" />
			</svg>
		</div>

		<!-- Text -->
		<h1
			class="text-2xl font-bold font-display mb-2"
			style="
				color: #e2e8f4; letter-spacing: -0.5px;
				opacity: {mounted ? 1 : 0};
				transform: translateY({mounted ? 0 : 8}px);
				transition: all 0.5s ease 0.3s;
			"
		>
			You're all set
		</h1>
		<p
			class="text-sm mb-1"
			style="
				color: #64748b;
				opacity: {mounted ? 1 : 0};
				transform: translateY({mounted ? 0 : 8}px);
				transition: all 0.5s ease 0.4s;
			"
		>
			Your subscription is now active.
		</p>
		<p
			class="text-xs font-mono"
			style="
				color: #334155;
				opacity: {mounted ? 1 : 0};
				transition: opacity 0.5s ease 0.5s;
			"
		>
			Redirecting in {countdown}s
		</p>
	</div>
</div>
