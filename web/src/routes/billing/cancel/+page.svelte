<script lang="ts">
	import { onMount } from 'svelte';
	import { goto } from '$app/navigation';

	let mounted = $state(false);

	onMount(() => {
		requestAnimationFrame(() => { mounted = true; });
	});
</script>

<svelte:head>
	<title>Checkout Cancelled - Fault</title>
</svelte:head>

<div class="flex items-center justify-center" style="min-height: calc(100vh - 8rem);">
	<div
		class="w-full max-w-sm text-center"
		style="opacity: {mounted ? 1 : 0}; transform: translateY({mounted ? 0 : 12}px); transition: all 0.6s cubic-bezier(0.16, 1, 0.3, 1);"
	>
		<!-- Arrow back icon -->
		<div
			class="inline-flex items-center justify-center w-12 h-12 rounded-full mb-6"
			style="
				background: rgba(244,63,94,0.04);
				border: 1px solid rgba(244,63,94,0.06);
				opacity: {mounted ? 1 : 0};
				transform: scale({mounted ? 1 : 0.8});
				transition: all 0.4s ease 0.1s;
			"
		>
			<svg width="20" height="20" viewBox="0 0 24 24" fill="none" stroke="#64748b" stroke-width="2" stroke-linecap="round" stroke-linejoin="round">
				<path d="M19 12H5M12 19l-7-7 7-7" />
			</svg>
		</div>

		<h1
			class="text-xl font-bold font-display mb-2"
			style="
				color: #e2e8f4; letter-spacing: -0.5px;
				opacity: {mounted ? 1 : 0};
				transform: translateY({mounted ? 0 : 8}px);
				transition: all 0.5s ease 0.2s;
			"
		>
			Checkout cancelled
		</h1>
		<p
			class="text-sm mb-6"
			style="
				color: #64748b;
				opacity: {mounted ? 1 : 0};
				transform: translateY({mounted ? 0 : 8}px);
				transition: all 0.5s ease 0.3s;
			"
		>
			No charges were made. You can upgrade anytime.
		</p>
		<button
			class="px-5 py-2.5 rounded-lg text-sm font-medium transition-all cursor-pointer"
			style="
				background: #151821; color: #e2e8f4; border: 1px solid rgba(244,63,94,0.06);
				opacity: {mounted ? 1 : 0};
				transform: translateY({mounted ? 0 : 8}px);
				transition: all 0.5s ease 0.4s;
			"
			onmouseenter={(e) => { e.currentTarget.style.borderColor = 'rgba(244,63,94,0.18)'; e.currentTarget.style.background = 'rgba(244,63,94,0.04)'; }}
			onmouseleave={(e) => { e.currentTarget.style.borderColor = 'rgba(244,63,94,0.06)'; e.currentTarget.style.background = '#151821'; }}
			onclick={() => goto('/billing')}
		>
			Back to Billing
		</button>
	</div>
</div>
