<script lang="ts">
	import { onMount } from 'svelte';
	import { goto } from '$app/navigation';
	import { Spinner } from '@jabbey/atlas';

	let countdown = $state(3);

	onMount(() => {
		const interval = setInterval(() => {
			countdown -= 1;
			if (countdown <= 0) {
				clearInterval(interval);
				goto('/billing');
			}
		}, 1000);
		return () => clearInterval(interval);
	});
</script>

<svelte:head>
	<title>Subscription Active - Fault</title>
</svelte:head>

<div class="space-y-6">
	<div class="rounded-xl p-8 text-center" style="background: #0e1017; border: 1px solid rgba(52,211,153,0.15);">
		<div
			class="inline-flex items-center justify-center w-12 h-12 rounded-full mb-4"
			style="background: rgba(52,211,153,0.08);"
		>
			<svg class="h-6 w-6" style="color: #34d399;" fill="none" stroke="currentColor" viewBox="0 0 24 24">
				<path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M5 13l4 4L19 7" />
			</svg>
		</div>
		<h1 class="text-2xl font-bold font-display mb-2" style="color: #e2e8f4; letter-spacing: -0.5px;">
			You're all set
		</h1>
		<p class="text-sm mb-6" style="color: #64748b;">
			Your subscription is active. Redirecting to billing in {countdown}s...
		</p>
		<Spinner size="sm" />
	</div>
</div>
