<script lang="ts">
	import { goto } from '$app/navigation';
</script>

<svelte:head>
	<title>Checkout Cancelled - Fault</title>
</svelte:head>

<div class="space-y-6">
	<div class="rounded-xl p-8 text-center" style="background: #0e1017; border: 1px solid rgba(244,63,94,0.06);">
		<h1 class="text-2xl font-bold font-display mb-2" style="color: #e2e8f4; letter-spacing: -0.5px;">
			Checkout cancelled
		</h1>
		<p class="text-sm mb-6" style="color: #64748b;">
			No charges were made. You can upgrade anytime.
		</p>
		<button
			class="px-4 py-2 rounded-lg text-sm font-medium transition-all cursor-pointer"
			style="background: #151821; color: #e2e8f4; border: 1px solid rgba(244,63,94,0.06);"
			onmouseenter={(e) => { e.currentTarget.style.borderColor = 'rgba(244,63,94,0.18)'; }}
			onmouseleave={(e) => { e.currentTarget.style.borderColor = 'rgba(244,63,94,0.06)'; }}
			onclick={() => goto('/billing')}
		>
			Back to Billing
		</button>
	</div>
</div>
