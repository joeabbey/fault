<script lang="ts">
	import { api } from '$lib/api/client';
	import { currentUser, currentEmail } from '$lib/stores/auth';
	import { Spinner } from '@jabbey/atlas';

	let rotating = $state(false);
	let error = $state<string | null>(null);
	let newKey = $state<string | null>(null);
	let copied = $state(false);
	let confirmRotate = $state(false);

	const hasKey = $derived($currentUser?.has_api_key || newKey !== null);

	async function handleCreate() {
		rotating = true;
		error = null;
		try {
			const response = await api.rotateKey();
			newKey = response.api_key;
		} catch (e) {
			error = e instanceof Error ? e.message : 'Failed to create key';
		} finally {
			rotating = false;
		}
	}

	async function handleRotate() {
		if (!confirmRotate) {
			confirmRotate = true;
			return;
		}

		rotating = true;
		error = null;
		try {
			const response = await api.rotateKey();
			newKey = response.api_key;
			confirmRotate = false;
		} catch (e) {
			error = e instanceof Error ? e.message : 'Failed to rotate key';
		} finally {
			rotating = false;
		}
	}

	async function copyKey() {
		if (newKey) {
			await navigator.clipboard.writeText(newKey);
			copied = true;
			setTimeout(() => {
				copied = false;
			}, 2000);
		}
	}

	function cancelRotate() {
		confirmRotate = false;
	}
</script>

<svelte:head>
	<title>API Keys - Fault</title>
</svelte:head>

<div class="space-y-6">
	<h1 class="text-2xl font-bold font-display text-foreground" style="letter-spacing: -0.5px;">
		API Keys
	</h1>

	{#if error}
		<div
			class="px-4 py-3 rounded-lg text-sm"
			style="background: rgba(244,63,94,0.08); border: 1px solid rgba(244,63,94,0.15); color: #fb7185;"
		>
			{error}
		</div>
	{/if}

	<!-- Account Info -->
	<div class="rounded-xl p-6 bg-card border border-border">
		<h2 class="text-sm font-medium mb-1 text-muted">Account</h2>
		{#if $currentEmail}
			<p class="text-sm text-foreground">{$currentEmail}</p>
		{/if}
		<p class="mt-2 text-xs text-muted-foreground">
			API keys authenticate CLI tools. Use the dashboard with Google sign-in.
		</p>
	</div>

	<!-- New Key Display -->
	{#if newKey}
		<div
			class="rounded-xl p-6 bg-card"
			style="border: 2px solid #f43f5e;"
		>
			<h2 class="text-lg font-semibold mb-2 font-display text-foreground">New API Key</h2>
			<p class="text-sm mb-4 text-muted">
				Save this key now — it won't be shown again.{#if hasKey} Your old key has been invalidated.{/if}
			</p>
			<div class="rounded-lg p-4 mb-4 bg-accent border border-border">
				<code class="text-sm break-all font-mono" style="color: #fb7185;">{newKey}</code>
			</div>
			<button
				class="px-4 py-2 rounded-lg text-sm font-medium transition-all cursor-pointer bg-accent text-foreground border border-border hover:border-[rgba(244,63,94,0.18)]"
				onclick={copyKey}
			>
				{copied ? 'Copied!' : 'Copy Key'}
			</button>
		</div>
	{/if}

	<!-- Create or Rotate Key -->
	<div class="rounded-xl p-6 bg-card border border-border">
		{#if hasKey}
			<h2 class="text-sm font-medium mb-2 text-foreground">Rotate Key</h2>
			<p class="text-sm mb-4 text-muted">
				Generate a new API key. Your current key will be immediately invalidated.
				Update your configuration files and environment variables after rotating.
			</p>

			{#if confirmRotate}
				<div
					class="mb-4 px-4 py-3 rounded-lg text-sm"
					style="background: rgba(251,191,36,0.06); border: 1px solid rgba(251,191,36,0.12); color: #fbbf24;"
				>
					Are you sure? Your current key will stop working immediately.
				</div>
				<div class="flex gap-3">
					<button
						class="px-4 py-2 rounded-lg text-sm font-semibold transition-all cursor-pointer"
						style="background: #f43f5e; color: #fff; border: none;"
						onmouseenter={(e) => e.currentTarget.style.background = '#e11d48'}
						onmouseleave={(e) => e.currentTarget.style.background = '#f43f5e'}
						onclick={handleRotate}
						disabled={rotating}
					>
						{rotating ? 'Rotating...' : 'Yes, Rotate Key'}
					</button>
					<button
						class="px-4 py-2 rounded-lg text-sm font-medium transition-all cursor-pointer bg-accent text-foreground border border-border hover:border-[rgba(244,63,94,0.18)]"
						onclick={cancelRotate}
					>
						Cancel
					</button>
				</div>
			{:else}
				<button
					class="px-4 py-2 rounded-lg text-sm font-medium transition-all cursor-pointer bg-accent text-foreground border border-border hover:border-[rgba(244,63,94,0.18)] hover:bg-[rgba(244,63,94,0.04)]"
					onclick={handleRotate}
				>
					Rotate API Key
				</button>
			{/if}
		{:else}
			<h2 class="text-sm font-medium mb-2 text-foreground">Create Key</h2>
			<p class="text-sm mb-4 text-muted">
				Generate an API key to authenticate the Fault CLI and other integrations.
			</p>
			<button
				class="px-4 py-2 rounded-lg text-sm font-semibold transition-all cursor-pointer"
				style="background: #f43f5e; color: #fff; border: none;"
				onmouseenter={(e) => e.currentTarget.style.background = '#e11d48'}
				onmouseleave={(e) => e.currentTarget.style.background = '#f43f5e'}
				onclick={handleCreate}
				disabled={rotating}
			>
				{#if rotating}
					<Spinner size="sm" />
				{/if}
				Create API Key
			</button>
		{/if}
	</div>

	<!-- Usage in CLI -->
	<div class="rounded-xl p-6 bg-card border border-border">
		<h2 class="text-sm font-medium mb-2 text-foreground">Using Your API Key</h2>
		<p class="text-sm mb-4 text-muted">
			Set your API key in your environment or Fault config file:
		</p>
		<div class="rounded-lg p-4 space-y-2 bg-accent border border-border">
			<code class="text-sm font-mono block text-muted-foreground">
				# Environment variable
			</code>
			<code class="text-sm font-mono block" style="color: #fb7185;">
				export FAULT_API_KEY=fk_your_key_here
			</code>
			<div class="pt-2"></div>
			<code class="text-sm font-mono block text-muted-foreground">
				# Or in .fault.toml
			</code>
			<code class="text-sm font-mono block" style="color: #fb7185;">
				api_key = "fk_your_key_here"
			</code>
		</div>
	</div>
</div>
