<script lang="ts">
	import { api } from '$lib/api/client';
	import { auth, currentEmail } from '$lib/stores/auth';
	import { Card, Button, Badge, Alert } from '@jabbey/atlas';

	let rotating = $state(false);
	let error = $state<string | null>(null);
	let newKey = $state<string | null>(null);
	let copied = $state(false);
	let confirmRotate = $state(false);

	// Get masked key from localStorage
	function getMaskedKey(): string {
		const key = localStorage.getItem('fault_api_key');
		if (!key) return 'No key stored';
		if (key.length < 8) return '****';
		return key.substring(0, 3) + '_****...' + key.substring(key.length - 4);
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
			auth.updateKey(response.api_key);
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
			setTimeout(() => { copied = false; }, 2000);
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
	<h1 class="text-2xl font-bold text-foreground font-display">API Keys</h1>

	{#if error}
		<Alert variant="error">{error}</Alert>
	{/if}

	<!-- Current Key -->
	<Card class="p-6">
		<h2 class="text-sm font-medium text-muted mb-1">Current API Key</h2>
		<div class="flex items-center gap-3">
			<code class="text-sm font-mono text-foreground bg-accent px-3 py-2 rounded-md">
				{getMaskedKey()}
			</code>
			<Badge variant="success">Active</Badge>
		</div>
		{#if $currentEmail}
			<p class="mt-2 text-xs text-muted">Associated with {$currentEmail}</p>
		{/if}
	</Card>

	<!-- New Key Display -->
	{#if newKey}
		<Card class="p-6 ring-2 ring-primary-500">
			<h2 class="text-lg font-semibold text-foreground mb-2">New API Key</h2>
			<p class="text-sm text-muted mb-4">
				Save this key now — it won't be shown again. Your old key has been invalidated.
			</p>
			<div class="bg-accent rounded-lg p-4 mb-4">
				<code class="text-sm text-foreground break-all font-mono">{newKey}</code>
			</div>
			<Button variant="secondary" onclick={copyKey}>
				{copied ? 'Copied!' : 'Copy Key'}
			</Button>
		</Card>
	{/if}

	<!-- Rotate Key -->
	<Card class="p-6">
		<h2 class="text-sm font-medium text-foreground mb-2">Rotate Key</h2>
		<p class="text-sm text-muted mb-4">
			Generate a new API key. Your current key will be immediately invalidated.
			Update your configuration files and environment variables after rotating.
		</p>

		{#if confirmRotate}
			<Alert variant="warning" class="mb-4">
				Are you sure? Your current key will stop working immediately.
			</Alert>
			<div class="flex gap-3">
				<Button variant="destructive" onclick={handleRotate} disabled={rotating}>
					{rotating ? 'Rotating...' : 'Yes, Rotate Key'}
				</Button>
				<Button variant="secondary" onclick={cancelRotate}>
					Cancel
				</Button>
			</div>
		{:else}
			<Button variant="secondary" onclick={handleRotate}>
				Rotate API Key
			</Button>
		{/if}
	</Card>

	<!-- Usage in CLI -->
	<Card class="p-6">
		<h2 class="text-sm font-medium text-foreground mb-2">Using Your API Key</h2>
		<p class="text-sm text-muted mb-4">
			Set your API key in your environment or Fault config file:
		</p>
		<div class="bg-accent rounded-lg p-4 space-y-2">
			<code class="text-sm text-foreground font-mono block">
				<span class="text-muted"># Environment variable</span>
			</code>
			<code class="text-sm text-primary-600 dark:text-primary-400 font-mono block">
				export FAULT_API_KEY=fk_your_key_here
			</code>
			<code class="text-sm text-foreground font-mono block mt-3">
				<span class="text-muted"># Or in .fault.toml</span>
			</code>
			<code class="text-sm text-primary-600 dark:text-primary-400 font-mono block">
				api_key = "fk_your_key_here"
			</code>
		</div>
	</Card>
</div>
