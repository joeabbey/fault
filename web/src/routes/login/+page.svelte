<script lang="ts">
	import { goto } from '$app/navigation';
	import { auth } from '$lib/stores/auth';
	import { api, AuthError } from '$lib/api/client';
	import { Button, Card, Input, Alert, Spinner } from '@jabbey/atlas';

	let mode = $state<'signin' | 'signup'>('signin');
	let email = $state('');
	let apiKey = $state('');
	let loading = $state(false);
	let error = $state<string | null>(null);
	let newKey = $state<string | null>(null);
	let copied = $state(false);

	async function handleSignup() {
		if (!email.trim()) {
			error = 'Email is required';
			return;
		}
		loading = true;
		error = null;

		try {
			const response = await api.signup(email.trim());
			newKey = response.api_key;
		} catch (e) {
			error = e instanceof Error ? e.message : 'Signup failed';
		} finally {
			loading = false;
		}
	}

	async function handleSignin() {
		if (!apiKey.trim()) {
			error = 'API key is required';
			return;
		}
		loading = true;
		error = null;

		try {
			// Temporarily set the key to validate it
			localStorage.setItem('fault_api_key', apiKey.trim());
			const usage = await api.usage();
			auth.login(apiKey.trim(), usage.email);
			goto('/');
		} catch (e) {
			localStorage.removeItem('fault_api_key');
			if (e instanceof AuthError) {
				error = 'Invalid API key';
			} else {
				error = e instanceof Error ? e.message : 'Sign in failed';
			}
		} finally {
			loading = false;
		}
	}

	function saveAndContinue() {
		if (newKey) {
			auth.login(newKey, email.trim());
			goto('/');
		}
	}

	async function copyKey() {
		if (newKey) {
			await navigator.clipboard.writeText(newKey);
			copied = true;
			setTimeout(() => { copied = false; }, 2000);
		}
	}
</script>

<svelte:head>
	<title>Sign In - Fault</title>
</svelte:head>

<div class="min-h-screen flex items-center justify-center bg-background px-4">
	<div class="w-full max-w-md">
		<div class="text-center mb-8">
			<div class="flex items-center justify-center gap-1.5 mb-2">
				<span class="text-3xl font-bold text-primary-500 font-display">//</span>
				<span class="text-3xl font-bold text-foreground font-display">Fault</span>
			</div>
			<p class="text-muted text-sm">Validate AI agent code changes</p>
		</div>

		{#if newKey}
			<!-- Show the new API key after signup -->
			<Card class="p-6">
				<h2 class="text-lg font-semibold text-foreground mb-2">Your API Key</h2>
				<p class="text-sm text-muted mb-4">
					Save this key now — it won't be shown again.
				</p>

				<div class="bg-accent rounded-lg p-4 mb-4">
					<code class="text-sm text-foreground break-all font-mono">{newKey}</code>
				</div>

				<div class="flex gap-3">
					<Button variant="secondary" onclick={copyKey} class="flex-1">
						{copied ? 'Copied!' : 'Copy Key'}
					</Button>
					<Button variant="primary" onclick={saveAndContinue} class="flex-1">
						Continue to Dashboard
					</Button>
				</div>
			</Card>
		{:else}
			<Card class="p-6">
				<!-- Mode tabs -->
				<div class="flex border-b border-border mb-6">
					<button
						class="flex-1 pb-3 text-sm font-medium border-b-2 transition-colors {mode === 'signin' ? 'border-primary-500 text-primary-600 dark:text-primary-400' : 'border-transparent text-muted hover:text-foreground'}"
						onclick={() => { mode = 'signin'; error = null; }}
					>
						Sign In
					</button>
					<button
						class="flex-1 pb-3 text-sm font-medium border-b-2 transition-colors {mode === 'signup' ? 'border-primary-500 text-primary-600 dark:text-primary-400' : 'border-transparent text-muted hover:text-foreground'}"
						onclick={() => { mode = 'signup'; error = null; }}
					>
						Sign Up
					</button>
				</div>

				{#if error}
					<Alert variant="error" class="mb-4">{error}</Alert>
				{/if}

				{#if mode === 'signin'}
					<form onsubmit={(e) => { e.preventDefault(); handleSignin(); }}>
						<Input
							label="API Key"
							type="password"
							bind:value={apiKey}
							placeholder="fk_..."
							helperText="Paste your Fault API key"
						/>
						<div class="mt-6">
							<Button variant="primary" type="submit" disabled={loading} class="w-full">
								{#if loading}
									<Spinner size="sm" class="mr-2" />
								{/if}
								Sign In
							</Button>
						</div>
					</form>
				{:else}
					<form onsubmit={(e) => { e.preventDefault(); handleSignup(); }}>
						<Input
							label="Email"
							type="email"
							bind:value={email}
							placeholder="you@example.com"
							helperText="We'll generate an API key for you"
						/>
						<div class="mt-6">
							<Button variant="primary" type="submit" disabled={loading} class="w-full">
								{#if loading}
									<Spinner size="sm" class="mr-2" />
								{/if}
								Create Account
							</Button>
						</div>
					</form>
				{/if}
			</Card>
		{/if}

		<p class="text-center text-xs text-muted mt-6">
			Free tier includes 50 LLM calls/month.
			<a href="https://fault.jabbey.io" class="text-primary-500 hover:underline">Learn more</a>
		</p>
	</div>
</div>
