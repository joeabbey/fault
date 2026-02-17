<script lang="ts">
	import { page } from '$app/stores';

	const errorParam = $derived($page.url.searchParams.get('error'));

	const errorMessages: Record<string, string> = {
		invalid_state: 'Invalid session. Please try again.',
		unauthorized: 'Your email is not authorized for this application.',
		exchange_failed: 'Failed to authenticate with Google. Please try again.',
		userinfo_failed: 'Failed to get account info. Please try again.',
		create_failed: 'Failed to create account. Please try again.',
		internal: 'An internal error occurred. Please try again.',
		access_denied: 'Access was denied. Please try again.'
	};

	function handleLogin() {
		window.location.href = '/api/auth/google/login';
	}
</script>

<svelte:head>
	<title>Sign In - Fault</title>
</svelte:head>

<div class="min-h-screen flex items-center justify-center px-4" style="background: #07080c;">
	<!-- Dot grid inherited from body::before -->

	<div class="w-full max-w-sm relative z-10">
		<!-- Logo -->
		<div class="text-center mb-10">
			<div class="flex items-center justify-center gap-2 mb-3">
				<span
					class="inline-flex items-center justify-center w-9 h-9 rounded-md text-sm font-bold font-mono"
					style="background: #f43f5e; color: #07080c; letter-spacing: -1px;"
				>
					//
				</span>
				<span class="text-2xl font-bold font-display" style="color: #e2e8f4; letter-spacing: -0.5px;">
					Fault
				</span>
			</div>
			<p style="color: #64748b; font-size: 14px;">Validate AI agent code changes</p>
		</div>

		{#if errorParam}
			<div
				class="mb-4 px-4 py-3 rounded-lg text-sm"
				style="background: rgba(244,63,94,0.08); border: 1px solid rgba(244,63,94,0.15); color: #fb7185;"
			>
				{errorMessages[errorParam] || 'Authentication failed. Please try again.'}
			</div>
		{/if}

		<!-- Sign-in card -->
		<div
			class="rounded-xl p-6"
			style="background: #0e1017; border: 1px solid rgba(244,63,94,0.06);"
		>
			<button
				onclick={handleLogin}
				class="w-full flex items-center justify-center gap-3 px-4 py-3 rounded-lg text-sm font-semibold transition-all cursor-pointer"
				style="background: #f43f5e; color: #fff; border: none;"
				onmouseenter={(e) => e.currentTarget.style.background = '#e11d48'}
				onmouseleave={(e) => e.currentTarget.style.background = '#f43f5e'}
			>
				<svg class="h-5 w-5" viewBox="0 0 24 24">
					<path
						fill="currentColor"
						d="M22.56 12.25c0-.78-.07-1.53-.2-2.25H12v4.26h5.92a5.06 5.06 0 01-2.2 3.32v2.77h3.57c2.08-1.92 3.28-4.74 3.28-8.1z"
					/>
					<path
						fill="currentColor"
						d="M12 23c2.97 0 5.46-.98 7.28-2.66l-3.57-2.77c-.98.66-2.23 1.06-3.71 1.06-2.86 0-5.29-1.93-6.16-4.53H2.18v2.84C3.99 20.53 7.7 23 12 23z"
					/>
					<path
						fill="currentColor"
						d="M5.84 14.09c-.22-.66-.35-1.36-.35-2.09s.13-1.43.35-2.09V7.07H2.18C1.43 8.55 1 10.22 1 12s.43 3.45 1.18 4.93l2.85-2.22.81-.62z"
					/>
					<path
						fill="currentColor"
						d="M12 5.38c1.62 0 3.06.56 4.21 1.64l3.15-3.15C17.45 2.09 14.97 1 12 1 7.7 1 3.99 3.47 2.18 7.07l3.66 2.84c.87-2.6 3.3-4.53 6.16-4.53z"
					/>
				</svg>
				Sign in with Google
			</button>
			<p class="text-center text-xs mt-4" style="color: #334155;">
				Free tier includes 50 LLM calls/month
			</p>
		</div>

		<p class="text-center text-xs mt-6" style="color: #334155;">
			<a href="https://fault.jabbey.io" style="color: #fb7185;" class="hover:underline">Learn more</a>
			about Fault
		</p>
	</div>
</div>
