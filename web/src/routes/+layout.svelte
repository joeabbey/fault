<script lang="ts">
	import '../app.css';
	import { onMount } from 'svelte';
	import { goto } from '$app/navigation';
	import { page } from '$app/stores';
	import { browser } from '$app/environment';
	import { createRawSnippet } from 'svelte';
	import { auth, isAuthenticated, isLoading, currentEmail } from '$lib/stores/auth';
	import { api, AuthError } from '$lib/api/client';
	import { DashboardLayout, Dropdown, Spinner } from '@jabbey/atlas';

	let { children: pageContent } = $props();

	const publicRoutes = ['/login'];
	let darkMode = $state(false);
	let sidebarCollapsed = $state(false);

	const activeNavId = $derived.by(() => {
		const path = $page.url.pathname;
		if (path === '/') return 'dashboard';
		const segment = path.split('/')[1];
		return segment || 'dashboard';
	});

	function icon(d: string) {
		return createRawSnippet(() => ({
			render: () =>
				`<svg class="h-5 w-5" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="${d}" /></svg>`
		}));
	}

	const navItems = [
		{
			id: 'dashboard',
			label: 'Dashboard',
			href: '/',
			icon: icon(
				'M9 19v-6a2 2 0 00-2-2H5a2 2 0 00-2 2v6a2 2 0 002 2h2a2 2 0 002-2zm0 0V9a2 2 0 012-2h2a2 2 0 012 2v10m-6 0a2 2 0 002 2h2a2 2 0 002-2m0 0V5a2 2 0 012-2h2a2 2 0 012 2v14a2 2 0 01-2 2h-2a2 2 0 01-2-2z'
			)
		},
		{
			id: 'billing',
			label: 'Billing',
			href: '/billing',
			icon: icon(
				'M3 10h18M7 15h1m4 0h1m-7 4h12a3 3 0 003-3V8a3 3 0 00-3-3H6a3 3 0 00-3 3v8a3 3 0 003 3z'
			)
		},
		{
			id: 'api-keys',
			label: 'API Keys',
			href: '/api-keys',
			icon: icon(
				'M15 7a2 2 0 012 2m4 0a6 6 0 01-7.743 5.743L11 17H9v2H7v2H4a1 1 0 01-1-1v-2.586a1 1 0 01.293-.707l5.964-5.964A6 6 0 1121 9z'
			)
		}
	];

	const userMenuItems = [
		{ id: 'divider', label: '', divider: true },
		{ id: 'logout', label: 'Sign out', destructive: true }
	];

	onMount(() => {
		// Initialize auth from localStorage
		auth.init();

		// Initialize dark mode
		const stored = localStorage.getItem('darkMode');
		if (stored !== null) {
			darkMode = stored === 'true';
		} else {
			darkMode = window.matchMedia('(prefers-color-scheme: dark)').matches;
		}
		applyDarkMode(darkMode);

		// Sidebar collapsed state
		const storedCollapsed = localStorage.getItem('sidebarCollapsed');
		if (storedCollapsed !== null) {
			sidebarCollapsed = storedCollapsed === 'true';
		} else if (window.innerWidth < 768) {
			sidebarCollapsed = true;
		}
	});

	// Validate API key on auth change
	$effect(() => {
		if (browser && !$isLoading && $isAuthenticated) {
			api.usage().catch((err) => {
				if (err instanceof AuthError) {
					auth.logout();
				}
			});
		}
	});

	// Route guard
	$effect(() => {
		if (browser && !$isLoading) {
			const pathname = $page.url.pathname;
			const isPublicRoute = publicRoutes.some((route) => pathname.startsWith(route));

			if ($isAuthenticated) {
				if (pathname === '/login') {
					goto('/');
				}
			} else if (!isPublicRoute) {
				goto('/login');
			}
		}
	});

	// Persist sidebar collapsed state
	$effect(() => {
		if (browser) {
			localStorage.setItem('sidebarCollapsed', String(sidebarCollapsed));
		}
	});

	function applyDarkMode(isDark: boolean) {
		if (isDark) {
			document.documentElement.classList.add('dark');
		} else {
			document.documentElement.classList.remove('dark');
		}
	}

	function toggleDarkMode() {
		darkMode = !darkMode;
		if (browser) {
			localStorage.setItem('darkMode', String(darkMode));
		}
		applyDarkMode(darkMode);
	}

	function handleUserMenuSelect(itemId: string) {
		if (itemId === 'logout') {
			auth.logout();
			goto('/login');
		}
	}
</script>

{#if $isLoading}
	<div class="min-h-screen flex items-center justify-center bg-background">
		<Spinner size="lg" />
	</div>
{:else if $isAuthenticated}
	<DashboardLayout {navItems} {activeNavId} bind:collapsed={sidebarCollapsed}>
		{#snippet logo()}
			{#if sidebarCollapsed}
				<span class="text-lg font-bold text-primary-500 font-display">//</span>
			{:else}
				<div class="flex items-center gap-1.5">
					<span class="text-lg font-bold text-primary-500 font-display">//</span>
					<span class="text-lg font-bold text-foreground font-display">Fault</span>
				</div>
			{/if}
		{/snippet}

		{#snippet userMenu()}
			<div class="space-y-1">
				<!-- Dark mode toggle -->
				<button
					onclick={toggleDarkMode}
					class="w-full flex items-center gap-3 px-3 py-2 rounded-md text-sm text-muted hover:bg-secondary-50 dark:hover:bg-secondary-800 transition-colors"
					aria-label="Toggle dark mode"
				>
					{#if darkMode}
						<svg class="h-5 w-5 flex-shrink-0" fill="currentColor" viewBox="0 0 20 20">
							<path
								fill-rule="evenodd"
								d="M10 2a1 1 0 011 1v1a1 1 0 11-2 0V3a1 1 0 011-1zm4 8a4 4 0 11-8 0 4 4 0 018 0zm-.464 4.95l.707.707a1 1 0 001.414-1.414l-.707-.707a1 1 0 00-1.414 1.414zm2.12-10.607a1 1 0 010 1.414l-.706.707a1 1 0 11-1.414-1.414l.707-.707a1 1 0 011.414 0zM17 11a1 1 0 100-2h-1a1 1 0 100 2h1zm-7 4a1 1 0 011 1v1a1 1 0 11-2 0v-1a1 1 0 011-1zM5.05 6.464A1 1 0 106.465 5.05l-.708-.707a1 1 0 00-1.414 1.414l.707.707zm1.414 8.486l-.707.707a1 1 0 01-1.414-1.414l.707-.707a1 1 0 011.414 1.414zM4 11a1 1 0 100-2H3a1 1 0 000 2h1z"
								clip-rule="evenodd"
							/>
						</svg>
					{:else}
						<svg class="h-5 w-5 flex-shrink-0" fill="currentColor" viewBox="0 0 20 20">
							<path
								d="M17.293 13.293A8 8 0 016.707 2.707a8.001 8.001 0 1010.586 10.586z"
							/>
						</svg>
					{/if}
					{#if !sidebarCollapsed}
						<span>{darkMode ? 'Light mode' : 'Dark mode'}</span>
					{/if}
				</button>

				<!-- User info -->
				{#if $currentEmail}
					<Dropdown items={userMenuItems} align="start" onselect={handleUserMenuSelect}>
						{#snippet trigger()}
							<div
								class="flex items-center gap-3 px-3 py-2 rounded-md hover:bg-secondary-50 dark:hover:bg-secondary-800 transition-colors cursor-pointer"
							>
								<div class="h-8 w-8 rounded-full bg-primary-600 flex items-center justify-center text-white text-sm font-medium flex-shrink-0">
									{$currentEmail.charAt(0).toUpperCase()}
								</div>
								{#if !sidebarCollapsed}
									<div class="flex-1 min-w-0">
										<p class="text-sm font-medium text-foreground truncate">
											{$currentEmail}
										</p>
									</div>
								{/if}
							</div>
						{/snippet}
					</Dropdown>
				{/if}
			</div>
		{/snippet}

		{#snippet children()}
			<div class="p-6">
				{@render pageContent()}
			</div>
		{/snippet}
	</DashboardLayout>
{:else}
	{@render pageContent()}
{/if}
