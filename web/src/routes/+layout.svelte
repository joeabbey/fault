<script lang="ts">
	import '../app.css';
	import { onMount } from 'svelte';
	import { goto } from '$app/navigation';
	import { page } from '$app/stores';
	import { browser } from '$app/environment';
	import { createRawSnippet } from 'svelte';
	import { auth, isAuthenticated, isLoading, currentEmail } from '$lib/stores/auth';
	import { theme } from '$lib/stores/theme';
	import { DashboardLayout, Dropdown, Spinner } from '@jabbey/atlas';

	let { children: pageContent } = $props();

	const publicRoutes = ['/login', '/logout', '/docs', '/terms', '/privacy', '/about'];
	let sidebarCollapsed = $state(false);

	const activeNavId = $derived.by(() => {
		const path = $page.url.pathname;
		if (path === '/dashboard') return 'dashboard';
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
			href: '/dashboard',
			icon: icon(
				'M9 19v-6a2 2 0 00-2-2H5a2 2 0 00-2 2v6a2 2 0 002 2h2a2 2 0 002-2zm0 0V9a2 2 0 012-2h2a2 2 0 012 2v10m-6 0a2 2 0 002 2h2a2 2 0 002-2m0 0V5a2 2 0 012-2h2a2 2 0 012 2v14a2 2 0 01-2 2h-2a2 2 0 01-2-2z'
			)
		},
		{
			id: 'history',
			label: 'History',
			href: '/history',
			icon: icon(
				'M12 8v4l3 3m6-3a9 9 0 11-18 0 9 9 0 0118 0z'
			)
		},
		{
			id: 'specs',
			label: 'Specs',
			href: '/specs',
			icon: icon(
				'M9 12h6m-6 4h6m2 5H7a2 2 0 01-2-2V5a2 2 0 012-2h5.586a1 1 0 01.707.293l5.414 5.414a1 1 0 01.293.707V19a2 2 0 01-2 2z'
			)
		},
		{
			id: 'org',
			label: 'Team',
			href: '/org',
			icon: icon(
				'M17 20h5v-2a3 3 0 00-5.356-1.857M17 20H7m10 0v-2c0-.656-.126-1.283-.356-1.857M7 20H2v-2a3 3 0 015.356-1.857M7 20v-2c0-.656.126-1.283.356-1.857m0 0a5.002 5.002 0 019.288 0M15 7a3 3 0 11-6 0 3 3 0 016 0zm6 3a2 2 0 11-4 0 2 2 0 014 0zM7 10a2 2 0 11-4 0 2 2 0 014 0z'
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
		auth.init();
		theme.init();

		// Sidebar collapsed state
		const storedCollapsed = localStorage.getItem('sidebarCollapsed');
		if (storedCollapsed !== null) {
			sidebarCollapsed = storedCollapsed === 'true';
		} else if (window.innerWidth < 768) {
			sidebarCollapsed = true;
		}
	});

	// Route guard
	$effect(() => {
		if (browser && !$isLoading) {
			const pathname = $page.url.pathname;
			const isPublicRoute = pathname === '/' || publicRoutes.some((route) => pathname.startsWith(route));

			if ($isAuthenticated) {
				if (pathname === '/login') {
					goto('/dashboard');
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

	function handleUserMenuSelect(itemId: string) {
		if (itemId === 'logout') {
			auth.logout();
			goto('/login');
		}
	}

	function toggleTheme() {
		theme.toggle();
	}

	const isPublicPage = $derived.by(() => {
		const pathname = $page.url.pathname;
		return pathname === '/' || publicRoutes.some((route) => pathname.startsWith(route));
	});
</script>

{#if $isLoading}
	<div class="min-h-screen flex items-center justify-center bg-background">
		<Spinner size="lg" />
	</div>
{:else if isPublicPage}
	{@render pageContent()}
{:else if $isAuthenticated}
	<DashboardLayout {navItems} {activeNavId} bind:collapsed={sidebarCollapsed}>
		{#snippet logo()}
			<a href="/" class="flex items-center gap-2 no-underline">
				<span
					class="inline-flex items-center justify-center w-7 h-7 rounded-md text-xs font-bold tracking-tighter font-mono"
					style="background: #f43f5e; color: #07080c; letter-spacing: -1px;"
				>
					//
				</span>
				{#if !sidebarCollapsed}
					<span class="text-lg font-bold font-display text-foreground" style="letter-spacing: -0.5px;">
						Fault
					</span>
				{/if}
			</a>
		{/snippet}

		{#snippet userMenu()}
			<div class="space-y-1">
				{#if !sidebarCollapsed}
					<div class="px-3 py-1.5">
						<button
							onclick={toggleTheme}
							class="flex items-center gap-2 rounded-md px-2 py-1.5 text-sm text-muted hover:bg-secondary-100 dark:hover:bg-secondary-800 transition-colors cursor-pointer"
						>
							<svg class="h-4 w-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
								<path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M12 3v1m0 16v1m9-9h-1M4 12H3m15.364 6.364l-.707-.707M6.343 6.343l-.707-.707m12.728 0l-.707.707M6.343 17.657l-.707.707M16 12a4 4 0 11-8 0 4 4 0 018 0z" />
							</svg>
							Toggle theme
						</button>
					</div>
				{/if}
				{#if $currentEmail}
					<Dropdown items={userMenuItems} align="start" onselect={handleUserMenuSelect}>
						{#snippet trigger()}
							<div
								class="flex items-center gap-3 px-3 py-2 rounded-md transition-colors cursor-pointer text-muted hover:bg-secondary-100 dark:hover:bg-secondary-800"
							>
								<div
									class="h-8 w-8 rounded-full flex items-center justify-center text-sm font-medium flex-shrink-0 bg-primary-100 text-primary-400 dark:bg-primary-950 dark:text-primary-400"
								>
									{$currentEmail.charAt(0).toUpperCase()}
								</div>
								{#if !sidebarCollapsed}
									<div class="flex-1 min-w-0">
										<p class="text-sm font-medium truncate text-foreground">
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
