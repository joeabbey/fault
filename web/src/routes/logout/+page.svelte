<script lang="ts">
	import { onMount } from 'svelte';
	import { goto } from '$app/navigation';
	import { auth } from '$lib/stores/auth';

	onMount(() => {
		auth.logout();
		goto('/');
	});
</script>

<div class="min-h-screen flex items-center justify-center bg-background">
	<p class="text-muted-foreground">Signing out...</p>
</div>
