<script lang="ts">
	import {
		// Core
		Button, Card, Badge, Spinner, Alert, Avatar, Tooltip, Skeleton,
		// Form
		Input, Textarea, Select, Checkbox, RadioGroup, Switch, Slider,
		// Layout
		PageHeader, EmptyState, StatCard, Divider, Tabs, Accordion,
		// Overlay
		Modal, Drawer, Dropdown, Toast, toast,
		// Navigation
		Breadcrumbs, Pagination,
		// Data
		Table
	} from '$lib';

	let darkMode = $state(false);
	let modalOpen = $state(false);
	let drawerOpen = $state(false);
	let currentPage = $state(1);
	let sliderValue = $state(50);
	let switchValue = $state(true);
	let tabsActiveTab = $state('tab1');

	function toggleDarkMode() {
		darkMode = !darkMode;
		document.documentElement.classList.toggle('dark', darkMode);
	}

	const selectOptions = [
		{ value: 'react', label: 'React' },
		{ value: 'vue', label: 'Vue' },
		{ value: 'svelte', label: 'Svelte' },
		{ value: 'angular', label: 'Angular' }
	];

	const radioOptions = [
		{ value: 'small', label: 'Small', description: 'Good for personal projects' },
		{ value: 'medium', label: 'Medium', description: 'Suitable for small teams' },
		{ value: 'large', label: 'Large', description: 'Enterprise-grade solution' }
	];

	const dropdownItems = [
		{ id: 'edit', label: 'Edit' },
		{ id: 'duplicate', label: 'Duplicate' },
		{ id: 'divider1', label: '', divider: true },
		{ id: 'delete', label: 'Delete', destructive: true }
	];

	const tabItems = [
		{ id: 'tab1', label: 'Overview' },
		{ id: 'tab2', label: 'Settings' },
		{ id: 'tab3', label: 'Notifications' }
	];

	const accordionItems = [
		{ id: 'item1', title: 'What is Atlas?' },
		{ id: 'item2', title: 'How do I install it?' },
		{ id: 'item3', title: 'Is it accessible?' }
	];

	const tableColumns = [
		{ key: 'name', label: 'Name', sortable: true },
		{ key: 'email', label: 'Email', sortable: true },
		{ key: 'role', label: 'Role' },
		{ key: 'status', label: 'Status', align: 'center' as const }
	];

	const tableData = [
		{ name: 'John Doe', email: 'john@example.com', role: 'Admin', status: 'Active' },
		{ name: 'Jane Smith', email: 'jane@example.com', role: 'User', status: 'Active' },
		{ name: 'Bob Johnson', email: 'bob@example.com', role: 'User', status: 'Inactive' }
	];

	const breadcrumbItems = [
		{ label: 'Home', href: '/' },
		{ label: 'Components', href: '/components' },
		{ label: 'Button' }
	];
</script>

<svelte:head>
	<title>Atlas Design System</title>
</svelte:head>

<Toast position="bottom-right" />

<div class="min-h-screen p-8">
	<PageHeader
		title="Atlas"
		subtitle="Design System for Mantle, Conduit, and Stratum"
	>
		{#snippet actions()}
			<Button variant="secondary" onclick={toggleDarkMode}>
				{darkMode ? 'Light Mode' : 'Dark Mode'}
			</Button>
		{/snippet}
	</PageHeader>

	<main class="space-y-16">
		<!-- Buttons -->
		<section>
			<h2 class="text-2xl font-semibold mb-6">Buttons</h2>
			<div class="space-y-6">
				<div class="flex flex-wrap gap-4">
					<Button variant="primary">Primary</Button>
					<Button variant="secondary">Secondary</Button>
					<Button variant="ghost">Ghost</Button>
					<Button variant="outline">Outline</Button>
					<Button variant="destructive">Destructive</Button>
					<Button variant="link">Link</Button>
				</div>
				<div class="flex items-center gap-4">
					<Button size="sm">Small</Button>
					<Button size="md">Medium</Button>
					<Button size="lg">Large</Button>
					<Button disabled>Disabled</Button>
					<Button loading>Loading</Button>
				</div>
			</div>
		</section>

		<!-- Form Components -->
		<section>
			<h2 class="text-2xl font-semibold mb-6">Form Components</h2>
			<div class="grid grid-cols-1 md:grid-cols-2 gap-6 max-w-4xl">
				<Input label="Email" type="email" placeholder="you@example.com" helperText="We'll never share your email" />
				<Input label="Password" type="password" placeholder="Enter password" error="Password is required" />
				<Select label="Framework" options={selectOptions} placeholder="Select a framework" />
				<Textarea label="Description" placeholder="Tell us about yourself..." rows={3} showCount maxLength={200} />
				<div class="space-y-4">
					<Checkbox label="I agree to the terms" description="You must agree to continue" />
					<Switch label="Enable notifications" bind:checked={switchValue} />
				</div>
				<div>
					<Slider label="Volume" bind:value={sliderValue} showValue showMinMax />
				</div>
				<div class="md:col-span-2">
					<RadioGroup
						label="Select a plan"
						options={radioOptions}
						orientation="horizontal"
					/>
				</div>
			</div>
		</section>

		<!-- Cards -->
		<section>
			<h2 class="text-2xl font-semibold mb-6">Cards</h2>
			<div class="grid grid-cols-1 md:grid-cols-3 gap-6">
				<Card>
					<h3 class="font-semibold mb-2">Default Card</h3>
					<p class="text-muted text-sm">Basic card with default styling.</p>
				</Card>
				<Card variant="elevated">
					<h3 class="font-semibold mb-2">Elevated Card</h3>
					<p class="text-muted text-sm">Card with shadow elevation.</p>
				</Card>
				<Card variant="interactive" onclick={() => toast.info('Card clicked!')}>
					<h3 class="font-semibold mb-2">Interactive Card</h3>
					<p class="text-muted text-sm">Click me for a toast!</p>
				</Card>
			</div>
		</section>

		<!-- Stats -->
		<section>
			<h2 class="text-2xl font-semibold mb-6">Stat Cards</h2>
			<div class="grid grid-cols-1 md:grid-cols-4 gap-6">
				<StatCard label="Total Users" value="12,345" change={12} changeLabel="vs last month" />
				<StatCard label="Revenue" value="$45,678" change={-3.2} changeLabel="vs last month" />
				<StatCard label="Orders" value="1,234" change={8.1} changeLabel="vs last week" />
				<StatCard label="Conversion" value="3.2%" change={0.5} changeLabel="vs yesterday" />
			</div>
		</section>

		<!-- Badges -->
		<section>
			<h2 class="text-2xl font-semibold mb-6">Badges</h2>
			<div class="flex flex-wrap gap-3">
				<Badge>Default</Badge>
				<Badge variant="primary">Primary</Badge>
				<Badge variant="success" dot>Active</Badge>
				<Badge variant="warning" dot>Pending</Badge>
				<Badge variant="error" dot>Error</Badge>
				<Badge variant="outline">Outline</Badge>
			</div>
		</section>

		<!-- Alerts -->
		<section>
			<h2 class="text-2xl font-semibold mb-6">Alerts</h2>
			<div class="space-y-4 max-w-2xl">
				<Alert variant="info">This is an informational message.</Alert>
				<Alert variant="success">{#snippet title()}Success!{/snippet}Operation completed successfully.</Alert>
				<Alert variant="warning" dismissible>Warning: Please review before continuing.</Alert>
				<Alert variant="error">{#snippet title()}Error{/snippet}Something went wrong.</Alert>
			</div>
		</section>

		<!-- Tabs -->
		<section>
			<h2 class="text-2xl font-semibold mb-6">Tabs</h2>
			<div class="max-w-2xl">
				<Tabs tabs={tabItems} bind:activeTab={tabsActiveTab}>
					{#snippet children(tabId)}
						{#if tabId === 'tab1'}
							<p>This is the overview tab content. It shows general information about the item.</p>
						{:else if tabId === 'tab2'}
							<p>Settings tab content. Configure your preferences here.</p>
						{:else}
							<p>Notification preferences and history.</p>
						{/if}
					{/snippet}
				</Tabs>
			</div>
		</section>

		<!-- Accordion -->
		<section>
			<h2 class="text-2xl font-semibold mb-6">Accordion</h2>
			<div class="max-w-2xl">
				<Accordion items={accordionItems}>
					{#snippet children(itemId)}
						{#if itemId === 'item1'}
							Atlas is a comprehensive design system for Svelte 5 with Tailwind CSS v4.
						{:else if itemId === 'item2'}
							Install via npm: <code class="bg-secondary-100 dark:bg-secondary-800 px-1 rounded">npm install @jabbey/atlas</code>
						{:else}
							Yes! All components are built with WCAG 2.1 AA accessibility in mind.
						{/if}
					{/snippet}
				</Accordion>
			</div>
		</section>

		<!-- Table -->
		<section>
			<h2 class="text-2xl font-semibold mb-6">Table</h2>
			<div class="max-w-4xl">
				<Table columns={tableColumns} data={tableData} selectable />
			</div>
		</section>

		<!-- Navigation -->
		<section>
			<h2 class="text-2xl font-semibold mb-6">Navigation</h2>
			<div class="space-y-6">
				<div>
					<h3 class="text-sm font-medium text-muted mb-3">Breadcrumbs</h3>
					<Breadcrumbs items={breadcrumbItems} />
				</div>
				<div>
					<h3 class="text-sm font-medium text-muted mb-3">Pagination</h3>
					<Pagination bind:currentPage totalPages={10} />
				</div>
			</div>
		</section>

		<!-- Overlays -->
		<section>
			<h2 class="text-2xl font-semibold mb-6">Overlays</h2>
			<div class="flex flex-wrap gap-4">
				<Button onclick={() => (modalOpen = true)}>Open Modal</Button>
				<Button variant="secondary" onclick={() => (drawerOpen = true)}>Open Drawer</Button>
				<Dropdown items={dropdownItems} onselect={(id) => toast.info(`Selected: ${id}`)}>
					{#snippet trigger()}
						<Button variant="outline">Dropdown</Button>
					{/snippet}
				</Dropdown>
				<Button variant="ghost" onclick={() => toast.success('Hello from Toast!')}>Show Toast</Button>
			</div>

			<Modal bind:open={modalOpen} title="Modal Title" description="This is a modal dialog.">
				<p>Modal content goes here. You can put any content inside.</p>
				{#snippet footer()}
					<Button variant="ghost" onclick={() => (modalOpen = false)}>Cancel</Button>
					<Button onclick={() => (modalOpen = false)}>Confirm</Button>
				{/snippet}
			</Modal>

			<Drawer bind:open={drawerOpen} title="Drawer Title" position="right">
				<p>Drawer content goes here. Useful for forms, settings, or additional information.</p>
				{#snippet footer()}
					<Button variant="ghost" onclick={() => (drawerOpen = false)}>Cancel</Button>
					<Button onclick={() => (drawerOpen = false)}>Save</Button>
				{/snippet}
			</Drawer>
		</section>

		<!-- Avatars & Tooltips -->
		<section>
			<h2 class="text-2xl font-semibold mb-6">Avatars & Tooltips</h2>
			<div class="flex items-center gap-4">
				<Tooltip content="John Doe">
					<Avatar src="https://images.unsplash.com/photo-1472099645785-5658abf4ff4e?w=100&h=100&fit=crop" alt="John Doe" />
				</Tooltip>
				<Tooltip content="Jane Smith">
					<Avatar src="https://images.unsplash.com/photo-1438761681033-6461ffad8d80?w=100&h=100&fit=crop" alt="Jane Smith" />
				</Tooltip>
				<Tooltip content="With Initials">
					<Avatar alt="Alice Brown" />
				</Tooltip>
				<Tooltip content="Custom Size">
					<Avatar size="lg" alt="Large Avatar" />
				</Tooltip>
			</div>
		</section>

		<!-- Loading States -->
		<section>
			<h2 class="text-2xl font-semibold mb-6">Loading States</h2>
			<div class="flex items-center gap-8 mb-6">
				<Spinner size="sm" />
				<Spinner size="md" />
				<Spinner size="lg" />
			</div>
			<div class="grid grid-cols-1 md:grid-cols-3 gap-6">
				<Card>
					<div class="flex items-center gap-4 mb-4">
						<Skeleton variant="circle" width="48px" height="48px" />
						<div class="flex-1">
							<Skeleton variant="text" class="w-1/2 mb-2" />
							<Skeleton variant="text" class="w-1/3" />
						</div>
					</div>
					<Skeleton variant="text" lines={3} />
				</Card>
				<Card>
					<Skeleton variant="rect" height="120px" class="mb-4" />
					<Skeleton variant="text" class="w-3/4 mb-2" />
					<Skeleton variant="text" class="w-1/2" />
				</Card>
			</div>
		</section>

		<!-- Empty State -->
		<section>
			<h2 class="text-2xl font-semibold mb-6">Empty State</h2>
			<Card>
				<EmptyState
					title="No items found"
					description="Get started by creating your first item."
				>
					{#snippet action()}
						<Button>Create Item</Button>
					{/snippet}
				</EmptyState>
			</Card>
		</section>

		<!-- Color Palette -->
		<section>
			<h2 class="text-2xl font-semibold mb-6">Color Palette</h2>
			<div class="space-y-6">
				{#each ['primary', 'secondary', 'success', 'warning', 'error'] as colorName}
					<div>
						<h3 class="text-sm font-medium mb-2 capitalize">{colorName}</h3>
						<div class="flex gap-1">
							{#each ['50', '100', '200', '300', '400', '500', '600', '700', '800', '900', '950'] as shade}
								<div
									class="w-10 h-10 rounded shadow-sm"
									style="background-color: var(--color-{colorName}-{shade})"
									title="{colorName}-{shade}"
								></div>
							{/each}
						</div>
					</div>
				{/each}
			</div>
		</section>
	</main>

	<Divider class="my-16" />

	<footer class="text-center">
		<p class="text-sm text-muted">Atlas Design System v0.1.0 &middot; 32 Components</p>
	</footer>
</div>
