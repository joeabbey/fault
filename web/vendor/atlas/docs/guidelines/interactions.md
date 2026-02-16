# Interaction Patterns

Guidelines for loading states, optimistic updates, pagination, real-time features, drag-and-drop, keyboard navigation, and animation. Reference this when building interactive features.

---

## Loading States

### Skeleton Screens vs Spinners

| Use Skeletons When | Use Spinners When |
|-------------------|-------------------|
| Content takes >1.5 seconds | Inline actions/feedback |
| Loading multiple elements | Background processes |
| Maintaining visual context | Content loads <1 second |
| Cards, lists, tables, charts | Single component loading |

**Key finding:** Skeletons perceived 30% faster than spinners with identical load times.

### Skeleton Implementation

```svelte
<script lang="ts">
  interface Props {
    loading?: boolean;
    lines?: number;
  }

  let { loading = true, lines = 3 }: Props = $props();
</script>

{#if loading}
  <div class="space-y-3 animate-pulse">
    {#each Array(lines) as _}
      <div class="h-4 bg-secondary-200 dark:bg-secondary-700 rounded"></div>
    {/each}
  </div>
{:else}
  <slot />
{/if}
```

### Best Practices

1. **Match layout structure** - Skeleton mirrors final content
2. **Fast animations** - Slow pulse feels slow
3. **Progressive replacement** - Replace as content loads
4. **Never combine** - Don't use skeleton + spinner
5. **Reduced motion** - Respect `prefers-reduced-motion`

---

## Optimistic Updates

Show changes immediately before server confirmation.

### When to Use

| Do Use For | Don't Use For |
|------------|---------------|
| Likes, favorites | Financial transactions |
| Toggles, status changes | Destructive actions |
| List reordering | High-stakes operations |
| Comments | Complex validations |

### Implementation Pattern

```typescript
// Svelte store pattern
function createTodosStore() {
  const { subscribe, set, update } = writable<Todo[]>([]);

  return {
    subscribe,

    async toggle(id: string) {
      // Snapshot for rollback
      const previous = get({ subscribe });

      // Optimistic update
      update(todos => todos.map(t =>
        t.id === id ? { ...t, completed: !t.completed } : t
      ));

      try {
        await api.toggleTodo(id);
      } catch (error) {
        // Rollback on failure
        set(previous);
        toast.error('Update failed. Changes reverted.');
        throw error;
      }
    }
  };
}
```

### Rollback Strategies

1. **Full rollback** - Restore entire previous state (simplest)
2. **Partial rollback** - Keep visible but mark failed
3. **Retry with backoff** - Attempt retry before showing error

### Pitfalls to Avoid

- Not implementing rollback
- Assuming APIs are idempotent
- Debouncing API calls (should be immediate)
- Forgetting to refetch real state after mutation

---

## Pagination Patterns

### Infinite Scroll vs Pagination vs Load More

| Aspect | Infinite Scroll | Pagination | Load More |
|--------|-----------------|------------|-----------|
| Best for | Social feeds | E-commerce, search | Hybrid use cases |
| Accessibility | Poor | Good | Good |
| Performance | Degrades | Consistent | Good |
| SEO | Problematic | Excellent | Acceptable |
| User control | Less | Full | Moderate |

### Accessibility Issues with Infinite Scroll

- Screen readers struggle with endless content
- Keyboard users can't reach footer
- Focus management is complex
- New content must be announced

### Recommended: Load More Button

```svelte
<script lang="ts">
  let items = $state<Item[]>([]);
  let hasMore = $state(true);
  let loading = $state(false);

  async function loadMore() {
    loading = true;
    const { data, hasNext } = await api.getItems(page + 1);
    items = [...items, ...data];
    hasMore = hasNext;
    loading = false;

    // Announce to screen readers
    announcer.textContent = `Loaded ${data.length} more items`;
  }
</script>

<ul role="list">
  {#each items as item (item.id)}
    <li>{item.name}</li>
  {/each}
</ul>

{#if hasMore}
  <button onclick={loadMore} disabled={loading} aria-busy={loading}>
    {loading ? 'Loading...' : 'Load More'}
  </button>
{/if}

<div aria-live="polite" class="sr-only" bind:this={announcer}></div>
```

### Virtualization

For very long lists (1000+ items), render only visible items:

```typescript
import { createVirtualizer } from '@tanstack/svelte-virtual';

const virtualizer = createVirtualizer({
  count: items.length,
  getScrollElement: () => scrollElement,
  estimateSize: () => 50,
  overscan: 5,
});
```

---

## Real-Time Updates

### Presence Indicators (Figma/Notion Style)

**Key components:**
1. **Presence avatars** - Who's viewing
2. **Live cursors** - Cursor positions
3. **Activity indicators** - Who's typing/editing
4. **Follow mode** - Jump to another's view

### Cursor Component

```svelte
<script lang="ts">
  interface Props {
    user: { name: string; color: string; cursor: { x: number; y: number } };
  }

  let { user }: Props = $props();
</script>

{#if user.cursor}
  <div
    class="absolute pointer-events-none z-50 transition-all duration-75"
    style="left: {user.cursor.x}px; top: {user.cursor.y}px;"
  >
    <svg width="24" height="24" viewBox="0 0 24 24" fill={user.color}>
      <path d="M5.5 3.21V20.8l5.72-5.66h8.28L5.5 3.21z"/>
    </svg>
    <span
      class="ml-2 px-2 py-1 text-xs text-white rounded-full"
      style="background-color: {user.color};"
    >
      {user.name}
    </span>
  </div>
{/if}
```

---

## Drag and Drop

### Accessibility Requirements

- **Keyboard**: Space to grab, arrows to move, space to drop
- **Screen readers**: Announce state, position, actions
- **Touch**: Larger handles, avoid scroll conflicts

### Always Provide Alternatives

```svelte
<ul use:dndzone={{ items }}>
  {#each items as item, index (item.id)}
    <li class="flex items-center gap-2">
      <button class="drag-handle" aria-label="Drag to reorder">
        <GripIcon />
      </button>

      <span>{item.name}</span>

      <!-- Alternative: move buttons -->
      <div class="ml-auto flex gap-1">
        <button
          onclick={() => moveItem(index, 'up')}
          disabled={index === 0}
          aria-label="Move up"
        >
          <ChevronUpIcon />
        </button>
        <button
          onclick={() => moveItem(index, 'down')}
          disabled={index === items.length - 1}
          aria-label="Move down"
        >
          <ChevronDownIcon />
        </button>
      </div>
    </li>
  {/each}
</ul>

<div id="dnd-instructions" class="sr-only">
  Press space to grab. Use arrows to move. Press space to drop.
</div>
```

### Touch Device Considerations

- Minimum 44x44px drag handles
- Use dedicated handles, not drag-anywhere
- Avoid vertical drag on scrollable containers
- Consider swipe gestures as alternatives

---

## Keyboard Navigation

### Core Principles

1. All interactive elements focusable (Tab/Shift+Tab)
2. Visible focus indicators (3:1 contrast minimum)
3. Logical tab order (left-to-right, top-to-bottom)
4. Skip links for main content
5. Arrow keys for widget navigation

### Keyboard Shortcuts

```svelte
<script lang="ts">
  import { onMount } from 'svelte';

  const shortcuts: Record<string, () => void> = {
    'n': () => createNew(),
    '/': () => focusSearch(),
    '?': () => showShortcuts(),
    'Escape': () => closeModal(),
  };

  onMount(() => {
    function handleKeydown(e: KeyboardEvent) {
      // Don't trigger in inputs
      if (['INPUT', 'TEXTAREA'].includes((e.target as HTMLElement).tagName)) {
        return;
      }

      // Skip if modifier keys
      if (e.ctrlKey || e.metaKey || e.altKey) return;

      const handler = shortcuts[e.key];
      if (handler) {
        e.preventDefault();
        handler();
      }
    }

    document.addEventListener('keydown', handleKeydown);
    return () => document.removeEventListener('keydown', handleKeydown);
  });
</script>
```

### Focus Management for Modals

```svelte
<script lang="ts">
  let dialogRef: HTMLDialogElement;
  let triggerRef: HTMLElement;

  function openModal() {
    triggerRef = document.activeElement as HTMLElement;
    dialogRef.showModal();
    dialogRef.querySelector<HTMLElement>('[autofocus], button')?.focus();
  }

  function closeModal() {
    dialogRef.close();
    triggerRef?.focus(); // Return focus
  }
</script>

<dialog bind:this={dialogRef} onclose={closeModal}>
  <h2>Title</h2>
  <slot />
  <button onclick={closeModal}>Close</button>
</dialog>
```

---

## Animation Guidelines

### Purposeful Animation

**Animations should:**
- Improve clarity
- Guide users toward actions
- Provide feedback
- Run at 60fps minimum

**Animations should NOT:**
- Be purely decorative
- Delay user tasks
- Cause vestibular discomfort
- Block content

### Reduced Motion

```css
/* Default: reduced motion version */
.modal-enter {
  animation: fade 150ms ease-out;
}

/* Enhanced for those who prefer it */
@media (prefers-reduced-motion: no-preference) {
  .modal-enter {
    animation: scale-fade 200ms ease-out;
  }
}

/* Global reset for reduced motion */
@media (prefers-reduced-motion: reduce) {
  *,
  *::before,
  *::after {
    animation-duration: 0.001ms !important;
    transition-duration: 0.001ms !important;
  }
}
```

### JavaScript Detection

```typescript
import { readable } from 'svelte/store';
import { browser } from '$app/environment';

export const prefersReducedMotion = readable(false, (set) => {
  if (!browser) return;

  const query = window.matchMedia('(prefers-reduced-motion: reduce)');
  set(query.matches);

  const handler = (e: MediaQueryListEvent) => set(e.matches);
  query.addEventListener('change', handler);

  return () => query.removeEventListener('change', handler);
});

// Usage: $prefersReducedMotion ? 0 : 200
```

### Timing Guidelines

| Type | Duration | Easing |
|------|----------|--------|
| Micro-interactions | 100-150ms | ease-out |
| Component transitions | 200-300ms | ease-in-out |
| Page transitions | 300-500ms | ease-in-out |
| Complex orchestrated | 500-1000ms | custom |

---

## Performance Perception

### Progressive Loading

1. **Critical content first** - Above-fold immediately
2. **Skeleton for structure** - Layout while loading
3. **Progressive images** - LQIP that blur-up
4. **Lazy load below fold** - Defer non-visible

### Perception Tricks

- **Keydown vs keyup** - Trigger on keydown saves ~200ms
- **Skeleton animation** - Keeps attention occupied
- **Optimistic updates** - Show result before confirmation
- **Preload on hover** - Fetch when user hovers link

---

## Summary: Modern Patterns

| Pattern | Implementation |
|---------|----------------|
| Instant feedback | Optimistic updates everywhere |
| Smooth transitions | 150-300ms CSS transitions |
| Keyboard-first | Comprehensive shortcuts (press ?) |
| Real-time presence | Live cursors, typing indicators |
| Progressive disclosure | Load more, not infinite scroll |
| Command palette | Cmd+K for quick actions |
