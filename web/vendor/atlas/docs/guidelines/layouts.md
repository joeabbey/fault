# Layout Patterns

Guidelines for structuring web application layouts. Reference this when building dashboards, settings pages, forms, and responsive interfaces.

---

## Dashboard Layouts

### Collapsible Sidebar + Content Area

The dominant pattern for 2025-2026. Used by Linear, Notion, Vercel.

```
+------------------+----------------------------------------+
|  SIDEBAR (224px) |  CONTENT AREA (fluid)                  |
|------------------|----------------------------------------|
|  Logo/Brand      |  Header Bar (breadcrumbs, actions)     |
|  Search (Cmd+K)  |----------------------------------------|
|  Navigation      |  Main Content                          |
|  - Sections      |  - Widgets/Cards                       |
|  - Accordion     |  - Data Tables                         |
|  User Profile    |  - Detail Views                        |
+------------------+----------------------------------------+
```

**Specifications:**

| Element | Value | Rationale |
|---------|-------|-----------|
| Sidebar width | 224px (collapsed: 64px) | Wide enough for labels, not wasteful |
| Grid system | 8px base unit | Visual consistency |
| Content area | Fluid, min 320px | Adapts to available space |
| Header height | 56px | Room for breadcrumbs + actions |

**Implementation:**

```svelte
<script>
  let sidebarCollapsed = $state(false);
</script>

<div class="flex h-screen">
  <aside class={cn(
    "flex flex-col border-r border-border bg-card transition-all",
    sidebarCollapsed ? "w-16" : "w-56"
  )}>
    <div class="p-4">
      <Logo collapsed={sidebarCollapsed} />
    </div>
    <nav class="flex-1 overflow-y-auto p-2">
      <SidebarNav collapsed={sidebarCollapsed} />
    </nav>
    <div class="p-4 border-t border-border">
      <UserMenu collapsed={sidebarCollapsed} />
    </div>
  </aside>

  <main class="flex-1 flex flex-col overflow-hidden">
    <header class="h-14 border-b border-border px-6 flex items-center">
      <slot name="header" />
    </header>
    <div class="flex-1 overflow-y-auto p-6">
      <slot />
    </div>
  </main>
</div>
```

### Navigation Principles

1. **Command palette**: `Cmd+K` for global search/commands (essential)
2. **Keyboard shortcuts**: Every action should have a keyboard equivalent
3. **Contextual actions**: Show relevant actions based on current view
4. **Breadcrumbs**: Always show location in hierarchy

---

## Settings Page Layouts

### Two-Column Settings Pattern

```
+----------------------------+----------------------------------+
|  SETTINGS NAV (240px)      |  SETTINGS CONTENT                |
|----------------------------|----------------------------------|
|  Account                   |  Section Title                   |
|  Profile                   |  Description text                |
|  Billing           [active]|----------------------------------|
|  Team                      |  Form Fields                     |
|  Integrations              |  [Save] [Cancel]                 |
+----------------------------+----------------------------------+
```

**Best Practices:**

- Fixed left nav, scrollable content area
- Role-based visibility (show only relevant sections)
- Active state indicator on current section
- Group related settings in collapsible sections
- Tooltips for complex options

---

## Form Layouts

### Single-Column Forms (Default)

Use for most forms. Multi-column increases cognitive load and errors.

```
+----------------------------------------+
|  Form Title                            |
|  Helper text explaining purpose        |
|----------------------------------------|
|  Label                                 |
|  [Input Field                    ]     |
|  Helper text                           |
|                                        |
|  Label                                 |
|  [Input Field                    ]     |
|                                        |
|  [Primary Action]  [Secondary]         |
+----------------------------------------+
```

**Guidelines:**

- Top-to-bottom flow (F-pattern reading)
- Labels above inputs, not beside
- Related fields can be inline (First Name | Last Name)
- Helper text below inputs
- Error messages inline, near the field
- Primary action on the left (or right for RTL)

### Multi-Step Wizard Pattern

Use for complex tasks with 3-5 steps. Can boost conversion 300%.

```
Step 1 ──●── Step 2 ──○── Step 3 ──○── Review

+----------------------------------------+
|  Step 2: Payment Details               |
|  Progress: ████████░░░░░░░░ 40%        |
|----------------------------------------|
|  [Form fields for this step]           |
|                                        |
|  [Back]              [Continue]        |
+----------------------------------------+
```

**When to use wizards:**
- 3-5 steps (2 is trivial, 10+ is overwhelming)
- Complex onboarding flows
- Multi-part data collection

**When NOT to use:**
- Expert users doing frequent tasks
- Simple forms with few fields
- When users need non-linear access

---

## Content Layouts

### Choosing View Types

| Pattern | Best For | Characteristics |
|---------|----------|-----------------|
| **List View** | Text-heavy, reading | F-pattern, more items visible |
| **Grid View** | Visual content, thumbnails | Image-dominated, even attention |
| **Data Table** | Comparison, analysis | Rows/columns, sortable/filterable |

### Data Table Best Practices

- Sticky headers (keep column names visible on scroll)
- Sticky first column on mobile
- Virtual scrolling for 1000+ rows
- Progressive disclosure (hide advanced filters)
- Clear empty states

---

## Responsive Patterns

### Breakpoints (2025 Standard)

```css
/* Mobile first - base styles for smallest screens */

/* Small phones */
@media (min-width: 320px) { }

/* Large phones */
@media (min-width: 480px) { }

/* Tablets */
@media (min-width: 768px) { }

/* Small desktop */
@media (min-width: 1024px) { }

/* Large desktop */
@media (min-width: 1280px) { }
```

**Principles:**
- Use 3-5 breakpoints maximum
- Let content dictate breakpoints, not devices
- Use relative units (`rem`, `em`)

### CSS Container Queries

Use for component-level responsiveness:

```css
.card-container {
  container-type: inline-size;
}

@container (min-width: 400px) {
  .card {
    display: grid;
    grid-template-columns: 150px 1fr;
  }
}
```

| Approach | Use Case |
|----------|----------|
| Media queries | Page-level layout changes |
| Container queries | Component-level adaptation |

### Mobile Adaptations

| Desktop Element | Mobile Adaptation |
|-----------------|-------------------|
| Sidebar | Hamburger menu or bottom nav |
| Multi-column form | Single column |
| Data table | Card stack or horizontal scroll |
| Grid (3-4 cols) | 1-2 columns |
| Horizontal wizard | Vertical progress |

---

## Visual Hierarchy

### The 3-Second Rule

Users form judgments in 50ms, decide to stay in 10-20 seconds.

**Hierarchy Techniques:**

| Technique | Application |
|-----------|-------------|
| Size | Larger = more important |
| Contrast | High contrast for CTAs (use sparingly) |
| Spacing | White space groups and separates |
| Position | Top-left = first seen |
| Weight | Bold for emphasis |

### Bento Grid Pattern

Modular card-based layouts for dashboards:

```
+------------------+--------+--------+
|                  |        |        |
|   Feature Card   | Stat   | Stat   |
|   (2x2)          | Card   | Card   |
|                  |        |        |
+------------------+--------+--------+
|        |         |                 |
| Card   |  Card   |  Large Card     |
|        |         |  (2x1)          |
+--------+---------+-----------------+
```

---

## Accessibility for Layouts

### Focus Management

- Visible focus indicators (3:1 contrast minimum)
- Logical tab order (top-to-bottom, left-to-right)
- Focus trapping in modals
- Return focus when modal closes
- Escape key closes overlays

### Checklist

- [ ] Don't rely on color alone for status
- [ ] Icons have text labels for screen readers
- [ ] Sticky headers don't obscure focused elements
- [ ] Skip links bypass repetitive navigation
- [ ] Touch targets minimum 44x44px on mobile

---

## Common Mistakes

| Mistake | Solution |
|---------|----------|
| Cluttered layouts | Use white space liberally |
| Non-responsive | Start mobile-first |
| Confusing navigation | Keep primary nav to 5-7 items |
| Ignoring grid | Use 8px grid consistently |
| Overstuffed headers | Keep headers minimal |
| Long forms | Multi-step if needed |

---

## Design Tokens

```css
:root {
  /* Spacing (8px grid) */
  --space-1: 0.25rem;  /* 4px */
  --space-2: 0.5rem;   /* 8px */
  --space-3: 0.75rem;  /* 12px */
  --space-4: 1rem;     /* 16px */
  --space-6: 1.5rem;   /* 24px */
  --space-8: 2rem;     /* 32px */

  /* Layout */
  --sidebar-width: 224px;
  --sidebar-collapsed: 64px;
  --header-height: 56px;
  --content-max-width: 1280px;
}
```
