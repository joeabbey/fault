# Landing Page & Docs Migration — Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Move Fault's landing page from Go-embedded HTML into SvelteKit + Atlas components, add docs rendering, and make the components reusable for Conduit and Mantle.

**Architecture:** Six new Atlas marketing components receive product data via props. Fault's web app gets new public routes (`/`, `/docs/[slug]`) that compose these components with typed data files. Go server drops the embedded landing page; SPA catch-all takes over.

**Tech Stack:** Svelte 5, Tailwind v4, Atlas design system, TypeScript, Go

**Repos:**
- Atlas: `/home/jabbey/orbit/atlas/`
- Fault: `/home/jabbey/orbit/fault/`

**Important context:**
- Always run `source ~/.nvm/nvm.sh && nvm use 22` before any npm commands
- Atlas uses Svelte 5 runes (`$props()`, `$state()`, `$derived()`, `$effect()`)
- Atlas components use `cn()` from `$lib/utils/cn` for class composition
- All components follow the pattern in `Button.svelte`: TypeScript `interface Props`, `let { ...}: Props = $props()`, Tailwind classes
- Fault vendors Atlas at `web/vendor/atlas/` via `file:./vendor/atlas` in package.json
- The existing `landing.html` is the authoritative source for all content — use it, not `docs/landing-page.md`

---

## Phase 1: Atlas Marketing Components

These 6 tasks are independent and can run in parallel.

### Task 1: Hero Component

**Files:**
- Create: `atlas/src/lib/components/Hero.svelte`
- Modify: `atlas/src/lib/components/index.ts` (add export)
- Modify: `atlas/src/lib/index.ts` (add to component exports)

**Step 1: Create the Hero component**

Create `src/lib/components/Hero.svelte`. This is a two-column hero section with:
- Left: optional badge pill (with pulsing dot), headline with one highlighted gradient word, subheadline, install command with copy button, CTA subtext
- Right: animated terminal preview with staggered line-by-line reveal

Props interface:
```typescript
interface Props {
  badge?: string;
  headline: string;
  highlightWord?: string;
  subheadline: string;
  installCommand?: string;
  ctaSubtext?: string;
  terminalTitle?: string;
  terminalLines?: { text: string; type?: 'command' | 'info' | 'error' | 'warning' | 'success' | 'summary'; delay?: number }[];
  class?: string;
  children?: import('svelte').Snippet;
}
```

Layout: CSS grid `1.2fr 1fr`, collapse to single column at `lg:` breakpoint. The terminal preview uses staggered CSS `animation-delay` on each line (opacity 0→1, translateY 6px→0). The copy button uses `navigator.clipboard.writeText()` with a 2s "Copied!" state via `$state()`.

Use Tailwind classes and semantic Atlas tokens. The gradient highlight on the keyword uses `bg-gradient-to-r from-primary-500 to-warning-500 bg-clip-text text-transparent`. The pulsing badge dot uses a CSS keyframe animation.

Reference the hero section (lines 1-180 approximately) of `/home/jabbey/orbit/fault/pkg/cloud/landing.html` for the exact visual treatment, but implement it as a generic Svelte component that works with any product's theme.

**Step 2: Export the component**

Add `Hero` to the default export in `src/lib/components/index.ts` and to `src/lib/index.ts` under a new `// Marketing` section.

**Step 3: Verify it builds**

```bash
source ~/.nvm/nvm.sh && nvm use 22
cd /home/jabbey/orbit/atlas
npm run package
```

Expected: Build succeeds, `dist/components/Hero.svelte` exists.

**Step 4: Commit**

```bash
git add src/lib/components/Hero.svelte src/lib/components/index.ts src/lib/index.ts
git commit -m "Add Hero marketing component"
```

---

### Task 2: StatsBar Component

**Files:**
- Create: `atlas/src/lib/components/StatsBar.svelte`
- Modify: `atlas/src/lib/components/index.ts`
- Modify: `atlas/src/lib/index.ts`

**Step 1: Create the component**

Props interface:
```typescript
interface Props {
  stats: { value: string; label: string }[];
  class?: string;
}
```

Renders a full-width section with a responsive grid (auto-fill columns). Each stat has a large gradient number (same gradient as Hero highlight: primary→warning) in display font, with a smaller mono label beneath. Grid collapses to 2-col at `sm:` breakpoint.

Reference the stats bar section of `landing.html` for the visual treatment.

**Step 2: Export, build, commit**

Same pattern as Task 1. Add to exports, run `npm run package`, commit with message "Add StatsBar marketing component".

---

### Task 3: FeatureGrid Component

**Files:**
- Create: `atlas/src/lib/components/FeatureGrid.svelte`
- Modify: `atlas/src/lib/components/index.ts`
- Modify: `atlas/src/lib/index.ts`

**Step 1: Create the component**

Props interface:
```typescript
interface Props {
  label?: string;
  headline: string;
  description?: string;
  features: { icon?: string; title: string; description: string; badge?: string; badgeVariant?: 'default' | 'warning' | 'success' }[];
  columns?: 1 | 2 | 3;
  class?: string;
}
```

Renders a section with label (small uppercase muted text), headline (h2), description, then an auto-fill card grid. Each card has:
- Optional emoji icon (top-left)
- Optional badge (top-right, using Atlas `Badge` component)
- Title in bold
- Description in muted text
- Hover effect: border color transitions to `border-hover` token

Default columns: 2. Grid uses `grid-template-columns: repeat(auto-fill, minmax(280px, 1fr))` for responsive behavior.

This component is used for features, analyzers, and any card grid. Reference the features and analyzers sections of `landing.html`.

**Step 2: Export, build, commit**

Message: "Add FeatureGrid marketing component"

---

### Task 4: ComparisonTable Component

**Files:**
- Create: `atlas/src/lib/components/ComparisonTable.svelte`
- Modify: `atlas/src/lib/components/index.ts`
- Modify: `atlas/src/lib/index.ts`

**Step 1: Create the component**

Props interface:
```typescript
interface Props {
  label?: string;
  headline: string;
  description?: string;
  columns: string[];
  rows: { check: string; values: string[] }[];
  class?: string;
}
```

Renders a section with label/headline/description, then a styled table. Values are color-coded:
- Contains "Yes" → success color (green)
- Contains "No" → dim text color
- Contains "Partial" → warning color (yellow)
- Other strings → default text

Table has rounded corners, border, subtle row hover. Responsive: on mobile, consider horizontal scroll or stacked layout.

Reference the comparison table in `landing.html`.

**Step 2: Export, build, commit**

Message: "Add ComparisonTable marketing component"

---

### Task 5: StepsList Component

**Files:**
- Create: `atlas/src/lib/components/StepsList.svelte`
- Modify: `atlas/src/lib/components/index.ts`
- Modify: `atlas/src/lib/index.ts`

**Step 1: Create the component**

Props interface:
```typescript
interface Props {
  label?: string;
  headline: string;
  steps: { title: string; description: string }[];
  class?: string;
}
```

Renders a vertical numbered list with connector lines. Each step has:
- A numbered circle (1, 2, 3...) with primary color background
- A vertical connector line (2px, primary at low opacity) between circles
- Step title in bold
- Step description in muted text

Reference the "How it works" section of `landing.html`.

**Step 2: Export, build, commit**

Message: "Add StepsList marketing component"

---

### Task 6: DocsRenderer Component

**Files:**
- Create: `atlas/src/lib/components/DocsRenderer.svelte`
- Modify: `atlas/src/lib/components/index.ts`
- Modify: `atlas/src/lib/index.ts`

**Step 1: Create the component**

Props interface:
```typescript
interface Props {
  title: string;
  content: string; // HTML string from rendered markdown
  sidebar?: { label: string; href: string; active?: boolean }[];
  class?: string;
}
```

Renders a docs page layout:
- Optional sidebar (left, 240px, sticky) with nav links. Active link highlighted with primary color.
- Main content area with the `{@html content}` rendered markdown
- Content area has prose-like styling: heading sizes, code blocks (mono font, surface background), inline code, links (primary color), lists, tables, blockquotes
- Responsive: sidebar collapses to top nav on mobile

Style the rendered HTML content using Tailwind's `prose` classes or equivalent scoped styles targeting `h1`-`h6`, `p`, `code`, `pre`, `a`, `ul`, `ol`, `table`, `blockquote` within the content container.

**Step 2: Export, build, commit**

Message: "Add DocsRenderer marketing component"

---

### Task 7: Package and Vendor Atlas

**Files:**
- Modify: `atlas/src/lib/components/index.ts` (if not done already)
- Modify: `atlas/src/lib/index.ts` (if not done already)

**Step 1: Verify all 6 components export correctly**

Check that `src/lib/components/index.ts` exports:
```typescript
export { default as Hero } from './Hero.svelte';
export { default as StatsBar } from './StatsBar.svelte';
export { default as FeatureGrid } from './FeatureGrid.svelte';
export { default as ComparisonTable } from './ComparisonTable.svelte';
export { default as StepsList } from './StepsList.svelte';
export { default as DocsRenderer } from './DocsRenderer.svelte';
```

And `src/lib/index.ts` includes them under `// Marketing`:
```typescript
// Marketing
Hero,
StatsBar,
FeatureGrid,
ComparisonTable,
StepsList,
DocsRenderer,
```

**Step 2: Build the package**

```bash
source ~/.nvm/nvm.sh && nvm use 22
cd /home/jabbey/orbit/atlas
npm run package
```

**Step 3: Vendor into Fault**

```bash
cd /home/jabbey/orbit
make vendor
```

This rsyncs Atlas into `fault/web/vendor/atlas/`.

**Step 4: Verify Fault can import the new components**

```bash
cd /home/jabbey/orbit/fault/web
source ~/.nvm/nvm.sh && nvm use 22
npm install
npm run check
```

Expected: No TypeScript errors about missing components.

**Step 5: Commit Atlas**

```bash
cd /home/jabbey/orbit/atlas
git add -A
git commit -m "Add 6 marketing components: Hero, StatsBar, FeatureGrid, ComparisonTable, StepsList, DocsRenderer"
```

---

## Phase 2: Fault Data Files and Routes

Tasks 8-9 can start after Phase 1 vendoring is complete. Tasks 8a-8f (data files) can run in parallel.

### Task 8: Extract Product Data Files

**Files:**
- Create: `fault/web/src/lib/data/languages.ts`
- Create: `fault/web/src/lib/data/analyzers.ts`
- Create: `fault/web/src/lib/data/features.ts`
- Create: `fault/web/src/lib/data/comparison.ts`
- Create: `fault/web/src/lib/data/pricing.ts`
- Create: `fault/web/src/lib/data/steps.ts`
- Create: `fault/web/src/lib/data/integrations.ts`
- Create: `fault/web/src/lib/data/docs.ts`

**Step 1: Create all data files**

Extract the exact content from `fault/pkg/cloud/landing.html` into typed TypeScript arrays. Each file exports a single typed const array.

`languages.ts`:
```typescript
export const languages = [
  { name: 'Go', color: '#00ADD8' },
  { name: 'TypeScript', color: '#3178C6' },
  // ... all 42 from landing.html
] as const;
```

`analyzers.ts`:
```typescript
export interface Analyzer {
  emoji: string;
  name: string;
  description: string;
  version?: string;
}
export const analyzers: Analyzer[] = [
  { emoji: '🔗', name: 'imports', description: 'Broken imports, missing modules' },
  // ... all 17 from landing.html
];
```

`features.ts`: All 16 feature cards with icon, title, description, optional badge/badgeVariant.

`comparison.ts`: All 13 comparison rows.

`pricing.ts`: 3 tier objects with name, price, description, features array, highlighted boolean.

`steps.ts`: 4 "How it works" steps.

`integrations.ts`: 6 integration tool names.

`docs.ts`: Manifest of available docs:
```typescript
export const docs = [
  { slug: 'getting-started', title: 'Getting Started', description: 'Install and configure Fault' },
  { slug: 'claude-code-integration', title: 'Claude Code Integration', description: 'Use Fault with Claude Code' },
  { slug: 'aider-integration', title: 'aider Integration', description: 'Use Fault with aider' },
  { slug: 'github-actions', title: 'GitHub Actions', description: 'CI/CD with Fault' },
  { slug: 'languages', title: 'Language Support', description: 'Supported languages and parsers' },
] as const;
```

**CRITICAL:** Use the exact content from `landing.html`, not from the outdated `docs/landing-page.md`. The HTML is the authoritative source. Read it carefully and extract every item.

**Step 2: Commit**

```bash
cd /home/jabbey/orbit/fault
git add web/src/lib/data/
git commit -m "Extract landing page content into typed data files"
```

---

### Task 9: Landing Page Route

**Files:**
- Create: `fault/web/src/routes/(public)/+layout.svelte`
- Create: `fault/web/src/routes/(public)/+page.svelte`
- Modify: `fault/web/src/routes/+layout.svelte` (route guard update)
- Delete: `fault/web/src/routes/+page.svelte` (old redirect-to-dashboard)

**Step 1: Create the public route group**

SvelteKit route groups `(public)` don't affect the URL path. Create `src/routes/(public)/+layout.svelte` — this is a minimal layout for public pages (no sidebar, no auth required). It renders just the page content with no DashboardLayout wrapper:

```svelte
<script lang="ts">
  let { children } = $props();
</script>

{@render children()}
```

**Step 2: Create the landing page**

Create `src/routes/(public)/+page.svelte`. This composes Atlas marketing components with the data files:

```svelte
<script lang="ts">
  import { Hero, StatsBar, FeatureGrid, ComparisonTable, StepsList, Navbar, Badge, Button } from '@jabbey/atlas';
  import { PricingSection } from '@jabbey/atlas/templates';
  import { languages } from '$lib/data/languages';
  import { analyzers } from '$lib/data/analyzers';
  import { features } from '$lib/data/features';
  import { comparison } from '$lib/data/comparison';
  import { pricing } from '$lib/data/pricing';
  import { steps } from '$lib/data/steps';
  import { integrations } from '$lib/data/integrations';
</script>

<svelte:head>
  <title>Fault - Catch AI agent mistakes before they hit your codebase</title>
  <meta name="description" content="Fault validates multi-file changes from Claude Code, aider, and Cursor — 16 analyzers plus custom rules across 42 languages." />
  <meta property="og:title" content="Fault - Catch AI agent mistakes before they hit your codebase" />
  <meta property="og:description" content="Pre-commit validation for AI-generated code. 42 languages, works offline, free forever." />
  <meta property="og:type" content="website" />
  <meta property="og:url" content="https://fault.jabbey.io" />
  <meta name="twitter:card" content="summary_large_image" />
  <meta name="twitter:title" content="Fault - Catch AI agent mistakes before they hit your codebase" />
  <meta name="twitter:description" content="Pre-commit validation for AI-generated code. 42 languages, works offline, free forever." />
</svelte:head>
```

Then compose the sections in order matching the current landing page:
1. Navbar (GitHub, Docs, Dashboard links)
2. Hero (with terminal preview lines from data)
3. StatsBar (4 stats)
4. Problem section (can be a simple section with headline + bulleted list — use `FeatureGrid` or inline markup)
5. Languages section (pill grid — may need inline markup or a simple `{#each}` over languages)
6. Analyzers section (FeatureGrid)
7. Features section (FeatureGrid)
8. How It Works (StepsList)
9. Comparison (ComparisonTable)
10. Integrations (pill tags)
11. Pricing (PricingSection from Atlas templates)
12. Final CTA (install command block)
13. Footer

Some sections (problem bullets, language pills, integrations pills, final CTA, footer) are simple enough to be inline markup rather than Atlas components. Don't over-abstract — if it's 10 lines of HTML, just write it inline.

**Step 3: Update the route guard**

In `src/routes/+layout.svelte`, change line 13:
```typescript
// Before:
const publicRoutes = ['/login'];

// After:
const publicRoutes = ['/login', '/docs'];
```

The `(public)` route group means `/` renders via the public layout. The root layout's route guard logic uses `pathname.startsWith(route)` — but `/` would match everything. Instead, update the guard to also check if the route is exactly `/`:

```typescript
const publicRoutes = ['/login', '/docs'];

// In the $effect:
const isPublicRoute = pathname === '/' || publicRoutes.some((route) => pathname.startsWith(route));
```

**Step 4: Delete the old root page**

The old `src/routes/+page.svelte` (which just did `goto('/dashboard')`) is replaced by the `(public)/+page.svelte`. Delete it.

**Step 5: Handle authenticated users at /**

When an authenticated user visits `/`, they should see the landing page (not be redirected to dashboard). The current guard redirects authenticated users from `/login` to `/dashboard` — that behavior stays. But `/` is public for everyone.

**Step 6: Verify it builds**

```bash
source ~/.nvm/nvm.sh && nvm use 22
cd /home/jabbey/orbit/fault/web
npm run build
```

**Step 7: Commit**

```bash
cd /home/jabbey/orbit/fault
git add web/src/routes/
git commit -m "Add SvelteKit landing page using Atlas components"
```

---

### Task 10: Docs Routes

**Files:**
- Create: `fault/web/src/routes/(public)/docs/+page.svelte`
- Create: `fault/web/src/routes/(public)/docs/[slug]/+page.svelte`
- Create: `fault/web/src/routes/(public)/docs/[slug]/+page.ts`
- Create: `fault/web/static/docs/` (copy markdown files)
- Create: `fault/Makefile` target or build script to copy docs

**Step 1: Copy markdown docs to static directory**

Add a Makefile target or npm script that copies `fault/docs/*.md` (excluding `plans/` and `landing-page.md`) into `fault/web/static/docs/`:

```bash
mkdir -p web/static/docs
cp docs/getting-started.md docs/claude-code-integration.md docs/aider-integration.md docs/github-actions.md docs/languages.md web/static/docs/
```

Add this to the Fault Makefile as a `build-docs` target, and call it before `npm run build` in the web build.

**Step 2: Create docs index page**

`src/routes/(public)/docs/+page.svelte` — lists all available docs using the `docs.ts` manifest. Simple card grid linking to each doc. Uses the same Navbar as the landing page.

**Step 3: Create docs slug page**

`src/routes/(public)/docs/[slug]/+page.ts` — load function that fetches `/docs/{slug}.md` from the static directory:

```typescript
export async function load({ params, fetch }) {
  const response = await fetch(`/docs/${params.slug}.md`);
  if (!response.ok) throw new Error('Doc not found');
  const markdown = await response.text();
  return { slug: params.slug, markdown };
}
```

`src/routes/(public)/docs/[slug]/+page.svelte` — renders the markdown using the Atlas `DocsRenderer` component. For markdown→HTML conversion, use a lightweight client-side markdown parser. Options:
- `marked` (popular, small) — add as a dependency: `npm install marked`
- Or use a simple regex-based approach for basic markdown

```svelte
<script lang="ts">
  import { DocsRenderer, Navbar } from '@jabbey/atlas';
  import { marked } from 'marked';
  import { docs } from '$lib/data/docs';

  let { data } = $props();

  const html = marked.parse(data.markdown);
  const sidebar = docs.map(d => ({
    label: d.title,
    href: `/docs/${d.slug}`,
    active: d.slug === data.slug
  }));
  const currentDoc = docs.find(d => d.slug === data.slug);
</script>

<svelte:head>
  <title>{currentDoc?.title ?? 'Docs'} - Fault</title>
</svelte:head>

<!-- Same Navbar as landing page -->
<!-- DocsRenderer with sidebar and content -->
<DocsRenderer title={currentDoc?.title ?? ''} content={html} {sidebar} />
```

**Step 4: Install marked**

```bash
source ~/.nvm/nvm.sh && nvm use 22
cd /home/jabbey/orbit/fault/web
npm install marked
```

**Step 5: Verify build**

```bash
npm run build
```

**Step 6: Commit**

```bash
cd /home/jabbey/orbit/fault
git add web/src/routes/ web/static/docs/ web/package.json web/package-lock.json Makefile
git commit -m "Add docs pages with markdown rendering"
```

---

## Phase 3: Go Server Cleanup

### Task 11: Remove Go-Served Landing Page

**Files:**
- Modify: `fault/pkg/cloud/server.go`
- Delete: `fault/pkg/cloud/landing.html`

**Step 1: Remove the landing page handler**

In `pkg/cloud/server.go`:
1. Delete line `//go:embed landing.html` and `var landingPage []byte`
2. Delete the `handleLandingPage` function (lines 265-269)
3. Delete the route registration `s.mux.HandleFunc("GET /{$}", handleLandingPage)` (line 253)

The SPA catch-all at `s.mux.HandleFunc("GET /", s.spaHandler())` will now serve `index.html` for `/`, which loads SvelteKit's router, which renders the landing page.

**Step 2: Delete landing.html**

```bash
rm fault/pkg/cloud/landing.html
```

**Step 3: Verify Go builds**

```bash
cd /home/jabbey/orbit/fault
make build-cloud
```

Expected: Build succeeds. The `landing.html` embed is gone so no reference to it should remain.

**Step 4: Commit**

```bash
git add pkg/cloud/server.go
git rm pkg/cloud/landing.html
git commit -m "Remove Go-served landing page, SvelteKit handles root route"
```

---

## Phase 4: Integration Testing

### Task 12: End-to-End Verification

**Step 1: Build everything**

```bash
source ~/.nvm/nvm.sh && nvm use 22

# Build Atlas
cd /home/jabbey/orbit/atlas && npm run package

# Vendor into Fault
cd /home/jabbey/orbit && make vendor

# Build Fault web
cd /home/jabbey/orbit/fault/web && npm install && npm run build

# Build Fault Go
cd /home/jabbey/orbit/fault && make build-cloud
```

**Step 2: Verify visually**

Start the dev server and verify each page:
```bash
cd /home/jabbey/orbit/fault/web
npm run dev
```

Check:
- [ ] `/` shows the landing page with all sections
- [ ] Landing page content matches the old `landing.html` (hero, stats, problem, languages, analyzers, features, how it works, comparison, integrations, pricing, CTA, footer)
- [ ] Terminal preview animation works
- [ ] Copy button works on install command
- [ ] "Dashboard" nav link goes to `/login` (or `/dashboard` if authed)
- [ ] "Docs" nav link goes to `/docs`
- [ ] "GitHub" nav link opens GitHub repo
- [ ] `/docs` shows docs index with all doc links
- [ ] `/docs/getting-started` renders the markdown correctly
- [ ] `/docs/[slug]` sidebar navigation works
- [ ] `/login` still works (auth flow unbroken)
- [ ] Authenticated users see dashboard at `/dashboard`
- [ ] OG meta tags present in page source

**Step 3: Commit any fixes**

**Step 4: Final commit**

```bash
cd /home/jabbey/orbit/fault
git add -A
git commit -m "Landing page migration complete: SvelteKit + Atlas components"
```

---

## Agent Team Assignment

| Task | Agent | Can Parallelize With |
|------|-------|---------------------|
| Task 1 (Hero) | atlas-hero | Tasks 2-6, Task 8 |
| Task 2 (StatsBar) | atlas-stats | Tasks 1, 3-6, Task 8 |
| Task 3 (FeatureGrid) | atlas-features | Tasks 1-2, 4-6, Task 8 |
| Task 4 (ComparisonTable) | atlas-comparison | Tasks 1-3, 5-6, Task 8 |
| Task 5 (StepsList) | atlas-steps | Tasks 1-4, 6, Task 8 |
| Task 6 (DocsRenderer) | atlas-docs | Tasks 1-5, Task 8 |
| Task 7 (Package + Vendor) | atlas-vendor | — (needs 1-6 done) |
| Task 8 (Data files) | fault-data | Tasks 1-6 |
| Task 9 (Landing route) | fault-landing | — (needs 7, 8 done) |
| Task 10 (Docs routes) | fault-docs | — (needs 7, 8 done) |
| Task 11 (Go cleanup) | fault-go | — (needs 9, 10 done) |
| Task 12 (Integration) | fault-verify | — (needs all done) |

**Execution order:**
1. Tasks 1-6 + Task 8 (all parallel — Atlas components + Fault data files)
2. Task 7 (package Atlas, vendor into Fault)
3. Tasks 9-10 (parallel — landing page + docs routes)
4. Task 11 (Go cleanup)
5. Task 12 (integration testing)
