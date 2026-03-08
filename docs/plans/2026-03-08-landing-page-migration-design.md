# Landing Page & Docs Migration — Design Doc

**Date:** 2026-03-08
**Status:** Approved

## Problem

1. **Landing page drift:** `pkg/cloud/landing.html` is a 57KB monolithic HTML file embedded in the Go binary. After development sessions, the copy (analyzer counts, feature lists, pricing) falls behind the actual product. Updating requires manually editing a massive HTML file.

2. **No docs page:** The "Docs" nav link points to the GitHub README. Users have no way to browse documentation on the site itself. Markdown docs exist at `fault/docs/*.md` but are not rendered anywhere in the web app.

## Solution

Move the landing page into SvelteKit using reusable Atlas components. Add a docs route that renders existing markdown. Extract product data into typed TypeScript data files so the landing page stays current when features change.

## Architecture

### What changes where

| Layer | Change |
|-------|--------|
| **Atlas** | Add 6 marketing components: Hero, StatsBar, FeatureGrid, ComparisonTable, StepsList, DocsRenderer |
| **Fault web** | New `/` route (landing), `/docs/[slug]` route (docs), product data files, public route guard update |
| **Fault Go** | Delete `//go:embed landing.html`, `handleLandingPage`, `GET /{$}` route. SPA catch-all takes over. |

### Replayability

Atlas components are content-agnostic. Conduit and Mantle get the same landing page and docs patterns by:
1. Installing the updated Atlas
2. Creating their own `src/lib/data/` files with product-specific content
3. Adding `/` and `/docs/[slug]` routes that compose Atlas components with their data

## Atlas Components (6 new)

| Component | Props | What it renders |
|-----------|-------|-----------------|
| `Hero` | `badge`, `headline`, `highlightWord`, `subheadline`, `installCommand`, `ctaSubtext`, `terminalLines[]`, `terminalTitle` | Two-column hero with animated terminal preview and copy-to-clipboard |
| `StatsBar` | `stats: {value: string, label: string}[]` | Horizontal row of large gradient numbers |
| `FeatureGrid` | `label`, `headline`, `features: {icon?, title, description, badge?}[]` | Auto-fill card grid with optional badges |
| `ComparisonTable` | `label`, `headline`, `description`, `columns: string[]`, `rows: {check: string, values: ('yes'\|'no'\|'partial'\|string)[]}[]` | Color-coded comparison table |
| `StepsList` | `label`, `headline`, `steps: {title: string, description: string}[]` | Numbered vertical steps with connector lines |
| `DocsRenderer` | `content` (HTML string), `title`, `sidebar: {label: string, href: string}[]` | Docs layout with sidebar nav |

Existing Atlas components reused directly: `Navbar`, `Button`, `Badge`, `Card`, `PricingSection`.

## Fault Web Changes

### New routes

```
/              → Public landing page
/docs          → Public docs index
/docs/[slug]   → Public individual doc page
/login         → Public auth (existing)
/dashboard/*   → Private (existing)
```

### Route guard update

Add `'/'` and `'/docs'` to `publicRoutes` in `+layout.svelte`. Paths starting with `/docs/` are also public.

### Data files (`src/lib/data/`)

| File | Content |
|------|---------|
| `languages.ts` | 42 `{name, color}` objects |
| `analyzers.ts` | 17 `{emoji, name, version?, description}` objects |
| `features.ts` | 16 `{icon?, title, description, badge?}` objects |
| `comparison.ts` | 13 comparison rows |
| `pricing.ts` | 3 tier objects for PricingSection |
| `docs.ts` | Doc manifest: `{slug, title, description}` per markdown file |

### Landing page composition

`src/routes/+page.svelte` imports Atlas components and data files. No hardcoded copy in the template. Update a data file → landing page reflects it automatically.

### Docs rendering

Markdown files from `fault/docs/` are copied into `static/docs/` at build time (via a build script or Makefile target). The `/docs/[slug]` route fetches the markdown, renders it to HTML client-side, and passes it to the `DocsRenderer` component.

## Go Server Changes

In `pkg/cloud/server.go`:
- Delete `//go:embed landing.html`
- Delete `var landingPage []byte`
- Delete `handleLandingPage` function
- Delete `s.mux.HandleFunc("GET /{$}", handleLandingPage)` route
- The existing SPA catch-all (`handleSPA`) serves `WebDir/index.html` for `/`, which loads SvelteKit's client-side router

## Migration Checklist

- [ ] Build Atlas components, package, vendor into Fault
- [ ] Extract landing page content into data files
- [ ] Build `/` route composing Atlas components
- [ ] Build `/docs/[slug]` route with markdown rendering
- [ ] Update route guard for public routes
- [ ] Remove Go-served landing page
- [ ] Verify landing page renders identically to current
- [ ] Verify docs pages render correctly
- [ ] Test auth flow still works (public → login → dashboard)
