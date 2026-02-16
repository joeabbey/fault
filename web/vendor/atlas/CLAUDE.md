# Atlas Design System

Context for Claude Code when working with the Atlas design system.

## What is Atlas?

Atlas is a **comprehensive design system** for Svelte 5 applications with Tailwind CSS v4. It provides design tokens, utility functions, and 40+ reusable components that power Mantle, Conduit, Stratum, and future projects.

## Architecture

```
src/
├── lib/
│   ├── components/        # Svelte 5 components
│   │   ├── Button.svelte
│   │   ├── Card.svelte
│   │   └── ...
│   ├── tokens/            # Design tokens (TypeScript)
│   │   ├── colors.ts
│   │   ├── spacing.ts
│   │   ├── typography.ts
│   │   ├── shadows.ts
│   │   └── radii.ts
│   ├── styles/            # CSS
│   │   └── base.css       # Tailwind v4 @theme config
│   ├── utils/             # Utilities
│   │   └── cn.ts          # clsx + tailwind-merge
│   └── index.ts           # Public exports
├── routes/                # Demo/docs site
└── app.css                # App styles (imports base.css)
```

## Key Design Decisions

| Decision | Choice | Rationale |
|----------|--------|-----------|
| Framework | Svelte 5 | Runes, better performance |
| Styling | Tailwind v4 | CSS-first config, native CSS vars |
| Colors | OKLCH | Perceptually uniform, better for palettes |
| Primitives | Bits UI | Headless, accessible, composable |
| Utilities | clsx + tailwind-merge | Class composition without conflicts |

## Color System

Colors use **OKLCH** for perceptual uniformity:
- `primary` - Blue (brand color)
- `secondary` - Slate (neutral)
- `success` - Green
- `warning` - Amber
- `error` - Red

Each scale has shades: 50, 100, 200, 300, 400, 500, 600, 700, 800, 900, 950

Semantic colors (CSS custom properties):
- `--color-background` / `--color-foreground`
- `--color-muted` / `--color-muted-foreground`
- `--color-card` / `--color-card-foreground`
- `--color-border`, `--color-ring`, `--color-input`

## Dark Mode

Uses class-based strategy (`.dark` on html element):
- CSS custom properties switch automatically
- Components use semantic color tokens
- Toggle via: `document.documentElement.classList.toggle('dark')`

## Component Patterns

### Props with Svelte 5 Runes

```svelte
<script lang="ts">
  import { cn } from '$lib/utils/cn';

  interface Props {
    variant?: 'primary' | 'secondary';
    size?: 'sm' | 'md' | 'lg';
    class?: string;
    children: import('svelte').Snippet;
  }

  let { variant = 'primary', size = 'md', class: className, children }: Props = $props();
</script>

<button class={cn(baseClasses, variants[variant], sizes[size], className)}>
  {@render children()}
</button>
```

### Accessibility

- All interactive components support keyboard navigation
- ARIA attributes where needed
- Focus indicators via `focus-visible:ring-2`
- Reduced motion respected via `prefers-reduced-motion`

## Development Commands

```bash
npm install                 # Install dependencies
npm run dev                 # Dev server (localhost:5173)
npm run build               # Build library
npm run package             # Build npm package
npm run check               # TypeScript check
npm run test                # Run tests
```

## Adding a New Component

1. Create `src/lib/components/ComponentName.svelte`
2. Follow the props pattern with TypeScript interface
3. Use `cn()` for class composition
4. Export from `src/lib/index.ts`
5. Add demo to `src/routes/+page.svelte` or create new route

## Consuming Projects

### Installation

```bash
npm install @jabbey/atlas
```

### Usage

```svelte
<script>
  import { Button, Card } from '@jabbey/atlas';
  import '@jabbey/atlas/styles/base.css';
</script>

<Card>
  <Button variant="primary">Click me</Button>
</Card>
```

### Tailwind Setup

Import the base styles in your app.css:
```css
@import '@jabbey/atlas/styles/base.css';
@import 'tailwindcss';
```

## Testing

- Unit tests: `npm run test`
- Visual testing: Run dev server and check `/` route
- Accessibility: Use axe DevTools in browser

## File Size Guidelines

- Individual components: < 200 lines
- Keep token files focused on single concern
- Use tree-shakeable exports
