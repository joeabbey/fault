# Atlas Design Guidelines

Comprehensive UX guidelines for building consistent, accessible, and user-friendly applications. These documents are designed to be referenced by AI assistants and developers when building features.

---

## How to Use These Guidelines

**For AI Context:** Add relevant guideline files to your project's CLAUDE.md or include them in prompts when building specific features.

**For Developers:** Reference these as living documentation when making design decisions.

---

## Guidelines

### [Layout Patterns](./layouts.md)
Dashboard layouts, settings pages, form layouts, responsive patterns, and visual hierarchy.

**Reference when:** Building page structures, navigation, responsive layouts.

### [User Flows](./user-flows.md)
Onboarding, authentication, error recovery, empty states, and first-run experiences.

**Reference when:** Designing user journeys, signup flows, error handling.

### [Page Templates](./page-templates.md)
Landing pages, pricing pages, documentation, marketing pages, and legal pages.

**Reference when:** Building public-facing pages, marketing sites.

### [Content Guidelines](./content.md)
Microcopy, error messages, empty states, CTAs, voice/tone, and inclusive language.

**Reference when:** Writing UI text, error messages, notifications.

### [Interaction Patterns](./interactions.md)
Loading states, optimistic updates, pagination, real-time features, keyboard navigation, animation.

**Reference when:** Building interactive features, handling async operations.

### [Billing Patterns](./billing.md)
Pricing tables, payment forms, upgrade flows, invoices, usage displays, feature gating, cancellation.

**Reference when:** Building billing features, pricing pages, subscription management.

---

## Quick Reference

### Key Metrics

| Metric | Benchmark |
|--------|-----------|
| Time-to-First-Value | <5 minutes |
| Onboarding Completion | >80% |
| Day-30 Retention | >40% |
| Trial-to-Paid | >25% |
| Page Load (FCP) | <1.8 seconds |

### Core Principles

1. **Clarity over cleverness** - Be clear, not entertaining
2. **Progressive disclosure** - Reveal complexity gradually
3. **Keyboard-first** - Every action has a shortcut
4. **Accessibility by default** - WCAG 2.1 AA minimum
5. **Transparency builds trust** - No dark patterns
6. **Respect user autonomy** - Easy in, easy out

### Design Tokens

```css
/* Spacing (8px grid) */
--space-1: 0.25rem;   /* 4px */
--space-2: 0.5rem;    /* 8px */
--space-4: 1rem;      /* 16px */
--space-6: 1.5rem;    /* 24px */
--space-8: 2rem;      /* 32px */

/* Layout */
--sidebar-width: 224px;
--header-height: 56px;
--content-max-width: 1280px;
```

### Breakpoints

```css
--bp-sm: 480px;   /* Large phones */
--bp-md: 768px;   /* Tablets */
--bp-lg: 1024px;  /* Small desktop */
--bp-xl: 1280px;  /* Large desktop */
```

---

## Related Resources

- [Atlas Components](/src/lib/components/) - 32 Svelte components
- [Design Tokens](/src/lib/styles/base.css) - CSS custom properties
- [Utility Functions](/src/lib/utils/) - cn() and helpers

---

## Contributing

These guidelines are living documents. Update them as:
- New patterns emerge
- Research reveals better practices
- User testing provides insights
- Regulations change

Last updated: February 2025
