# Page Templates

Guidelines for landing pages, pricing pages, documentation, and marketing pages. Reference this when building public-facing pages.

---

## Landing Pages

### Hero Section

The most critical real estate. Users form opinions in 2.6 seconds, spend 57% of time above the fold.

```
+─────────────────────────────────────────────────────────────────+
│  Navigation: Logo | Links (3-5 max) | CTA Button               │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  Headline (H1): Clear value proposition, 6-12 words             │
│                                                                 │
│  Subheadline: Expand on benefit, 15-25 words                    │
│                                                                 │
│  [Primary CTA Button]  [Secondary CTA - optional]               │
│                                                                 │
│  Social Proof: "Trusted by 10,000+ companies"                   │
│  [Logo] [Logo] [Logo] [Logo] [Logo]                             │
│                                                                 │
│  Hero Visual: Screenshot, demo video, or illustration           │
│                                                                 │
+─────────────────────────────────────────────────────────────────+
```

**Conversion data:**
- Single CTA focus: Multiple offers decrease conversions by 266%
- Removing navigation increases conversions 16-28%
- Pages under 1 second load bring 2.5-5x more conversions

### Feature Showcase

Transform features into benefits:

**Instead of:** "Advanced encryption"
**Write:** "Experience peace of mind with bank-level security"

```
+─────────────────────────────────────────────────────────────────+
│  "Everything you need to [achieve outcome]"                     │
├─────────────────────────────────────────────────────────────────┤
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐              │
│  │   Icon      │  │   Icon      │  │   Icon      │              │
│  │   Title     │  │   Title     │  │   Title     │              │
│  │   Benefit   │  │   Benefit   │  │   Benefit   │              │
│  │   (2 lines) │  │   (2 lines) │  │   (2 lines) │              │
│  └─────────────┘  └─────────────┘  └─────────────┘              │
+─────────────────────────────────────────────────────────────────+
```

### Social Proof

Testimonials increase conversions by 34%.

**Options:**

1. **Logo Wall** - B2B with recognizable clients
   ```
   "Trusted by teams at"
   [Stripe] [Notion] [Linear] [Vercel]
   ```

2. **Testimonial Cards**
   ```
   "Quote highlighting specific benefit"
   [Photo] Name, Title @ Company
   ```

3. **Stats Bar**
   ```
   [ 50,000+ users ] [ 99.9% uptime ] [ 4.9/5 rating ]
   ```

**Placement:**
- Near CTAs on product pages
- Beside pricing information
- At checkout to reduce abandonment
- Don't hide all social proof at the bottom

### Interactive Demo

Modern buyers want to try before engaging sales.

```
+─────────────────────────────────────────────────────────────────+
│  "See [Product] in action"                                      │
│                                                                 │
│  ┌─────────────────────────────────────────────────────────┐    │
│  │              Interactive Product Demo                   │    │
│  │              or Embedded Video                          │    │
│  └─────────────────────────────────────────────────────────┘    │
│                                                                 │
│  [Try free - no credit card required]                           │
+─────────────────────────────────────────────────────────────────+
```

---

## Pricing Pages

### Tier Structure

**3-4 tiers is optimal.** More causes decision paralysis.

```
+─────────────────────────────────────────────────────────────────+
│  "Simple, transparent pricing"                                  │
│                                                                 │
│  [Monthly ○ ] [● Annual - Save 20%]                             │
├─────────────────────────────────────────────────────────────────┤
│  ┌──────────┐  ┌──────────────┐  ┌──────────┐                   │
│  │  FREE    │  │ PRO ★        │  │ ENTERPRISE│                  │
│  │          │  │ Most Popular │  │           │                  │
│  │  $0/mo   │  │  $29/mo      │  │ Contact   │                  │
│  │          │  │              │  │           │                  │
│  │  ✓ 3     │  │  ✓ Unlimited │  │  ✓ Custom │                  │
│  │  ✓ Basic │  │  ✓ Advanced  │  │  ✓ SSO    │                  │
│  │  ✓ Email │  │  ✓ Priority  │  │  ✓ SLA    │                  │
│  │          │  │              │  │           │                  │
│  │ [Start]  │  │ [Start Free] │  │ [Contact] │                  │
│  └──────────┘  └──────────────┘  └──────────┘                   │
+─────────────────────────────────────────────────────────────────+
```

**Best practices:**
- Highlight recommended plan with badge/border
- Show savings for annual clearly
- Keep feature lists to 3-5 points per tier
- Use comparison table for detailed features

### Feature Comparison Matrix

```
+─────────────────────────────────────────────────────────────────+
│  Feature               │ Free  │ Pro   │ Enterprise            │
├─────────────────────────────────────────────────────────────────┤
│  Users                 │ 3     │ 10    │ Unlimited             │
│  Storage               │ 1GB   │ 50GB  │ Unlimited             │
│  API Access            │ ✓     │ ✓     │ ✓                     │
│  SSO                   │ ─     │ ─     │ ✓                     │
│  Priority Support      │ ─     │ ✓     │ ✓                     │
+─────────────────────────────────────────────────────────────────+
```

### Interactive Elements

- Toggle for monthly/annual
- Dropdowns for team size
- Expandable sections for features
- Tooltips for explanations
- Calculator for usage-based pricing

### FAQ Section

```
▶ Can I change plans later?
▶ What payment methods do you accept?
▶ Is there a free trial?
▶ What happens if I exceed limits?
▶ Do you offer refunds?
```

---

## Documentation Pages

### Navigation Pattern

```
+─────────────────────────────────────────────────────────────────+
│  [Logo]  [Search____________________]  [GitHub] [Discord]       │
├──────────┬──────────────────────────────────────────────────────┤
│  Getting │  Breadcrumbs: Docs > API > Authentication            │
│  Started │                                                      │
│  > Quick │  # Authentication                        [On Page]   │
│    Start │                                          ├ Overview  │
│  > Setup │  Learn how to authenticate with          ├ API Keys  │
│          │  the API using API keys or OAuth.        ├ OAuth     │
│  API     │                                          └ Examples  │
│  > Auth  │  ## Overview                                         │
│  > Users │                                                      │
│  > Data  │  ```javascript                                       │
│          │  const client = new API({ key });                    │
│  Guides  │  ```                                                 │
│  > ...   │                                                      │
│          │  [◀ Previous: Setup]  [Next: Users ▶]                │
+──────────┴──────────────────────────────────────────────────────+
```

### Key Features

- **Search-first**: Large search box, Cmd+K shortcut
- **Dark/light mode**: Toggle for preference
- **Code tabs**: Switch between languages
- **Copy buttons**: One-click code copying
- **On-page navigation**: Jump to sections
- **Previous/Next**: Navigate linearly

### Code Examples

```
┌─────────────────────────────────────────────────────────────────┐
│  [JavaScript ▼]  [Python]  [Go]  [cURL]                         │
├─────────────────────────────────────────────────────────────────┤
│  // Create a new user                                      [📋] │
│  const user = await client.users.create({                       │
│    email: 'user@example.com',                                   │
│    name: 'Jane Doe'                                             │
│  });                                                            │
└─────────────────────────────────────────────────────────────────┘
```

---

## Marketing Pages

### About Us Page

```
+─────────────────────────────────────────────────────────────────+
│  "Our Mission"                                                  │
│  One sentence mission statement                                 │
├─────────────────────────────────────────────────────────────────┤
│  [Hero image/video of team]                                     │
├─────────────────────────────────────────────────────────────────┤
│  "Our Story" - Origin narrative (4 sentences per paragraph)     │
│                                                                 │
│  Timeline:                                                      │
│  2020 ──●── 2021 ──●── 2022 ──●── 2023 ──●── 2024              │
│  Founded  Series A   1M users  Global     IPO                   │
├─────────────────────────────────────────────────────────────────┤
│  "Our Values"                                                   │
│  [Icon] Value 1    [Icon] Value 2    [Icon] Value 3             │
├─────────────────────────────────────────────────────────────────┤
│  "Meet the Team"                                                │
│  [Photo] [Photo] [Photo] [Photo]                                │
├─────────────────────────────────────────────────────────────────┤
│  Trust indicators: [Forbes] [TechCrunch] [YC]                   │
+─────────────────────────────────────────────────────────────────+
```

### Careers Page

- Video hero with real employees
- Benefits grid (Remote, Equity, Health, Learning)
- Employee testimonials
- Filterable job listings by team/location
- DEI commitment with transparency stats

### Blog Layout

- Featured post with large hero image
- Grid of latest posts (image, category, title, read time)
- Newsletter signup
- Category filtering

---

## Legal Pages

### Best Practices

- High-level summary at top
- "Last updated" date prominently
- "What's new" section for changes
- Table of contents for navigation
- Plain language translations alongside legal text

### Structure

```
+─────────────────────────────────────────────────────────────────+
│  Privacy Policy                                                 │
│  Last updated: January 15, 2025                                 │
├──────────┬──────────────────────────────────────────────────────┤
│  Table   │  ## Summary                                          │
│  of      │  [Plain language - 2-3 sentences]                    │
│  Contents│                                                      │
│          │  ## What's New                                       │
│  Summary │  • Added AI data usage section                       │
│  What's  │  • Updated retention periods                         │
│  New     │                                                      │
│  Data    │  ## 1. Data We Collect                               │
│  ...     │                                                      │
│          │  In plain English: We collect email, name,           │
│          │  and usage data to provide our service.              │
│          │                                                      │
│          │  [Expandable: Full legal text ▼]                     │
+──────────┴──────────────────────────────────────────────────────+
```

### Accessibility Statement

Required elements:
- Conformance status (WCAG 2.1 Level AA)
- Feedback contact (email, phone)
- Technical specifications
- Assessment approach
- Last reviewed date

---

## Mobile Considerations

### Key Statistics

- 64% of browsing on mobile (2025)
- Only 300-400px above fold on phones
- 79% mobile cart abandonment vs 67% desktop

### Navigation Patterns

**Hamburger Menu:**
- Saves space, familiar
- Low discoverability, extra taps
- Use for comprehensive nav

**Bottom Navigation:**
- Thumb-friendly, always visible
- Limited to ~5 options
- Use for core frequent actions

**Hybrid (Recommended for SaaS):**
- Top: Logo + search + hamburger
- Bottom: Sticky CTAs for key actions

### Touch Targets

- Minimum 48x48px
- 8px spacing between targets
