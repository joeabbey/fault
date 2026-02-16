# Billing and Pricing Patterns

Guidelines for pricing tables, payment forms, upgrade flows, invoices, usage displays, feature gating, and cancellation. Reference this when building billing features.

---

## Core Principle

**Transparency builds trust.** Users who understand their costs become loyal customers. Dark patterns may boost short-term metrics but destroy long-term value.

---

## Pricing Table Design

### Structure

**3-4 tiers is optimal.** More causes decision paralysis.

```
+──────────┬──────────────┬──────────+
│  FREE    │  PRO ★       │ BUSINESS │
│          │  Most Popular│          │
│  $0/mo   │  $29/mo      │  $99/mo  │
│          │              │          │
│  ✓ 3     │  ✓ Unlimited │  ✓ All   │
│  ✓ Basic │  ✓ Advanced  │  ✓ SSO   │
│  ✓ Email │  ✓ Priority  │  ✓ SLA   │
│          │              │          │
│ [Start]  │ [Start Free] │ [Contact]│
+──────────┴──────────────┴──────────+
```

### Best Practices

- **Highlight recommended plan** with badge or border
- **Show savings clearly** for annual plans
- **Keep feature lists short** (3-5 per tier)
- **Use comparison table** for detailed features
- **Explain each tier's ideal user**

### Annual vs Monthly Toggle

- Position prominently above pricing cards
- Show savings percentage clearly ("Save 20%")
- Don't hide monthly pricing
- Consider defaulting to monthly (feels more honest)

### Pricing Calculator (Usage-Based)

```
+─────────────────────────────────────────+
│  "Estimate your monthly cost"           │
│                                         │
│  Events/month: [____10,000____] ◄────►  │
│  Team members: [____5_________]         │
│                                         │
│  Estimated: $49/month                   │
│  [View breakdown ▼]                     │
+─────────────────────────────────────────+
```

---

## Payment Form UX

### Form Design

- **Single-column vertical flow** (multi-column increases errors)
- **Group related inputs** (billing info, payment info)
- **Full-width CTAs** easy to tap
- **Value labels**: "Pay $64.95" not "Submit"

### Card Input

- Auto-format: `1111 2222 3333 4444`
- Expiration: `MM/YY` (auto-insert slash)
- Appropriate mobile keyboards
- Large, tap-friendly inputs

### Error Handling

- Real-time validation (not submit-and-fail)
- Inline messages next to problem
- Specific: "Card number incomplete" not "Invalid"

### Express Checkout

**Essential for conversion.** Mobile cart abandonment is 79%.

```
+─────────────────────────────────────────+
│  [Apple Pay] [Google Pay] [PayPal]      │  ← Top of payment
├─────────────────────────────────────────┤
│  Or pay with card                       │
│  ...                                    │
+─────────────────────────────────────────+
```

### Reduce Friction

- Don't force account creation before purchase
- Audit every field (need full address or just postal?)
- Guest checkout for first-time purchases
- Offer account creation AFTER purchase

---

## Upgrade/Downgrade Flows

### Upgrade Flow

Most companies prorate upgrades immediately:
1. Immediate access to new features
2. Charge difference for remainder of period
3. Show math clearly before confirmation

```
+─────────────────────────────────────────+
│  Upgrade to Pro                         │
├─────────────────────────────────────────┤
│  Current: Free plan                     │
│  New: Pro ($29/month)                   │
│                                         │
│  You'll gain:                           │
│  ✓ Unlimited projects                   │
│  ✓ Advanced analytics                   │
│  ✓ Priority support                     │
│                                         │
│  Prorated charge today: $14.50          │
│  (15 days remaining in cycle)           │
│                                         │
│  [Cancel]  [Upgrade Now]                │
+─────────────────────────────────────────+
```

### Downgrade Flow

Two approaches:
1. **Immediate** with credits (Slack, Notion)
2. **Scheduled** at end of cycle (Zoom, GitHub) - simpler

**Show what they'll lose:**

```
+─────────────────────────────────────────+
│  Downgrade to Free                      │
├─────────────────────────────────────────┤
│  You'll lose access to:                 │
│  ✗ Advanced analytics                   │
│  ✗ Priority support                     │
│  ✗ 47 of 50 projects (over free limit)  │
│                                         │
│  Change takes effect: Feb 15, 2025      │
│                                         │
│  [Keep Pro]  [Downgrade]                │
+─────────────────────────────────────────+
```

---

## Invoice and Billing History

### Invoice List

```
+────────────────────────────────────────────────────+
│  Invoices                                          │
├────────────────────────────────────────────────────┤
│  INV-2024-001  │  Jan 15  │  $29.00  │  Paid  │ ⬇  │
│  INV-2024-002  │  Feb 15  │  $29.00  │  Paid  │ ⬇  │
│  INV-2024-003  │  Mar 15  │  $29.00  │  Due   │ ⬇  │
+────────────────────────────────────────────────────+
```

### Billing Overview

```
+─────────────────────────────────────────+
│  Billing                                │
├─────────────────────────────────────────┤
│  Current plan: Pro ($29/month)          │
│  Next billing: Mar 15, 2025             │
│  Payment method: •••• 4242              │
│                                         │
│  [Change plan] [Update payment]         │
+─────────────────────────────────────────+
```

### Design Best Practices

- Clear status indicators (green=paid, red=failed)
- PDF download for any invoice
- Filter by date range, status
- Self-service everything possible

---

## Usage-Based Pricing Displays

### Real-Time Usage Dashboard

Prevents "bill shock" and reduces support tickets.

```
+─────────────────────────────────────────+
│  API Calls                              │
│  ████████████████░░░░  8,234 / 10,000   │
│  82% used · Resets Mar 1                │
│                                         │
│  [View usage details]                   │
+─────────────────────────────────────────+
```

### Components

**Meters and Progress Bars:**
- Current vs limit
- Percentage with visual
- Reset date indicator

**Alerts:**
- Warning at 75%, 90%, 100%
- Email notifications for limits
- In-app banners for warnings

**Transparency:**
- Explain overage costs BEFORE they happen
- Show unit pricing clearly
- Display current tier and next threshold

---

## Feature Gating

### Ethical Feature Gating

**Do:**
- Let users experience "aha moment" first
- Let users "peek" at capabilities
- Time prompts for relevance
- Show clear path to unlock

**Don't:**
- Overgate early in onboarding
- Hide the "real" product
- Surprise with limitations
- Use aggressive/repeated prompts

### Upgrade Prompt Pattern

```svelte
<script lang="ts">
  export let feature: string;
  export let requiredPlan: string;
</script>

<div class="p-4 bg-secondary-50 rounded-lg border border-secondary-200">
  <h3 class="font-semibold">Unlock {feature}</h3>
  <p class="text-sm text-muted mt-1">
    This feature is available on {requiredPlan} and above.
  </p>
  <div class="mt-3 flex gap-2">
    <Button href="/pricing">View plans</Button>
    <Button variant="ghost" onclick={dismiss}>Maybe later</Button>
  </div>
</div>
```

### Trial Expiration

- Average trial-to-paid: ~25%
- Shorter trials (7 days) yield higher conversion
- **Reminder timing**: 3 days, 1 day, day-of
- Offer alternatives: extension, downgrade, special offer
- Collect feedback on why they haven't upgraded

---

## Cancellation Flows

### The Ethical Imperative

> "If a customer wants out, they'll find a way. Dark patterns damage relationships, destroy trust, and kill referrals."

**Dark patterns to AVOID:**
- Multi-step obstacle courses
- Hidden cancellation buttons
- Required phone calls
- Ambiguous confirmations
- Artificial delays

**Make cancellation symmetrically easy to signup.**

### Regulatory Context (2025)

- Amazon: $2.5B settlement for deceptive practices
- EU Digital Services Act actively enforcing
- FTC guidelines against "roach motel" patterns

### Effective Ethical Flow

**Step 1: Exit Survey (Optional)**
```
"Before you go, help us improve"
[ ] Too expensive
[ ] Missing features I need
[ ] Found an alternative
[ ] Not using it enough
[ ] Other: _________
[Skip] [Continue]
```

**Step 2: Offer Relevant Alternatives**
Based on survey response:
- Price issue → Offer discount or cheaper plan
- Not using → Offer pause subscription
- Missing features → Highlight roadmap or workaround

**Step 3: Easy Confirmation**
```
+─────────────────────────────────────────+
│  Cancel subscription                    │
├─────────────────────────────────────────┤
│  Your Pro plan will end on Mar 15.      │
│  After that:                            │
│  • You'll move to the Free plan         │
│  • Your data will be preserved          │
│  • You can reactivate anytime           │
│                                         │
│  [Keep subscription]  [Cancel]          │
+─────────────────────────────────────────+
```

**Step 4: Positive Offboarding**
- Confirmation email with next steps
- Data export option
- Easy reactivation path
- No guilt-tripping

---

## Failed Payment Recovery (Dunning)

### The Opportunity

- Failed payments average 9% of annual billings
- Comprehensive recovery recovers 35-50%
- High performers achieve 75-85%

### Smart Retry Strategy

AI-powered retries (Stripe Smart Retries):
- Analyzes 500+ signals
- Recovers 23% more than fixed schedules
- Optimal window: 21-28 days
- Optimal attempts: 4-6 retries

### Dunning Email Sequence

**Email 1 (Day 1):** Friendly notification
```
Subject: Quick update on your payment

Hi [Name],

Your payment of $29 didn't go through. This happens
sometimes - no worries!

[Update payment method]

Questions? Just reply to this email.
```

**Email 2 (Day 3-5):** Emphasize value
```
Subject: Keep your [Product] access

Don't lose access to:
• Your 47 active projects
• Advanced analytics
• Priority support

[Update payment - takes 30 seconds]
```

**Email 3 (Day 7-10):** Urgency
```
Subject: Action needed to keep your account

Your account will be downgraded in 3 days
unless we can process your payment.

[Update payment now]
```

**Email 4 (Day 14+):** Final notice
```
Subject: Your [Product] account was downgraded

We've moved your account to the Free plan.
Your data is safe - upgrade anytime to restore
full access.

[Reactivate account]
```

### Prevention

- **Card Account Updater**: Auto-updates expired cards (30-50% reduction)
- **Expiration reminders**: 30, 15, 7 days before expiry
- **Payment verification**: Validate on signup

### Payment Update UX

- Direct tokenized link (no login required)
- Zero-friction design
- Accept Apple Pay/Google Pay
- Pre-fill known information
- Clear success confirmation

---

## Summary: Building Trust

### Core Principles

1. **Transparency over tricks** - All costs upfront
2. **Symmetry** - Easy in, easy out
3. **Respect autonomy** - Informed decisions
4. **Value over manipulation** - Demonstrate, don't trap
5. **Self-service first** - No support calls needed

### 2025 Trends

- Credits and hybrid pricing up 126%
- AI-powered recovery improving rates
- Regulatory enforcement increasing
- Express checkout is table stakes
- Micro-trials (3-7 days) replacing long trials

### The Business Case

Ethical billing practices lead to:
- Higher customer lifetime value
- Better word-of-mouth referrals
- Reduced support burden
- Regulatory compliance
- Competitive advantage
