# User Flow Patterns

Guidelines for onboarding, authentication, error recovery, and state transitions. Reference this when designing user journeys.

---

## Onboarding Flows

### Progressive Disclosure

Reveal complexity gradually. Can reduce task completion time by 20-40%.

**Principles:**
- Limit users to 3-4 choices per step
- Start with 1-2 fields per screen
- Hold back power-user tools until users gain confidence
- Use checklists where each item opens a focused screen

**Example: Profile Setup**

```
Step 1: "What brings you here?"
  [ ] Finding new opportunities
  [ ] Managing existing projects
  [ ] Both

Step 2: "What are your strongest skills?"
  [Multi-select tags]

Step 3: "How much time can you dedicate?"
  [Slider: hours per week]

[Skip for now] [Continue]
```

### Personalization

Ask users their goal on first open, then tailor the experience. Increases activation by 30-50%.

**Segmentation questions:**
- Role/persona (founder, developer, designer)
- Use case (personal, team, enterprise)
- Experience level (new to category, switching from competitor)

### Activation Milestones

Get users to their "aha moment" quickly.

**Metrics to track:**

| Metric | Description |
|--------|-------------|
| Time-to-First-Value | Time from signup to activation |
| Activation Rate | % completing activation (benchmark: ~36%) |
| Day-30 Retention | Critical predictor of success |
| Feature Adoption | Higher = stickier product |

**Define clear activation events:**
- First project created
- First collaboration
- First insight discovered
- First export/share

---

## Authentication Flows

### Passkeys (The New Default)

By 2025, ~70% of users have at least one passkey.

**Identifier-First Pattern:**

```
1. User enters email
2. System checks for passkey
3. If exists → passkey prompt
4. If not → fall back to password/magic link
```

This is more effective than a separate "Sign in with Passkey" button.

**Business impact:**
- 28% reduction in checkout abandonment
- 50% reduction in login abandonment
- 4x improvement in sign-in success rates

### Authentication Hierarchy (2025)

1. **Passkeys** - Primary, most secure
2. **OAuth** (Google, GitHub) - Convenient, trusted
3. **Magic links** - Transitional, low-risk use cases
4. **Password + MFA** - Legacy, being phased out
5. **SMS OTP** - Deprecated, avoid for new implementations

### Adaptive MFA

Challenge users based on risk, not always at login:

- Low-risk login → silent checks (device recognition, behavioral)
- High-risk action → step-up authentication
- New device → additional verification

---

## Error Recovery Flows

### Form Validation

**Best Practices:**

1. **Inline validation** - Validate when user finishes field
2. **Show all errors** - Don't reveal one by one
3. **Keep submit enabled** - Disabling creates confusion
4. **Position errors near field** - Below or next to problem
5. **Actionable messages** - "Please enter an email" not "Invalid"

**Error Message Template:**

```
[What went wrong]. [How to fix it].

Examples:
"This field is required. Enter your email to continue."
"Passwords don't match. Re-enter in both fields."
"This email is registered. Sign in or use different email."
```

### System Errors

```
"We couldn't [action]. [Recovery option]."

Examples:
"We couldn't save your changes. Check your connection and try again."
"We couldn't load your dashboard. Refresh or contact support."
```

### Accessibility

- Never use red color alone (combine with icons, text)
- Use `aria-live="polite"` for dynamic errors
- Focus management to error summary

### Forgiving Interfaces

- Implement undo for reversible actions
- Confirmation dialogs for destructive actions
- Autocorrect and suggestions
- Allow validator overrides when appropriate

---

## Empty-to-Populated State Transitions

Empty states are onboarding opportunities.

### Structure

```
+----------------------------------------+
|  [Illustration or Icon]                |
|                                        |
|  Title (positive statement)            |
|  "Start by adding your first project"  |
|                                        |
|  Body (benefit + next action)          |
|  "Projects help you organize work."    |
|                                        |
|  [Primary CTA]                         |
+----------------------------------------+
```

**Writing guidelines:**
- Positive statements ("Start by adding" not "You don't have")
- Explain benefit of taking action
- Single, clear CTA

### Strategies

| Strategy | When to Use |
|----------|-------------|
| Pre-load demo data | Dashboards, charts (nobody likes empty graphs) |
| Sample screenshots | Show product in action |
| Onboarding video | Complex products needing "aha" |
| AI assistant prompt | "Need help? Ask our assistant" |

### Empty State Types

**First use (onboarding):**
```
"No projects yet"
"Projects help you organize work and collaborate."
[Create your first project]
```

**User cleared (success):**
```
"All caught up!"
"New tasks will appear here when assigned."
[View completed tasks]
```

**No results (search):**
```
"No results for 'budget spreadsheet'"
"Try fewer keywords or check spelling."
[Clear filters] [Browse all]
```

**Error state:**
```
"Couldn't load your data"
"This might be a connection issue."
[Try again] [Contact support]
```

---

## First-Run Experiences

### The Critical Window

- 8 out of 10 users abandon apps because they don't understand them
- 40-60% of free trial users use product once and never return

### Learn-by-Doing Approach

Notion's pattern: Getting Started page with a functional checklist. Users learn by using, not by reading.

**Key Principles:**

1. **Minimal path to value** - 3-5 essential steps maximum
2. **Skip options** - "Skip for now" on non-critical items
3. **Task-based guidance** - Short, focused tutorials
4. **Progress indicators** - "You're 60% of the way"
5. **Milestone celebrations** - Acknowledge progress

---

## Anti-Patterns to Avoid

### 1. Feature Dumping
Don't give users a 300-page manual. Focus on outcomes, not features.

### 2. One-Size-Fits-All
Startup founders need different flows than enterprise admins.

### 3. Information Overload
Every extra signup field costs ~7% conversion.

### 4. Empty Dashboards Without Guidance
Zeros and placeholders are disheartening.

### 5. No Clear Path to Value
Users need to know what success looks like.

### 6. Abandoning After Week One
Onboarding isn't one-and-done. Guide users to new features.

### 7. Over-Automation
59% of customers feel companies neglect human touch.

### 8. Non-Standard UI
Increases mistakes, creates friction.

### 9. 20-Step Product Tours Immediately
Trigger micro-guidance when users interact with features.

---

## Measuring Success

| Metric | Description | Target |
|--------|-------------|--------|
| Onboarding Completion | % finishing onboarding | >80% |
| Time-to-First-Value | Time to activation | <5 min |
| Day-30 Retention | Active after 30 days | >40% |
| Trial-to-Paid | Trial conversions | >25% |
| Support Tickets (First 30 Days) | Friction indicator | Low |

**Impact of effective onboarding:**
- 2x higher net revenue retention
- Multi-channel support increases completion 34% → 62%
- Bad mobile onboarding loses 75% within 3 days
