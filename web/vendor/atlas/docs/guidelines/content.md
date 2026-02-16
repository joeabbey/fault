# Content Guidelines

Guidelines for microcopy, error messages, empty states, CTAs, and voice/tone. Reference this when writing UI text.

---

## Button Labels

### The Verb + Object Pattern

Effective buttons tell users exactly what happens when clicked.

| Bad | Good | Why |
|-----|------|-----|
| Submit | Send Invoice | Specific action + object |
| Click Here | Download Report | Clear outcome |
| Next | Continue to Payment | Sets expectations |
| Yes | Delete Account | Removes ambiguity |
| OK | Save Changes | Action-oriented |

**Key principles:**
- Use active imperative form (commands)
- Keep to 2-3 words maximum
- Drop articles when meaning is clear
- Match precision to severity ("Delete" vs "Remove")

**A/B insight:** "Submit" → "Send invoice" increased clicks 18%.

---

## Form Labels and Placeholders

### Structure

| Element | Bad | Good |
|---------|-----|------|
| Label | Enter your email address: | Email |
| Placeholder | Email | jane@example.com |
| Helper text | (none) | We'll send your receipt here |

**Critical rule:** Never replace labels with placeholders. Labels must persist above the field.

### Placeholder Patterns

**Do:**
- Format examples: `(555) 123-4567`, `MM/DD/YYYY`
- Realistic examples: `e.g., John Doe`
- Keep concise: 3-5 words

**Don't:**
- Instructions as placeholders (they disappear)
- Low-contrast gray (must meet 4.5:1 WCAG)
- Validation rules only in placeholders

**Templates:**
```
Search: "Search articles, topics, or authors..."
Date: "MM/DD/YYYY"
Phone: "(555) 123-4567"
Password: "8+ characters, 1 number, 1 symbol"
Currency: "$1,000.00"
```

---

## Error Messages

### The Three Components

Every error needs:
1. **What went wrong** (specific)
2. **Why it happened** (if not obvious)
3. **How to fix it** (actionable)

### Tone Guidelines

| Principle | Bad | Good |
|-----------|-----|------|
| Don't blame | "You entered an invalid email" | "This email format isn't recognized" |
| Be specific | "Error" | "We couldn't find an account with that email" |
| Offer solutions | "Invalid password" | "Password must be 8+ characters. Try adding a number." |
| Human language | "Error 403: Access denied" | "You don't have permission to view this page" |
| Stay calm | "WRONG PASSWORD!" | "That password didn't work. Need to reset it?" |

### Templates

**Form validation:**
```
[What's wrong]. [How to fix it].

"This field is required. Enter your email to continue."
"Passwords don't match. Re-enter in both fields."
"This email is registered. Sign in or use different email."
```

**System errors:**
```
"We couldn't [action]. [Recovery option]."

"We couldn't save your changes. Check connection and try again."
"We couldn't load your dashboard. Refresh or contact support."
```

**Inline validation:**
```
Email: "Enter a valid email (e.g., name@company.com)"
Password: "Add 2 more characters to meet minimum"
Date: "Enter date as MM/DD/YYYY"
```

### Avoid

- "Something went wrong" (too vague)
- "Invalid input" (no guidance)
- "Error: null" (technical jargon)
- "Oops!" with no explanation
- Humor that gets stale on repeat

---

## Empty State Messaging

### Three-Part Structure

1. **Headline**: What's empty (acknowledge)
2. **Motivation**: Why they should care (benefit)
3. **CTA**: What to do next (one clear action)

### Templates by Type

**First use (onboarding):**
```
"No projects yet"
"Projects help you organize work and collaborate with your team."
[Create your first project]
```

**User cleared (success):**
```
"All caught up!"
"Nice work! New tasks will appear here when assigned."
[View completed tasks]
```

**No results (search):**
```
"No results for 'budget spreadsheet'"
"Try fewer keywords or check spelling."
[Clear filters] [Browse all templates]
```

**Error state:**
```
"Couldn't load your data"
"This might be a connection issue on our end."
[Try again] [Contact support]
```

### Tone Principles

- Sound like a helpful friend
- Use contractions ("You haven't" not "You have not")
- Be encouraging, not condescending
- One CTA is better than three vague options

---

## Call-to-Action Copy

### Urgency Without Manipulation

**Ethical urgency:**
- Time words: "Start today," "Get started now"
- Genuine scarcity: "3 spots remaining" (only if true)
- Benefit framing: "Start saving" not "Buy now"
- Low-commitment: "Try free" not "Subscribe"

**"Now" and "today" raise conversion ~14% without feeling manipulative.**

### CTAs by Intent Level

| Stage | Examples |
|-------|----------|
| Discovery | "Learn more," "See how it works," "Explore" |
| Consideration | "Start free trial," "Get a demo," "See pricing" |
| Decision | "Create account," "Start building," "Get started free" |
| Conversion | "Subscribe now," "Upgrade to Pro," "Complete purchase" |

### Value-Focused CTAs

| Generic (avoid) | Value-focused (prefer) |
|-----------------|------------------------|
| Submit | Get your free report |
| Register | Save your seat |
| Download | Get the guide |
| Sign up | Start your free trial |
| Buy | Add to bag |
| Click here | See the demo |

### Anti-Patterns

- Clickbait: "Click for a surprise!"
- Fake urgency: "Only 2 left!" (when there are 200)
- High-pressure: "Don't miss out!" repeatedly
- Deceptive: "Free" with hidden costs
- Guilt-tripping: "No, I don't want to save money"

---

## Notification and Alert Copy

### Length Constraints

- Push notifications: 25 characters or fewer
- In-app messages: 10 words or less
- Toast messages: One idea, one sentence

### Templates

**Success:**
```
"Changes saved"
"[Item] created successfully"
"Email sent to [recipient]"
```

**Informational:**
```
"[Thing] is [status]"
"New [item] from [source]"
"[Number] new [items] since last visit"
```

**Warning:**
```
"[Thing] will [consequence] in [timeframe]"
"Your [resource] is running low"
"This action will [consequence]. Continue?"
```

**Error:**
```
"Couldn't [action]. [Recovery suggestion]."
"[Action] failed. [Alternative or retry]."
```

### Avoiding Fatigue

- 71% uninstall apps due to notifications
- Behaviorally triggered = 9x more likely to be opened
- Default notification channels to mute
- Batch low-priority notifications

---

## Confirmation Dialogs

### Required Elements

1. **Specific headline**: Question or statement about action
2. **Consequence explanation**: What will happen
3. **Reversibility indicator**: Can it be undone?
4. **Descriptive buttons**: Never just Yes/No

### Templates

**Reversible deletion:**
```
Title: Delete "Project Alpha"?
Body: This moves it to trash. Restore within 30 days.
Buttons: [Cancel] [Delete]
```

**Permanent deletion:**
```
Title: Permanently delete "Project Alpha"?
Body: This cannot be undone. All data will be lost.
Buttons: [Cancel] [Delete permanently]
```

**Account destruction:**
```
Title: Delete your account?
Body: This permanently deletes:
• Your profile and settings
• All 12 projects
• All collaboration history

This cannot be undone.

[Type "DELETE" to confirm]
Buttons: [Cancel] [Delete my account]
```

### Button Labels

| Action | Avoid | Use |
|--------|-------|-----|
| Delete | Yes / No | Cancel / Delete |
| Discard | OK / Cancel | Keep editing / Discard |
| Unsubscribe | Yes / No | Stay subscribed / Unsubscribe |
| Log out all | Confirm | Stay logged in / Log out all |

### When NOT to Use

- Reversible actions (use undo toast instead)
- Low-consequence actions (moving, sorting)
- Frequent actions (creates fatigue)

---

## Voice and Tone

### Mailchimp's Approach (Industry Standard)

**Voice (constant):**
- Plainspoken: Clarity above all
- Genuine: Warm, accessible
- Dry humor: Winking, not shouting
- Smart but not snobbish

**Tone (situational):**

| Situation | Tone | Example |
|-----------|------|---------|
| Success | Celebratory, not over-the-top | "Nice! Your campaign is on its way." |
| Error | Empathetic, helpful | "We hit a snag. Here's what happened..." |
| Warning | Clear, calm | "Heads up: This affects all subscribers." |
| Onboarding | Encouraging | "Let's get set up. Only takes a minute." |

**Golden rule:** "Always more important to be clear than entertaining."

---

## Inclusive Language

### Gender-Neutral

| Avoid | Use |
|-------|-----|
| Guys, ladies | Everyone, folks, team |
| He/she | They |
| Mankind | Humanity, people |
| Man-hours | Person-hours |

### Ability-Sensitive

| Avoid | Use |
|-------|-----|
| Normal users | Non-disabled, typical |
| Crazy, insane | Surprising, unexpected |
| Blind to | Unaware of |
| Crippled | Broken, non-functional |

### Culturally Aware

- Don't use flags for language selection
- Avoid idioms that don't translate
- Use diverse names in examples
- Be aware of color meanings across cultures

---

## Localization

### Design for Translation

- German: 2x English length
- French: 3x English length
- **Design layouts for 200% expansion**

### Copy That Translates

| Hard | Easier |
|------|--------|
| "Piece of cake" | "This is easy" |
| Puns and wordplay | Direct statements |
| Cultural references | Universal concepts |
| Colloquialisms | Plain language |

---

## Dark Patterns to Avoid

### Deceptive Practices

1. **Confirmshaming**
   - Bad: "No thanks, I don't want to save money"
   - Good: "No thanks" / "Skip"

2. **Roach motel**
   - Bad: Easy signup, hidden cancellation
   - Good: Cancel in same location as signup

3. **Hidden costs**
   - Bad: Surprise fees at checkout
   - Good: All-inclusive pricing upfront

4. **Forced continuity**
   - Bad: Auto-charge after trial with no warning
   - Good: Reminder 3-5 days before billing

5. **Misdirection**
   - Bad: Pre-checked boxes, double negatives
   - Good: Clear opt-in, single affirmatives

### Regulatory Context (2025)

- EU Digital Services Act actively enforcing
- FTC increased scrutiny
- GDPR: Pre-checked boxes = violation
- 97% of European sites use at least one dark pattern

### Business Case for Ethics

Dark patterns damage long-term trust. Transparent design builds:
- Higher customer lifetime value
- Better word-of-mouth
- Reduced support burden
- Competitive advantage
