CREATE TABLE IF NOT EXISTS organizations (
    id TEXT PRIMARY KEY,
    name TEXT NOT NULL,
    slug TEXT NOT NULL UNIQUE,
    owner_id TEXT NOT NULL REFERENCES users(id),
    plan TEXT NOT NULL DEFAULT 'team',
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

CREATE INDEX IF NOT EXISTS idx_organizations_slug ON organizations(slug);
CREATE INDEX IF NOT EXISTS idx_organizations_owner ON organizations(owner_id);

CREATE TABLE IF NOT EXISTS org_members (
    id TEXT PRIMARY KEY,
    org_id TEXT NOT NULL REFERENCES organizations(id) ON DELETE CASCADE,
    user_id TEXT NOT NULL REFERENCES users(id),
    role TEXT NOT NULL DEFAULT 'member',
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    UNIQUE(org_id, user_id)
);

CREATE INDEX IF NOT EXISTS idx_org_members_org ON org_members(org_id);
CREATE INDEX IF NOT EXISTS idx_org_members_user ON org_members(user_id);

ALTER TABLE runs ADD COLUMN IF NOT EXISTS org_id TEXT REFERENCES organizations(id);
CREATE INDEX IF NOT EXISTS idx_runs_org_timestamp ON runs(org_id, timestamp DESC) WHERE org_id IS NOT NULL;

ALTER TABLE spec_results ADD COLUMN IF NOT EXISTS org_id TEXT REFERENCES organizations(id);
CREATE INDEX IF NOT EXISTS idx_spec_results_org ON spec_results(org_id, created_at DESC) WHERE org_id IS NOT NULL;
