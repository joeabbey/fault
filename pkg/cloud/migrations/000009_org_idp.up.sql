CREATE TABLE IF NOT EXISTS org_idp_configs (
    id TEXT PRIMARY KEY,
    org_id TEXT NOT NULL REFERENCES organizations(id) ON DELETE CASCADE,
    provider TEXT NOT NULL DEFAULT 'oidc',
    client_id TEXT NOT NULL,
    client_secret TEXT NOT NULL,
    discovery_url TEXT NOT NULL,
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);
CREATE UNIQUE INDEX IF NOT EXISTS idx_org_idp_configs_org ON org_idp_configs(org_id);
