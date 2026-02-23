CREATE TABLE IF NOT EXISTS spec_results (
    id TEXT PRIMARY KEY,
    user_id TEXT NOT NULL REFERENCES users(id),
    spec_hash TEXT NOT NULL DEFAULT '',
    spec_title TEXT NOT NULL DEFAULT '',
    total_requirements INTEGER NOT NULL DEFAULT 0,
    anchored_count INTEGER NOT NULL DEFAULT 0,
    implemented_count INTEGER NOT NULL DEFAULT 0,
    partial_count INTEGER NOT NULL DEFAULT 0,
    missing_count INTEGER NOT NULL DEFAULT 0,
    overall_score DOUBLE PRECISION NOT NULL DEFAULT 0,
    result_json JSONB NOT NULL DEFAULT '[]'::jsonb,
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

CREATE INDEX IF NOT EXISTS idx_spec_results_user_created ON spec_results(user_id, created_at DESC);
