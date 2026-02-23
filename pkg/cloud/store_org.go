package cloud

import (
	"context"
	"fmt"
	"time"

	"github.com/google/uuid"
)

// CreateOrganization inserts a new organization.
func (s *PostgresStore) CreateOrganization(ctx context.Context, org *Organization) error {
	if org.ID == "" {
		org.ID = uuid.New().String()
	}
	now := time.Now()
	if org.CreatedAt.IsZero() {
		org.CreatedAt = now
	}
	if org.UpdatedAt.IsZero() {
		org.UpdatedAt = now
	}
	if org.Plan == "" {
		org.Plan = "team"
	}

	_, err := s.pool.Exec(ctx,
		`INSERT INTO organizations (id, name, slug, owner_id, plan, created_at, updated_at)
		 VALUES ($1, $2, $3, $4, $5, $6, $7)`,
		org.ID, org.Name, org.Slug, org.OwnerID, org.Plan, org.CreatedAt, org.UpdatedAt,
	)
	if err != nil {
		return fmt.Errorf("creating organization: %w", err)
	}
	return nil
}

// GetOrganization retrieves an organization by ID.
func (s *PostgresStore) GetOrganization(ctx context.Context, orgID string) (*Organization, error) {
	var org Organization
	err := s.pool.QueryRow(ctx,
		`SELECT id, name, slug, owner_id, plan, created_at, updated_at
		 FROM organizations WHERE id = $1`, orgID,
	).Scan(&org.ID, &org.Name, &org.Slug, &org.OwnerID, &org.Plan, &org.CreatedAt, &org.UpdatedAt)
	if err != nil {
		return nil, fmt.Errorf("getting organization: %w", err)
	}
	return &org, nil
}

// GetOrganizationBySlug retrieves an organization by its unique slug.
func (s *PostgresStore) GetOrganizationBySlug(ctx context.Context, slug string) (*Organization, error) {
	var org Organization
	err := s.pool.QueryRow(ctx,
		`SELECT id, name, slug, owner_id, plan, created_at, updated_at
		 FROM organizations WHERE slug = $1`, slug,
	).Scan(&org.ID, &org.Name, &org.Slug, &org.OwnerID, &org.Plan, &org.CreatedAt, &org.UpdatedAt)
	if err != nil {
		return nil, fmt.Errorf("getting organization by slug: %w", err)
	}
	return &org, nil
}

// ListUserOrganizations returns all organizations a user belongs to.
func (s *PostgresStore) ListUserOrganizations(ctx context.Context, userID string) ([]Organization, error) {
	rows, err := s.pool.Query(ctx,
		`SELECT o.id, o.name, o.slug, o.owner_id, o.plan, o.created_at, o.updated_at
		 FROM organizations o
		 JOIN org_members m ON m.org_id = o.id
		 WHERE m.user_id = $1
		 ORDER BY o.name`, userID,
	)
	if err != nil {
		return nil, fmt.Errorf("listing user organizations: %w", err)
	}
	defer rows.Close()

	orgs := make([]Organization, 0)
	for rows.Next() {
		var org Organization
		if err := rows.Scan(&org.ID, &org.Name, &org.Slug, &org.OwnerID, &org.Plan, &org.CreatedAt, &org.UpdatedAt); err != nil {
			return nil, fmt.Errorf("scanning organization: %w", err)
		}
		orgs = append(orgs, org)
	}
	return orgs, nil
}

// AddOrgMember adds a user to an organization with the given role.
func (s *PostgresStore) AddOrgMember(ctx context.Context, orgID, userID, role string) error {
	id := uuid.New().String()
	_, err := s.pool.Exec(ctx,
		`INSERT INTO org_members (id, org_id, user_id, role, created_at)
		 VALUES ($1, $2, $3, $4, NOW())`,
		id, orgID, userID, role,
	)
	if err != nil {
		return fmt.Errorf("adding org member: %w", err)
	}
	return nil
}

// RemoveOrgMember removes a user from an organization.
func (s *PostgresStore) RemoveOrgMember(ctx context.Context, orgID, userID string) error {
	_, err := s.pool.Exec(ctx,
		`DELETE FROM org_members WHERE org_id = $1 AND user_id = $2`,
		orgID, userID,
	)
	if err != nil {
		return fmt.Errorf("removing org member: %w", err)
	}
	return nil
}

// ListOrgMembers returns all members of an organization with their user details.
func (s *PostgresStore) ListOrgMembers(ctx context.Context, orgID string) ([]OrgMember, error) {
	rows, err := s.pool.Query(ctx,
		`SELECT m.id, m.org_id, m.user_id, m.role, m.created_at,
		        COALESCE(u.email, ''), COALESCE(u.name, '')
		 FROM org_members m
		 JOIN users u ON u.id = m.user_id
		 WHERE m.org_id = $1
		 ORDER BY m.created_at`, orgID,
	)
	if err != nil {
		return nil, fmt.Errorf("listing org members: %w", err)
	}
	defer rows.Close()

	members := make([]OrgMember, 0)
	for rows.Next() {
		var m OrgMember
		if err := rows.Scan(&m.ID, &m.OrgID, &m.UserID, &m.Role, &m.CreatedAt, &m.Email, &m.Name); err != nil {
			return nil, fmt.Errorf("scanning org member: %w", err)
		}
		members = append(members, m)
	}
	return members, nil
}

// GetOrgMembership returns a user's membership in an organization, or nil if not a member.
func (s *PostgresStore) GetOrgMembership(ctx context.Context, orgID, userID string) (*OrgMember, error) {
	var m OrgMember
	err := s.pool.QueryRow(ctx,
		`SELECT m.id, m.org_id, m.user_id, m.role, m.created_at,
		        COALESCE(u.email, ''), COALESCE(u.name, '')
		 FROM org_members m
		 JOIN users u ON u.id = m.user_id
		 WHERE m.org_id = $1 AND m.user_id = $2`, orgID, userID,
	).Scan(&m.ID, &m.OrgID, &m.UserID, &m.Role, &m.CreatedAt, &m.Email, &m.Name)
	if err != nil {
		return nil, nil // not a member
	}
	return &m, nil
}

// ListOrgRuns returns recent runs for an organization, ordered by timestamp descending.
func (s *PostgresStore) ListOrgRuns(ctx context.Context, orgID string, limit, offset int) ([]Run, error) {
	if limit <= 0 {
		limit = 20
	}
	if limit > 100 {
		limit = 100
	}

	rows, err := s.pool.Query(ctx,
		`SELECT id, user_id, repo_url, branch, commit_sha, commit_range, mode,
		        timestamp, duration_ms, files_changed, errors, warnings, infos,
		        total_issues, issues, confidence_score, summary, metadata, created_at
		 FROM runs WHERE org_id = $1
		 ORDER BY timestamp DESC
		 LIMIT $2 OFFSET $3`, orgID, limit, offset,
	)
	if err != nil {
		return nil, fmt.Errorf("listing org runs: %w", err)
	}
	defer rows.Close()

	runs := make([]Run, 0)
	for rows.Next() {
		var r Run
		if err := rows.Scan(&r.ID, &r.UserID, &r.RepoURL, &r.Branch, &r.CommitSHA, &r.CommitRange,
			&r.Mode, &r.Timestamp, &r.DurationMs, &r.FilesChanged,
			&r.Errors, &r.Warnings, &r.Infos, &r.TotalIssues,
			&r.Issues, &r.ConfidenceScore, &r.Summary, &r.Metadata, &r.CreatedAt); err != nil {
			return nil, fmt.Errorf("scanning org run: %w", err)
		}
		runs = append(runs, r)
	}
	return runs, nil
}

// GetOrgRunStats returns aggregate statistics for an organization's runs.
func (s *PostgresStore) GetOrgRunStats(ctx context.Context, orgID string) (*RunStats, error) {
	var stats RunStats
	err := s.pool.QueryRow(ctx,
		`SELECT COUNT(*), COALESCE(SUM(total_issues), 0),
		        COALESCE(AVG(errors), 0), COALESCE(AVG(warnings), 0),
		        COALESCE(AVG(duration_ms), 0)
		 FROM runs WHERE org_id = $1`, orgID,
	).Scan(&stats.TotalRuns, &stats.TotalIssues,
		&stats.AvgErrors, &stats.AvgWarnings, &stats.AvgDuration)
	if err != nil {
		return nil, fmt.Errorf("getting org run stats: %w", err)
	}
	return &stats, nil
}

// OrgConfig represents an organization's shared configuration.
type OrgConfig struct {
	ID         string    `json:"id"`
	OrgID      string    `json:"org_id"`
	ConfigYAML string    `json:"config_yaml"`
	Version    int       `json:"version"`
	UpdatedBy  string    `json:"updated_by"`
	CreatedAt  time.Time `json:"created_at"`
	UpdatedAt  time.Time `json:"updated_at"`
}

func (s *PostgresStore) GetOrgConfig(ctx context.Context, orgID string) (*OrgConfig, error) {
	var c OrgConfig
	err := s.pool.QueryRow(ctx,
		`SELECT id, org_id, config_yaml, version, updated_by, created_at, updated_at
		 FROM org_configs WHERE org_id = $1`, orgID,
	).Scan(&c.ID, &c.OrgID, &c.ConfigYAML, &c.Version, &c.UpdatedBy, &c.CreatedAt, &c.UpdatedAt)
	if err != nil {
		return nil, nil // not found
	}
	return &c, nil
}

func (s *PostgresStore) SaveOrgConfig(ctx context.Context, cfg *OrgConfig) error {
	if cfg.ID == "" {
		cfg.ID = uuid.New().String()
	}
	now := time.Now()
	_, err := s.pool.Exec(ctx,
		`INSERT INTO org_configs (id, org_id, config_yaml, version, updated_by, created_at, updated_at)
		 VALUES ($1, $2, $3, $4, $5, $6, $6)
		 ON CONFLICT (org_id) DO UPDATE SET
		   config_yaml = EXCLUDED.config_yaml,
		   version = org_configs.version + 1,
		   updated_by = EXCLUDED.updated_by,
		   updated_at = $6`,
		cfg.ID, cfg.OrgID, cfg.ConfigYAML, cfg.Version, cfg.UpdatedBy, now,
	)
	if err != nil {
		return fmt.Errorf("saving org config: %w", err)
	}
	return nil
}

// GetOrgIDPConfig retrieves the OIDC IdP configuration for an organization.
func (s *PostgresStore) GetOrgIDPConfig(ctx context.Context, orgID string) (*OrgIDPConfig, error) {
	var c OrgIDPConfig
	err := s.pool.QueryRow(ctx,
		`SELECT id, org_id, provider, client_id, client_secret, discovery_url, created_at, updated_at
		 FROM org_idp_configs WHERE org_id = $1`, orgID,
	).Scan(&c.ID, &c.OrgID, &c.Provider, &c.ClientID, &c.ClientSecret, &c.DiscoveryURL, &c.CreatedAt, &c.UpdatedAt)
	if err != nil {
		return nil, nil // not found
	}
	return &c, nil
}

// SaveOrgIDPConfig upserts an OIDC IdP configuration for an organization.
func (s *PostgresStore) SaveOrgIDPConfig(ctx context.Context, cfg *OrgIDPConfig) error {
	if cfg.ID == "" {
		cfg.ID = uuid.New().String()
	}
	now := time.Now()
	_, err := s.pool.Exec(ctx,
		`INSERT INTO org_idp_configs (id, org_id, provider, client_id, client_secret, discovery_url, created_at, updated_at)
		 VALUES ($1, $2, $3, $4, $5, $6, $7, $7)
		 ON CONFLICT (org_id) DO UPDATE SET
		   provider = EXCLUDED.provider,
		   client_id = EXCLUDED.client_id,
		   client_secret = EXCLUDED.client_secret,
		   discovery_url = EXCLUDED.discovery_url,
		   updated_at = $7`,
		cfg.ID, cfg.OrgID, cfg.Provider, cfg.ClientID, cfg.ClientSecret, cfg.DiscoveryURL, now,
	)
	if err != nil {
		return fmt.Errorf("saving org IDP config: %w", err)
	}
	return nil
}

// DeleteOrgIDPConfig removes the OIDC IdP configuration for an organization.
func (s *PostgresStore) DeleteOrgIDPConfig(ctx context.Context, orgID string) error {
	_, err := s.pool.Exec(ctx,
		`DELETE FROM org_idp_configs WHERE org_id = $1`, orgID,
	)
	if err != nil {
		return fmt.Errorf("deleting org IDP config: %w", err)
	}
	return nil
}
