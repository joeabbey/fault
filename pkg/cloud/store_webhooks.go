package cloud

import (
	"context"
	"fmt"
	"time"

	"github.com/google/uuid"
	"github.com/jackc/pgx/v5"
)

// OrgWebhook represents a webhook configuration for an organization.
type OrgWebhook struct {
	ID        string    `json:"id"`
	OrgID     string    `json:"org_id"`
	URL       string    `json:"url"`
	Secret    string    `json:"-"`
	Events    []string  `json:"events"`
	Active    bool      `json:"active"`
	CreatedAt time.Time `json:"created_at"`
	UpdatedAt time.Time `json:"updated_at"`
}

// CreateWebhook inserts a new webhook for an organization.
func (s *PostgresStore) CreateWebhook(ctx context.Context, wh *OrgWebhook) error {
	if wh.ID == "" {
		wh.ID = uuid.New().String()
	}
	now := time.Now()
	if wh.CreatedAt.IsZero() {
		wh.CreatedAt = now
	}
	if wh.UpdatedAt.IsZero() {
		wh.UpdatedAt = now
	}

	_, err := s.pool.Exec(ctx,
		`INSERT INTO org_webhooks (id, org_id, url, secret, events, active, created_at, updated_at)
		 VALUES ($1, $2, $3, $4, $5, $6, $7, $8)`,
		wh.ID, wh.OrgID, wh.URL, wh.Secret, wh.Events, wh.Active, wh.CreatedAt, wh.UpdatedAt,
	)
	if err != nil {
		return fmt.Errorf("creating webhook: %w", err)
	}
	return nil
}

// ListWebhooks returns all webhooks for an organization.
func (s *PostgresStore) ListWebhooks(ctx context.Context, orgID string) ([]OrgWebhook, error) {
	rows, err := s.pool.Query(ctx,
		`SELECT id, org_id, url, secret, events, active, created_at, updated_at
		 FROM org_webhooks WHERE org_id = $1
		 ORDER BY created_at`, orgID,
	)
	if err != nil {
		return nil, fmt.Errorf("listing webhooks: %w", err)
	}
	defer rows.Close()

	webhooks := make([]OrgWebhook, 0)
	for rows.Next() {
		var wh OrgWebhook
		if err := rows.Scan(&wh.ID, &wh.OrgID, &wh.URL, &wh.Secret, &wh.Events, &wh.Active, &wh.CreatedAt, &wh.UpdatedAt); err != nil {
			return nil, fmt.Errorf("scanning webhook: %w", err)
		}
		webhooks = append(webhooks, wh)
	}
	if err := rows.Err(); err != nil {
		return nil, fmt.Errorf("iterating webhooks: %w", err)
	}
	return webhooks, nil
}

// DeleteWebhook removes a webhook by ID, scoped to an organization.
func (s *PostgresStore) DeleteWebhook(ctx context.Context, orgID, webhookID string) error {
	tag, err := s.pool.Exec(ctx,
		`DELETE FROM org_webhooks WHERE id = $1 AND org_id = $2`,
		webhookID, orgID,
	)
	if err != nil {
		return fmt.Errorf("deleting webhook: %w", err)
	}
	if tag.RowsAffected() == 0 {
		return pgx.ErrNoRows
	}
	return nil
}
