package cloud

import (
	"context"
	"encoding/json"
	"fmt"
	"time"

	"github.com/google/uuid"
)

// AuditEntry represents a single entry in the audit log.
type AuditEntry struct {
	ID           string          `json:"id"`
	OrgID        string          `json:"org_id"`
	UserID       string          `json:"user_id"`
	Action       string          `json:"action"`
	ResourceType string          `json:"resource_type"`
	ResourceID   string          `json:"resource_id"`
	Details      json.RawMessage `json:"details"`
	CreatedAt    time.Time       `json:"created_at"`
	UserEmail    string          `json:"user_email,omitempty"`
}

// InsertAuditEntry inserts a new audit log entry.
func (s *PostgresStore) InsertAuditEntry(ctx context.Context, entry *AuditEntry) error {
	if entry.ID == "" {
		entry.ID = uuid.New().String()
	}
	if entry.CreatedAt.IsZero() {
		entry.CreatedAt = time.Now()
	}
	if entry.Details == nil {
		entry.Details = json.RawMessage("{}")
	}

	_, err := s.pool.Exec(ctx,
		`INSERT INTO audit_log (id, org_id, user_id, action, resource_type, resource_id, details, created_at)
		 VALUES ($1, $2, $3, $4, $5, $6, $7, $8)`,
		entry.ID, entry.OrgID, entry.UserID, entry.Action, entry.ResourceType, entry.ResourceID, entry.Details, entry.CreatedAt,
	)
	if err != nil {
		return fmt.Errorf("inserting audit entry: %w", err)
	}
	return nil
}

// ListAuditEntries returns audit log entries for an organization, ordered by most recent first.
func (s *PostgresStore) ListAuditEntries(ctx context.Context, orgID string, limit, offset int) ([]AuditEntry, error) {
	if limit <= 0 {
		limit = 50
	}
	if limit > 200 {
		limit = 200
	}

	rows, err := s.pool.Query(ctx,
		`SELECT a.id, a.org_id, a.user_id, a.action, a.resource_type, a.resource_id, a.details, a.created_at,
		        COALESCE(u.email, '')
		 FROM audit_log a
		 LEFT JOIN users u ON u.id = a.user_id
		 WHERE a.org_id = $1
		 ORDER BY a.created_at DESC
		 LIMIT $2 OFFSET $3`, orgID, limit, offset,
	)
	if err != nil {
		return nil, fmt.Errorf("listing audit entries: %w", err)
	}
	defer rows.Close()

	entries := make([]AuditEntry, 0)
	for rows.Next() {
		var e AuditEntry
		if err := rows.Scan(&e.ID, &e.OrgID, &e.UserID, &e.Action, &e.ResourceType, &e.ResourceID, &e.Details, &e.CreatedAt, &e.UserEmail); err != nil {
			return nil, fmt.Errorf("scanning audit entry: %w", err)
		}
		entries = append(entries, e)
	}
	if err := rows.Err(); err != nil {
		return nil, fmt.Errorf("iterating audit entries: %w", err)
	}
	return entries, nil
}
