package cloud

import (
	"context"
	"encoding/json"
	"fmt"
	"time"

	"github.com/google/uuid"
)

// SpecResult represents a stored spec validation result.
type SpecResult struct {
	ID               string          `json:"id"`
	UserID           string          `json:"user_id"`
	SpecHash         string          `json:"spec_hash"`
	SpecTitle        string          `json:"spec_title"`
	TotalRequirements int            `json:"total_requirements"`
	AnchoredCount    int             `json:"anchored_count"`
	ImplementedCount int             `json:"implemented_count"`
	PartialCount     int             `json:"partial_count"`
	MissingCount     int             `json:"missing_count"`
	OverallScore     float64         `json:"overall_score"`
	ResultJSON       json.RawMessage `json:"result_json"`
	CreatedAt        time.Time       `json:"created_at"`
}

// SaveSpecResult inserts a new spec validation result.
func (s *PostgresStore) SaveSpecResult(ctx context.Context, result *SpecResult) error {
	if result.ID == "" {
		result.ID = uuid.New().String()
	}
	if result.CreatedAt.IsZero() {
		result.CreatedAt = time.Now()
	}
	if result.ResultJSON == nil {
		result.ResultJSON = json.RawMessage("[]")
	}

	_, err := s.pool.Exec(ctx,
		`INSERT INTO spec_results (id, user_id, spec_hash, spec_title,
		                           total_requirements, anchored_count,
		                           implemented_count, partial_count, missing_count,
		                           overall_score, result_json, created_at)
		 VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12)`,
		result.ID, result.UserID, result.SpecHash, result.SpecTitle,
		result.TotalRequirements, result.AnchoredCount,
		result.ImplementedCount, result.PartialCount, result.MissingCount,
		result.OverallScore, result.ResultJSON, result.CreatedAt,
	)
	if err != nil {
		return fmt.Errorf("saving spec result: %w", err)
	}
	return nil
}

// GetSpecResults returns recent spec validation results for a user.
func (s *PostgresStore) GetSpecResults(ctx context.Context, userID string, limit, offset int) ([]SpecResult, error) {
	if limit <= 0 {
		limit = 20
	}
	if limit > 100 {
		limit = 100
	}

	rows, err := s.pool.Query(ctx,
		`SELECT id, user_id, spec_hash, spec_title,
		        total_requirements, anchored_count,
		        implemented_count, partial_count, missing_count,
		        overall_score, result_json, created_at
		 FROM spec_results WHERE user_id = $1
		 ORDER BY created_at DESC
		 LIMIT $2 OFFSET $3`, userID, limit, offset,
	)
	if err != nil {
		return nil, fmt.Errorf("listing spec results: %w", err)
	}
	defer rows.Close()

	results := make([]SpecResult, 0)
	for rows.Next() {
		var r SpecResult
		if err := rows.Scan(&r.ID, &r.UserID, &r.SpecHash, &r.SpecTitle,
			&r.TotalRequirements, &r.AnchoredCount,
			&r.ImplementedCount, &r.PartialCount, &r.MissingCount,
			&r.OverallScore, &r.ResultJSON, &r.CreatedAt); err != nil {
			return nil, fmt.Errorf("scanning spec result: %w", err)
		}
		results = append(results, r)
	}
	return results, nil
}
