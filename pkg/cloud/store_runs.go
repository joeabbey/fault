package cloud

import (
	"context"
	"encoding/json"
	"fmt"
	"time"

	"github.com/google/uuid"
)

// Run represents a single audit run stored in the database.
type Run struct {
	ID              string          `json:"id"`
	UserID          string          `json:"user_id"`
	OrgID           string          `json:"org_id,omitempty"`
	RepoURL         string          `json:"repo_url"`
	Branch          string          `json:"branch"`
	CommitSHA       string          `json:"commit_sha"`
	CommitRange     string          `json:"commit_range"`
	Mode            string          `json:"mode"`
	Timestamp       time.Time       `json:"timestamp"`
	DurationMs      int             `json:"duration_ms"`
	FilesChanged    int             `json:"files_changed"`
	Errors          int             `json:"errors"`
	Warnings        int             `json:"warnings"`
	Infos           int             `json:"infos"`
	TotalIssues     int             `json:"total_issues"`
	Issues          json.RawMessage `json:"issues"`
	ConfidenceScore *float64        `json:"confidence_score,omitempty"`
	Summary         string          `json:"summary"`
	Metadata        json.RawMessage `json:"metadata"`
	CreatedAt       time.Time       `json:"created_at"`
}

// RunStats provides aggregate statistics for a user's runs.
type RunStats struct {
	TotalRuns    int     `json:"total_runs"`
	TotalIssues  int     `json:"total_issues"`
	AvgErrors    float64 `json:"avg_errors"`
	AvgWarnings  float64 `json:"avg_warnings"`
	AvgDuration  float64 `json:"avg_duration_ms"`
}

// CreateRun inserts a new audit run record.
func (s *PostgresStore) CreateRun(ctx context.Context, run *Run) error {
	if run.ID == "" {
		run.ID = uuid.New().String()
	}
	if run.Timestamp.IsZero() {
		run.Timestamp = time.Now()
	}
	if run.CreatedAt.IsZero() {
		run.CreatedAt = time.Now()
	}
	if run.Issues == nil {
		run.Issues = json.RawMessage("[]")
	}
	if run.Metadata == nil {
		run.Metadata = json.RawMessage("{}")
	}

	_, err := s.pool.Exec(ctx,
		`INSERT INTO runs (id, user_id, org_id, repo_url, branch, commit_sha, commit_range, mode,
		                    timestamp, duration_ms, files_changed, errors, warnings, infos,
		                    total_issues, issues, confidence_score, summary, metadata, created_at)
		 VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14, $15, $16, $17, $18, $19, $20)`,
		run.ID, run.UserID, nullIfEmpty(run.OrgID), run.RepoURL, run.Branch, run.CommitSHA, run.CommitRange,
		run.Mode, run.Timestamp, run.DurationMs, run.FilesChanged,
		run.Errors, run.Warnings, run.Infos, run.TotalIssues,
		run.Issues, run.ConfidenceScore, run.Summary, run.Metadata, run.CreatedAt,
	)
	if err != nil {
		return fmt.Errorf("creating run: %w", err)
	}
	return nil
}

// GetRun retrieves a single run by ID for a given user.
func (s *PostgresStore) GetRun(ctx context.Context, userID, runID string) (*Run, error) {
	var r Run
	err := s.pool.QueryRow(ctx,
		`SELECT id, user_id, COALESCE(org_id, ''), repo_url, branch, commit_sha, commit_range, mode,
		        timestamp, duration_ms, files_changed, errors, warnings, infos,
		        total_issues, issues, confidence_score, summary, metadata, created_at
		 FROM runs WHERE id = $1 AND user_id = $2`, runID, userID,
	).Scan(&r.ID, &r.UserID, &r.OrgID, &r.RepoURL, &r.Branch, &r.CommitSHA, &r.CommitRange,
		&r.Mode, &r.Timestamp, &r.DurationMs, &r.FilesChanged,
		&r.Errors, &r.Warnings, &r.Infos, &r.TotalIssues,
		&r.Issues, &r.ConfidenceScore, &r.Summary, &r.Metadata, &r.CreatedAt)
	if err != nil {
		return nil, fmt.Errorf("getting run: %w", err)
	}
	return &r, nil
}

// ListRuns returns recent runs for a user, ordered by timestamp descending.
func (s *PostgresStore) ListRuns(ctx context.Context, userID string, limit, offset int) ([]Run, error) {
	if limit <= 0 {
		limit = 20
	}
	if limit > 100 {
		limit = 100
	}

	rows, err := s.pool.Query(ctx,
		`SELECT id, user_id, COALESCE(org_id, ''), repo_url, branch, commit_sha, commit_range, mode,
		        timestamp, duration_ms, files_changed, errors, warnings, infos,
		        total_issues, issues, confidence_score, summary, metadata, created_at
		 FROM runs WHERE user_id = $1
		 ORDER BY timestamp DESC
		 LIMIT $2 OFFSET $3`, userID, limit, offset,
	)
	if err != nil {
		return nil, fmt.Errorf("listing runs: %w", err)
	}
	defer rows.Close()

	runs := make([]Run, 0)
	for rows.Next() {
		var r Run
		if err := rows.Scan(&r.ID, &r.UserID, &r.OrgID, &r.RepoURL, &r.Branch, &r.CommitSHA, &r.CommitRange,
			&r.Mode, &r.Timestamp, &r.DurationMs, &r.FilesChanged,
			&r.Errors, &r.Warnings, &r.Infos, &r.TotalIssues,
			&r.Issues, &r.ConfidenceScore, &r.Summary, &r.Metadata, &r.CreatedAt); err != nil {
			return nil, fmt.Errorf("scanning run: %w", err)
		}
		runs = append(runs, r)
	}
	return runs, nil
}

// nullIfEmpty returns nil for empty strings (for nullable DB columns).
func nullIfEmpty(s string) interface{} {
	if s == "" {
		return nil
	}
	return s
}

// GetRunStats returns aggregate statistics for a user's runs.
func (s *PostgresStore) GetRunStats(ctx context.Context, userID string) (*RunStats, error) {
	var stats RunStats
	err := s.pool.QueryRow(ctx,
		`SELECT COUNT(*), COALESCE(SUM(total_issues), 0),
		        COALESCE(AVG(errors), 0), COALESCE(AVG(warnings), 0),
		        COALESCE(AVG(duration_ms), 0)
		 FROM runs WHERE user_id = $1`, userID,
	).Scan(&stats.TotalRuns, &stats.TotalIssues,
		&stats.AvgErrors, &stats.AvgWarnings, &stats.AvgDuration)
	if err != nil {
		return nil, fmt.Errorf("getting run stats: %w", err)
	}
	return &stats, nil
}
