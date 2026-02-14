package cloud

import (
	"context"
	"crypto/rand"
	"crypto/sha256"
	"encoding/hex"
	"fmt"
	"time"

	"github.com/google/uuid"
	"github.com/jackc/pgx/v5/pgxpool"
)

// User represents an authenticated API user.
type User struct {
	ID           string    `json:"id"`
	Email        string    `json:"email"`
	Plan         string    `json:"plan"` // free, pro, team
	APIKeyHash   string    `json:"-"`
	CreatedAt    time.Time `json:"created_at"`
	LastActiveAt time.Time `json:"last_active_at"`
}

// MonthlyUsage tracks per-user LLM usage for a given month.
type MonthlyUsage struct {
	UserID       string `json:"user_id"`
	Month        string `json:"month"` // YYYY-MM
	LLMCalls     int    `json:"llm_calls"`
	TokensInput  int64  `json:"tokens_input"`
	TokensOutput int64  `json:"tokens_output"`
	Analyses     int    `json:"analyses"`
}

// Usage represents token counts from a single LLM call.
type Usage struct {
	TokensInput  int64
	TokensOutput int64
}

// Store defines the interface for cloud data access.
type Store interface {
	GetUserByAPIKeyHash(ctx context.Context, hash string) (*User, error)
	CreateUser(ctx context.Context, email string) (*User, error)
	GenerateAPIKey(ctx context.Context, userID string) (string, error)
	IncrementUsage(ctx context.Context, userID string, tokens Usage) error
	GetUsage(ctx context.Context, userID string, month string) (*MonthlyUsage, error)
	Close()
}

// HashAPIKey computes the SHA-256 hash of a raw API key.
func HashAPIKey(raw string) string {
	h := sha256.Sum256([]byte(raw))
	return hex.EncodeToString(h[:])
}

// GenerateRawAPIKey creates a new random API key with the fk_ prefix.
func GenerateRawAPIKey() (string, error) {
	b := make([]byte, 32)
	if _, err := rand.Read(b); err != nil {
		return "", fmt.Errorf("generating random bytes: %w", err)
	}
	return "fk_" + hex.EncodeToString(b), nil
}

// PostgresStore implements Store using PostgreSQL via pgx.
type PostgresStore struct {
	pool *pgxpool.Pool
}

// NewPostgresStore creates a new PostgresStore connected to the given database URL.
func NewPostgresStore(ctx context.Context, databaseURL string) (*PostgresStore, error) {
	pool, err := pgxpool.New(ctx, databaseURL)
	if err != nil {
		return nil, fmt.Errorf("connecting to database: %w", err)
	}

	if err := pool.Ping(ctx); err != nil {
		pool.Close()
		return nil, fmt.Errorf("pinging database: %w", err)
	}

	return &PostgresStore{pool: pool}, nil
}

// Close releases the database connection pool.
func (s *PostgresStore) Close() {
	s.pool.Close()
}

// GetUserByAPIKeyHash looks up a user by the SHA-256 hash of their API key.
func (s *PostgresStore) GetUserByAPIKeyHash(ctx context.Context, hash string) (*User, error) {
	var u User
	err := s.pool.QueryRow(ctx,
		`SELECT id, email, plan, api_key_hash, created_at, COALESCE(last_active_at, created_at)
		 FROM users WHERE api_key_hash = $1`, hash,
	).Scan(&u.ID, &u.Email, &u.Plan, &u.APIKeyHash, &u.CreatedAt, &u.LastActiveAt)
	if err != nil {
		return nil, fmt.Errorf("looking up user by API key: %w", err)
	}

	// Update last active timestamp
	_, _ = s.pool.Exec(ctx,
		`UPDATE users SET last_active_at = NOW() WHERE id = $1`, u.ID)

	return &u, nil
}

// CreateUser inserts a new user with the given email address.
func (s *PostgresStore) CreateUser(ctx context.Context, email string) (*User, error) {
	id := uuid.New().String()
	now := time.Now()

	_, err := s.pool.Exec(ctx,
		`INSERT INTO users (id, email, plan, created_at, last_active_at)
		 VALUES ($1, $2, 'free', $3, $3)`,
		id, email, now,
	)
	if err != nil {
		return nil, fmt.Errorf("creating user: %w", err)
	}

	return &User{
		ID:           id,
		Email:        email,
		Plan:         "free",
		CreatedAt:    now,
		LastActiveAt: now,
	}, nil
}

// GenerateAPIKey creates a new API key for the given user, replacing any existing key.
// Returns the raw key (with fk_ prefix). Only the SHA-256 hash is stored.
func (s *PostgresStore) GenerateAPIKey(ctx context.Context, userID string) (string, error) {
	raw, err := GenerateRawAPIKey()
	if err != nil {
		return "", err
	}

	hash := HashAPIKey(raw)
	_, err = s.pool.Exec(ctx,
		`UPDATE users SET api_key_hash = $1 WHERE id = $2`,
		hash, userID,
	)
	if err != nil {
		return "", fmt.Errorf("storing API key hash: %w", err)
	}

	return raw, nil
}

// IncrementUsage adds token counts and increments the call/analysis counters
// for the current month. Creates the usage row if it does not exist.
func (s *PostgresStore) IncrementUsage(ctx context.Context, userID string, tokens Usage) error {
	month := time.Now().Format("2006-01")
	id := uuid.New().String()

	_, err := s.pool.Exec(ctx,
		`INSERT INTO usage (id, user_id, month, llm_calls, tokens_input, tokens_output, analyses)
		 VALUES ($1, $2, $3, 1, $4, $5, 1)
		 ON CONFLICT (user_id, month) DO UPDATE SET
		   llm_calls = usage.llm_calls + 1,
		   tokens_input = usage.tokens_input + $4,
		   tokens_output = usage.tokens_output + $5,
		   analyses = usage.analyses + 1`,
		id, userID, month, tokens.TokensInput, tokens.TokensOutput,
	)
	if err != nil {
		return fmt.Errorf("incrementing usage: %w", err)
	}

	return nil
}

// GetUsage returns usage data for a user in the specified month (YYYY-MM format).
// Returns zero-value usage if no data exists for that month.
func (s *PostgresStore) GetUsage(ctx context.Context, userID string, month string) (*MonthlyUsage, error) {
	var u MonthlyUsage
	err := s.pool.QueryRow(ctx,
		`SELECT user_id, month, llm_calls, tokens_input, tokens_output, analyses
		 FROM usage WHERE user_id = $1 AND month = $2`,
		userID, month,
	).Scan(&u.UserID, &u.Month, &u.LLMCalls, &u.TokensInput, &u.TokensOutput, &u.Analyses)
	if err != nil {
		// No usage row — return zeroes
		return &MonthlyUsage{
			UserID: userID,
			Month:  month,
		}, nil
	}

	return &u, nil
}
