package cloud

import (
	"context"
	"crypto/rand"
	"crypto/sha256"
	_ "embed"
	"encoding/hex"
	"fmt"
	"time"

	"github.com/google/uuid"
	"github.com/jackc/pgx/v5/pgxpool"
)

//go:embed migrations/000001_init.up.sql
var initSQL string

//go:embed migrations/000002_billing.up.sql
var billingSQL string

//go:embed migrations/000003_google_auth.up.sql
var googleAuthSQL string

//go:embed migrations/000004_runs.up.sql
var runsSQL string

//go:embed migrations/000005_spec_results.up.sql
var specResultsSQL string

//go:embed migrations/000006_organizations.up.sql
var organizationsSQL string

//go:embed migrations/000007_webhooks.up.sql
var webhooksSQL string

//go:embed migrations/000008_org_configs.up.sql
var orgConfigsSQL string

// User represents an authenticated API user.
type User struct {
	ID               string    `json:"id"`
	Email            string    `json:"email"`
	Plan             string    `json:"plan"` // free, pro, team
	APIKeyHash       string    `json:"-"`
	StripeCustomerID string    `json:"stripe_customer_id,omitempty"`
	GoogleID         string    `json:"google_id,omitempty"`
	Name             string    `json:"name,omitempty"`
	PictureURL       string    `json:"picture_url,omitempty"`
	CreatedAt        time.Time `json:"created_at"`
	LastActiveAt     time.Time `json:"last_active_at"`
}

// Subscription tracks a Stripe subscription's state locally.
type Subscription struct {
	ID                   string     `json:"id"`
	UserID               string     `json:"user_id"`
	StripeSubscriptionID string     `json:"stripe_subscription_id,omitempty"`
	StripeCustomerID     string     `json:"stripe_customer_id,omitempty"`
	Plan                 string     `json:"plan"`
	Status               string     `json:"status"`
	CurrentPeriodStart   *time.Time `json:"current_period_start,omitempty"`
	CurrentPeriodEnd     *time.Time `json:"current_period_end,omitempty"`
	CreatedAt            time.Time  `json:"created_at"`
	UpdatedAt            time.Time  `json:"updated_at"`
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

// Organization represents a team organization.
type Organization struct {
	ID        string    `json:"id"`
	Name      string    `json:"name"`
	Slug      string    `json:"slug"`
	OwnerID   string    `json:"owner_id"`
	Plan      string    `json:"plan"`
	CreatedAt time.Time `json:"created_at"`
	UpdatedAt time.Time `json:"updated_at"`
}

// OrgMember represents a user's membership in an organization.
type OrgMember struct {
	ID        string    `json:"id"`
	OrgID     string    `json:"org_id"`
	UserID    string    `json:"user_id"`
	Role      string    `json:"role"` // owner, admin, member
	CreatedAt time.Time `json:"created_at"`
	Email     string    `json:"email,omitempty"` // populated from join
	Name      string    `json:"name,omitempty"`  // populated from join
}

// Store defines the interface for cloud data access.
type Store interface {
	GetUserByAPIKeyHash(ctx context.Context, hash string) (*User, error)
	GetUserByEmail(ctx context.Context, email string) (*User, error)
	GetUserByID(ctx context.Context, userID string) (*User, error)
	GetUserByStripeCustomerID(ctx context.Context, customerID string) (*User, error)
	GetUserByGoogleID(ctx context.Context, googleID string) (*User, error)
	CreateUser(ctx context.Context, email string) (*User, error)
	CreateUserFromGoogle(ctx context.Context, email, googleID, name, pictureURL string) (*User, error)
	GenerateAPIKey(ctx context.Context, userID string) (string, error)
	UpdateUserPlan(ctx context.Context, userID string, plan string) error
	SetStripeCustomerID(ctx context.Context, userID string, customerID string) error
	SetGoogleID(ctx context.Context, userID, googleID string) error
	UpdateUserProfile(ctx context.Context, userID, name, pictureURL string) error
	IncrementUsage(ctx context.Context, userID string, tokens Usage) error
	GetUsage(ctx context.Context, userID string, month string) (*MonthlyUsage, error)
	UpsertSubscription(ctx context.Context, sub *Subscription) error
	GetSubscriptionByUserID(ctx context.Context, userID string) (*Subscription, error)
	CreateRun(ctx context.Context, run *Run) error
	GetRun(ctx context.Context, userID, runID string) (*Run, error)
	ListRuns(ctx context.Context, userID string, limit, offset int) ([]Run, error)
	GetRunStats(ctx context.Context, userID string) (*RunStats, error)
	SaveSpecResult(ctx context.Context, result *SpecResult) error
	GetSpecResults(ctx context.Context, userID string, limit, offset int) ([]SpecResult, error)

	// Organization methods
	CreateOrganization(ctx context.Context, org *Organization) error
	GetOrganization(ctx context.Context, orgID string) (*Organization, error)
	GetOrganizationBySlug(ctx context.Context, slug string) (*Organization, error)
	ListUserOrganizations(ctx context.Context, userID string) ([]Organization, error)
	AddOrgMember(ctx context.Context, orgID, userID, role string) error
	RemoveOrgMember(ctx context.Context, orgID, userID string) error
	ListOrgMembers(ctx context.Context, orgID string) ([]OrgMember, error)
	GetOrgMembership(ctx context.Context, orgID, userID string) (*OrgMember, error)
	ListOrgRuns(ctx context.Context, orgID string, limit, offset int) ([]Run, error)
	GetOrgRunStats(ctx context.Context, orgID string) (*RunStats, error)

	// Webhook methods
	CreateWebhook(ctx context.Context, wh *OrgWebhook) error
	ListWebhooks(ctx context.Context, orgID string) ([]OrgWebhook, error)
	DeleteWebhook(ctx context.Context, orgID, webhookID string) error

	// Org config methods
	GetOrgConfig(ctx context.Context, orgID string) (*OrgConfig, error)
	SaveOrgConfig(ctx context.Context, cfg *OrgConfig) error

	// Audit log methods
	InsertAuditEntry(ctx context.Context, entry *AuditEntry) error
	ListAuditEntries(ctx context.Context, orgID string, limit, offset int) ([]AuditEntry, error)

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

// Migrate runs database migrations. Safe to call on every startup since
// the DDL uses IF NOT EXISTS.
func (s *PostgresStore) Migrate(ctx context.Context) error {
	_, err := s.pool.Exec(ctx, initSQL)
	if err != nil {
		return fmt.Errorf("running init migration: %w", err)
	}
	_, err = s.pool.Exec(ctx, billingSQL)
	if err != nil {
		return fmt.Errorf("running billing migration: %w", err)
	}
	_, err = s.pool.Exec(ctx, googleAuthSQL)
	if err != nil {
		return fmt.Errorf("running google auth migration: %w", err)
	}
	_, err = s.pool.Exec(ctx, runsSQL)
	if err != nil {
		return fmt.Errorf("running runs migration: %w", err)
	}
	_, err = s.pool.Exec(ctx, specResultsSQL)
	if err != nil {
		return fmt.Errorf("running spec_results migration: %w", err)
	}
	_, err = s.pool.Exec(ctx, organizationsSQL)
	if err != nil {
		return fmt.Errorf("running organizations migration: %w", err)
	}
	_, err = s.pool.Exec(ctx, webhooksSQL)
	if err != nil {
		return fmt.Errorf("running webhooks migration: %w", err)
	}
	_, err = s.pool.Exec(ctx, orgConfigsSQL)
	if err != nil {
		return fmt.Errorf("running org configs migration: %w", err)
	}
	return nil
}

// Close releases the database connection pool.
func (s *PostgresStore) Close() {
	s.pool.Close()
}

// GetUserByAPIKeyHash looks up a user by the SHA-256 hash of their API key.
func (s *PostgresStore) GetUserByAPIKeyHash(ctx context.Context, hash string) (*User, error) {
	var u User
	err := s.pool.QueryRow(ctx,
		`SELECT id, email, plan, api_key_hash, COALESCE(stripe_customer_id, ''),
		        COALESCE(google_id, ''), COALESCE(name, ''), COALESCE(picture_url, ''),
		        created_at, COALESCE(last_active_at, created_at)
		 FROM users WHERE api_key_hash = $1`, hash,
	).Scan(&u.ID, &u.Email, &u.Plan, &u.APIKeyHash, &u.StripeCustomerID,
		&u.GoogleID, &u.Name, &u.PictureURL,
		&u.CreatedAt, &u.LastActiveAt)
	if err != nil {
		return nil, fmt.Errorf("looking up user by API key: %w", err)
	}

	// Update last active timestamp
	_, _ = s.pool.Exec(ctx,
		`UPDATE users SET last_active_at = NOW() WHERE id = $1`, u.ID)

	return &u, nil
}

// GetUserByEmail looks up a user by email address. Returns nil, nil if not found.
func (s *PostgresStore) GetUserByEmail(ctx context.Context, email string) (*User, error) {
	var u User
	err := s.pool.QueryRow(ctx,
		`SELECT id, email, plan, COALESCE(api_key_hash, ''), COALESCE(stripe_customer_id, ''),
		        COALESCE(google_id, ''), COALESCE(name, ''), COALESCE(picture_url, ''),
		        created_at, COALESCE(last_active_at, created_at)
		 FROM users WHERE email = $1`, email,
	).Scan(&u.ID, &u.Email, &u.Plan, &u.APIKeyHash, &u.StripeCustomerID,
		&u.GoogleID, &u.Name, &u.PictureURL,
		&u.CreatedAt, &u.LastActiveAt)
	if err != nil {
		return nil, nil // not found
	}
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
