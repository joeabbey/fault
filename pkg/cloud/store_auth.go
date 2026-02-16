package cloud

import (
	"context"
	"fmt"
	"time"

	"github.com/google/uuid"
)

// GetUserByGoogleID looks up a user by their Google ID.
// Returns nil, nil if not found.
func (s *PostgresStore) GetUserByGoogleID(ctx context.Context, googleID string) (*User, error) {
	var u User
	err := s.pool.QueryRow(ctx,
		`SELECT id, email, plan, COALESCE(api_key_hash, ''), COALESCE(stripe_customer_id, ''),
		        COALESCE(google_id, ''), COALESCE(name, ''), COALESCE(picture_url, ''),
		        created_at, COALESCE(last_active_at, created_at)
		 FROM users WHERE google_id = $1`, googleID,
	).Scan(&u.ID, &u.Email, &u.Plan, &u.APIKeyHash, &u.StripeCustomerID,
		&u.GoogleID, &u.Name, &u.PictureURL,
		&u.CreatedAt, &u.LastActiveAt)
	if err != nil {
		return nil, nil // not found
	}

	// Update last active timestamp
	_, _ = s.pool.Exec(ctx,
		`UPDATE users SET last_active_at = NOW() WHERE id = $1`, u.ID)

	return &u, nil
}

// CreateUserFromGoogle creates a new user from Google OAuth info.
func (s *PostgresStore) CreateUserFromGoogle(ctx context.Context, email, googleID, name, pictureURL string) (*User, error) {
	id := uuid.New().String()
	now := time.Now()

	_, err := s.pool.Exec(ctx,
		`INSERT INTO users (id, email, plan, google_id, name, picture_url, created_at, last_active_at)
		 VALUES ($1, $2, 'free', $3, $4, $5, $6, $6)`,
		id, email, googleID, name, pictureURL, now,
	)
	if err != nil {
		return nil, fmt.Errorf("creating user from Google: %w", err)
	}

	return &User{
		ID:           id,
		Email:        email,
		Plan:         "free",
		GoogleID:     googleID,
		Name:         name,
		PictureURL:   pictureURL,
		CreatedAt:    now,
		LastActiveAt: now,
	}, nil
}

// SetGoogleID sets the Google ID on a user (for linking existing API key users).
func (s *PostgresStore) SetGoogleID(ctx context.Context, userID, googleID string) error {
	_, err := s.pool.Exec(ctx,
		`UPDATE users SET google_id = $1 WHERE id = $2`,
		googleID, userID,
	)
	if err != nil {
		return fmt.Errorf("setting Google ID: %w", err)
	}
	return nil
}

// UpdateUserProfile updates the user's name and picture URL.
func (s *PostgresStore) UpdateUserProfile(ctx context.Context, userID, name, pictureURL string) error {
	_, err := s.pool.Exec(ctx,
		`UPDATE users SET name = $1, picture_url = $2 WHERE id = $3`,
		name, pictureURL, userID,
	)
	if err != nil {
		return fmt.Errorf("updating user profile: %w", err)
	}
	return nil
}
