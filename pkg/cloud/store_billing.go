package cloud

import (
	"context"
	"fmt"
	"time"

	"github.com/google/uuid"
)

// GetUserByID looks up a user by their internal ID.
func (s *PostgresStore) GetUserByID(ctx context.Context, userID string) (*User, error) {
	var u User
	err := s.pool.QueryRow(ctx,
		`SELECT id, email, plan, COALESCE(api_key_hash, ''), COALESCE(stripe_customer_id, ''), created_at, COALESCE(last_active_at, created_at)
		 FROM users WHERE id = $1`, userID,
	).Scan(&u.ID, &u.Email, &u.Plan, &u.APIKeyHash, &u.StripeCustomerID, &u.CreatedAt, &u.LastActiveAt)
	if err != nil {
		return nil, fmt.Errorf("looking up user by ID: %w", err)
	}
	return &u, nil
}

// GetUserByStripeCustomerID looks up a user by their Stripe customer ID.
// Returns nil, nil if not found.
func (s *PostgresStore) GetUserByStripeCustomerID(ctx context.Context, customerID string) (*User, error) {
	var u User
	err := s.pool.QueryRow(ctx,
		`SELECT id, email, plan, COALESCE(api_key_hash, ''), COALESCE(stripe_customer_id, ''), created_at, COALESCE(last_active_at, created_at)
		 FROM users WHERE stripe_customer_id = $1`, customerID,
	).Scan(&u.ID, &u.Email, &u.Plan, &u.APIKeyHash, &u.StripeCustomerID, &u.CreatedAt, &u.LastActiveAt)
	if err != nil {
		return nil, nil // not found
	}
	return &u, nil
}

// UpdateUserPlan sets the plan field on a user.
func (s *PostgresStore) UpdateUserPlan(ctx context.Context, userID string, plan string) error {
	_, err := s.pool.Exec(ctx,
		`UPDATE users SET plan = $1 WHERE id = $2`,
		plan, userID,
	)
	if err != nil {
		return fmt.Errorf("updating user plan: %w", err)
	}
	return nil
}

// SetStripeCustomerID sets the Stripe customer ID on a user.
func (s *PostgresStore) SetStripeCustomerID(ctx context.Context, userID string, customerID string) error {
	_, err := s.pool.Exec(ctx,
		`UPDATE users SET stripe_customer_id = $1 WHERE id = $2`,
		customerID, userID,
	)
	if err != nil {
		return fmt.Errorf("setting stripe customer ID: %w", err)
	}
	return nil
}

// UpsertSubscription creates or updates a subscription record.
func (s *PostgresStore) UpsertSubscription(ctx context.Context, sub *Subscription) error {
	if sub.ID == "" {
		sub.ID = uuid.New().String()
	}
	now := time.Now()
	if sub.CreatedAt.IsZero() {
		sub.CreatedAt = now
	}
	sub.UpdatedAt = now

	_, err := s.pool.Exec(ctx,
		`INSERT INTO subscriptions (id, user_id, stripe_subscription_id, stripe_customer_id, plan, status, current_period_start, current_period_end, created_at, updated_at)
		 VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10)
		 ON CONFLICT (stripe_subscription_id) DO UPDATE SET
		   plan = EXCLUDED.plan,
		   status = EXCLUDED.status,
		   current_period_start = EXCLUDED.current_period_start,
		   current_period_end = EXCLUDED.current_period_end,
		   updated_at = EXCLUDED.updated_at`,
		sub.ID, sub.UserID, sub.StripeSubscriptionID, sub.StripeCustomerID,
		sub.Plan, sub.Status, sub.CurrentPeriodStart, sub.CurrentPeriodEnd,
		sub.CreatedAt, sub.UpdatedAt,
	)
	if err != nil {
		return fmt.Errorf("upserting subscription: %w", err)
	}
	return nil
}

// GetSubscriptionByUserID returns the most recent subscription for a user.
// Returns nil, nil if not found.
func (s *PostgresStore) GetSubscriptionByUserID(ctx context.Context, userID string) (*Subscription, error) {
	var sub Subscription
	err := s.pool.QueryRow(ctx,
		`SELECT id, user_id, COALESCE(stripe_subscription_id, ''), COALESCE(stripe_customer_id, ''),
		        plan, status, current_period_start, current_period_end, created_at, updated_at
		 FROM subscriptions WHERE user_id = $1
		 ORDER BY created_at DESC LIMIT 1`, userID,
	).Scan(&sub.ID, &sub.UserID, &sub.StripeSubscriptionID, &sub.StripeCustomerID,
		&sub.Plan, &sub.Status, &sub.CurrentPeriodStart, &sub.CurrentPeriodEnd,
		&sub.CreatedAt, &sub.UpdatedAt)
	if err != nil {
		return nil, nil // not found
	}
	return &sub, nil
}
