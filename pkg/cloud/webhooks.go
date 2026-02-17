package cloud

import (
	"context"
	"log/slog"
	"time"

	"github.com/google/uuid"
	"github.com/stripe/stripe-go/v76"
)

// FaultWebhookCallbacks implements the magma stripe.WebhookCallbacks interface
// to handle Stripe subscription lifecycle events.
type FaultWebhookCallbacks struct {
	store  Store
	prices map[string]string // reversed: Stripe price ID → plan name
	logger *slog.Logger
}

// NewFaultWebhookCallbacks creates webhook callbacks with a reversed price map
// (priceID → planName) for mapping Stripe events back to plan names.
func NewFaultWebhookCallbacks(store Store, prices map[string]string, logger *slog.Logger) *FaultWebhookCallbacks {
	reversed := make(map[string]string)
	for plan, priceID := range prices {
		reversed[priceID] = plan
	}
	return &FaultWebhookCallbacks{
		store:  store,
		prices: reversed,
		logger: logger,
	}
}

// OnSubscriptionCreated handles new subscription creation from Stripe.
func (cb *FaultWebhookCallbacks) OnSubscriptionCreated(ctx context.Context, sub *stripe.Subscription) error {
	userID := sub.Metadata["fault_user_id"]
	if userID == "" {
		// Checkout session metadata isn't copied to subscription — look up by customer ID
		user, _ := cb.store.GetUserByStripeCustomerID(ctx, sub.Customer.ID)
		if user != nil {
			userID = user.ID
		} else {
			cb.logger.Warn("subscription created but can't find user", "stripe_sub_id", sub.ID)
			return nil
		}
	}

	plan := cb.planFromSubscription(sub)
	cb.logger.Info("subscription created", "user_id", userID, "plan", plan, "stripe_sub_id", sub.ID)

	now := time.Now()
	periodStart := time.Unix(sub.CurrentPeriodStart, 0)
	periodEnd := time.Unix(sub.CurrentPeriodEnd, 0)

	if err := cb.store.UpsertSubscription(ctx, &Subscription{
		ID:                   uuid.New().String(),
		UserID:               userID,
		StripeSubscriptionID: sub.ID,
		StripeCustomerID:     sub.Customer.ID,
		Plan:                 plan,
		Status:               string(sub.Status),
		CurrentPeriodStart:   &periodStart,
		CurrentPeriodEnd:     &periodEnd,
		CreatedAt:            now,
		UpdatedAt:            now,
	}); err != nil {
		cb.logger.Error("failed to upsert subscription", "error", err)
		return err
	}

	if err := cb.store.UpdateUserPlan(ctx, userID, plan); err != nil {
		cb.logger.Error("failed to update user plan", "error", err)
		return err
	}

	return nil
}

// OnSubscriptionUpdated handles subscription changes (plan, status, period).
func (cb *FaultWebhookCallbacks) OnSubscriptionUpdated(ctx context.Context, sub *stripe.Subscription) error {
	userID := sub.Metadata["fault_user_id"]
	if userID == "" {
		// Try looking up by Stripe customer ID
		user, _ := cb.store.GetUserByStripeCustomerID(ctx, sub.Customer.ID)
		if user != nil {
			userID = user.ID
		} else {
			cb.logger.Warn("subscription updated but can't find user", "stripe_sub_id", sub.ID)
			return nil
		}
	}

	plan := cb.planFromSubscription(sub)
	cb.logger.Info("subscription updated", "user_id", userID, "plan", plan, "status", sub.Status)

	periodStart := time.Unix(sub.CurrentPeriodStart, 0)
	periodEnd := time.Unix(sub.CurrentPeriodEnd, 0)

	if err := cb.store.UpsertSubscription(ctx, &Subscription{
		ID:                   uuid.New().String(),
		UserID:               userID,
		StripeSubscriptionID: sub.ID,
		StripeCustomerID:     sub.Customer.ID,
		Plan:                 plan,
		Status:               string(sub.Status),
		CurrentPeriodStart:   &periodStart,
		CurrentPeriodEnd:     &periodEnd,
	}); err != nil {
		cb.logger.Error("failed to upsert subscription", "error", err)
		return err
	}

	if err := cb.store.UpdateUserPlan(ctx, userID, plan); err != nil {
		cb.logger.Error("failed to update user plan", "error", err)
		return err
	}

	return nil
}

// OnSubscriptionDeleted handles subscription cancellation.
func (cb *FaultWebhookCallbacks) OnSubscriptionDeleted(ctx context.Context, sub *stripe.Subscription) error {
	userID := sub.Metadata["fault_user_id"]
	if userID == "" {
		user, _ := cb.store.GetUserByStripeCustomerID(ctx, sub.Customer.ID)
		if user != nil {
			userID = user.ID
		} else {
			cb.logger.Warn("subscription deleted but can't find user", "stripe_sub_id", sub.ID)
			return nil
		}
	}

	cb.logger.Info("subscription deleted", "user_id", userID, "stripe_sub_id", sub.ID)

	periodStart := time.Unix(sub.CurrentPeriodStart, 0)
	periodEnd := time.Unix(sub.CurrentPeriodEnd, 0)

	if err := cb.store.UpsertSubscription(ctx, &Subscription{
		ID:                   uuid.New().String(),
		UserID:               userID,
		StripeSubscriptionID: sub.ID,
		StripeCustomerID:     sub.Customer.ID,
		Plan:                 "free",
		Status:               "canceled",
		CurrentPeriodStart:   &periodStart,
		CurrentPeriodEnd:     &periodEnd,
	}); err != nil {
		cb.logger.Error("failed to upsert subscription", "error", err)
		return err
	}

	// Downgrade user to free
	if err := cb.store.UpdateUserPlan(ctx, userID, "free"); err != nil {
		cb.logger.Error("failed to downgrade user plan", "error", err)
		return err
	}

	return nil
}

// OnPaymentSucceeded logs successful payments.
func (cb *FaultWebhookCallbacks) OnPaymentSucceeded(ctx context.Context, invoice *stripe.Invoice) error {
	cb.logger.Info("payment succeeded",
		"customer", invoice.Customer.ID,
		"amount", invoice.AmountPaid,
		"currency", invoice.Currency,
	)
	return nil
}

// OnPaymentFailed logs failed payments.
func (cb *FaultWebhookCallbacks) OnPaymentFailed(ctx context.Context, invoice *stripe.Invoice) error {
	cb.logger.Warn("payment failed",
		"customer", invoice.Customer.ID,
		"amount", invoice.AmountDue,
		"currency", invoice.Currency,
	)
	return nil
}

// planFromSubscription extracts the plan name from a Stripe subscription
// by looking up the first item's price ID in the reversed prices map.
func (cb *FaultWebhookCallbacks) planFromSubscription(sub *stripe.Subscription) string {
	if sub.Items != nil {
		for _, item := range sub.Items.Data {
			if item.Price != nil {
				if plan, ok := cb.prices[item.Price.ID]; ok {
					return plan
				}
			}
		}
	}
	// Fallback: check metadata
	if plan, ok := sub.Metadata["plan"]; ok {
		return plan
	}
	return "free"
}
