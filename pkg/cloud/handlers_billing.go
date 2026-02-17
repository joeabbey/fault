package cloud

import (
	"encoding/json"
	"log/slog"
	"net/http"

	"github.com/joeabbey/magma/pkg/limits"
	magmastripe "github.com/joeabbey/magma/pkg/stripe"
)

// BillingHandlers holds dependencies for billing-related HTTP handlers.
type BillingHandlers struct {
	store          Store
	stripeClient   *magmastripe.Client
	limitsEngine   *limits.Engine
	webhookHandler *magmastripe.WebhookHandler
	prices         map[string]string // plan name → Stripe price ID
	appURL         string
	logger         *slog.Logger
}

// NewBillingHandlers creates a new BillingHandlers.
func NewBillingHandlers(
	store Store,
	stripeClient *magmastripe.Client,
	limitsEngine *limits.Engine,
	webhookHandler *magmastripe.WebhookHandler,
	prices map[string]string,
	appURL string,
	logger *slog.Logger,
) *BillingHandlers {
	return &BillingHandlers{
		store:          store,
		stripeClient:   stripeClient,
		limitsEngine:   limitsEngine,
		webhookHandler: webhookHandler,
		prices:         prices,
		appURL:         appURL,
		logger:         logger,
	}
}

// CheckoutRequest is the payload for creating a checkout session.
type CheckoutRequest struct {
	Plan string `json:"plan"` // "pro" or "team"
}

// CheckoutResponse is returned with the Stripe checkout URL.
type CheckoutResponse struct {
	CheckoutURL string `json:"checkout_url"`
}

// PortalResponse is returned with the Stripe billing portal URL.
type PortalResponse struct {
	PortalURL string `json:"portal_url"`
}

// SubscriptionResponse is returned by the subscription status endpoint.
type SubscriptionResponse struct {
	Plan         string `json:"plan"`
	Status       string `json:"status"`
	LLMCalls     int    `json:"llm_calls"`
	LLMLimit     int    `json:"llm_limit"`
	LLMRemaining int    `json:"llm_remaining"`
}

// HandleCheckout creates a Stripe checkout session for plan upgrade.
func (bh *BillingHandlers) HandleCheckout(w http.ResponseWriter, r *http.Request) {
	user := UserFromContext(r.Context())
	if user == nil {
		http.Error(w, `{"error":"unauthorized"}`, http.StatusUnauthorized)
		return
	}

	var req CheckoutRequest
	if err := json.NewDecoder(r.Body).Decode(&req); err != nil {
		http.Error(w, `{"error":"invalid request body"}`, http.StatusBadRequest)
		return
	}

	priceID, ok := bh.prices[req.Plan]
	if !ok || req.Plan == "free" {
		http.Error(w, `{"error":"invalid plan, must be 'pro' or 'team'"}`, http.StatusBadRequest)
		return
	}

	// Get or create Stripe customer
	cust, err := bh.stripeClient.GetOrCreateCustomer(user.Email, "", map[string]string{
		"fault_user_id": user.ID,
	})
	if err != nil {
		bh.logger.Error("failed to get/create Stripe customer", "error", err, "user_id", user.ID)
		http.Error(w, `{"error":"failed to create checkout session"}`, http.StatusInternalServerError)
		return
	}

	// Save Stripe customer ID if new
	if user.StripeCustomerID == "" {
		if err := bh.store.SetStripeCustomerID(r.Context(), user.ID, cust.ID); err != nil {
			bh.logger.Error("failed to save stripe customer ID", "error", err, "user_id", user.ID)
		}
	}

	successURL := bh.appURL + "/billing/success?session_id={CHECKOUT_SESSION_ID}"
	cancelURL := bh.appURL + "/billing/cancel"

	sess, err := bh.stripeClient.CreateCheckoutSession(magmastripe.CheckoutParams{
		CustomerID:          cust.ID,
		PriceID:             priceID,
		Quantity:            1,
		SuccessURL:          successURL,
		CancelURL:           cancelURL,
		AllowPromotionCodes: true,
		Metadata: map[string]string{
			"fault_user_id": user.ID,
			"plan":          req.Plan,
		},
	})
	if err != nil {
		bh.logger.Error("failed to create checkout session", "error", err, "user_id", user.ID)
		http.Error(w, `{"error":"failed to create checkout session"}`, http.StatusInternalServerError)
		return
	}

	bh.logger.Info("checkout session created", "user_id", user.ID, "plan", req.Plan)

	writeJSON(w, http.StatusOK, CheckoutResponse{
		CheckoutURL: sess.URL,
	})
}

// HandlePortal creates a Stripe billing portal session.
func (bh *BillingHandlers) HandlePortal(w http.ResponseWriter, r *http.Request) {
	user := UserFromContext(r.Context())
	if user == nil {
		http.Error(w, `{"error":"unauthorized"}`, http.StatusUnauthorized)
		return
	}

	if user.StripeCustomerID == "" {
		http.Error(w, `{"error":"no billing account found, upgrade first"}`, http.StatusBadRequest)
		return
	}

	returnURL := bh.appURL + "/billing"

	sess, err := bh.stripeClient.CreatePortalSession(user.StripeCustomerID, returnURL)
	if err != nil {
		bh.logger.Error("failed to create portal session", "error", err, "user_id", user.ID)
		http.Error(w, `{"error":"failed to create portal session"}`, http.StatusInternalServerError)
		return
	}

	writeJSON(w, http.StatusOK, PortalResponse{
		PortalURL: sess.URL,
	})
}

// HandleSubscription returns the current subscription status and usage.
func (bh *BillingHandlers) HandleSubscription(w http.ResponseWriter, r *http.Request) {
	user := UserFromContext(r.Context())
	if user == nil {
		http.Error(w, `{"error":"unauthorized"}`, http.StatusUnauthorized)
		return
	}

	check := bh.limitsEngine.Check(r.Context(), user.ID, "llm_calls")

	writeJSON(w, http.StatusOK, SubscriptionResponse{
		Plan:         user.Plan,
		Status:       "active",
		LLMCalls:     check.Limit - check.Remaining,
		LLMLimit:     check.Limit,
		LLMRemaining: check.Remaining,
	})
}

// HandleWebhook proxies Stripe webhook events to the magma WebhookHandler.
func (bh *BillingHandlers) HandleWebhook(w http.ResponseWriter, r *http.Request) {
	bh.webhookHandler.Handle(w, r)
}
