package cloud

import (
	"context"
	"encoding/json"
	"net/http"
	"time"
)

// CreateWebhookRequest is the payload for creating a webhook.
type CreateWebhookRequest struct {
	URL    string   `json:"url"`
	Secret string   `json:"secret"`
	Events []string `json:"events"`
}

// HandleCreateWebhook creates a new webhook for an organization.
func (h *Handlers) HandleCreateWebhook(w http.ResponseWriter, r *http.Request) {
	user := UserFromContext(r.Context())
	org, _ := h.requireOrgMembership(w, r, "owner", "admin")
	if org == nil {
		return
	}

	var req CreateWebhookRequest
	if err := json.NewDecoder(r.Body).Decode(&req); err != nil {
		http.Error(w, `{"error":"invalid request body"}`, http.StatusBadRequest)
		return
	}

	if req.URL == "" {
		http.Error(w, `{"error":"url is required"}`, http.StatusBadRequest)
		return
	}
	if len(req.Events) == 0 {
		req.Events = []string{"*"}
	}

	wh := &OrgWebhook{
		OrgID:  org.ID,
		URL:    req.URL,
		Secret: req.Secret,
		Events: req.Events,
		Active: true,
	}

	if err := h.store.CreateWebhook(r.Context(), wh); err != nil {
		h.logger.Error("failed to create webhook", "error", err, "org_id", org.ID)
		http.Error(w, `{"error":"failed to create webhook"}`, http.StatusInternalServerError)
		return
	}

	// Audit log
	if user != nil {
		go func() {
			details, _ := json.Marshal(map[string]interface{}{"url": req.URL, "events": req.Events})
			_ = h.store.InsertAuditEntry(context.Background(), &AuditEntry{
				OrgID:        org.ID,
				UserID:       user.ID,
				Action:       "webhook.created",
				ResourceType: "webhook",
				ResourceID:   wh.ID,
				Details:      details,
			})
		}()
	}

	h.logger.Info("webhook created", "webhook_id", wh.ID, "org_id", org.ID)

	writeJSON(w, http.StatusCreated, wh)
}

// HandleListWebhooks returns all webhooks for an organization.
func (h *Handlers) HandleListWebhooks(w http.ResponseWriter, r *http.Request) {
	org, _ := h.requireOrgMembership(w, r)
	if org == nil {
		return
	}

	webhooks, err := h.store.ListWebhooks(r.Context(), org.ID)
	if err != nil {
		h.logger.Error("failed to list webhooks", "error", err, "org_id", org.ID)
		http.Error(w, `{"error":"failed to list webhooks"}`, http.StatusInternalServerError)
		return
	}

	writeJSON(w, http.StatusOK, webhooks)
}

// HandleDeleteWebhook removes a webhook from an organization.
func (h *Handlers) HandleDeleteWebhook(w http.ResponseWriter, r *http.Request) {
	user := UserFromContext(r.Context())
	org, _ := h.requireOrgMembership(w, r, "owner", "admin")
	if org == nil {
		return
	}

	webhookID := r.PathValue("webhookId")
	if webhookID == "" {
		http.Error(w, `{"error":"webhook ID is required"}`, http.StatusBadRequest)
		return
	}

	if err := h.store.DeleteWebhook(r.Context(), org.ID, webhookID); err != nil {
		h.logger.Error("failed to delete webhook", "error", err, "org_id", org.ID)
		http.Error(w, `{"error":"webhook not found"}`, http.StatusNotFound)
		return
	}

	// Audit log
	if user != nil {
		go func() {
			details, _ := json.Marshal(map[string]interface{}{"webhook_id": webhookID})
			_ = h.store.InsertAuditEntry(context.Background(), &AuditEntry{
				OrgID:        org.ID,
				UserID:       user.ID,
				Action:       "webhook.deleted",
				ResourceType: "webhook",
				ResourceID:   webhookID,
				Details:      details,
			})
		}()
	}

	h.logger.Info("webhook deleted", "webhook_id", webhookID, "org_id", org.ID)

	writeJSON(w, http.StatusOK, map[string]string{"status": "deleted"})
}

// fireOrgWebhooks sends webhook events to all active webhooks for an organization.
func (h *Handlers) fireOrgWebhooks(orgID string, event string, data interface{}) {
	if orgID == "" {
		return
	}
	webhooks, err := h.store.ListWebhooks(context.Background(), orgID)
	if err != nil {
		h.logger.Error("failed to list webhooks", "error", err, "org_id", orgID)
		return
	}
	payload := WebhookPayload{
		Event:     event,
		Timestamp: time.Now(),
		Data:      data,
	}
	for _, wh := range webhooks {
		if !wh.Active {
			continue
		}
		for _, e := range wh.Events {
			if e == event || e == "*" {
				go DeliverWebhook(&wh, payload, h.logger)
				break
			}
		}
	}
}
