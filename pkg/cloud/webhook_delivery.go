package cloud

import (
	"bytes"
	"crypto/hmac"
	"crypto/sha256"
	"encoding/hex"
	"encoding/json"
	"fmt"
	"log/slog"
	"net/http"
	"time"
)

// WebhookPayload is the JSON body sent to webhook URLs.
type WebhookPayload struct {
	Event     string      `json:"event"`
	Timestamp time.Time   `json:"timestamp"`
	Data      interface{} `json:"data"`
}

// DeliverWebhook signs the payload with HMAC-SHA256 and sends it to the webhook URL.
// Retries up to 3 times with exponential backoff on non-2xx responses.
func DeliverWebhook(wh *OrgWebhook, payload WebhookPayload, logger *slog.Logger) {
	body, err := json.Marshal(payload)
	if err != nil {
		logger.Error("failed to marshal webhook payload", "error", err, "webhook_id", wh.ID)
		return
	}

	signature := ""
	if wh.Secret != "" {
		mac := hmac.New(sha256.New, []byte(wh.Secret))
		mac.Write(body)
		signature = hex.EncodeToString(mac.Sum(nil))
	}

	backoffs := []time.Duration{1 * time.Second, 2 * time.Second, 4 * time.Second}
	client := &http.Client{Timeout: 10 * time.Second}

	for attempt := 0; attempt <= len(backoffs); attempt++ {
		if attempt > 0 {
			time.Sleep(backoffs[attempt-1])
		}

		req, err := http.NewRequest("POST", wh.URL, bytes.NewReader(body))
		if err != nil {
			logger.Error("failed to create webhook request", "error", err, "webhook_id", wh.ID)
			return
		}

		req.Header.Set("Content-Type", "application/json")
		req.Header.Set("X-Fault-Event", payload.Event)
		if signature != "" {
			req.Header.Set("X-Fault-Signature", fmt.Sprintf("sha256=%s", signature))
		}

		resp, err := client.Do(req)
		if err != nil {
			logger.Warn("webhook delivery failed", "error", err, "webhook_id", wh.ID, "attempt", attempt+1)
			continue
		}
		resp.Body.Close()

		if resp.StatusCode >= 200 && resp.StatusCode < 300 {
			logger.Debug("webhook delivered", "webhook_id", wh.ID, "status", resp.StatusCode)
			return
		}

		logger.Warn("webhook non-2xx response", "webhook_id", wh.ID, "status", resp.StatusCode, "attempt", attempt+1)
	}

	logger.Error("webhook delivery exhausted retries", "webhook_id", wh.ID, "url", wh.URL)
}
