package cloud

import (
	"net/http"
	"time"
)

// ExportResponse contains all user data for GDPR data portability.
type ExportResponse struct {
	ExportedAt  string         `json:"exported_at"`
	Account     ExportAccount  `json:"account"`
	Usage       []MonthlyUsage `json:"usage"`
	Runs        []Run          `json:"runs"`
	SpecResults []SpecResult   `json:"spec_results"`
}

// ExportAccount contains the user's account information for export.
type ExportAccount struct {
	ID        string `json:"id"`
	Email     string `json:"email"`
	Plan      string `json:"plan"`
	CreatedAt string `json:"created_at"`
}

// HandleExportData returns all data associated with the authenticated user.
// This is the GDPR Article 20 (right to data portability) implementation.
func (h *Handlers) HandleExportData(w http.ResponseWriter, r *http.Request) {
	user := UserFromContext(r.Context())
	if user == nil {
		http.Error(w, `{"error":"unauthorized"}`, http.StatusUnauthorized)
		return
	}

	ctx := r.Context()

	// Collect all usage data across every month
	allUsage, err := h.store.GetAllUsage(ctx, user.ID)
	if err != nil {
		h.logger.Error("failed to get usage for export", "error", err, "user_id", user.ID)
		http.Error(w, `{"error":"failed to export data"}`, http.StatusInternalServerError)
		return
	}

	// Get runs (up to 1000 — store caps at 100 per page)
	runs, err := h.store.ListRuns(ctx, user.ID, 1000, 0)
	if err != nil {
		h.logger.Error("failed to list runs for export", "error", err, "user_id", user.ID)
		http.Error(w, `{"error":"failed to export data"}`, http.StatusInternalServerError)
		return
	}

	// Get spec results (up to 1000 — store caps at 100 per page)
	specResults, err := h.store.GetSpecResults(ctx, user.ID, 1000, 0)
	if err != nil {
		h.logger.Error("failed to get spec results for export", "error", err, "user_id", user.ID)
		http.Error(w, `{"error":"failed to export data"}`, http.StatusInternalServerError)
		return
	}

	// Ensure non-nil slices for JSON (nil → null, empty → [])
	if allUsage == nil {
		allUsage = make([]MonthlyUsage, 0)
	}
	if runs == nil {
		runs = make([]Run, 0)
	}
	if specResults == nil {
		specResults = make([]SpecResult, 0)
	}

	export := ExportResponse{
		ExportedAt: time.Now().UTC().Format(time.RFC3339),
		Account: ExportAccount{
			ID:        user.ID,
			Email:     user.Email,
			Plan:      user.Plan,
			CreatedAt: user.CreatedAt.UTC().Format(time.RFC3339),
		},
		Usage:       allUsage,
		Runs:        runs,
		SpecResults: specResults,
	}

	h.logger.Info("user data exported", "user_id", user.ID)

	w.Header().Set("Content-Disposition", `attachment; filename="fault-export.json"`)
	writeJSON(w, http.StatusOK, export)
}
