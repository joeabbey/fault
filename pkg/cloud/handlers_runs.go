package cloud

import (
	"encoding/json"
	"net/http"
	"strconv"
	"strings"
	"time"

	"github.com/google/uuid"
)

// CreateRunRequest is the payload for uploading an audit run.
type CreateRunRequest struct {
	RepoURL         string          `json:"repo_url"`
	Branch          string          `json:"branch"`
	CommitSHA       string          `json:"commit_sha"`
	CommitRange     string          `json:"commit_range"`
	Mode            string          `json:"mode"`
	DurationMs      int             `json:"duration_ms"`
	FilesChanged    int             `json:"files_changed"`
	Errors          int             `json:"errors"`
	Warnings        int             `json:"warnings"`
	Infos           int             `json:"infos"`
	TotalIssues     int             `json:"total_issues"`
	Issues          json.RawMessage `json:"issues"`
	ConfidenceScore *float64        `json:"confidence_score,omitempty"`
	Summary         string          `json:"summary"`
	Metadata        json.RawMessage `json:"metadata,omitempty"`
	OrgID           string          `json:"org_id,omitempty"`
}

// CreateRunResponse is returned after creating a run.
type CreateRunResponse struct {
	ID        string    `json:"id"`
	CreatedAt time.Time `json:"created_at"`
}

// ListRunsResponse is returned by the list runs endpoint.
type ListRunsResponse struct {
	Runs   []Run `json:"runs"`
	Total  int   `json:"total"`
	Limit  int   `json:"limit"`
	Offset int   `json:"offset"`
}

// HandleCreateRun accepts an uploaded audit run result.
func (h *Handlers) HandleCreateRun(w http.ResponseWriter, r *http.Request) {
	user := UserFromContext(r.Context())
	if user == nil {
		http.Error(w, `{"error":"unauthorized"}`, http.StatusUnauthorized)
		return
	}

	var req CreateRunRequest
	if err := json.NewDecoder(r.Body).Decode(&req); err != nil {
		http.Error(w, `{"error":"invalid request body"}`, http.StatusBadRequest)
		return
	}

	if req.Issues == nil {
		req.Issues = json.RawMessage("[]")
	}
	if req.Metadata == nil {
		req.Metadata = json.RawMessage("{}")
	}
	if req.Mode == "" {
		req.Mode = "audit"
	}

	// Auto-tag with org if user belongs to one
	orgID := req.OrgID
	if orgID == "" {
		orgs, err := h.store.ListUserOrganizations(r.Context(), user.ID)
		if err == nil && len(orgs) > 0 {
			orgID = orgs[0].ID
		}
	}

	now := time.Now()
	run := &Run{
		ID:              uuid.New().String(),
		UserID:          user.ID,
		OrgID:           orgID,
		RepoURL:         req.RepoURL,
		Branch:          req.Branch,
		CommitSHA:       req.CommitSHA,
		CommitRange:     req.CommitRange,
		Mode:            req.Mode,
		Timestamp:       now,
		DurationMs:      req.DurationMs,
		FilesChanged:    req.FilesChanged,
		Errors:          req.Errors,
		Warnings:        req.Warnings,
		Infos:           req.Infos,
		TotalIssues:     req.TotalIssues,
		Issues:          req.Issues,
		ConfidenceScore: req.ConfidenceScore,
		Summary:         req.Summary,
		Metadata:        req.Metadata,
		CreatedAt:       now,
	}

	if err := h.store.CreateRun(r.Context(), run); err != nil {
		h.logger.Error("failed to create run", "error", err, "user_id", user.ID)
		http.Error(w, `{"error":"failed to create run"}`, http.StatusInternalServerError)
		return
	}

	h.logger.Info("run created", "run_id", run.ID, "user_id", user.ID, "repo", req.RepoURL)

	writeJSON(w, http.StatusCreated, CreateRunResponse{
		ID:        run.ID,
		CreatedAt: run.CreatedAt,
	})
}

// HandleListRuns returns paginated runs for the authenticated user.
func (h *Handlers) HandleListRuns(w http.ResponseWriter, r *http.Request) {
	user := UserFromContext(r.Context())
	if user == nil {
		http.Error(w, `{"error":"unauthorized"}`, http.StatusUnauthorized)
		return
	}

	limit := parseIntParam(r, "limit", 20)
	offset := parseIntParam(r, "offset", 0)

	runs, err := h.store.ListRuns(r.Context(), user.ID, limit, offset)
	if err != nil {
		h.logger.Error("failed to list runs", "error", err, "user_id", user.ID)
		http.Error(w, `{"error":"failed to list runs"}`, http.StatusInternalServerError)
		return
	}

	writeJSON(w, http.StatusOK, ListRunsResponse{
		Runs:   runs,
		Total:  len(runs),
		Limit:  limit,
		Offset: offset,
	})
}

// HandleGetRun returns a single run by ID.
func (h *Handlers) HandleGetRun(w http.ResponseWriter, r *http.Request) {
	user := UserFromContext(r.Context())
	if user == nil {
		http.Error(w, `{"error":"unauthorized"}`, http.StatusUnauthorized)
		return
	}

	runID := r.PathValue("id")
	if runID == "" {
		// Fallback: extract from URL path for Go <1.22 compat
		parts := strings.Split(r.URL.Path, "/")
		if len(parts) > 0 {
			runID = parts[len(parts)-1]
		}
	}

	if runID == "" {
		http.Error(w, `{"error":"run ID is required"}`, http.StatusBadRequest)
		return
	}

	run, err := h.store.GetRun(r.Context(), user.ID, runID)
	if err != nil {
		http.Error(w, `{"error":"run not found"}`, http.StatusNotFound)
		return
	}

	writeJSON(w, http.StatusOK, run)
}

// HandleGetRunStats returns aggregate run statistics for the authenticated user.
func (h *Handlers) HandleGetRunStats(w http.ResponseWriter, r *http.Request) {
	user := UserFromContext(r.Context())
	if user == nil {
		http.Error(w, `{"error":"unauthorized"}`, http.StatusUnauthorized)
		return
	}

	stats, err := h.store.GetRunStats(r.Context(), user.ID)
	if err != nil {
		h.logger.Error("failed to get run stats", "error", err, "user_id", user.ID)
		http.Error(w, `{"error":"failed to get run stats"}`, http.StatusInternalServerError)
		return
	}

	writeJSON(w, http.StatusOK, stats)
}

// parseIntParam extracts an integer query parameter with a default value.
func parseIntParam(r *http.Request, name string, defaultVal int) int {
	s := r.URL.Query().Get(name)
	if s == "" {
		return defaultVal
	}
	v, err := strconv.Atoi(s)
	if err != nil {
		return defaultVal
	}
	return v
}
