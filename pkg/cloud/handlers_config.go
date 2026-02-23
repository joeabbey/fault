package cloud

import (
	"encoding/json"
	"io"
	"net/http"
)

// GetOrgConfigResponse is returned by the get org config endpoint.
type GetOrgConfigResponse struct {
	ConfigYAML string `json:"config_yaml"`
	Version    int    `json:"version"`
	UpdatedBy  string `json:"updated_by,omitempty"`
	UpdatedAt  string `json:"updated_at,omitempty"`
}

// SaveOrgConfigRequest is the payload for saving org config.
type SaveOrgConfigRequest struct {
	ConfigYAML string `json:"config_yaml"`
}

// HandleGetOrgConfig returns the organization's shared config.
func (h *Handlers) HandleGetOrgConfig(w http.ResponseWriter, r *http.Request) {
	user := UserFromContext(r.Context())
	if user == nil {
		http.Error(w, `{"error":"unauthorized"}`, http.StatusUnauthorized)
		return
	}

	slug := r.PathValue("slug")
	org, err := h.store.GetOrganizationBySlug(r.Context(), slug)
	if err != nil || org == nil {
		http.Error(w, `{"error":"organization not found"}`, http.StatusNotFound)
		return
	}

	// Check membership
	member, err := h.store.GetOrgMembership(r.Context(), org.ID, user.ID)
	if err != nil || member == nil {
		http.Error(w, `{"error":"not a member of this organization"}`, http.StatusForbidden)
		return
	}

	cfg, err := h.store.GetOrgConfig(r.Context(), org.ID)
	if err != nil || cfg == nil {
		writeJSON(w, http.StatusOK, GetOrgConfigResponse{})
		return
	}

	writeJSON(w, http.StatusOK, GetOrgConfigResponse{
		ConfigYAML: cfg.ConfigYAML,
		Version:    cfg.Version,
		UpdatedBy:  cfg.UpdatedBy,
		UpdatedAt:  cfg.UpdatedAt.Format("2006-01-02T15:04:05Z07:00"),
	})
}

// HandleSaveOrgConfig saves the organization's shared config.
func (h *Handlers) HandleSaveOrgConfig(w http.ResponseWriter, r *http.Request) {
	user := UserFromContext(r.Context())
	if user == nil {
		http.Error(w, `{"error":"unauthorized"}`, http.StatusUnauthorized)
		return
	}

	slug := r.PathValue("slug")
	org, err := h.store.GetOrganizationBySlug(r.Context(), slug)
	if err != nil || org == nil {
		http.Error(w, `{"error":"organization not found"}`, http.StatusNotFound)
		return
	}

	// Check admin/owner membership
	member, err := h.store.GetOrgMembership(r.Context(), org.ID, user.ID)
	if err != nil || member == nil {
		http.Error(w, `{"error":"not a member of this organization"}`, http.StatusForbidden)
		return
	}
	if member.Role != "owner" && member.Role != "admin" {
		http.Error(w, `{"error":"admin or owner role required"}`, http.StatusForbidden)
		return
	}

	var req SaveOrgConfigRequest
	if err := json.NewDecoder(r.Body).Decode(&req); err != nil {
		http.Error(w, `{"error":"invalid request body"}`, http.StatusBadRequest)
		return
	}

	if err := h.store.SaveOrgConfig(r.Context(), &OrgConfig{
		OrgID:      org.ID,
		ConfigYAML: req.ConfigYAML,
		UpdatedBy:  user.ID,
	}); err != nil {
		h.logger.Error("failed to save org config", "error", err)
		http.Error(w, `{"error":"failed to save config"}`, http.StatusInternalServerError)
		return
	}

	writeJSON(w, http.StatusOK, map[string]string{"status": "ok"})
}

// HandlePullOrgConfig returns the org config as raw YAML (for CLI consumption).
func (h *Handlers) HandlePullOrgConfig(w http.ResponseWriter, r *http.Request) {
	user := UserFromContext(r.Context())
	if user == nil {
		http.Error(w, `{"error":"unauthorized"}`, http.StatusUnauthorized)
		return
	}

	slug := r.PathValue("slug")
	org, err := h.store.GetOrganizationBySlug(r.Context(), slug)
	if err != nil || org == nil {
		http.Error(w, `{"error":"organization not found"}`, http.StatusNotFound)
		return
	}

	member, err := h.store.GetOrgMembership(r.Context(), org.ID, user.ID)
	if err != nil || member == nil {
		http.Error(w, `{"error":"not a member of this organization"}`, http.StatusForbidden)
		return
	}

	cfg, err := h.store.GetOrgConfig(r.Context(), org.ID)
	if err != nil || cfg == nil {
		w.Header().Set("Content-Type", "text/plain")
		w.WriteHeader(http.StatusNoContent)
		return
	}

	w.Header().Set("Content-Type", "text/yaml")
	io.WriteString(w, cfg.ConfigYAML)
}
