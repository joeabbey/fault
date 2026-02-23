package cloud

import (
	"net/http"
)

// ListSpecResultsResponse is returned by the spec results listing endpoint.
type ListSpecResultsResponse struct {
	Results []SpecResult `json:"results"`
	Total   int          `json:"total"`
	Limit   int          `json:"limit"`
	Offset  int          `json:"offset"`
}

// HandleListSpecResults returns paginated spec validation results for the authenticated user.
func (h *Handlers) HandleListSpecResults(w http.ResponseWriter, r *http.Request) {
	user := UserFromContext(r.Context())
	if user == nil {
		http.Error(w, `{"error":"unauthorized"}`, http.StatusUnauthorized)
		return
	}

	limit := parseIntParam(r, "limit", 20)
	offset := parseIntParam(r, "offset", 0)

	results, err := h.store.GetSpecResults(r.Context(), user.ID, limit, offset)
	if err != nil {
		h.logger.Error("failed to list spec results", "error", err, "user_id", user.ID)
		http.Error(w, `{"error":"failed to list spec results"}`, http.StatusInternalServerError)
		return
	}

	writeJSON(w, http.StatusOK, ListSpecResultsResponse{
		Results: results,
		Total:   len(results),
		Limit:   limit,
		Offset:  offset,
	})
}
