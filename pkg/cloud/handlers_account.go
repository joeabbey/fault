package cloud

import (
	"net/http"
)

// HandleDeleteAccount deletes the authenticated user's account and all associated data.
// This is the GDPR Article 17 (right to erasure) implementation.
func (h *Handlers) HandleDeleteAccount(w http.ResponseWriter, r *http.Request) {
	user := UserFromContext(r.Context())
	if user == nil {
		http.Error(w, `{"error":"unauthorized"}`, http.StatusUnauthorized)
		return
	}

	if err := h.store.DeleteUser(r.Context(), user.ID); err != nil {
		h.logger.Error("failed to delete user account", "error", err, "user_id", user.ID)
		http.Error(w, `{"error":"failed to delete account"}`, http.StatusInternalServerError)
		return
	}

	h.logger.Info("user account deleted", "user_id", user.ID, "email", user.Email)

	writeJSON(w, http.StatusOK, map[string]string{
		"status":  "deleted",
		"message": "Your account and all associated data have been permanently deleted.",
	})
}
