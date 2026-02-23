package cloud

import (
	"context"
	"encoding/json"
	"net/http"
	"regexp"
)

var slugRegexp = regexp.MustCompile(`^[a-z0-9][a-z0-9-]{1,38}[a-z0-9]$`)

// CreateOrgRequest is the payload for creating an organization.
type CreateOrgRequest struct {
	Name string `json:"name"`
	Slug string `json:"slug"`
}

// OrgDetailResponse is returned by the get org endpoint.
type OrgDetailResponse struct {
	Organization
	MemberCount int `json:"member_count"`
}

// AddMemberRequest is the payload for adding an org member.
type AddMemberRequest struct {
	Email string `json:"email"`
	Role  string `json:"role"`
}

// ListOrgRunsResponse is returned by the org runs listing endpoint.
type ListOrgRunsResponse struct {
	Runs   []Run `json:"runs"`
	Total  int   `json:"total"`
	Limit  int   `json:"limit"`
	Offset int   `json:"offset"`
}

// requireOrgMembership checks that the user is a member of the org identified by slug.
// Returns the org and membership, or writes an error response and returns nil.
func (h *Handlers) requireOrgMembership(w http.ResponseWriter, r *http.Request, roles ...string) (*Organization, *OrgMember) {
	user := UserFromContext(r.Context())
	if user == nil {
		http.Error(w, `{"error":"unauthorized"}`, http.StatusUnauthorized)
		return nil, nil
	}

	slug := r.PathValue("slug")
	if slug == "" {
		http.Error(w, `{"error":"org slug is required"}`, http.StatusBadRequest)
		return nil, nil
	}

	org, err := h.store.GetOrganizationBySlug(r.Context(), slug)
	if err != nil {
		http.Error(w, `{"error":"organization not found"}`, http.StatusNotFound)
		return nil, nil
	}

	member, err := h.store.GetOrgMembership(r.Context(), org.ID, user.ID)
	if err != nil || member == nil {
		http.Error(w, `{"error":"not a member of this organization"}`, http.StatusForbidden)
		return nil, nil
	}

	if len(roles) > 0 {
		allowed := false
		for _, role := range roles {
			if member.Role == role {
				allowed = true
				break
			}
		}
		if !allowed {
			http.Error(w, `{"error":"insufficient permissions"}`, http.StatusForbidden)
			return nil, nil
		}
	}

	return org, member
}

// HandleCreateOrg creates a new organization.
func (h *Handlers) HandleCreateOrg(w http.ResponseWriter, r *http.Request) {
	user := UserFromContext(r.Context())
	if user == nil {
		http.Error(w, `{"error":"unauthorized"}`, http.StatusUnauthorized)
		return
	}

	if user.Plan != "team" {
		http.Error(w, `{"error":"team plan required to create organizations"}`, http.StatusForbidden)
		return
	}

	var req CreateOrgRequest
	if err := json.NewDecoder(r.Body).Decode(&req); err != nil {
		http.Error(w, `{"error":"invalid request body"}`, http.StatusBadRequest)
		return
	}

	if req.Name == "" {
		http.Error(w, `{"error":"name is required"}`, http.StatusBadRequest)
		return
	}
	if !slugRegexp.MatchString(req.Slug) {
		http.Error(w, `{"error":"slug must be 3-40 lowercase alphanumeric characters or hyphens"}`, http.StatusBadRequest)
		return
	}

	org := &Organization{
		Name:    req.Name,
		Slug:    req.Slug,
		OwnerID: user.ID,
		Plan:    "team",
	}

	if err := h.store.CreateOrganization(r.Context(), org); err != nil {
		h.logger.Error("failed to create organization", "error", err, "user_id", user.ID)
		http.Error(w, `{"error":"failed to create organization"}`, http.StatusInternalServerError)
		return
	}

	// Add creator as owner member
	if err := h.store.AddOrgMember(r.Context(), org.ID, user.ID, "owner"); err != nil {
		h.logger.Error("failed to add owner member", "error", err, "org_id", org.ID)
		http.Error(w, `{"error":"failed to add owner to organization"}`, http.StatusInternalServerError)
		return
	}

	h.logger.Info("organization created", "org_id", org.ID, "slug", org.Slug, "user_id", user.ID)

	writeJSON(w, http.StatusCreated, org)
}

// HandleListOrgs returns the authenticated user's organizations.
func (h *Handlers) HandleListOrgs(w http.ResponseWriter, r *http.Request) {
	user := UserFromContext(r.Context())
	if user == nil {
		http.Error(w, `{"error":"unauthorized"}`, http.StatusUnauthorized)
		return
	}

	orgs, err := h.store.ListUserOrganizations(r.Context(), user.ID)
	if err != nil {
		h.logger.Error("failed to list organizations", "error", err, "user_id", user.ID)
		http.Error(w, `{"error":"failed to list organizations"}`, http.StatusInternalServerError)
		return
	}

	writeJSON(w, http.StatusOK, orgs)
}

// HandleGetOrg returns organization details with member count.
func (h *Handlers) HandleGetOrg(w http.ResponseWriter, r *http.Request) {
	org, _ := h.requireOrgMembership(w, r)
	if org == nil {
		return
	}

	members, err := h.store.ListOrgMembers(r.Context(), org.ID)
	if err != nil {
		h.logger.Error("failed to list org members", "error", err, "org_id", org.ID)
		http.Error(w, `{"error":"failed to get organization details"}`, http.StatusInternalServerError)
		return
	}

	writeJSON(w, http.StatusOK, OrgDetailResponse{
		Organization: *org,
		MemberCount:  len(members),
	})
}

// HandleAddOrgMember adds a user to the organization by email.
func (h *Handlers) HandleAddOrgMember(w http.ResponseWriter, r *http.Request) {
	org, _ := h.requireOrgMembership(w, r, "owner", "admin")
	if org == nil {
		return
	}

	var req AddMemberRequest
	if err := json.NewDecoder(r.Body).Decode(&req); err != nil {
		http.Error(w, `{"error":"invalid request body"}`, http.StatusBadRequest)
		return
	}

	if req.Email == "" {
		http.Error(w, `{"error":"email is required"}`, http.StatusBadRequest)
		return
	}
	if req.Role == "" {
		req.Role = "member"
	}
	if req.Role != "member" && req.Role != "admin" {
		http.Error(w, `{"error":"role must be member or admin"}`, http.StatusBadRequest)
		return
	}

	targetUser, err := h.store.GetUserByEmail(r.Context(), req.Email)
	if err != nil || targetUser == nil {
		http.Error(w, `{"error":"user not found"}`, http.StatusNotFound)
		return
	}

	// Check if already a member
	existing, _ := h.store.GetOrgMembership(r.Context(), org.ID, targetUser.ID)
	if existing != nil {
		http.Error(w, `{"error":"user is already a member"}`, http.StatusConflict)
		return
	}

	if err := h.store.AddOrgMember(r.Context(), org.ID, targetUser.ID, req.Role); err != nil {
		h.logger.Error("failed to add org member", "error", err, "org_id", org.ID)
		http.Error(w, `{"error":"failed to add member"}`, http.StatusInternalServerError)
		return
	}

	h.logger.Info("org member added", "org_id", org.ID, "user_id", targetUser.ID, "role", req.Role)

	// Audit log
	user := UserFromContext(r.Context())
	if user != nil {
		go func() {
			details, _ := json.Marshal(map[string]interface{}{"email": req.Email, "role": req.Role})
			_ = h.store.InsertAuditEntry(context.Background(), &AuditEntry{
				OrgID:        org.ID,
				UserID:       user.ID,
				Action:       "member.added",
				ResourceType: "member",
				ResourceID:   targetUser.ID,
				Details:      details,
			})
		}()
	}

	writeJSON(w, http.StatusCreated, map[string]string{"status": "added"})
}

// HandleRemoveOrgMember removes a user from the organization.
func (h *Handlers) HandleRemoveOrgMember(w http.ResponseWriter, r *http.Request) {
	org, _ := h.requireOrgMembership(w, r, "owner", "admin")
	if org == nil {
		return
	}

	userID := r.PathValue("userId")
	if userID == "" {
		http.Error(w, `{"error":"user ID is required"}`, http.StatusBadRequest)
		return
	}

	// Cannot remove the owner
	if userID == org.OwnerID {
		http.Error(w, `{"error":"cannot remove organization owner"}`, http.StatusForbidden)
		return
	}

	if err := h.store.RemoveOrgMember(r.Context(), org.ID, userID); err != nil {
		h.logger.Error("failed to remove org member", "error", err, "org_id", org.ID)
		http.Error(w, `{"error":"failed to remove member"}`, http.StatusInternalServerError)
		return
	}

	h.logger.Info("org member removed", "org_id", org.ID, "user_id", userID)

	// Audit log
	user := UserFromContext(r.Context())
	if user != nil {
		go func() {
			details, _ := json.Marshal(map[string]interface{}{"removed_user_id": userID})
			_ = h.store.InsertAuditEntry(context.Background(), &AuditEntry{
				OrgID:        org.ID,
				UserID:       user.ID,
				Action:       "member.removed",
				ResourceType: "member",
				ResourceID:   userID,
				Details:      details,
			})
		}()
	}

	writeJSON(w, http.StatusOK, map[string]string{"status": "removed"})
}

// HandleListOrgMembers returns all members of the organization.
func (h *Handlers) HandleListOrgMembers(w http.ResponseWriter, r *http.Request) {
	org, _ := h.requireOrgMembership(w, r)
	if org == nil {
		return
	}

	members, err := h.store.ListOrgMembers(r.Context(), org.ID)
	if err != nil {
		h.logger.Error("failed to list org members", "error", err, "org_id", org.ID)
		http.Error(w, `{"error":"failed to list members"}`, http.StatusInternalServerError)
		return
	}

	writeJSON(w, http.StatusOK, members)
}

// HandleListOrgRuns returns paginated runs for the organization.
func (h *Handlers) HandleListOrgRuns(w http.ResponseWriter, r *http.Request) {
	org, _ := h.requireOrgMembership(w, r)
	if org == nil {
		return
	}

	limit := parseIntParam(r, "limit", 20)
	offset := parseIntParam(r, "offset", 0)

	runs, err := h.store.ListOrgRuns(r.Context(), org.ID, limit, offset)
	if err != nil {
		h.logger.Error("failed to list org runs", "error", err, "org_id", org.ID)
		http.Error(w, `{"error":"failed to list runs"}`, http.StatusInternalServerError)
		return
	}

	writeJSON(w, http.StatusOK, ListOrgRunsResponse{
		Runs:   runs,
		Total:  len(runs),
		Limit:  limit,
		Offset: offset,
	})
}

// HandleGetOrgRunStats returns aggregate run statistics for the organization.
func (h *Handlers) HandleGetOrgRunStats(w http.ResponseWriter, r *http.Request) {
	org, _ := h.requireOrgMembership(w, r)
	if org == nil {
		return
	}

	stats, err := h.store.GetOrgRunStats(r.Context(), org.ID)
	if err != nil {
		h.logger.Error("failed to get org run stats", "error", err, "org_id", org.ID)
		http.Error(w, `{"error":"failed to get run stats"}`, http.StatusInternalServerError)
		return
	}

	writeJSON(w, http.StatusOK, stats)
}

// ListAuditResponse is returned by the audit log listing endpoint.
type ListAuditResponse struct {
	Entries []AuditEntry `json:"entries"`
	Total   int          `json:"total"`
	Limit   int          `json:"limit"`
	Offset  int          `json:"offset"`
}

// HandleListAuditEntries returns the audit log for the organization.
func (h *Handlers) HandleListAuditEntries(w http.ResponseWriter, r *http.Request) {
	org, _ := h.requireOrgMembership(w, r)
	if org == nil {
		return
	}

	limit := parseIntParam(r, "limit", 50)
	offset := parseIntParam(r, "offset", 0)

	entries, err := h.store.ListAuditEntries(r.Context(), org.ID, limit, offset)
	if err != nil {
		h.logger.Error("failed to list audit entries", "error", err, "org_id", org.ID)
		http.Error(w, `{"error":"failed to list audit entries"}`, http.StatusInternalServerError)
		return
	}

	writeJSON(w, http.StatusOK, ListAuditResponse{
		Entries: entries,
		Total:   len(entries),
		Limit:   limit,
		Offset:  offset,
	})
}
