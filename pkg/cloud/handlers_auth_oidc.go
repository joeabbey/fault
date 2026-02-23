package cloud

import (
	"context"
	"crypto/rand"
	"encoding/base64"
	"encoding/hex"
	"encoding/json"
	"fmt"
	"io"
	"log/slog"
	"net/http"
	"strings"
	"sync"
	"time"

	"golang.org/x/oauth2"
)

// OIDCState holds state for the OIDC login flow.
type OIDCState struct {
	OrgSlug   string
	ExpiresAt time.Time
	Nonce     string
}

// OIDCHandlers manages OIDC SSO authentication flows.
type OIDCHandlers struct {
	store        Store
	logger       *slog.Logger
	jwtSecret    string
	appURL       string
	cookieDomain string
	cookieSecure bool
	mu           sync.Mutex
	states       map[string]OIDCState
}

// NewOIDCHandlers creates a new OIDCHandlers.
func NewOIDCHandlers(store Store, logger *slog.Logger, jwtSecret, appURL, cookieDomain string, cookieSecure bool) *OIDCHandlers {
	return &OIDCHandlers{
		store:        store,
		logger:       logger,
		jwtSecret:    jwtSecret,
		appURL:       appURL,
		cookieDomain: cookieDomain,
		cookieSecure: cookieSecure,
		states:       make(map[string]OIDCState),
	}
}

// oidcDiscovery holds the relevant fields from an OIDC discovery document.
type oidcDiscovery struct {
	AuthorizationEndpoint string `json:"authorization_endpoint"`
	TokenEndpoint         string `json:"token_endpoint"`
}

// fetchDiscovery fetches the OIDC discovery document from the provider.
func fetchDiscovery(ctx context.Context, discoveryURL string) (*oidcDiscovery, error) {
	wellKnown := strings.TrimRight(discoveryURL, "/") + "/.well-known/openid-configuration"
	req, err := http.NewRequestWithContext(ctx, http.MethodGet, wellKnown, nil)
	if err != nil {
		return nil, fmt.Errorf("creating discovery request: %w", err)
	}

	resp, err := http.DefaultClient.Do(req)
	if err != nil {
		return nil, fmt.Errorf("fetching discovery document: %w", err)
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusOK {
		return nil, fmt.Errorf("discovery endpoint returned %d", resp.StatusCode)
	}

	var doc oidcDiscovery
	if err := json.NewDecoder(resp.Body).Decode(&doc); err != nil {
		return nil, fmt.Errorf("decoding discovery document: %w", err)
	}

	if doc.AuthorizationEndpoint == "" || doc.TokenEndpoint == "" {
		return nil, fmt.Errorf("discovery document missing required endpoints")
	}

	return &doc, nil
}

// HandleOIDCLogin starts the OIDC login flow for an organization.
func (oh *OIDCHandlers) HandleOIDCLogin(w http.ResponseWriter, r *http.Request) {
	slug := r.PathValue("slug")
	if slug == "" {
		http.Error(w, `{"error":"org slug is required"}`, http.StatusBadRequest)
		return
	}

	org, err := oh.store.GetOrganizationBySlug(r.Context(), slug)
	if err != nil {
		http.Error(w, `{"error":"organization not found"}`, http.StatusNotFound)
		return
	}

	idpCfg, err := oh.store.GetOrgIDPConfig(r.Context(), org.ID)
	if err != nil || idpCfg == nil {
		http.Error(w, `{"error":"SSO not configured for this organization"}`, http.StatusNotFound)
		return
	}

	discovery, err := fetchDiscovery(r.Context(), idpCfg.DiscoveryURL)
	if err != nil {
		oh.logger.Error("failed to fetch OIDC discovery", "error", err, "org_id", org.ID)
		http.Error(w, `{"error":"failed to contact identity provider"}`, http.StatusBadGateway)
		return
	}

	// Generate state parameter
	stateBytes := make([]byte, 16)
	if _, err := rand.Read(stateBytes); err != nil {
		http.Error(w, `{"error":"internal error"}`, http.StatusInternalServerError)
		return
	}
	state := hex.EncodeToString(stateBytes)

	nonceBytes := make([]byte, 16)
	if _, err := rand.Read(nonceBytes); err != nil {
		http.Error(w, `{"error":"internal error"}`, http.StatusInternalServerError)
		return
	}
	nonce := hex.EncodeToString(nonceBytes)

	oh.mu.Lock()
	oh.states[state] = OIDCState{
		OrgSlug:   slug,
		ExpiresAt: time.Now().Add(10 * time.Minute),
		Nonce:     nonce,
	}
	oh.mu.Unlock()

	oauthCfg := &oauth2.Config{
		ClientID:     idpCfg.ClientID,
		ClientSecret: idpCfg.ClientSecret,
		Endpoint: oauth2.Endpoint{
			AuthURL:  discovery.AuthorizationEndpoint,
			TokenURL: discovery.TokenEndpoint,
		},
		RedirectURL: oh.appURL + "/api/auth/oidc/" + slug + "/callback",
		Scopes:      []string{"openid", "email", "profile"},
	}

	authURL := oauthCfg.AuthCodeURL(state, oauth2.SetAuthURLParam("nonce", nonce))
	http.Redirect(w, r, authURL, http.StatusTemporaryRedirect)
}

// HandleOIDCCallback handles the OIDC callback after IdP authentication.
func (oh *OIDCHandlers) HandleOIDCCallback(w http.ResponseWriter, r *http.Request) {
	slug := r.PathValue("slug")
	if slug == "" {
		http.Redirect(w, r, "/login?error=invalid_request", http.StatusTemporaryRedirect)
		return
	}

	// Check for error from IdP
	if errMsg := r.URL.Query().Get("error"); errMsg != "" {
		oh.logger.Warn("OIDC IdP error", "error", errMsg, "slug", slug)
		http.Redirect(w, r, "/login?error="+errMsg, http.StatusTemporaryRedirect)
		return
	}

	state := r.URL.Query().Get("state")
	code := r.URL.Query().Get("code")
	if state == "" || code == "" {
		http.Redirect(w, r, "/login?error=missing_params", http.StatusTemporaryRedirect)
		return
	}

	// Validate state
	oh.mu.Lock()
	oidcState, ok := oh.states[state]
	if ok {
		delete(oh.states, state)
	}
	oh.mu.Unlock()

	if !ok || time.Now().After(oidcState.ExpiresAt) || oidcState.OrgSlug != slug {
		http.Redirect(w, r, "/login?error=invalid_state", http.StatusTemporaryRedirect)
		return
	}

	// Look up org and IdP config
	org, err := oh.store.GetOrganizationBySlug(r.Context(), slug)
	if err != nil {
		http.Redirect(w, r, "/login?error=org_not_found", http.StatusTemporaryRedirect)
		return
	}

	idpCfg, err := oh.store.GetOrgIDPConfig(r.Context(), org.ID)
	if err != nil || idpCfg == nil {
		http.Redirect(w, r, "/login?error=sso_not_configured", http.StatusTemporaryRedirect)
		return
	}

	discovery, err := fetchDiscovery(r.Context(), idpCfg.DiscoveryURL)
	if err != nil {
		oh.logger.Error("failed to fetch OIDC discovery on callback", "error", err)
		http.Redirect(w, r, "/login?error=idp_error", http.StatusTemporaryRedirect)
		return
	}

	oauthCfg := &oauth2.Config{
		ClientID:     idpCfg.ClientID,
		ClientSecret: idpCfg.ClientSecret,
		Endpoint: oauth2.Endpoint{
			AuthURL:  discovery.AuthorizationEndpoint,
			TokenURL: discovery.TokenEndpoint,
		},
		RedirectURL: oh.appURL + "/api/auth/oidc/" + slug + "/callback",
		Scopes:      []string{"openid", "email", "profile"},
	}

	// Exchange code for token
	token, err := oauthCfg.Exchange(r.Context(), code)
	if err != nil {
		oh.logger.Error("failed to exchange OIDC code", "error", err, "slug", slug)
		http.Redirect(w, r, "/login?error=exchange_failed", http.StatusTemporaryRedirect)
		return
	}

	// Extract email from ID token
	// TODO: For production, verify the ID token signature against the IdP's JWKS
	idTokenRaw, ok := token.Extra("id_token").(string)
	if !ok || idTokenRaw == "" {
		oh.logger.Error("no id_token in OIDC response", "slug", slug)
		http.Redirect(w, r, "/login?error=no_id_token", http.StatusTemporaryRedirect)
		return
	}

	email, name, err := extractIDTokenClaims(idTokenRaw)
	if err != nil {
		oh.logger.Error("failed to extract ID token claims", "error", err, "slug", slug)
		http.Redirect(w, r, "/login?error=invalid_token", http.StatusTemporaryRedirect)
		return
	}

	if email == "" {
		oh.logger.Error("no email in ID token", "slug", slug)
		http.Redirect(w, r, "/login?error=no_email", http.StatusTemporaryRedirect)
		return
	}

	// Find or create user
	user, _ := oh.store.GetUserByEmail(r.Context(), email)
	if user == nil {
		user, err = oh.store.CreateUser(r.Context(), email)
		if err != nil {
			oh.logger.Error("failed to create user from OIDC", "error", err, "email", email)
			http.Redirect(w, r, "/login?error=create_failed", http.StatusTemporaryRedirect)
			return
		}
		if name != "" {
			_ = oh.store.UpdateUserProfile(r.Context(), user.ID, name, "")
		}
	}

	// Ensure user is a member of the org
	member, _ := oh.store.GetOrgMembership(r.Context(), org.ID, user.ID)
	if member == nil {
		if err := oh.store.AddOrgMember(r.Context(), org.ID, user.ID, "member"); err != nil {
			oh.logger.Error("failed to add OIDC user to org", "error", err, "email", email, "org_id", org.ID)
		}
	}

	// Generate JWT
	jwt, err := GenerateJWT(user.ID, user.Email, oh.jwtSecret, cookieExpiry)
	if err != nil {
		oh.logger.Error("failed to generate JWT", "error", err)
		http.Redirect(w, r, "/login?error=internal", http.StatusTemporaryRedirect)
		return
	}

	SetAuthCookie(w, jwt, oh.cookieDomain, oh.cookieSecure)
	oh.logger.Info("user logged in via OIDC SSO", "email", email, "user_id", user.ID, "org", slug)

	http.Redirect(w, r, "/dashboard", http.StatusTemporaryRedirect)
}

// extractIDTokenClaims base64-decodes the JWT payload to extract email and name.
// TODO: For production, verify the ID token signature against the IdP's JWKS.
func extractIDTokenClaims(idToken string) (email, name string, err error) {
	parts := strings.SplitN(idToken, ".", 3)
	if len(parts) < 2 {
		return "", "", fmt.Errorf("invalid ID token format")
	}

	// Decode the payload (second part)
	payload := parts[1]
	// Add padding if needed
	switch len(payload) % 4 {
	case 2:
		payload += "=="
	case 3:
		payload += "="
	}

	decoded, err := base64.URLEncoding.DecodeString(payload)
	if err != nil {
		// Try without padding (raw URL encoding)
		decoded, err = base64.RawURLEncoding.DecodeString(parts[1])
		if err != nil {
			return "", "", fmt.Errorf("decoding ID token payload: %w", err)
		}
	}

	var claims struct {
		Email string `json:"email"`
		Name  string `json:"name"`
	}
	if err := json.Unmarshal(decoded, &claims); err != nil {
		return "", "", fmt.Errorf("unmarshaling ID token claims: %w", err)
	}

	return claims.Email, claims.Name, nil
}

// SaveOIDCConfigRequest is the payload for saving an org's OIDC IdP configuration.
type SaveOIDCConfigRequest struct {
	ClientID     string `json:"client_id"`
	ClientSecret string `json:"client_secret"`
	DiscoveryURL string `json:"discovery_url"`
}

// HandleSaveOIDCConfig saves the OIDC IdP configuration for an organization.
// Requires owner or admin role.
func (h *Handlers) HandleSaveOIDCConfig(w http.ResponseWriter, r *http.Request) {
	org, member := h.requireOrgMembership(w, r, "owner", "admin")
	if org == nil {
		return
	}

	var req SaveOIDCConfigRequest
	if err := json.NewDecoder(r.Body).Decode(&req); err != nil {
		http.Error(w, `{"error":"invalid request body"}`, http.StatusBadRequest)
		return
	}

	if req.ClientID == "" || req.ClientSecret == "" || req.DiscoveryURL == "" {
		http.Error(w, `{"error":"client_id, client_secret, and discovery_url are required"}`, http.StatusBadRequest)
		return
	}

	if err := h.store.SaveOrgIDPConfig(r.Context(), &OrgIDPConfig{
		OrgID:        org.ID,
		Provider:     "oidc",
		ClientID:     req.ClientID,
		ClientSecret: req.ClientSecret,
		DiscoveryURL: req.DiscoveryURL,
	}); err != nil {
		h.logger.Error("failed to save IDP config", "error", err)
		http.Error(w, `{"error":"failed to save IdP config"}`, http.StatusInternalServerError)
		return
	}

	// Audit log
	user := UserFromContext(r.Context())
	if user != nil {
		go func() {
			details, _ := json.Marshal(map[string]interface{}{"discovery_url": req.DiscoveryURL})
			_ = h.store.InsertAuditEntry(context.Background(), &AuditEntry{
				OrgID:        org.ID,
				UserID:       user.ID,
				Action:       "idp.configured",
				ResourceType: "idp",
				ResourceID:   org.ID,
				Details:      details,
			})
		}()
	}

	_ = member // used by requireOrgMembership
	writeJSON(w, http.StatusOK, map[string]string{"status": "ok"})
}

// HandleGetOIDCConfig returns the OIDC IdP configuration for an organization.
// The client_secret is masked.
func (h *Handlers) HandleGetOIDCConfig(w http.ResponseWriter, r *http.Request) {
	org, _ := h.requireOrgMembership(w, r)
	if org == nil {
		return
	}

	cfg, err := h.store.GetOrgIDPConfig(r.Context(), org.ID)
	if err != nil || cfg == nil {
		writeJSON(w, http.StatusOK, map[string]interface{}{"configured": false})
		return
	}

	maskedSecret := "****"
	if len(cfg.ClientSecret) > 4 {
		maskedSecret = cfg.ClientSecret[:4] + "****"
	}

	writeJSON(w, http.StatusOK, map[string]interface{}{
		"configured":    true,
		"provider":      cfg.Provider,
		"client_id":     cfg.ClientID,
		"client_secret": maskedSecret,
		"discovery_url": cfg.DiscoveryURL,
		"created_at":    cfg.CreatedAt.Format(time.RFC3339),
		"updated_at":    cfg.UpdatedAt.Format(time.RFC3339),
	})
}

// HandleDeleteOIDCConfig deletes the OIDC IdP configuration for an organization.
// Requires owner or admin role.
func (h *Handlers) HandleDeleteOIDCConfig(w http.ResponseWriter, r *http.Request) {
	org, _ := h.requireOrgMembership(w, r, "owner", "admin")
	if org == nil {
		return
	}

	if err := h.store.DeleteOrgIDPConfig(r.Context(), org.ID); err != nil {
		h.logger.Error("failed to delete IDP config", "error", err)
		http.Error(w, `{"error":"failed to delete IdP config"}`, http.StatusInternalServerError)
		return
	}

	// Audit log
	user := UserFromContext(r.Context())
	if user != nil {
		go func() {
			_ = h.store.InsertAuditEntry(context.Background(), &AuditEntry{
				OrgID:        org.ID,
				UserID:       user.ID,
				Action:       "idp.deleted",
				ResourceType: "idp",
				ResourceID:   org.ID,
			})
		}()
	}

	writeJSON(w, http.StatusOK, map[string]string{"status": "ok"})
}

// cleanExpiredStates removes expired OIDC state entries.
// Called periodically or can be called on demand.
func (oh *OIDCHandlers) cleanExpiredStates() {
	oh.mu.Lock()
	defer oh.mu.Unlock()
	now := time.Now()
	for k, v := range oh.states {
		if now.After(v.ExpiresAt) {
			delete(oh.states, k)
		}
	}
}

// UserinfoResponse is used to fetch user info from the OIDC userinfo endpoint.
type UserinfoResponse struct {
	Email string `json:"email"`
	Name  string `json:"name"`
}

// fetchUserInfo fetches user info from the OIDC userinfo endpoint if available.
func fetchUserInfo(ctx context.Context, accessToken, userinfoURL string) (*UserinfoResponse, error) {
	if userinfoURL == "" {
		return nil, fmt.Errorf("no userinfo endpoint")
	}

	req, err := http.NewRequestWithContext(ctx, http.MethodGet, userinfoURL, nil)
	if err != nil {
		return nil, err
	}
	req.Header.Set("Authorization", "Bearer "+accessToken)

	resp, err := http.DefaultClient.Do(req)
	if err != nil {
		return nil, err
	}
	defer resp.Body.Close()

	body, err := io.ReadAll(resp.Body)
	if err != nil {
		return nil, err
	}

	var info UserinfoResponse
	if err := json.Unmarshal(body, &info); err != nil {
		return nil, err
	}
	return &info, nil
}
