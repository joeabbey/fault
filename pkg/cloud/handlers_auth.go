package cloud

import (
	"crypto/rand"
	"encoding/hex"
	"log/slog"
	"net/http"
	"strings"
)

// AuthHandlers holds dependencies for authentication handlers.
type AuthHandlers struct {
	store         Store
	logger        *slog.Logger
	clientID      string
	clientSecret  string
	jwtSecret     string
	baseURL       string
	cookieDomain  string
	cookieSecure  bool
	allowedEmails map[string]bool
}

// NewAuthHandlers creates auth handlers.
func NewAuthHandlers(store Store, logger *slog.Logger, clientID, clientSecret, jwtSecret, baseURL, cookieDomain string, cookieSecure bool, allowedEmails []string) *AuthHandlers {
	allowed := make(map[string]bool)
	for _, e := range allowedEmails {
		e = strings.TrimSpace(e)
		if e != "" {
			allowed[e] = true
		}
	}
	return &AuthHandlers{
		store:         store,
		logger:        logger,
		clientID:      clientID,
		clientSecret:  clientSecret,
		jwtSecret:     jwtSecret,
		baseURL:       baseURL,
		cookieDomain:  cookieDomain,
		cookieSecure:  cookieSecure,
		allowedEmails: allowed,
	}
}

// HandleGoogleLogin redirects the user to Google's OAuth consent page.
func (ah *AuthHandlers) HandleGoogleLogin(w http.ResponseWriter, r *http.Request) {
	state, err := generateState()
	if err != nil {
		ah.logger.Error("failed to generate OAuth state", "error", err)
		http.Error(w, "internal error", http.StatusInternalServerError)
		return
	}

	// Store state in a cookie for CSRF validation
	http.SetCookie(w, &http.Cookie{
		Name:     "oauth_state",
		Value:    state,
		Path:     "/",
		MaxAge:   600, // 10 minutes
		HttpOnly: true,
		Secure:   ah.cookieSecure,
		SameSite: http.SameSiteLaxMode,
	})

	redirectURI := ah.baseURL + "/api/auth/google/callback"
	authURL := GetGoogleAuthURL(ah.clientID, redirectURI, state)
	http.Redirect(w, r, authURL, http.StatusTemporaryRedirect)
}

// HandleGoogleCallback handles the OAuth callback from Google.
func (ah *AuthHandlers) HandleGoogleCallback(w http.ResponseWriter, r *http.Request) {
	// Validate state
	stateCookie, err := r.Cookie("oauth_state")
	if err != nil || stateCookie.Value == "" {
		ah.logger.Warn("missing OAuth state cookie")
		http.Redirect(w, r, "/login?error=invalid_state", http.StatusTemporaryRedirect)
		return
	}

	if r.URL.Query().Get("state") != stateCookie.Value {
		ah.logger.Warn("OAuth state mismatch")
		http.Redirect(w, r, "/login?error=invalid_state", http.StatusTemporaryRedirect)
		return
	}

	// Clear state cookie
	http.SetCookie(w, &http.Cookie{
		Name:     "oauth_state",
		Value:    "",
		Path:     "/",
		MaxAge:   -1,
		HttpOnly: true,
	})

	// Check for error from Google
	if errMsg := r.URL.Query().Get("error"); errMsg != "" {
		ah.logger.Warn("Google OAuth error", "error", errMsg)
		http.Redirect(w, r, "/login?error="+errMsg, http.StatusTemporaryRedirect)
		return
	}

	code := r.URL.Query().Get("code")
	if code == "" {
		http.Redirect(w, r, "/login?error=missing_code", http.StatusTemporaryRedirect)
		return
	}

	// Exchange code for token
	redirectURI := ah.baseURL + "/api/auth/google/callback"
	token, err := ExchangeGoogleCode(r.Context(), ah.clientID, ah.clientSecret, code, redirectURI)
	if err != nil {
		ah.logger.Error("failed to exchange Google code", "error", err)
		http.Redirect(w, r, "/login?error=exchange_failed", http.StatusTemporaryRedirect)
		return
	}

	// Get user info
	userInfo, err := GetGoogleUserInfo(r.Context(), token.AccessToken)
	if err != nil {
		ah.logger.Error("failed to get Google user info", "error", err)
		http.Redirect(w, r, "/login?error=userinfo_failed", http.StatusTemporaryRedirect)
		return
	}

	// Check allowed emails
	if len(ah.allowedEmails) > 0 && !ah.allowedEmails[userInfo.Email] {
		ah.logger.Warn("unauthorized email attempted login", "email", userInfo.Email)
		http.Redirect(w, r, "/login?error=unauthorized", http.StatusTemporaryRedirect)
		return
	}

	// Find or create user
	user, err := ah.store.GetUserByGoogleID(r.Context(), userInfo.ID)
	if err != nil {
		ah.logger.Error("failed to look up user by Google ID", "error", err)
		http.Redirect(w, r, "/login?error=internal", http.StatusTemporaryRedirect)
		return
	}

	if user == nil {
		// Check if there's an existing user with this email (from API key signup)
		user, _ = ah.store.GetUserByEmail(r.Context(), userInfo.Email)
		if user != nil {
			// Link Google ID to existing user
			if err := ah.store.SetGoogleID(r.Context(), user.ID, userInfo.ID); err != nil {
				ah.logger.Error("failed to link Google ID", "error", err)
			}
			if err := ah.store.UpdateUserProfile(r.Context(), user.ID, userInfo.Name, userInfo.PictureURL); err != nil {
				ah.logger.Error("failed to update user profile", "error", err)
			}
		} else {
			// Create new user
			user, err = ah.store.CreateUserFromGoogle(r.Context(), userInfo.Email, userInfo.ID, userInfo.Name, userInfo.PictureURL)
			if err != nil {
				ah.logger.Error("failed to create user from Google", "error", err)
				http.Redirect(w, r, "/login?error=create_failed", http.StatusTemporaryRedirect)
				return
			}
		}
	}

	// Generate JWT
	jwt, err := GenerateJWT(user.ID, user.Email, ah.jwtSecret, cookieExpiry)
	if err != nil {
		ah.logger.Error("failed to generate JWT", "error", err)
		http.Redirect(w, r, "/login?error=internal", http.StatusTemporaryRedirect)
		return
	}

	// Set cookie
	SetAuthCookie(w, jwt, ah.cookieDomain, ah.cookieSecure)

	ah.logger.Info("user logged in via Google", "email", userInfo.Email, "user_id", user.ID)

	// Redirect to dashboard
	http.Redirect(w, r, "/dashboard", http.StatusTemporaryRedirect)
}

// HandleAuthMe returns the current authenticated user (if any).
// This is a public endpoint — returns {"user": null} when not authenticated.
func (ah *AuthHandlers) HandleAuthMe(w http.ResponseWriter, r *http.Request) {
	// Try JWT cookie (this endpoint bypasses the auth middleware)
	if token := GetTokenFromCookie(r); token != "" {
		claims, err := ValidateJWT(token, ah.jwtSecret)
		if err == nil {
			user, err := ah.store.GetUserByID(r.Context(), claims.Sub)
			if err == nil && user != nil {
				writeJSON(w, http.StatusOK, map[string]interface{}{
					"user": map[string]interface{}{
						"id":          user.ID,
						"email":       user.Email,
						"name":        user.Name,
						"picture_url": user.PictureURL,
						"plan":        user.Plan,
						"has_api_key": user.APIKeyHash != "",
					},
				})
				return
			}
		}
	}

	writeJSON(w, http.StatusOK, map[string]interface{}{"user": nil})
}

// HandleLogout clears the auth cookie.
func (ah *AuthHandlers) HandleLogout(w http.ResponseWriter, r *http.Request) {
	ClearAuthCookie(w, ah.cookieDomain, ah.cookieSecure)
	writeJSON(w, http.StatusOK, map[string]string{"status": "ok"})
}

func generateState() (string, error) {
	b := make([]byte, 16)
	if _, err := rand.Read(b); err != nil {
		return "", err
	}
	return hex.EncodeToString(b), nil
}
