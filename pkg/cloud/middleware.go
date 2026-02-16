package cloud

import (
	"context"
	"fmt"
	"log/slog"
	"net/http"
	"strings"
	"time"

	"github.com/joeabbey/magma/pkg/limits"
)

type contextKey string

const userContextKey contextKey = "user"

// UserFromContext extracts the authenticated user from the request context.
func UserFromContext(ctx context.Context) *User {
	u, _ := ctx.Value(userContextKey).(*User)
	return u
}

// RequestLogger is middleware that logs each incoming request with method, path,
// status code, and duration.
func RequestLogger(logger *slog.Logger) func(http.Handler) http.Handler {
	return func(next http.Handler) http.Handler {
		return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
			start := time.Now()
			sw := &statusWriter{ResponseWriter: w, status: http.StatusOK}

			next.ServeHTTP(sw, r)

			logger.Info("request",
				"method", r.Method,
				"path", r.URL.Path,
				"status", sw.status,
				"duration_ms", time.Since(start).Milliseconds(),
				"remote", r.RemoteAddr,
			)
		})
	}
}

// CORSMiddleware adds CORS headers. For same-origin dashboard requests (cookie auth),
// CORS is not needed. These headers support CLI access from any origin.
func CORSMiddleware(next http.Handler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Access-Control-Allow-Origin", "*")
		w.Header().Set("Access-Control-Allow-Methods", "GET, POST, PUT, DELETE, OPTIONS")
		w.Header().Set("Access-Control-Allow-Headers", "Authorization, Content-Type")
		w.Header().Set("Access-Control-Max-Age", "86400")

		if r.Method == http.MethodOptions {
			w.WriteHeader(http.StatusNoContent)
			return
		}

		next.ServeHTTP(w, r)
	})
}

// AuthMiddleware validates authentication via JWT cookie or API key Bearer token.
// JWT cookie is tried first (dashboard auth), then API key (CLI auth).
// Requests to public endpoints bypass auth entirely.
func AuthMiddleware(store Store, jwtSecret string, logger *slog.Logger) func(http.Handler) http.Handler {
	return func(next http.Handler) http.Handler {
		return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
			// Skip auth for public endpoints
			if isPublicPath(r.URL.Path) {
				next.ServeHTTP(w, r)
				return
			}

			// Try JWT cookie first (dashboard auth)
			if token := GetTokenFromCookie(r); token != "" {
				claims, err := ValidateJWT(token, jwtSecret)
				if err == nil {
					user, err := store.GetUserByID(r.Context(), claims.Sub)
					if err == nil && user != nil {
						ctx := context.WithValue(r.Context(), userContextKey, user)
						next.ServeHTTP(w, r.WithContext(ctx))
						return
					}
				}
			}

			// Try API key Bearer token (CLI auth)
			authHeader := r.Header.Get("Authorization")
			if authHeader != "" && strings.HasPrefix(authHeader, "Bearer ") {
				rawKey := strings.TrimPrefix(authHeader, "Bearer ")
				if strings.HasPrefix(rawKey, "fk_") {
					hash := HashAPIKey(rawKey)
					user, err := store.GetUserByAPIKeyHash(r.Context(), hash)
					if err == nil && user != nil {
						ctx := context.WithValue(r.Context(), userContextKey, user)
						next.ServeHTTP(w, r.WithContext(ctx))
						return
					}
				}
			}

			http.Error(w, `{"error":"unauthorized"}`, http.StatusUnauthorized)
		})
	}
}

// UsageLimitsMiddleware checks the limits engine before allowing requests to
// analysis endpoints. Returns 429 with rate limit headers when usage is exceeded.
func UsageLimitsMiddleware(engine *limits.Engine, logger *slog.Logger) func(http.Handler) http.Handler {
	return func(next http.Handler) http.Handler {
		return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
			// Only gate analysis endpoints
			if !strings.HasPrefix(r.URL.Path, "/api/v1/analyze/") {
				next.ServeHTTP(w, r)
				return
			}

			user := UserFromContext(r.Context())
			if user == nil {
				// Auth middleware should have caught this; pass through
				next.ServeHTTP(w, r)
				return
			}

			check := engine.Check(r.Context(), user.ID, "llm_calls")

			// Set rate limit headers on all responses
			w.Header().Set("X-RateLimit-Limit", fmt.Sprintf("%d", check.Limit))
			w.Header().Set("X-RateLimit-Remaining", fmt.Sprintf("%d", check.Remaining))

			if !check.Allowed {
				w.Header().Set("Content-Type", "application/json")
				w.WriteHeader(http.StatusTooManyRequests)
				fmt.Fprintf(w, `{"error":"usage limit exceeded","plan":%q,"limit":%d,"used":%d,"upgrade":"POST /api/v1/billing/checkout"}`,
					user.Plan, check.Limit, check.Limit-check.Remaining)
				return
			}

			next.ServeHTTP(w, r)
		})
	}
}

// isPublicPath returns true for endpoints that do not require authentication.
func isPublicPath(path string) bool {
	publicPaths := []string{
		"/api/health",
		"/api/v1/signup",
		"/api/v1/billing/webhook",
		"/api/auth/google/login",
		"/api/auth/google/callback",
		"/api/auth/me",
		"/api/auth/logout",
	}
	for _, p := range publicPaths {
		if path == p {
			return true
		}
	}
	// Non-API paths (landing page, SPA, static files) are always public
	if !strings.HasPrefix(path, "/api/") {
		return true
	}
	return false
}

// statusWriter wraps http.ResponseWriter to capture the status code.
type statusWriter struct {
	http.ResponseWriter
	status int
}

func (sw *statusWriter) WriteHeader(code int) {
	sw.status = code
	sw.ResponseWriter.WriteHeader(code)
}
