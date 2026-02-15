package cloud

import (
	"context"
	"log/slog"
	"net/http"
	"strings"
	"time"
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

// CORSMiddleware adds permissive CORS headers for CLI access from any origin.
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

// APIKeyAuth is middleware that validates the Authorization: Bearer fk_xxx header.
// It extracts the API key, hashes it with SHA-256, and looks up the user in the store.
// Requests to public endpoints (like /api/health) bypass auth.
func APIKeyAuth(store Store, logger *slog.Logger) func(http.Handler) http.Handler {
	return func(next http.Handler) http.Handler {
		return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
			// Skip auth for public endpoints
			if isPublicPath(r.URL.Path) {
				next.ServeHTTP(w, r)
				return
			}

			authHeader := r.Header.Get("Authorization")
			if authHeader == "" {
				http.Error(w, `{"error":"missing Authorization header"}`, http.StatusUnauthorized)
				return
			}

			if !strings.HasPrefix(authHeader, "Bearer ") {
				http.Error(w, `{"error":"invalid Authorization header format"}`, http.StatusUnauthorized)
				return
			}

			rawKey := strings.TrimPrefix(authHeader, "Bearer ")
			if !strings.HasPrefix(rawKey, "fk_") {
				http.Error(w, `{"error":"invalid API key format"}`, http.StatusUnauthorized)
				return
			}

			hash := HashAPIKey(rawKey)
			user, err := store.GetUserByAPIKeyHash(r.Context(), hash)
			if err != nil {
				logger.Warn("auth failed", "error", err)
				http.Error(w, `{"error":"invalid API key"}`, http.StatusUnauthorized)
				return
			}

			ctx := context.WithValue(r.Context(), userContextKey, user)
			next.ServeHTTP(w, r.WithContext(ctx))
		})
	}
}

// isPublicPath returns true for endpoints that do not require authentication.
func isPublicPath(path string) bool {
	publicPaths := []string{
		"/api/health",
		"/api/v1/signup",
	}
	for _, p := range publicPaths {
		if path == p {
			return true
		}
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
