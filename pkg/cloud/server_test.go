package cloud

import (
	"context"
	"encoding/json"
	"fmt"
	"net/http"
	"net/http/httptest"
	"strings"
	"testing"
	"time"
)

// mockStore implements Store for testing without a real database.
type mockStore struct {
	users map[string]*User // keyed by API key hash
	usage map[string]*MonthlyUsage
}

func newMockStore() *mockStore {
	return &mockStore{
		users: make(map[string]*User),
		usage: make(map[string]*MonthlyUsage),
	}
}

func (m *mockStore) GetUserByAPIKeyHash(_ context.Context, hash string) (*User, error) {
	u, ok := m.users[hash]
	if !ok {
		return nil, fmt.Errorf("user not found")
	}
	return u, nil
}

func (m *mockStore) GetUserByEmail(_ context.Context, email string) (*User, error) {
	for _, u := range m.users {
		if u.Email == email {
			return u, nil
		}
	}
	return nil, nil
}

func (m *mockStore) CreateUser(_ context.Context, email string) (*User, error) {
	u := &User{
		ID:           "test-user-id",
		Email:        email,
		Plan:         "free",
		CreatedAt:    time.Now(),
		LastActiveAt: time.Now(),
	}
	return u, nil
}

func (m *mockStore) GenerateAPIKey(_ context.Context, userID string) (string, error) {
	raw := "fk_testkey1234567890abcdef1234567890abcdef1234567890abcdef1234abcd"
	hash := HashAPIKey(raw)
	if u := m.findUserByID(userID); u != nil {
		u.APIKeyHash = hash
		m.users[hash] = u
	}
	return raw, nil
}

func (m *mockStore) IncrementUsage(_ context.Context, userID string, tokens Usage) error {
	month := time.Now().Format("2006-01")
	key := userID + ":" + month
	u, ok := m.usage[key]
	if !ok {
		u = &MonthlyUsage{
			UserID: userID,
			Month:  month,
		}
		m.usage[key] = u
	}
	u.LLMCalls++
	u.TokensInput += tokens.TokensInput
	u.TokensOutput += tokens.TokensOutput
	u.Analyses++
	return nil
}

func (m *mockStore) GetUsage(_ context.Context, userID string, month string) (*MonthlyUsage, error) {
	key := userID + ":" + month
	u, ok := m.usage[key]
	if !ok {
		return &MonthlyUsage{
			UserID: userID,
			Month:  month,
		}, nil
	}
	return u, nil
}

func (m *mockStore) Close() {}

func (m *mockStore) findUserByID(id string) *User {
	for _, u := range m.users {
		if u.ID == id {
			return u
		}
	}
	return nil
}

// addTestUser adds a user to the mock store and returns the raw API key.
func (m *mockStore) addTestUser(email string) string {
	raw := "fk_testkey1234567890abcdef1234567890abcdef1234567890abcdef1234abcd"
	hash := HashAPIKey(raw)
	m.users[hash] = &User{
		ID:           "test-user-1",
		Email:        email,
		Plan:         "free",
		APIKeyHash:   hash,
		CreatedAt:    time.Now(),
		LastActiveAt: time.Now(),
	}
	return raw
}

func TestHealthEndpoint(t *testing.T) {
	store := newMockStore()
	srv := NewServer(Config{Port: "8082", LogLevel: "error"}, store)

	req := httptest.NewRequest(http.MethodGet, "/api/health", nil)
	w := httptest.NewRecorder()

	srv.Handler().ServeHTTP(w, req)

	if w.Code != http.StatusOK {
		t.Fatalf("expected 200, got %d", w.Code)
	}

	var resp HealthResponse
	if err := json.NewDecoder(w.Body).Decode(&resp); err != nil {
		t.Fatalf("decoding response: %v", err)
	}

	if resp.Status != "ok" {
		t.Errorf("expected status 'ok', got %q", resp.Status)
	}

	if resp.Version == "" {
		t.Error("expected non-empty version")
	}
}

func TestAuthMiddleware_NoHeader(t *testing.T) {
	store := newMockStore()
	srv := NewServer(Config{Port: "8082", LogLevel: "error"}, store)

	req := httptest.NewRequest(http.MethodGet, "/api/v1/usage", nil)
	w := httptest.NewRecorder()

	srv.Handler().ServeHTTP(w, req)

	if w.Code != http.StatusUnauthorized {
		t.Fatalf("expected 401, got %d", w.Code)
	}
}

func TestAuthMiddleware_InvalidFormat(t *testing.T) {
	store := newMockStore()
	srv := NewServer(Config{Port: "8082", LogLevel: "error"}, store)

	req := httptest.NewRequest(http.MethodGet, "/api/v1/usage", nil)
	req.Header.Set("Authorization", "Basic abc123")
	w := httptest.NewRecorder()

	srv.Handler().ServeHTTP(w, req)

	if w.Code != http.StatusUnauthorized {
		t.Fatalf("expected 401, got %d", w.Code)
	}
}

func TestAuthMiddleware_InvalidKey(t *testing.T) {
	store := newMockStore()
	srv := NewServer(Config{Port: "8082", LogLevel: "error"}, store)

	req := httptest.NewRequest(http.MethodGet, "/api/v1/usage", nil)
	req.Header.Set("Authorization", "Bearer fk_nonexistent")
	w := httptest.NewRecorder()

	srv.Handler().ServeHTTP(w, req)

	if w.Code != http.StatusUnauthorized {
		t.Fatalf("expected 401, got %d", w.Code)
	}
}

func TestAuthMiddleware_ValidKey(t *testing.T) {
	store := newMockStore()
	apiKey := store.addTestUser("test@example.com")
	srv := NewServer(Config{Port: "8082", LogLevel: "error"}, store)

	req := httptest.NewRequest(http.MethodGet, "/api/v1/usage", nil)
	req.Header.Set("Authorization", "Bearer "+apiKey)
	w := httptest.NewRecorder()

	srv.Handler().ServeHTTP(w, req)

	if w.Code != http.StatusOK {
		t.Fatalf("expected 200, got %d: %s", w.Code, w.Body.String())
	}
}

func TestUsageEndpoint(t *testing.T) {
	store := newMockStore()
	apiKey := store.addTestUser("test@example.com")

	// Pre-populate some usage
	month := time.Now().Format("2006-01")
	store.usage["test-user-1:"+month] = &MonthlyUsage{
		UserID:       "test-user-1",
		Month:        month,
		LLMCalls:     5,
		TokensInput:  1000,
		TokensOutput: 500,
		Analyses:     5,
	}

	srv := NewServer(Config{Port: "8082", LogLevel: "error"}, store)

	req := httptest.NewRequest(http.MethodGet, "/api/v1/usage", nil)
	req.Header.Set("Authorization", "Bearer "+apiKey)
	w := httptest.NewRecorder()

	srv.Handler().ServeHTTP(w, req)

	if w.Code != http.StatusOK {
		t.Fatalf("expected 200, got %d: %s", w.Code, w.Body.String())
	}

	var resp UsageResponse
	if err := json.NewDecoder(w.Body).Decode(&resp); err != nil {
		t.Fatalf("decoding response: %v", err)
	}

	if resp.LLMCalls != 5 {
		t.Errorf("expected 5 LLM calls, got %d", resp.LLMCalls)
	}
	if resp.TokensInput != 1000 {
		t.Errorf("expected 1000 tokens input, got %d", resp.TokensInput)
	}
	if resp.Email != "test@example.com" {
		t.Errorf("expected test@example.com, got %q", resp.Email)
	}
}

func TestUsageEndpoint_NoUsage(t *testing.T) {
	store := newMockStore()
	apiKey := store.addTestUser("test@example.com")
	srv := NewServer(Config{Port: "8082", LogLevel: "error"}, store)

	req := httptest.NewRequest(http.MethodGet, "/api/v1/usage", nil)
	req.Header.Set("Authorization", "Bearer "+apiKey)
	w := httptest.NewRecorder()

	srv.Handler().ServeHTTP(w, req)

	if w.Code != http.StatusOK {
		t.Fatalf("expected 200, got %d", w.Code)
	}

	var resp UsageResponse
	if err := json.NewDecoder(w.Body).Decode(&resp); err != nil {
		t.Fatalf("decoding response: %v", err)
	}

	if resp.LLMCalls != 0 {
		t.Errorf("expected 0 LLM calls, got %d", resp.LLMCalls)
	}
}

func TestAnalyzeConfidence_NoDiff(t *testing.T) {
	store := newMockStore()
	apiKey := store.addTestUser("test@example.com")
	srv := NewServer(Config{Port: "8082", LogLevel: "error"}, store)

	body := `{"diff":""}`
	req := httptest.NewRequest(http.MethodPost, "/api/v1/analyze/confidence",
		strings.NewReader(body))
	req.Header.Set("Authorization", "Bearer "+apiKey)
	req.Header.Set("Content-Type", "application/json")
	w := httptest.NewRecorder()

	srv.Handler().ServeHTTP(w, req)

	if w.Code != http.StatusBadRequest {
		t.Fatalf("expected 400, got %d: %s", w.Code, w.Body.String())
	}
}

func TestAnalyzeSpec_MissingFields(t *testing.T) {
	store := newMockStore()
	apiKey := store.addTestUser("test@example.com")
	srv := NewServer(Config{Port: "8082", LogLevel: "error"}, store)

	body := `{"diff":"some diff","spec":""}`
	req := httptest.NewRequest(http.MethodPost, "/api/v1/analyze/spec",
		strings.NewReader(body))
	req.Header.Set("Authorization", "Bearer "+apiKey)
	req.Header.Set("Content-Type", "application/json")
	w := httptest.NewRecorder()

	srv.Handler().ServeHTTP(w, req)

	if w.Code != http.StatusBadRequest {
		t.Fatalf("expected 400, got %d: %s", w.Code, w.Body.String())
	}
}

func TestCORSHeaders(t *testing.T) {
	store := newMockStore()
	srv := NewServer(Config{Port: "8082", LogLevel: "error"}, store)

	req := httptest.NewRequest(http.MethodOptions, "/api/health", nil)
	w := httptest.NewRecorder()

	srv.Handler().ServeHTTP(w, req)

	if w.Header().Get("Access-Control-Allow-Origin") != "*" {
		t.Error("expected CORS Allow-Origin header to be '*'")
	}

	if w.Header().Get("Access-Control-Allow-Methods") == "" {
		t.Error("expected CORS Allow-Methods header")
	}
}

func TestHashAPIKey(t *testing.T) {
	key := "fk_testkey123"
	hash1 := HashAPIKey(key)
	hash2 := HashAPIKey(key)

	if hash1 != hash2 {
		t.Error("same key should produce same hash")
	}

	hash3 := HashAPIKey("fk_differentkey")
	if hash1 == hash3 {
		t.Error("different keys should produce different hashes")
	}
}

func TestGenerateRawAPIKey(t *testing.T) {
	key, err := GenerateRawAPIKey()
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	if !strings.HasPrefix(key, "fk_") {
		t.Errorf("expected key to start with 'fk_', got %q", key)
	}

	// 32 bytes = 64 hex chars + "fk_" prefix = 67 chars
	if len(key) != 67 {
		t.Errorf("expected key length 67, got %d", len(key))
	}

	// Generate another and confirm uniqueness
	key2, err := GenerateRawAPIKey()
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if key == key2 {
		t.Error("expected unique keys")
	}
}

func TestHealthEndpoint_SkipsAuth(t *testing.T) {
	store := newMockStore()
	srv := NewServer(Config{Port: "8082", LogLevel: "error"}, store)

	// No Authorization header — health should still work
	req := httptest.NewRequest(http.MethodGet, "/api/health", nil)
	w := httptest.NewRecorder()

	srv.Handler().ServeHTTP(w, req)

	if w.Code != http.StatusOK {
		t.Fatalf("health should be accessible without auth, got %d", w.Code)
	}
}

func TestAuthMiddleware_NotFKPrefix(t *testing.T) {
	store := newMockStore()
	srv := NewServer(Config{Port: "8082", LogLevel: "error"}, store)

	req := httptest.NewRequest(http.MethodGet, "/api/v1/usage", nil)
	req.Header.Set("Authorization", "Bearer sk_notafaultkey")
	w := httptest.NewRecorder()

	srv.Handler().ServeHTTP(w, req)

	if w.Code != http.StatusUnauthorized {
		t.Fatalf("expected 401 for non-fk_ key, got %d", w.Code)
	}
}

func TestSignup_NewUser(t *testing.T) {
	store := newMockStore()
	srv := NewServer(Config{Port: "8082", LogLevel: "error"}, store)

	body := `{"email":"newuser@example.com"}`
	req := httptest.NewRequest(http.MethodPost, "/api/v1/signup", strings.NewReader(body))
	req.Header.Set("Content-Type", "application/json")
	w := httptest.NewRecorder()

	srv.Handler().ServeHTTP(w, req)

	if w.Code != http.StatusOK {
		t.Fatalf("expected 200, got %d: %s", w.Code, w.Body.String())
	}

	var resp SignupResponse
	if err := json.NewDecoder(w.Body).Decode(&resp); err != nil {
		t.Fatalf("decoding response: %v", err)
	}

	if resp.Email != "newuser@example.com" {
		t.Errorf("expected email newuser@example.com, got %q", resp.Email)
	}

	if !strings.HasPrefix(resp.APIKey, "fk_") {
		t.Errorf("expected API key with fk_ prefix, got %q", resp.APIKey)
	}
}

func TestSignup_ExistingUser(t *testing.T) {
	store := newMockStore()
	store.addTestUser("existing@example.com")
	srv := NewServer(Config{Port: "8082", LogLevel: "error"}, store)

	body := `{"email":"existing@example.com"}`
	req := httptest.NewRequest(http.MethodPost, "/api/v1/signup", strings.NewReader(body))
	req.Header.Set("Content-Type", "application/json")
	w := httptest.NewRecorder()

	srv.Handler().ServeHTTP(w, req)

	if w.Code != http.StatusOK {
		t.Fatalf("expected 200, got %d: %s", w.Code, w.Body.String())
	}

	var resp SignupResponse
	if err := json.NewDecoder(w.Body).Decode(&resp); err != nil {
		t.Fatalf("decoding response: %v", err)
	}

	if resp.Email != "existing@example.com" {
		t.Errorf("expected email existing@example.com, got %q", resp.Email)
	}

	if !strings.HasPrefix(resp.APIKey, "fk_") {
		t.Errorf("expected API key with fk_ prefix, got %q", resp.APIKey)
	}
}

func TestSignup_NoEmail(t *testing.T) {
	store := newMockStore()
	srv := NewServer(Config{Port: "8082", LogLevel: "error"}, store)

	body := `{"email":""}`
	req := httptest.NewRequest(http.MethodPost, "/api/v1/signup", strings.NewReader(body))
	req.Header.Set("Content-Type", "application/json")
	w := httptest.NewRecorder()

	srv.Handler().ServeHTTP(w, req)

	if w.Code != http.StatusBadRequest {
		t.Fatalf("expected 400, got %d: %s", w.Code, w.Body.String())
	}
}

func TestSignup_SkipsAuth(t *testing.T) {
	store := newMockStore()
	srv := NewServer(Config{Port: "8082", LogLevel: "error"}, store)

	// No Authorization header — signup should still work
	body := `{"email":"noauth@example.com"}`
	req := httptest.NewRequest(http.MethodPost, "/api/v1/signup", strings.NewReader(body))
	req.Header.Set("Content-Type", "application/json")
	w := httptest.NewRecorder()

	srv.Handler().ServeHTTP(w, req)

	if w.Code != http.StatusOK {
		t.Fatalf("signup should be accessible without auth, got %d: %s", w.Code, w.Body.String())
	}
}

func TestRotateKey(t *testing.T) {
	store := newMockStore()
	apiKey := store.addTestUser("rotate@example.com")
	srv := NewServer(Config{Port: "8082", LogLevel: "error"}, store)

	req := httptest.NewRequest(http.MethodPost, "/api/v1/api-keys/rotate", nil)
	req.Header.Set("Authorization", "Bearer "+apiKey)
	w := httptest.NewRecorder()

	srv.Handler().ServeHTTP(w, req)

	if w.Code != http.StatusOK {
		t.Fatalf("expected 200, got %d: %s", w.Code, w.Body.String())
	}

	var resp RotateKeyResponse
	if err := json.NewDecoder(w.Body).Decode(&resp); err != nil {
		t.Fatalf("decoding response: %v", err)
	}

	if resp.Email != "rotate@example.com" {
		t.Errorf("expected email rotate@example.com, got %q", resp.Email)
	}

	if !strings.HasPrefix(resp.APIKey, "fk_") {
		t.Errorf("expected API key with fk_ prefix, got %q", resp.APIKey)
	}
}

func TestRotateKey_NoAuth(t *testing.T) {
	store := newMockStore()
	srv := NewServer(Config{Port: "8082", LogLevel: "error"}, store)

	req := httptest.NewRequest(http.MethodPost, "/api/v1/api-keys/rotate", nil)
	w := httptest.NewRecorder()

	srv.Handler().ServeHTTP(w, req)

	if w.Code != http.StatusUnauthorized {
		t.Fatalf("expected 401, got %d", w.Code)
	}
}
