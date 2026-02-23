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

func (m *mockStore) GetUserByID(_ context.Context, userID string) (*User, error) {
	return m.findUserByID(userID), nil
}

func (m *mockStore) GetUserByStripeCustomerID(_ context.Context, customerID string) (*User, error) {
	for _, u := range m.users {
		if u.StripeCustomerID == customerID {
			return u, nil
		}
	}
	return nil, nil
}

func (m *mockStore) UpdateUserPlan(_ context.Context, userID string, plan string) error {
	if u := m.findUserByID(userID); u != nil {
		u.Plan = plan
	}
	return nil
}

func (m *mockStore) SetStripeCustomerID(_ context.Context, userID string, customerID string) error {
	if u := m.findUserByID(userID); u != nil {
		u.StripeCustomerID = customerID
	}
	return nil
}

func (m *mockStore) UpsertSubscription(_ context.Context, sub *Subscription) error {
	return nil
}

func (m *mockStore) GetSubscriptionByUserID(_ context.Context, userID string) (*Subscription, error) {
	return nil, nil
}

func (m *mockStore) GetUserByGoogleID(_ context.Context, googleID string) (*User, error) {
	for _, u := range m.users {
		if u.GoogleID == googleID {
			return u, nil
		}
	}
	return nil, nil
}

func (m *mockStore) CreateUserFromGoogle(_ context.Context, email, googleID, name, pictureURL string) (*User, error) {
	u := &User{
		ID:           "test-user-id",
		Email:        email,
		Plan:         "free",
		GoogleID:     googleID,
		Name:         name,
		PictureURL:   pictureURL,
		CreatedAt:    time.Now(),
		LastActiveAt: time.Now(),
	}
	return u, nil
}

func (m *mockStore) SetGoogleID(_ context.Context, userID, googleID string) error {
	if u := m.findUserByID(userID); u != nil {
		u.GoogleID = googleID
	}
	return nil
}

func (m *mockStore) UpdateUserProfile(_ context.Context, userID, name, pictureURL string) error {
	if u := m.findUserByID(userID); u != nil {
		u.Name = name
		u.PictureURL = pictureURL
	}
	return nil
}

func (m *mockStore) CreateRun(_ context.Context, run *Run) error {
	return nil
}

func (m *mockStore) GetRun(_ context.Context, userID, runID string) (*Run, error) {
	return nil, fmt.Errorf("not found")
}

func (m *mockStore) ListRuns(_ context.Context, userID string, limit, offset int) ([]Run, error) {
	return make([]Run, 0), nil
}

func (m *mockStore) GetRunStats(_ context.Context, userID string) (*RunStats, error) {
	return &RunStats{}, nil
}

func (m *mockStore) SaveSpecResult(_ context.Context, result *SpecResult) error {
	return nil
}

func (m *mockStore) GetSpecResults(_ context.Context, userID string, limit, offset int) ([]SpecResult, error) {
	return make([]SpecResult, 0), nil
}

func (m *mockStore) CreateOrganization(_ context.Context, org *Organization) error {
	return nil
}

func (m *mockStore) GetOrganization(_ context.Context, orgID string) (*Organization, error) {
	return nil, fmt.Errorf("not found")
}

func (m *mockStore) GetOrganizationBySlug(_ context.Context, slug string) (*Organization, error) {
	return nil, fmt.Errorf("not found")
}

func (m *mockStore) ListUserOrganizations(_ context.Context, userID string) ([]Organization, error) {
	return make([]Organization, 0), nil
}

func (m *mockStore) AddOrgMember(_ context.Context, orgID, userID, role string) error {
	return nil
}

func (m *mockStore) RemoveOrgMember(_ context.Context, orgID, userID string) error {
	return nil
}

func (m *mockStore) ListOrgMembers(_ context.Context, orgID string) ([]OrgMember, error) {
	return make([]OrgMember, 0), nil
}

func (m *mockStore) GetOrgMembership(_ context.Context, orgID, userID string) (*OrgMember, error) {
	return nil, nil
}

func (m *mockStore) ListOrgRuns(_ context.Context, orgID string, limit, offset int) ([]Run, error) {
	return make([]Run, 0), nil
}

func (m *mockStore) GetOrgRunStats(_ context.Context, orgID string) (*RunStats, error) {
	return &RunStats{}, nil
}

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

func TestLandingPage(t *testing.T) {
	store := newMockStore()
	srv := NewServer(Config{Port: "8082", LogLevel: "error"}, store)

	req := httptest.NewRequest(http.MethodGet, "/", nil)
	w := httptest.NewRecorder()

	srv.Handler().ServeHTTP(w, req)

	if w.Code != http.StatusOK {
		t.Fatalf("expected 200, got %d", w.Code)
	}

	ct := w.Header().Get("Content-Type")
	if ct != "text/html; charset=utf-8" {
		t.Errorf("expected text/html content type, got %q", ct)
	}

	if !strings.Contains(w.Body.String(), "Fault") {
		t.Error("expected body to contain 'Fault'")
	}
}

func TestLandingPage_NoAuth(t *testing.T) {
	store := newMockStore()
	srv := NewServer(Config{Port: "8082", LogLevel: "error"}, store)

	// No Authorization header — landing page should still work
	req := httptest.NewRequest(http.MethodGet, "/", nil)
	w := httptest.NewRecorder()

	srv.Handler().ServeHTTP(w, req)

	if w.Code != http.StatusOK {
		t.Fatalf("landing page should be accessible without auth, got %d", w.Code)
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

func TestUsageResponse_IncludesLimits(t *testing.T) {
	store := newMockStore()
	apiKey := store.addTestUser("limits@example.com")
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

	// Free plan should have 50 LLM call limit
	if resp.LLMLimit != 50 {
		t.Errorf("expected LLM limit of 50 for free plan, got %d", resp.LLMLimit)
	}

	if resp.LLMRemaining != 50 {
		t.Errorf("expected 50 remaining for unused user, got %d", resp.LLMRemaining)
	}
}

func TestUsageLimits_FreeUserExceeded(t *testing.T) {
	store := newMockStore()
	apiKey := store.addTestUser("limited@example.com")

	// Pre-populate usage at the limit
	month := time.Now().Format("2006-01")
	store.usage["test-user-1:"+month] = &MonthlyUsage{
		UserID:   "test-user-1",
		Month:    month,
		LLMCalls: 50, // at free limit
	}

	srv := NewServer(Config{Port: "8082", LogLevel: "error"}, store)

	req := httptest.NewRequest(http.MethodPost, "/api/v1/analyze/confidence",
		strings.NewReader(`{"diff":"some diff"}`))
	req.Header.Set("Authorization", "Bearer "+apiKey)
	req.Header.Set("Content-Type", "application/json")
	w := httptest.NewRecorder()

	srv.Handler().ServeHTTP(w, req)

	if w.Code != http.StatusTooManyRequests {
		t.Fatalf("expected 429 for exceeded free user, got %d: %s", w.Code, w.Body.String())
	}

	if w.Header().Get("X-RateLimit-Limit") != "50" {
		t.Errorf("expected X-RateLimit-Limit header of 50, got %q", w.Header().Get("X-RateLimit-Limit"))
	}
}

func TestUsageLimits_ProUserAllowed(t *testing.T) {
	store := newMockStore()
	apiKey := store.addTestUser("pro@example.com")

	// Make this user a pro user
	for _, u := range store.users {
		if u.Email == "pro@example.com" {
			u.Plan = "pro"
		}
	}

	// Give them 50 calls (would block free, but pro has 1000 limit)
	month := time.Now().Format("2006-01")
	store.usage["test-user-1:"+month] = &MonthlyUsage{
		UserID:   "test-user-1",
		Month:    month,
		LLMCalls: 50,
	}

	srv := NewServer(Config{Port: "8082", LogLevel: "error"}, store)

	req := httptest.NewRequest(http.MethodPost, "/api/v1/analyze/confidence",
		strings.NewReader(`{"diff":"some diff"}`))
	req.Header.Set("Authorization", "Bearer "+apiKey)
	req.Header.Set("Content-Type", "application/json")
	w := httptest.NewRecorder()

	srv.Handler().ServeHTTP(w, req)

	// Should NOT be 429 — pro user has 1000 limit
	if w.Code == http.StatusTooManyRequests {
		t.Fatalf("pro user should not be rate limited at 50 calls")
	}

	if w.Header().Get("X-RateLimit-Limit") != "1000" {
		t.Errorf("expected X-RateLimit-Limit of 1000 for pro, got %q", w.Header().Get("X-RateLimit-Limit"))
	}
}
