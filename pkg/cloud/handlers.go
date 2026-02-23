package cloud

import (
	"context"
	"encoding/json"
	"fmt"
	"log/slog"
	"net/http"
	"os"
	"strings"
	"time"

	"github.com/joeabbey/fault/pkg/llm"
	"github.com/joeabbey/magma/pkg/limits"
)

// Version is set at build time via ldflags.
var Version = "0.1.0-dev"

const defaultAnthropicModel = "claude-sonnet-4-20250514"

// HealthResponse is returned by the health check endpoint.
type HealthResponse struct {
	Status  string `json:"status"`
	Version string `json:"version"`
}

// AnalyzeConfidenceRequest is the payload for the confidence analysis endpoint.
type AnalyzeConfidenceRequest struct {
	Diff     string `json:"diff"`
	Language string `json:"language,omitempty"`
}

// AnalyzeSpecRequest is the payload for the spec comparison endpoint.
type AnalyzeSpecRequest struct {
	Diff     string `json:"diff"`
	Spec     string `json:"spec"`
	Language string `json:"language,omitempty"`
}

// AnalyzeResponse is returned by both analysis endpoints.
type AnalyzeResponse struct {
	Result      json.RawMessage `json:"result"`
	TokensInput int64           `json:"tokens_input"`
	TokensOut   int64           `json:"tokens_output"`
}

// UsageResponse is returned by the usage endpoint.
type UsageResponse struct {
	UserID       string `json:"user_id"`
	Email        string `json:"email"`
	Plan         string `json:"plan"`
	Month        string `json:"month"`
	LLMCalls     int    `json:"llm_calls"`
	TokensInput  int64  `json:"tokens_input"`
	TokensOutput int64  `json:"tokens_output"`
	Analyses     int    `json:"analyses"`
	LLMLimit     int    `json:"llm_limit"`
	LLMRemaining int    `json:"llm_remaining"`
}

// Handlers holds dependencies for HTTP route handlers.
type Handlers struct {
	store        Store
	limitsEngine *limits.Engine
	logger       *slog.Logger
}

// NewHandlers creates a new Handlers with the given store, limits engine, and logger.
func NewHandlers(store Store, limitsEngine *limits.Engine, logger *slog.Logger) *Handlers {
	return &Handlers{store: store, limitsEngine: limitsEngine, logger: logger}
}

// SignupRequest is the payload for the signup endpoint.
type SignupRequest struct {
	Email string `json:"email"`
}

// SignupResponse is returned by the signup endpoint.
type SignupResponse struct {
	APIKey string `json:"api_key"`
	Email  string `json:"email"`
}

// RotateKeyResponse is returned by the key rotation endpoint.
type RotateKeyResponse struct {
	APIKey string `json:"api_key"`
	Email  string `json:"email"`
}

// HandleSignup creates a new user and returns their API key.
// If the user already exists, a new key is generated (old key is invalidated).
func (h *Handlers) HandleSignup(w http.ResponseWriter, r *http.Request) {
	var req SignupRequest
	if err := json.NewDecoder(r.Body).Decode(&req); err != nil {
		http.Error(w, `{"error":"invalid request body"}`, http.StatusBadRequest)
		return
	}

	if req.Email == "" {
		http.Error(w, `{"error":"email is required"}`, http.StatusBadRequest)
		return
	}

	// Check for existing user
	existing, err := h.store.GetUserByEmail(r.Context(), req.Email)
	if err != nil {
		h.logger.Error("failed to look up user", "error", err)
		http.Error(w, `{"error":"internal error"}`, http.StatusInternalServerError)
		return
	}

	var user *User
	if existing != nil {
		user = existing
	} else {
		user, err = h.store.CreateUser(r.Context(), req.Email)
		if err != nil {
			h.logger.Error("failed to create user", "error", err)
			http.Error(w, `{"error":"failed to create user"}`, http.StatusInternalServerError)
			return
		}
	}

	// Generate API key (replaces any existing key)
	rawKey, err := h.store.GenerateAPIKey(r.Context(), user.ID)
	if err != nil {
		h.logger.Error("failed to generate API key", "error", err)
		http.Error(w, `{"error":"failed to generate API key"}`, http.StatusInternalServerError)
		return
	}

	h.logger.Info("user signed up", "email", req.Email, "new_user", existing == nil)

	writeJSON(w, http.StatusOK, SignupResponse{
		APIKey: rawKey,
		Email:  user.Email,
	})
}

// HandleRotateKey generates a new API key for the authenticated user,
// invalidating the old one. The new key is only shown once.
func (h *Handlers) HandleRotateKey(w http.ResponseWriter, r *http.Request) {
	user := UserFromContext(r.Context())
	if user == nil {
		http.Error(w, `{"error":"unauthorized"}`, http.StatusUnauthorized)
		return
	}

	rawKey, err := h.store.GenerateAPIKey(r.Context(), user.ID)
	if err != nil {
		h.logger.Error("failed to rotate API key", "error", err, "user_id", user.ID)
		http.Error(w, `{"error":"failed to rotate API key"}`, http.StatusInternalServerError)
		return
	}

	h.logger.Info("API key rotated", "user_id", user.ID, "email", user.Email)

	writeJSON(w, http.StatusOK, RotateKeyResponse{
		APIKey: rawKey,
		Email:  user.Email,
	})
}

// HandleHealth returns the health check response.
func (h *Handlers) HandleHealth(w http.ResponseWriter, r *http.Request) {
	writeJSON(w, http.StatusOK, HealthResponse{
		Status:  "ok",
		Version: Version,
	})
}

// HandleAnalyzeConfidence proxies a confidence analysis request to the Anthropic API.
func (h *Handlers) HandleAnalyzeConfidence(w http.ResponseWriter, r *http.Request) {
	user := UserFromContext(r.Context())
	if user == nil {
		http.Error(w, `{"error":"unauthorized"}`, http.StatusUnauthorized)
		return
	}

	var req AnalyzeConfidenceRequest
	if err := json.NewDecoder(r.Body).Decode(&req); err != nil {
		http.Error(w, `{"error":"invalid request body"}`, http.StatusBadRequest)
		return
	}

	if req.Diff == "" {
		http.Error(w, `{"error":"diff is required"}`, http.StatusBadRequest)
		return
	}

	systemPrompt := llm.LoadPrompt("confidence")
	if systemPrompt == "" {
		h.logger.Error("missing confidence prompt template")
		http.Error(w, `{"error":"server misconfigured"}`, http.StatusInternalServerError)
		return
	}

	userPrompt := req.Diff
	if req.Language != "" {
		userPrompt = fmt.Sprintf("Language: %s\n\n%s", req.Language, req.Diff)
	}

	result, usage, err := callAnthropic(r.Context(), systemPrompt, userPrompt)
	if err != nil {
		h.logger.Error("anthropic API call failed", "error", err)
		http.Error(w, `{"error":"LLM analysis failed"}`, http.StatusBadGateway)
		return
	}

	// Track usage
	if err := h.store.IncrementUsage(r.Context(), user.ID, usage); err != nil {
		h.logger.Error("failed to track usage", "error", err, "user_id", user.ID)
	}

	writeJSON(w, http.StatusOK, AnalyzeResponse{
		Result:      result,
		TokensInput: usage.TokensInput,
		TokensOut:   usage.TokensOutput,
	})
}

// HandleAnalyzeSpec proxies a spec comparison request to the Anthropic API.
func (h *Handlers) HandleAnalyzeSpec(w http.ResponseWriter, r *http.Request) {
	user := UserFromContext(r.Context())
	if user == nil {
		http.Error(w, `{"error":"unauthorized"}`, http.StatusUnauthorized)
		return
	}

	var req AnalyzeSpecRequest
	if err := json.NewDecoder(r.Body).Decode(&req); err != nil {
		http.Error(w, `{"error":"invalid request body"}`, http.StatusBadRequest)
		return
	}

	if req.Diff == "" || req.Spec == "" {
		http.Error(w, `{"error":"diff and spec are required"}`, http.StatusBadRequest)
		return
	}

	systemPrompt := llm.LoadPrompt("spec_compare")
	if systemPrompt == "" {
		h.logger.Error("missing spec_compare prompt template")
		http.Error(w, `{"error":"server misconfigured"}`, http.StatusInternalServerError)
		return
	}

	userPrompt := buildSpecUserPrompt(req.Spec, req.Diff, req.Language)
	result, usage, err := callAnthropic(r.Context(), systemPrompt, userPrompt)
	if err != nil {
		h.logger.Error("anthropic API call failed", "error", err)
		http.Error(w, `{"error":"LLM analysis failed"}`, http.StatusBadGateway)
		return
	}

	// Track usage
	if err := h.store.IncrementUsage(r.Context(), user.ID, usage); err != nil {
		h.logger.Error("failed to track usage", "error", err, "user_id", user.ID)
	}

	writeJSON(w, http.StatusOK, AnalyzeResponse{
		Result:      result,
		TokensInput: usage.TokensInput,
		TokensOut:   usage.TokensOutput,
	})
}

// HandleAnalyzeSpecStructured proxies a structured per-requirement spec comparison to the Anthropic API.
func (h *Handlers) HandleAnalyzeSpecStructured(w http.ResponseWriter, r *http.Request) {
	user := UserFromContext(r.Context())
	if user == nil {
		http.Error(w, `{"error":"unauthorized"}`, http.StatusUnauthorized)
		return
	}

	var req AnalyzeSpecRequest
	if err := json.NewDecoder(r.Body).Decode(&req); err != nil {
		http.Error(w, `{"error":"invalid request body"}`, http.StatusBadRequest)
		return
	}

	if req.Diff == "" || req.Spec == "" {
		http.Error(w, `{"error":"diff and spec are required"}`, http.StatusBadRequest)
		return
	}

	systemPrompt := llm.LoadPrompt("spec_structured")
	if systemPrompt == "" {
		h.logger.Error("missing spec_structured prompt template")
		http.Error(w, `{"error":"server misconfigured"}`, http.StatusInternalServerError)
		return
	}

	userPrompt := buildSpecUserPrompt(req.Spec, req.Diff, req.Language)
	result, usage, err := callAnthropic(r.Context(), systemPrompt, userPrompt)
	if err != nil {
		h.logger.Error("anthropic API call failed", "error", err)
		http.Error(w, `{"error":"LLM analysis failed"}`, http.StatusBadGateway)
		return
	}

	// Track usage
	if err := h.store.IncrementUsage(r.Context(), user.ID, usage); err != nil {
		h.logger.Error("failed to track usage", "error", err, "user_id", user.ID)
	}

	writeJSON(w, http.StatusOK, AnalyzeResponse{
		Result:      result,
		TokensInput: usage.TokensInput,
		TokensOut:   usage.TokensOutput,
	})
}

// HandleUsage returns the current month's usage for the authenticated user.
func (h *Handlers) HandleUsage(w http.ResponseWriter, r *http.Request) {
	user := UserFromContext(r.Context())
	if user == nil {
		http.Error(w, `{"error":"unauthorized"}`, http.StatusUnauthorized)
		return
	}

	month := time.Now().Format("2006-01")
	usage, err := h.store.GetUsage(r.Context(), user.ID, month)
	if err != nil {
		h.logger.Error("failed to get usage", "error", err, "user_id", user.ID)
		http.Error(w, `{"error":"failed to get usage"}`, http.StatusInternalServerError)
		return
	}

	check := h.limitsEngine.Check(r.Context(), user.ID, "llm_calls")

	writeJSON(w, http.StatusOK, UsageResponse{
		UserID:       user.ID,
		Email:        user.Email,
		Plan:         user.Plan,
		Month:        usage.Month,
		LLMCalls:     usage.LLMCalls,
		TokensInput:  usage.TokensInput,
		TokensOutput: usage.TokensOutput,
		Analyses:     usage.Analyses,
		LLMLimit:     check.Limit,
		LLMRemaining: check.Remaining,
	})
}

func buildSpecUserPrompt(spec string, diff string, language string) string {
	var b strings.Builder
	if language != "" {
		b.WriteString(fmt.Sprintf("Language: %s\n\n", language))
	}
	b.WriteString("## Specification\n\n")
	b.WriteString(spec)
	b.WriteString("\n\n## Code Changes (Diff)\n\n")
	b.WriteString(diff)
	return b.String()
}

// callAnthropic sends a prompt to the Anthropic Claude API and returns the raw JSON result
// along with token usage. The prompt is split into an LLM system prompt and a user prompt.
func callAnthropic(ctx context.Context, systemPrompt string, userPrompt string) (json.RawMessage, Usage, error) {
	apiKey := strings.TrimSpace(os.Getenv("ANTHROPIC_API_KEY"))
	if apiKey == "" {
		return nil, Usage{}, fmt.Errorf("ANTHROPIC_API_KEY not set")
	}

	model := strings.TrimSpace(os.Getenv("ANTHROPIC_MODEL"))
	if model == "" {
		model = defaultAnthropicModel
	}
	opts := []llm.Option{llm.WithModel(model)}
	client := llm.NewClient(apiKey, opts...)

	resp, err := client.SendMessage(ctx, systemPrompt, []llm.Message{
		{Role: "user", Content: userPrompt},
	})
	if err != nil {
		return nil, Usage{}, fmt.Errorf("calling Anthropic API: %w", err)
	}

	text := strings.TrimSpace(resp.TextContent())
	if text == "" {
		return nil, Usage{}, fmt.Errorf("empty response from Anthropic API")
	}

	if !json.Valid([]byte(text)) {
		if extracted := extractJSON(text); extracted != "" {
			text = extracted
		} else {
			return nil, Usage{}, fmt.Errorf("invalid JSON response from Anthropic API: %s", truncate(text, 200))
		}
	}

	return json.RawMessage(text), Usage{
		TokensInput:  int64(resp.Usage.InputTokens),
		TokensOutput: int64(resp.Usage.OutputTokens),
	}, nil
}

func extractJSON(s string) string {
	s = strings.TrimSpace(s)
	if strings.HasPrefix(s, "```") {
		if idx := strings.Index(s, "\n"); idx != -1 {
			s = s[idx+1:]
		}
		if end := strings.LastIndex(s, "```"); end != -1 {
			s = s[:end]
		}
		s = strings.TrimSpace(s)
	}

	start := strings.IndexAny(s, "{[")
	if start == -1 {
		return ""
	}
	end := strings.LastIndexAny(s, "}]")
	if end == -1 || end <= start {
		return ""
	}

	candidate := strings.TrimSpace(s[start : end+1])
	if json.Valid([]byte(candidate)) {
		return candidate
	}
	return ""
}

func truncate(s string, maxLen int) string {
	if len(s) <= maxLen {
		return s
	}
	return s[:maxLen] + "..."
}

// writeJSON encodes v as JSON and writes it to w with the given status code.
func writeJSON(w http.ResponseWriter, status int, v interface{}) {
	w.Header().Set("Content-Type", "application/json")
	w.WriteHeader(status)
	json.NewEncoder(w).Encode(v)
}
