package cloud

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"io"
	"log/slog"
	"net/http"
	"os"
	"time"
)

// Version is set at build time via ldflags.
var Version = "0.1.0-dev"

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
}

// AnthropicMessage represents a message in the Anthropic API format.
type AnthropicMessage struct {
	Role    string `json:"role"`
	Content string `json:"content"`
}

// AnthropicRequest is the request body for the Anthropic Claude API.
type AnthropicRequest struct {
	Model     string             `json:"model"`
	MaxTokens int                `json:"max_tokens"`
	Messages  []AnthropicMessage `json:"messages"`
}

// AnthropicUsage represents token usage from the Anthropic API response.
type AnthropicUsage struct {
	InputTokens  int64 `json:"input_tokens"`
	OutputTokens int64 `json:"output_tokens"`
}

// AnthropicContentBlock represents a content block in the Anthropic response.
type AnthropicContentBlock struct {
	Type string `json:"type"`
	Text string `json:"text"`
}

// AnthropicResponse is the response from the Anthropic Claude API.
type AnthropicResponse struct {
	Content []AnthropicContentBlock `json:"content"`
	Usage   AnthropicUsage          `json:"usage"`
}

// Handlers holds dependencies for HTTP route handlers.
type Handlers struct {
	store  Store
	logger *slog.Logger
}

// NewHandlers creates a new Handlers with the given store and logger.
func NewHandlers(store Store, logger *slog.Logger) *Handlers {
	return &Handlers{store: store, logger: logger}
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

	prompt := buildConfidencePrompt(req.Diff, req.Language)
	result, usage, err := callAnthropic(r.Context(), prompt)
	if err != nil {
		h.logger.Error("anthropic API call failed", "error", err)
		http.Error(w, `{"error":"LLM analysis failed"}`, http.StatusBadGateway)
		return
	}

	// Track usage
	if err := h.store.IncrementUsage(r.Context(), user.ID, Usage{
		TokensInput:  usage.InputTokens,
		TokensOutput: usage.OutputTokens,
	}); err != nil {
		h.logger.Error("failed to track usage", "error", err, "user_id", user.ID)
	}

	writeJSON(w, http.StatusOK, AnalyzeResponse{
		Result:      result,
		TokensInput: usage.InputTokens,
		TokensOut:   usage.OutputTokens,
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

	prompt := buildSpecPrompt(req.Diff, req.Spec, req.Language)
	result, usage, err := callAnthropic(r.Context(), prompt)
	if err != nil {
		h.logger.Error("anthropic API call failed", "error", err)
		http.Error(w, `{"error":"LLM analysis failed"}`, http.StatusBadGateway)
		return
	}

	// Track usage
	if err := h.store.IncrementUsage(r.Context(), user.ID, Usage{
		TokensInput:  usage.InputTokens,
		TokensOutput: usage.OutputTokens,
	}); err != nil {
		h.logger.Error("failed to track usage", "error", err, "user_id", user.ID)
	}

	writeJSON(w, http.StatusOK, AnalyzeResponse{
		Result:      result,
		TokensInput: usage.InputTokens,
		TokensOut:   usage.OutputTokens,
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

	writeJSON(w, http.StatusOK, UsageResponse{
		UserID:       user.ID,
		Email:        user.Email,
		Plan:         user.Plan,
		Month:        usage.Month,
		LLMCalls:     usage.LLMCalls,
		TokensInput:  usage.TokensInput,
		TokensOutput: usage.TokensOutput,
		Analyses:     usage.Analyses,
	})
}

// buildConfidencePrompt constructs the prompt for confidence scoring.
func buildConfidencePrompt(diff string, language string) string {
	langHint := ""
	if language != "" {
		langHint = fmt.Sprintf(" The code is written in %s.", language)
	}

	return fmt.Sprintf(`You are an expert code reviewer analyzing a git diff for potential issues.%s

Analyze the following diff and return a JSON object with:
- "confidence": a score from 0.0 to 1.0 indicating confidence the changes are correct
- "issues": an array of objects with "severity" (error/warning/info), "message", and "line" (if applicable)
- "summary": a brief summary of the analysis

Return ONLY valid JSON, no markdown formatting.

Diff:
%s`, langHint, diff)
}

// buildSpecPrompt constructs the prompt for spec comparison.
func buildSpecPrompt(diff string, spec string, language string) string {
	langHint := ""
	if language != "" {
		langHint = fmt.Sprintf(" The code is written in %s.", language)
	}

	return fmt.Sprintf(`You are an expert code reviewer comparing a git diff against a specification.%s

Analyze whether the diff correctly implements the specification. Return a JSON object with:
- "conformance": a score from 0.0 to 1.0 indicating how well the diff matches the spec
- "missing": an array of spec requirements not addressed in the diff
- "deviations": an array of ways the diff deviates from the spec
- "summary": a brief summary of the comparison

Return ONLY valid JSON, no markdown formatting.

Specification:
%s

Diff:
%s`, langHint, spec, diff)
}

// callAnthropic sends a prompt to the Anthropic Claude API and returns the raw
// JSON result along with token usage.
func callAnthropic(ctx context.Context, prompt string) (json.RawMessage, AnthropicUsage, error) {
	apiKey := os.Getenv("ANTHROPIC_API_KEY")
	if apiKey == "" {
		return nil, AnthropicUsage{}, fmt.Errorf("ANTHROPIC_API_KEY not set")
	}

	reqBody := AnthropicRequest{
		Model:     "claude-sonnet-4-20250514",
		MaxTokens: 4096,
		Messages: []AnthropicMessage{
			{Role: "user", Content: prompt},
		},
	}

	body, err := json.Marshal(reqBody)
	if err != nil {
		return nil, AnthropicUsage{}, fmt.Errorf("marshaling request: %w", err)
	}

	httpReq, err := http.NewRequestWithContext(ctx, http.MethodPost,
		"https://api.anthropic.com/v1/messages", bytes.NewReader(body))
	if err != nil {
		return nil, AnthropicUsage{}, fmt.Errorf("creating request: %w", err)
	}

	httpReq.Header.Set("Content-Type", "application/json")
	httpReq.Header.Set("x-api-key", apiKey)
	httpReq.Header.Set("anthropic-version", "2023-06-01")

	resp, err := http.DefaultClient.Do(httpReq)
	if err != nil {
		return nil, AnthropicUsage{}, fmt.Errorf("calling Anthropic API: %w", err)
	}
	defer resp.Body.Close()

	respBody, err := io.ReadAll(resp.Body)
	if err != nil {
		return nil, AnthropicUsage{}, fmt.Errorf("reading response: %w", err)
	}

	if resp.StatusCode != http.StatusOK {
		return nil, AnthropicUsage{}, fmt.Errorf("Anthropic API returned %d: %s",
			resp.StatusCode, string(respBody))
	}

	var anthropicResp AnthropicResponse
	if err := json.Unmarshal(respBody, &anthropicResp); err != nil {
		return nil, AnthropicUsage{}, fmt.Errorf("parsing response: %w", err)
	}

	// Extract text content
	var resultText string
	for _, block := range anthropicResp.Content {
		if block.Type == "text" {
			resultText = block.Text
			break
		}
	}

	// Try to parse as JSON; if it fails, wrap in a JSON string
	var result json.RawMessage
	if json.Valid([]byte(resultText)) {
		result = json.RawMessage(resultText)
	} else {
		wrapped, _ := json.Marshal(resultText)
		result = json.RawMessage(wrapped)
	}

	return result, anthropicResp.Usage, nil
}

// writeJSON encodes v as JSON and writes it to w with the given status code.
func writeJSON(w http.ResponseWriter, status int, v interface{}) {
	w.Header().Set("Content-Type", "application/json")
	w.WriteHeader(status)
	json.NewEncoder(w).Encode(v)
}
