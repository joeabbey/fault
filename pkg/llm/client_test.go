package llm

import (
	"context"
	"encoding/json"
	"net/http"
	"net/http/httptest"
	"sync/atomic"
	"testing"
	"time"
)

func TestNewClient_Defaults(t *testing.T) {
	c := NewClient("test-key")

	if c.apiKey != "test-key" {
		t.Errorf("apiKey = %q, want %q", c.apiKey, "test-key")
	}
	if c.baseURL != defaultBaseURL {
		t.Errorf("baseURL = %q, want %q", c.baseURL, defaultBaseURL)
	}
	if c.model != defaultModel {
		t.Errorf("model = %q, want %q", c.model, defaultModel)
	}
	if c.maxRetries != defaultMaxRetries {
		t.Errorf("maxRetries = %d, want %d", c.maxRetries, defaultMaxRetries)
	}
}

func TestNewClient_WithOptions(t *testing.T) {
	c := NewClient("test-key",
		WithBaseURL("https://custom.api.com"),
		WithModel("custom-model"),
		WithTimeout(30*time.Second),
		WithMaxRetries(5),
	)

	if c.baseURL != "https://custom.api.com" {
		t.Errorf("baseURL = %q, want %q", c.baseURL, "https://custom.api.com")
	}
	if c.model != "custom-model" {
		t.Errorf("model = %q, want %q", c.model, "custom-model")
	}
	if c.maxRetries != 5 {
		t.Errorf("maxRetries = %d, want %d", c.maxRetries, 5)
	}
}

func TestSendMessage_Success(t *testing.T) {
	server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		// Verify request
		if r.Method != http.MethodPost {
			t.Errorf("method = %q, want POST", r.Method)
		}
		if r.URL.Path != "/messages" {
			t.Errorf("path = %q, want /messages", r.URL.Path)
		}
		if r.Header.Get("x-api-key") != "test-key" {
			t.Errorf("x-api-key = %q, want test-key", r.Header.Get("x-api-key"))
		}
		if r.Header.Get("anthropic-version") != anthropicVersion {
			t.Errorf("anthropic-version = %q, want %q", r.Header.Get("anthropic-version"), anthropicVersion)
		}
		if r.Header.Get("Content-Type") != "application/json" {
			t.Errorf("Content-Type = %q, want application/json", r.Header.Get("Content-Type"))
		}

		// Verify request body
		var req Request
		if err := json.NewDecoder(r.Body).Decode(&req); err != nil {
			t.Fatalf("decoding request: %v", err)
		}
		if req.Model != defaultModel {
			t.Errorf("request model = %q, want %q", req.Model, defaultModel)
		}
		if req.System != "You are helpful." {
			t.Errorf("request system = %q, want %q", req.System, "You are helpful.")
		}

		// Send response
		resp := Response{
			ID:   "msg_123",
			Type: "message",
			Role: "assistant",
			Content: []ContentBlock{
				{Type: "text", Text: "Hello!"},
			},
			Usage:      Usage{InputTokens: 10, OutputTokens: 5},
			StopReason: "end_turn",
		}
		w.Header().Set("Content-Type", "application/json")
		json.NewEncoder(w).Encode(resp)
	}))
	defer server.Close()

	c := NewClient("test-key", WithBaseURL(server.URL))
	ctx := context.Background()

	resp, err := c.SendMessage(ctx, "You are helpful.", []Message{
		{Role: "user", Content: "Hello"},
	})
	if err != nil {
		t.Fatalf("SendMessage: %v", err)
	}

	if resp.TextContent() != "Hello!" {
		t.Errorf("TextContent() = %q, want %q", resp.TextContent(), "Hello!")
	}
	if resp.Usage.InputTokens != 10 {
		t.Errorf("InputTokens = %d, want 10", resp.Usage.InputTokens)
	}
	if resp.Usage.OutputTokens != 5 {
		t.Errorf("OutputTokens = %d, want 5", resp.Usage.OutputTokens)
	}
}

func TestExtractJSON_Success(t *testing.T) {
	type TestData struct {
		Name  string `json:"name"`
		Value int    `json:"value"`
	}

	server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		resp := Response{
			Content: []ContentBlock{
				{Type: "text", Text: `{"name": "test", "value": 42}`},
			},
			Usage: Usage{InputTokens: 10, OutputTokens: 5},
		}
		w.Header().Set("Content-Type", "application/json")
		json.NewEncoder(w).Encode(resp)
	}))
	defer server.Close()

	c := NewClient("test-key", WithBaseURL(server.URL))
	ctx := context.Background()

	var result TestData
	if err := c.ExtractJSON(ctx, "system", "give me json", &result); err != nil {
		t.Fatalf("ExtractJSON: %v", err)
	}

	if result.Name != "test" {
		t.Errorf("Name = %q, want %q", result.Name, "test")
	}
	if result.Value != 42 {
		t.Errorf("Value = %d, want 42", result.Value)
	}
}

func TestExtractJSON_InvalidJSON(t *testing.T) {
	server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		resp := Response{
			Content: []ContentBlock{
				{Type: "text", Text: "this is not JSON"},
			},
			Usage: Usage{InputTokens: 10, OutputTokens: 5},
		}
		w.Header().Set("Content-Type", "application/json")
		json.NewEncoder(w).Encode(resp)
	}))
	defer server.Close()

	c := NewClient("test-key", WithBaseURL(server.URL))
	ctx := context.Background()

	var result map[string]interface{}
	err := c.ExtractJSON(ctx, "system", "give me json", &result)
	if err == nil {
		t.Fatal("expected error for invalid JSON, got nil")
	}
}

func TestExtractJSON_EmptyResponse(t *testing.T) {
	server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		resp := Response{
			Content: []ContentBlock{},
			Usage:   Usage{InputTokens: 10, OutputTokens: 0},
		}
		w.Header().Set("Content-Type", "application/json")
		json.NewEncoder(w).Encode(resp)
	}))
	defer server.Close()

	c := NewClient("test-key", WithBaseURL(server.URL))
	ctx := context.Background()

	var result map[string]interface{}
	err := c.ExtractJSON(ctx, "system", "give me json", &result)
	if err == nil {
		t.Fatal("expected error for empty response, got nil")
	}
}

func TestSendMessage_APIError_400(t *testing.T) {
	server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.WriteHeader(http.StatusBadRequest)
		errResp := map[string]interface{}{
			"type": "error",
			"error": map[string]string{
				"type":    "invalid_request_error",
				"message": "bad request",
			},
		}
		json.NewEncoder(w).Encode(errResp)
	}))
	defer server.Close()

	c := NewClient("test-key", WithBaseURL(server.URL))
	ctx := context.Background()

	_, err := c.SendMessage(ctx, "", []Message{{Role: "user", Content: "hi"}})
	if err == nil {
		t.Fatal("expected error for 400 response, got nil")
	}

	apiErr, ok := err.(*APIError)
	if !ok {
		t.Fatalf("expected *APIError, got %T: %v", err, err)
	}
	if apiErr.StatusCode != 400 {
		t.Errorf("StatusCode = %d, want 400", apiErr.StatusCode)
	}
	if apiErr.Type != "invalid_request_error" {
		t.Errorf("Type = %q, want %q", apiErr.Type, "invalid_request_error")
	}
}

func TestSendMessage_APIError_500(t *testing.T) {
	server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.WriteHeader(http.StatusInternalServerError)
		w.Write([]byte("internal server error"))
	}))
	defer server.Close()

	c := NewClient("test-key", WithBaseURL(server.URL))
	ctx := context.Background()

	_, err := c.SendMessage(ctx, "", []Message{{Role: "user", Content: "hi"}})
	if err == nil {
		t.Fatal("expected error for 500 response, got nil")
	}

	apiErr, ok := err.(*APIError)
	if !ok {
		t.Fatalf("expected *APIError, got %T: %v", err, err)
	}
	if apiErr.StatusCode != 500 {
		t.Errorf("StatusCode = %d, want 500", apiErr.StatusCode)
	}
}

func TestSendMessage_Retry_429(t *testing.T) {
	var attempts int32

	server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		attempt := atomic.AddInt32(&attempts, 1)
		if attempt <= 2 {
			w.WriteHeader(http.StatusTooManyRequests)
			errResp := map[string]interface{}{
				"type": "error",
				"error": map[string]string{
					"type":    "rate_limit_error",
					"message": "rate limited",
				},
			}
			json.NewEncoder(w).Encode(errResp)
			return
		}

		// Third attempt succeeds
		resp := Response{
			Content: []ContentBlock{
				{Type: "text", Text: "Success after retries"},
			},
			Usage: Usage{InputTokens: 10, OutputTokens: 5},
		}
		w.Header().Set("Content-Type", "application/json")
		json.NewEncoder(w).Encode(resp)
	}))
	defer server.Close()

	c := NewClient("test-key", WithBaseURL(server.URL), WithMaxRetries(3))
	ctx := context.Background()

	resp, err := c.SendMessage(ctx, "", []Message{{Role: "user", Content: "hi"}})
	if err != nil {
		t.Fatalf("SendMessage: %v", err)
	}

	if resp.TextContent() != "Success after retries" {
		t.Errorf("TextContent() = %q, want %q", resp.TextContent(), "Success after retries")
	}

	if atomic.LoadInt32(&attempts) != 3 {
		t.Errorf("attempts = %d, want 3", atomic.LoadInt32(&attempts))
	}
}

func TestSendMessage_Retry_Exhausted(t *testing.T) {
	var attempts int32

	server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		atomic.AddInt32(&attempts, 1)
		w.WriteHeader(http.StatusTooManyRequests)
		errResp := map[string]interface{}{
			"type": "error",
			"error": map[string]string{
				"type":    "rate_limit_error",
				"message": "rate limited",
			},
		}
		json.NewEncoder(w).Encode(errResp)
	}))
	defer server.Close()

	// Use 0 retries so it fails immediately after 1 attempt total
	c := NewClient("test-key", WithBaseURL(server.URL), WithMaxRetries(1))
	ctx := context.Background()

	_, err := c.SendMessage(ctx, "", []Message{{Role: "user", Content: "hi"}})
	if err == nil {
		t.Fatal("expected error after exhausting retries, got nil")
	}

	// 1 initial + 1 retry = 2 attempts
	if atomic.LoadInt32(&attempts) != 2 {
		t.Errorf("attempts = %d, want 2", atomic.LoadInt32(&attempts))
	}
}

func TestSendMessage_ContextCancelled(t *testing.T) {
	server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		time.Sleep(5 * time.Second)
		w.WriteHeader(http.StatusOK)
	}))
	defer server.Close()

	c := NewClient("test-key", WithBaseURL(server.URL))
	ctx, cancel := context.WithTimeout(context.Background(), 100*time.Millisecond)
	defer cancel()

	_, err := c.SendMessage(ctx, "", []Message{{Role: "user", Content: "hi"}})
	if err == nil {
		t.Fatal("expected error for context cancellation, got nil")
	}
}

func TestTextContent_MultipleBlocks(t *testing.T) {
	resp := &Response{
		Content: []ContentBlock{
			{Type: "text", Text: "Hello "},
			{Type: "text", Text: "World"},
		},
	}

	if resp.TextContent() != "Hello World" {
		t.Errorf("TextContent() = %q, want %q", resp.TextContent(), "Hello World")
	}
}

func TestTextContent_NoBlocks(t *testing.T) {
	resp := &Response{
		Content: []ContentBlock{},
	}

	if resp.TextContent() != "" {
		t.Errorf("TextContent() = %q, want empty", resp.TextContent())
	}
}

func TestTruncate(t *testing.T) {
	tests := []struct {
		input  string
		maxLen int
		want   string
	}{
		{"short", 10, "short"},
		{"exactly10!", 10, "exactly10!"},
		{"this is a longer string", 10, "this is a ..."},
		{"", 5, ""},
	}

	for _, tt := range tests {
		got := truncate(tt.input, tt.maxLen)
		if got != tt.want {
			t.Errorf("truncate(%q, %d) = %q, want %q", tt.input, tt.maxLen, got, tt.want)
		}
	}
}

func TestAPIError_Error(t *testing.T) {
	tests := []struct {
		name string
		err  APIError
		want string
	}{
		{
			name: "with message",
			err: APIError{
				StatusCode: 400,
				Type:       "invalid_request_error",
				Message:    "bad request",
			},
			want: "anthropic API error (HTTP 400): invalid_request_error: bad request",
		},
		{
			name: "without message",
			err: APIError{
				StatusCode: 500,
				body:       "internal error",
			},
			want: "anthropic API error (HTTP 500): internal error",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := tt.err.Error()
			if got != tt.want {
				t.Errorf("Error() = %q, want %q", got, tt.want)
			}
		})
	}
}
