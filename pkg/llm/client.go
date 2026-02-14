package llm

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"io"
	"math"
	"net/http"
	"time"
)

const (
	defaultBaseURL        = "https://api.anthropic.com/v1"
	defaultModel          = "claude-sonnet-4-5-20250929"
	defaultTimeout        = 120 * time.Second
	defaultMaxRetries     = 3
	anthropicVersion      = "2023-06-01"
	defaultMaxTokens      = 4096
	retryBaseDelay        = 1 * time.Second
)

// Client is an HTTP client for the Anthropic Claude Messages API.
type Client struct {
	apiKey     string
	baseURL    string
	httpClient *http.Client
	model      string
	maxRetries int
}

// Message represents a single message in a conversation.
type Message struct {
	Role    string `json:"role"`
	Content string `json:"content"`
}

// Request represents an Anthropic Messages API request.
type Request struct {
	Model     string    `json:"model"`
	MaxTokens int       `json:"max_tokens"`
	Messages  []Message `json:"messages"`
	System    string    `json:"system,omitempty"`
}

// Response represents an Anthropic Messages API response.
type Response struct {
	ID         string         `json:"id"`
	Type       string         `json:"type"`
	Role       string         `json:"role"`
	Content    []ContentBlock `json:"content"`
	Model      string         `json:"model"`
	Usage      Usage          `json:"usage"`
	StopReason string         `json:"stop_reason"`
}

// ContentBlock represents a block of content in a response.
type ContentBlock struct {
	Type string `json:"type"`
	Text string `json:"text"`
}

// Usage tracks token consumption.
type Usage struct {
	InputTokens  int `json:"input_tokens"`
	OutputTokens int `json:"output_tokens"`
}

// APIError represents an error response from the Anthropic API.
type APIError struct {
	StatusCode int
	Type       string `json:"type"`
	Message    string `json:"message"`
	body       string
}

// apiErrorEnvelope wraps the error object in API responses.
type apiErrorEnvelope struct {
	Type  string   `json:"type"`
	Error APIError `json:"error"`
}

func (e *APIError) Error() string {
	if e.Message != "" {
		return fmt.Sprintf("anthropic API error (HTTP %d): %s: %s", e.StatusCode, e.Type, e.Message)
	}
	return fmt.Sprintf("anthropic API error (HTTP %d): %s", e.StatusCode, e.body)
}

// Option is a functional option for configuring the Client.
type Option func(*Client)

// WithBaseURL sets the API base URL.
func WithBaseURL(url string) Option {
	return func(c *Client) {
		c.baseURL = url
	}
}

// WithModel sets the model to use.
func WithModel(model string) Option {
	return func(c *Client) {
		c.model = model
	}
}

// WithTimeout sets the HTTP client timeout.
func WithTimeout(timeout time.Duration) Option {
	return func(c *Client) {
		c.httpClient.Timeout = timeout
	}
}

// WithMaxRetries sets the maximum number of retries on 429 responses.
func WithMaxRetries(n int) Option {
	return func(c *Client) {
		c.maxRetries = n
	}
}

// WithHTTPClient sets a custom HTTP client (useful for testing).
func WithHTTPClient(httpClient *http.Client) Option {
	return func(c *Client) {
		c.httpClient = httpClient
	}
}

// NewClient creates a new Anthropic API client.
func NewClient(apiKey string, opts ...Option) *Client {
	c := &Client{
		apiKey:     apiKey,
		baseURL:    defaultBaseURL,
		model:      defaultModel,
		maxRetries: defaultMaxRetries,
		httpClient: &http.Client{
			Timeout: defaultTimeout,
		},
	}

	for _, opt := range opts {
		opt(c)
	}

	return c
}

// SendMessage sends a message to the Anthropic Messages API and returns the response.
func (c *Client) SendMessage(ctx context.Context, system string, messages []Message) (*Response, error) {
	req := &Request{
		Model:     c.model,
		MaxTokens: defaultMaxTokens,
		Messages:  messages,
		System:    system,
	}

	return c.sendWithRetry(ctx, req)
}

// ExtractJSON sends a message and unmarshals the JSON response into the target.
func (c *Client) ExtractJSON(ctx context.Context, system string, userPrompt string, target interface{}) error {
	messages := []Message{
		{Role: "user", Content: userPrompt},
	}

	resp, err := c.SendMessage(ctx, system, messages)
	if err != nil {
		return fmt.Errorf("sending message: %w", err)
	}

	text := resp.TextContent()
	if text == "" {
		return fmt.Errorf("empty response from API")
	}

	if err := json.Unmarshal([]byte(text), target); err != nil {
		return fmt.Errorf("parsing JSON response: %w (response: %s)", err, truncate(text, 200))
	}

	return nil
}

// TextContent returns the concatenated text from all text content blocks.
func (r *Response) TextContent() string {
	var text string
	for _, block := range r.Content {
		if block.Type == "text" {
			text += block.Text
		}
	}
	return text
}

// sendWithRetry sends a request with exponential backoff retry on 429 errors.
func (c *Client) sendWithRetry(ctx context.Context, req *Request) (*Response, error) {
	var lastErr error

	for attempt := 0; attempt <= c.maxRetries; attempt++ {
		if attempt > 0 {
			delay := retryBaseDelay * time.Duration(math.Pow(2, float64(attempt-1)))
			select {
			case <-ctx.Done():
				return nil, ctx.Err()
			case <-time.After(delay):
			}
		}

		resp, err := c.send(ctx, req)
		if err == nil {
			return resp, nil
		}

		// Only retry on 429 (rate limit)
		apiErr, ok := err.(*APIError)
		if !ok || apiErr.StatusCode != http.StatusTooManyRequests {
			return nil, err
		}

		lastErr = err
	}

	return nil, fmt.Errorf("max retries (%d) exceeded: %w", c.maxRetries, lastErr)
}

// send performs a single API request.
func (c *Client) send(ctx context.Context, req *Request) (*Response, error) {
	body, err := json.Marshal(req)
	if err != nil {
		return nil, fmt.Errorf("marshaling request: %w", err)
	}

	httpReq, err := http.NewRequestWithContext(ctx, http.MethodPost, c.baseURL+"/messages", bytes.NewReader(body))
	if err != nil {
		return nil, fmt.Errorf("creating request: %w", err)
	}

	httpReq.Header.Set("Content-Type", "application/json")
	httpReq.Header.Set("x-api-key", c.apiKey)
	httpReq.Header.Set("anthropic-version", anthropicVersion)

	httpResp, err := c.httpClient.Do(httpReq)
	if err != nil {
		return nil, fmt.Errorf("sending request: %w", err)
	}
	defer httpResp.Body.Close()

	respBody, err := io.ReadAll(httpResp.Body)
	if err != nil {
		return nil, fmt.Errorf("reading response body: %w", err)
	}

	if httpResp.StatusCode != http.StatusOK {
		return nil, parseAPIError(httpResp.StatusCode, respBody)
	}

	var resp Response
	if err := json.Unmarshal(respBody, &resp); err != nil {
		return nil, fmt.Errorf("parsing response: %w", err)
	}

	return &resp, nil
}

// parseAPIError parses an error response body into an APIError.
func parseAPIError(statusCode int, body []byte) *APIError {
	var envelope apiErrorEnvelope
	if err := json.Unmarshal(body, &envelope); err == nil && envelope.Error.Type != "" {
		envelope.Error.StatusCode = statusCode
		return &envelope.Error
	}

	return &APIError{
		StatusCode: statusCode,
		body:       string(body),
	}
}

// truncate shortens a string to maxLen, appending "..." if truncated.
func truncate(s string, maxLen int) string {
	if len(s) <= maxLen {
		return s
	}
	return s[:maxLen] + "..."
}
