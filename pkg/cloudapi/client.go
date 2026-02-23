package cloudapi

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"io"
	"net/http"
	"strings"
	"time"

	"github.com/joeabbey/fault/pkg/analyzer"
	"github.com/joeabbey/fault/pkg/llm"
)

const DefaultBaseURL = "https://fault.jabbey.io"

type Client struct {
	baseURL    string
	apiKey     string
	httpClient *http.Client
}

type Option func(*Client)

func WithHTTPClient(hc *http.Client) Option {
	return func(c *Client) {
		if hc != nil {
			c.httpClient = hc
		}
	}
}

func WithTimeout(d time.Duration) Option {
	return func(c *Client) {
		if c.httpClient == nil {
			c.httpClient = &http.Client{}
		}
		c.httpClient.Timeout = d
	}
}

func New(baseURL string, apiKey string, opts ...Option) *Client {
	baseURL = strings.TrimRight(strings.TrimSpace(baseURL), "/")
	if baseURL == "" {
		baseURL = DefaultBaseURL
	}

	c := &Client{
		baseURL: baseURL,
		apiKey:  strings.TrimSpace(apiKey),
		httpClient: &http.Client{
			Timeout: 60 * time.Second,
		},
	}
	for _, opt := range opts {
		opt(c)
	}
	return c
}

type analyzeConfidenceRequest struct {
	Diff     string `json:"diff"`
	Language string `json:"language,omitempty"`
}

type analyzeSpecRequest struct {
	Diff     string `json:"diff"`
	Spec     string `json:"spec"`
	Language string `json:"language,omitempty"`
}

type analyzeResponse struct {
	Result      json.RawMessage `json:"result"`
	TokensInput int64           `json:"tokens_input"`
	TokensOut   int64           `json:"tokens_output"`
}

type errorResponse struct {
	Error string `json:"error"`
}

func (c *Client) AnalyzeConfidence(ctx context.Context, diffSummary string) (*llm.ConfidenceResult, error) {
	if strings.TrimSpace(diffSummary) == "" {
		return nil, fmt.Errorf("diff is empty")
	}

	var resp analyzeResponse
	if err := c.doJSON(ctx, http.MethodPost, "/api/v1/analyze/confidence", analyzeConfidenceRequest{
		Diff: diffSummary,
	}, &resp); err != nil {
		return nil, err
	}

	var result llm.ConfidenceResult
	if err := json.Unmarshal(resp.Result, &result); err != nil {
		return nil, fmt.Errorf("decoding confidence result: %w", err)
	}
	return &result, nil
}

func (c *Client) AnalyzeSpec(ctx context.Context, spec string, diffSummary string) (*llm.SpecResult, error) {
	if strings.TrimSpace(spec) == "" {
		return nil, fmt.Errorf("spec is empty")
	}
	if strings.TrimSpace(diffSummary) == "" {
		return nil, fmt.Errorf("diff is empty")
	}

	var resp analyzeResponse
	if err := c.doJSON(ctx, http.MethodPost, "/api/v1/analyze/spec", analyzeSpecRequest{
		Diff: diffSummary,
		Spec: spec,
	}, &resp); err != nil {
		return nil, err
	}

	var result llm.SpecResult
	if err := json.Unmarshal(resp.Result, &result); err != nil {
		return nil, fmt.Errorf("decoding spec result: %w", err)
	}
	return &result, nil
}

// AnalyzeSpecStructured sends a structured per-requirement spec comparison request.
func (c *Client) AnalyzeSpecStructured(ctx context.Context, specYAML string, diffSummary string) (*llm.StructuredSpecResult, error) {
	if strings.TrimSpace(specYAML) == "" {
		return nil, fmt.Errorf("spec is empty")
	}
	if strings.TrimSpace(diffSummary) == "" {
		return nil, fmt.Errorf("diff is empty")
	}

	var resp analyzeResponse
	if err := c.doJSON(ctx, http.MethodPost, "/api/v1/analyze/spec/structured", analyzeSpecRequest{
		Diff: diffSummary,
		Spec: specYAML,
	}, &resp); err != nil {
		return nil, err
	}

	var result llm.StructuredSpecResult
	if err := json.Unmarshal(resp.Result, &result); err != nil {
		return nil, fmt.Errorf("decoding structured spec result: %w", err)
	}
	return &result, nil
}

// RunUpload contains the data sent to the Fault Cloud when uploading an audit run.
type RunUpload struct {
	RepoURL         string                `json:"repo_url"`
	Branch          string                `json:"branch"`
	CommitSHA       string                `json:"commit_sha"`
	CommitRange     string                `json:"commit_range"`
	Duration        time.Duration         `json:"duration_ms"`
	FilesChanged    int                   `json:"files_changed"`
	Issues          []analyzer.Issue      `json:"issues"`
	ConfidenceScore *analyzer.Confidence  `json:"confidence_score,omitempty"`
	Summary         string                `json:"summary"`
}

// UploadRun sends audit results to the Fault Cloud API.
func (c *Client) UploadRun(ctx context.Context, run *RunUpload) error {
	return c.doJSON(ctx, http.MethodPost, "/api/v1/runs", run, nil)
}

func (c *Client) doJSON(ctx context.Context, method string, path string, body any, out any) error {
	if c.apiKey == "" {
		return fmt.Errorf("FAULT_API_KEY is not set")
	}

	var buf bytes.Buffer
	if err := json.NewEncoder(&buf).Encode(body); err != nil {
		return fmt.Errorf("encoding request: %w", err)
	}

	req, err := http.NewRequestWithContext(ctx, method, c.baseURL+path, &buf)
	if err != nil {
		return fmt.Errorf("creating request: %w", err)
	}
	req.Header.Set("Content-Type", "application/json")
	req.Header.Set("Authorization", "Bearer "+c.apiKey)

	httpResp, err := c.httpClient.Do(req)
	if err != nil {
		return fmt.Errorf("request failed: %w", err)
	}
	defer httpResp.Body.Close()

	respBody, err := io.ReadAll(httpResp.Body)
	if err != nil {
		return fmt.Errorf("reading response: %w", err)
	}

	if httpResp.StatusCode < 200 || httpResp.StatusCode >= 300 {
		var er errorResponse
		if json.Unmarshal(respBody, &er) == nil && er.Error != "" {
			return fmt.Errorf("fault cloud error (HTTP %d): %s", httpResp.StatusCode, er.Error)
		}
		return fmt.Errorf("fault cloud error (HTTP %d): %s", httpResp.StatusCode, strings.TrimSpace(string(respBody)))
	}

	if out == nil {
		return nil
	}
	if err := json.Unmarshal(respBody, out); err != nil {
		return fmt.Errorf("decoding response: %w", err)
	}
	return nil
}

