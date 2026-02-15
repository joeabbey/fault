package cloudapi

import (
	"context"
	"encoding/json"
	"net/http"
	"net/http/httptest"
	"strings"
	"testing"

	"github.com/joeabbey/fault/pkg/llm"
)

func TestClientAnalyzeConfidence(t *testing.T) {
	ts := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		if r.URL.Path != "/api/v1/analyze/confidence" {
			http.NotFound(w, r)
			return
		}
		if r.Header.Get("Authorization") != "Bearer fk_test" {
			w.WriteHeader(http.StatusUnauthorized)
			w.Write([]byte(`{"error":"invalid API key"}`))
			return
		}
		var req analyzeConfidenceRequest
		if err := json.NewDecoder(r.Body).Decode(&req); err != nil {
			w.WriteHeader(http.StatusBadRequest)
			w.Write([]byte(`{"error":"bad request"}`))
			return
		}
		if strings.TrimSpace(req.Diff) == "" {
			w.WriteHeader(http.StatusBadRequest)
			w.Write([]byte(`{"error":"diff is required"}`))
			return
		}

		resp := analyzeResponse{
			Result:      json.RawMessage(`{"overall":0.9,"per_file":{"a.go":0.9},"reasoning":"looks fine"}`),
			TokensInput: 10,
			TokensOut:   20,
		}
		w.Header().Set("Content-Type", "application/json")
		json.NewEncoder(w).Encode(resp)
	}))
	defer ts.Close()

	c := New(ts.URL, "fk_test")
	got, err := c.AnalyzeConfidence(context.Background(), "File: a.go (modified)\nChanges: +1/-0 lines\n")
	if err != nil {
		t.Fatalf("AnalyzeConfidence error: %v", err)
	}
	if got.Overall != 0.9 {
		t.Fatalf("overall = %v, want %v", got.Overall, 0.9)
	}
	if got.PerFile == nil || got.PerFile["a.go"] != 0.9 {
		t.Fatalf("per_file[a.go] = %v, want %v", got.PerFile["a.go"], 0.9)
	}
	if got.Reasoning == "" {
		t.Fatalf("expected non-empty reasoning")
	}
}

func TestClientAnalyzeSpec(t *testing.T) {
	ts := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		if r.URL.Path != "/api/v1/analyze/spec" {
			http.NotFound(w, r)
			return
		}
		if r.Header.Get("Authorization") != "Bearer fk_test" {
			w.WriteHeader(http.StatusUnauthorized)
			w.Write([]byte(`{"error":"invalid API key"}`))
			return
		}
		var req analyzeSpecRequest
		if err := json.NewDecoder(r.Body).Decode(&req); err != nil {
			w.WriteHeader(http.StatusBadRequest)
			w.Write([]byte(`{"error":"bad request"}`))
			return
		}
		if strings.TrimSpace(req.Spec) == "" || strings.TrimSpace(req.Diff) == "" {
			w.WriteHeader(http.StatusBadRequest)
			w.Write([]byte(`{"error":"diff and spec are required"}`))
			return
		}

		resp := analyzeResponse{
			Result:      json.RawMessage(`{"implemented":["A"],"missing":["B"],"unexpected":[],"score":0.5}`),
			TokensInput: 10,
			TokensOut:   20,
		}
		w.Header().Set("Content-Type", "application/json")
		json.NewEncoder(w).Encode(resp)
	}))
	defer ts.Close()

	c := New(ts.URL, "fk_test")
	got, err := c.AnalyzeSpec(context.Background(), "A and B", "File: a.go (modified)\nChanges: +1/-0 lines\n")
	if err != nil {
		t.Fatalf("AnalyzeSpec error: %v", err)
	}
	want := llm.SpecResult{
		Implemented: []string{"A"},
		Missing:     []string{"B"},
		Unexpected:  []string{},
		Score:       0.5,
	}
	if got.Score != want.Score {
		t.Fatalf("score = %v, want %v", got.Score, want.Score)
	}
	if len(got.Implemented) != 1 || got.Implemented[0] != "A" {
		t.Fatalf("implemented = %v, want %v", got.Implemented, want.Implemented)
	}
	if len(got.Missing) != 1 || got.Missing[0] != "B" {
		t.Fatalf("missing = %v, want %v", got.Missing, want.Missing)
	}
	if got.Unexpected == nil || len(got.Unexpected) != 0 {
		t.Fatalf("unexpected = %v, want empty", got.Unexpected)
	}
}

func TestClientNon200Error(t *testing.T) {
	ts := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.WriteHeader(http.StatusUnauthorized)
		w.Write([]byte(`{"error":"nope"}`))
	}))
	defer ts.Close()

	c := New(ts.URL, "fk_test")
	_, err := c.AnalyzeConfidence(context.Background(), "x")
	if err == nil {
		t.Fatalf("expected error")
	}
	if !strings.Contains(err.Error(), "nope") {
		t.Fatalf("expected error to mention server message, got %v", err)
	}
}

