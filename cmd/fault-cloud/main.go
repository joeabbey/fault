package main

import (
	"context"
	"fmt"
	"log"
	"os"

	"github.com/joeabbey/fault/pkg/cloud"
)

// Version is set at build time via -ldflags.
var Version = "0.1.0-dev"

func main() {
	// Set the package-level version so /api/health reflects it
	cloud.Version = Version

	cfg := cloud.ConfigFromEnv()

	if cfg.DatabaseURL == "" {
		fmt.Fprintln(os.Stderr, "error: DATABASE_URL environment variable is required")
		os.Exit(1)
	}

	if os.Getenv("ANTHROPIC_API_KEY") == "" {
		fmt.Fprintln(os.Stderr, "warning: ANTHROPIC_API_KEY not set, LLM endpoints will fail")
	}

	ctx := context.Background()
	store, err := cloud.NewPostgresStore(ctx, cfg.DatabaseURL)
	if err != nil {
		log.Fatalf("connecting to database: %v", err)
	}
	defer store.Close()

	srv := cloud.NewServer(cfg, store)
	if err := srv.Start(); err != nil {
		log.Fatalf("server error: %v", err)
	}
}
