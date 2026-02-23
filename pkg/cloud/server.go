package cloud

import (
	"context"
	_ "embed"
	"fmt"
	"log/slog"
	"net/http"
	"os"
	"os/signal"
	"path/filepath"
	"strings"
	"syscall"
	"time"

	"github.com/joeabbey/magma/pkg/limits"
	magmastripe "github.com/joeabbey/magma/pkg/stripe"
)

//go:embed install.sh
var installScript []byte

//go:embed landing.html
var landingPage []byte

// Config holds configuration for the cloud API server.
type Config struct {
	Port        string
	DatabaseURL string
	LogLevel    string

	// Google OAuth
	GoogleClientID     string
	GoogleClientSecret string
	JWTSecret          string
	CookieDomain       string
	CookieSecure       bool
	AllowedEmails      []string

	// Stripe billing (optional — billing disabled if StripeSecretKey is empty)
	StripeSecretKey     string
	StripeWebhookSecret string
	StripePriceIDPro    string
	StripePriceIDTeam   string
	AppURL              string

	// Web dashboard directory (optional — SPA serving disabled if empty)
	WebDir string
}

// ConfigFromEnv loads server configuration from environment variables.
func ConfigFromEnv() Config {
	port := os.Getenv("PORT")
	if port == "" {
		port = "8082"
	}

	logLevel := os.Getenv("LOG_LEVEL")
	if logLevel == "" {
		logLevel = "info"
	}

	appURL := os.Getenv("APP_URL")
	if appURL == "" {
		appURL = "https://fault.jabbey.io"
	}

	cookieSecure := os.Getenv("COOKIE_SECURE") != "false" // default true

	var allowedEmails []string
	if emails := os.Getenv("ALLOWED_EMAILS"); emails != "" {
		for _, e := range strings.Split(emails, ",") {
			e = strings.TrimSpace(e)
			if e != "" {
				allowedEmails = append(allowedEmails, e)
			}
		}
	}

	return Config{
		Port:                port,
		DatabaseURL:         os.Getenv("DATABASE_URL"),
		LogLevel:            logLevel,
		GoogleClientID:      os.Getenv("GOOGLE_CLIENT_ID"),
		GoogleClientSecret:  os.Getenv("GOOGLE_CLIENT_SECRET"),
		JWTSecret:           os.Getenv("JWT_SECRET"),
		CookieDomain:        os.Getenv("COOKIE_DOMAIN"),
		CookieSecure:        cookieSecure,
		AllowedEmails:       allowedEmails,
		StripeSecretKey:     os.Getenv("STRIPE_SECRET_KEY"),
		StripeWebhookSecret: os.Getenv("STRIPE_WEBHOOK_SECRET"),
		StripePriceIDPro:    os.Getenv("STRIPE_PRICE_ID_PRO"),
		StripePriceIDTeam:   os.Getenv("STRIPE_PRICE_ID_TEAM"),
		AppURL:              appURL,
		WebDir:              os.Getenv("WEB_DIR"),
	}
}

// Server is the cloud API HTTP server.
type Server struct {
	cfg             Config
	store           Store
	logger          *slog.Logger
	handlers        *Handlers
	authHandlers    *AuthHandlers
	limitsEngine    *limits.Engine
	billingHandlers *BillingHandlers
	mux             *http.ServeMux
}

// NewServer creates a new Server with the given config and store.
func NewServer(cfg Config, store Store) *Server {
	level := slog.LevelInfo
	switch cfg.LogLevel {
	case "debug":
		level = slog.LevelDebug
	case "warn":
		level = slog.LevelWarn
	case "error":
		level = slog.LevelError
	}

	logger := slog.New(slog.NewJSONHandler(os.Stdout, &slog.HandlerOptions{
		Level: level,
	}))

	// Always create the limits engine (works without Stripe)
	storageAdapter := NewLimitsStorageAdapter(store)
	planProvider := NewFaultPlanProvider(store)
	limitsEngine := limits.NewEngine(storageAdapter, planProvider)

	s := &Server{
		cfg:          cfg,
		store:        store,
		logger:       logger,
		handlers:     NewHandlers(store, limitsEngine, logger),
		limitsEngine: limitsEngine,
		mux:          http.NewServeMux(),
	}

	// Set up Google OAuth auth handlers if configured
	if cfg.GoogleClientID != "" && cfg.JWTSecret != "" {
		s.authHandlers = NewAuthHandlers(
			store, logger,
			cfg.GoogleClientID, cfg.GoogleClientSecret, cfg.JWTSecret,
			cfg.AppURL, cfg.CookieDomain, cfg.CookieSecure, cfg.AllowedEmails,
		)
		logger.Info("google OAuth enabled")
	} else {
		logger.Info("google OAuth disabled (GOOGLE_CLIENT_ID or JWT_SECRET not set)")
	}

	// Set up Stripe billing if configured
	if cfg.StripeSecretKey != "" {
		prices := map[string]string{
			"pro":  cfg.StripePriceIDPro,
			"team": cfg.StripePriceIDTeam,
		}

		stripeClient := magmastripe.NewClient(magmastripe.Config{
			SecretKey:     cfg.StripeSecretKey,
			WebhookSecret: cfg.StripeWebhookSecret,
			Prices:        prices,
		})

		callbacks := NewFaultWebhookCallbacks(store, prices, logger)
		webhookHandler := magmastripe.NewWebhookHandler(stripeClient, callbacks)

		s.billingHandlers = NewBillingHandlers(
			store, stripeClient, limitsEngine, webhookHandler,
			prices, cfg.AppURL, logger,
		)

		logger.Info("stripe billing enabled")
	} else {
		logger.Info("stripe billing disabled (STRIPE_SECRET_KEY not set)")
	}

	s.registerRoutes()
	return s
}

// registerRoutes sets up the HTTP route handlers.
func (s *Server) registerRoutes() {
	// API routes
	s.mux.HandleFunc("GET /api/health", s.handlers.HandleHealth)
	s.mux.HandleFunc("POST /api/v1/signup", s.handlers.HandleSignup)
	s.mux.HandleFunc("POST /api/v1/analyze/confidence", s.handlers.HandleAnalyzeConfidence)
	s.mux.HandleFunc("POST /api/v1/analyze/spec", s.handlers.HandleAnalyzeSpec)
	s.mux.HandleFunc("POST /api/v1/analyze/spec/structured", s.handlers.HandleAnalyzeSpecStructured)
	s.mux.HandleFunc("GET /api/v1/usage", s.handlers.HandleUsage)
	s.mux.HandleFunc("POST /api/v1/api-keys/rotate", s.handlers.HandleRotateKey)

	// Runs routes
	s.mux.HandleFunc("POST /api/v1/runs", s.handlers.HandleCreateRun)
	s.mux.HandleFunc("GET /api/v1/runs", s.handlers.HandleListRuns)
	s.mux.HandleFunc("GET /api/v1/runs/stats", s.handlers.HandleGetRunStats)
	s.mux.HandleFunc("GET /api/v1/runs/{id}", s.handlers.HandleGetRun)

	// Spec routes
	s.mux.HandleFunc("GET /api/v1/spec/results", s.handlers.HandleListSpecResults)

	// Organization routes
	s.mux.HandleFunc("POST /api/v1/orgs", s.handlers.HandleCreateOrg)
	s.mux.HandleFunc("GET /api/v1/orgs", s.handlers.HandleListOrgs)
	s.mux.HandleFunc("GET /api/v1/orgs/{slug}", s.handlers.HandleGetOrg)
	s.mux.HandleFunc("POST /api/v1/orgs/{slug}/members", s.handlers.HandleAddOrgMember)
	s.mux.HandleFunc("DELETE /api/v1/orgs/{slug}/members/{userId}", s.handlers.HandleRemoveOrgMember)
	s.mux.HandleFunc("GET /api/v1/orgs/{slug}/members", s.handlers.HandleListOrgMembers)
	s.mux.HandleFunc("GET /api/v1/orgs/{slug}/runs", s.handlers.HandleListOrgRuns)
	s.mux.HandleFunc("GET /api/v1/orgs/{slug}/runs/stats", s.handlers.HandleGetOrgRunStats)

	// Auth routes (only if Google OAuth is configured)
	if s.authHandlers != nil {
		s.mux.HandleFunc("GET /api/auth/google/login", s.authHandlers.HandleGoogleLogin)
		s.mux.HandleFunc("GET /api/auth/google/callback", s.authHandlers.HandleGoogleCallback)
		s.mux.HandleFunc("GET /api/auth/me", s.authHandlers.HandleAuthMe)
		s.mux.HandleFunc("POST /api/auth/logout", s.authHandlers.HandleLogout)
	}

	// Billing routes (only if Stripe is configured)
	if s.billingHandlers != nil {
		s.mux.HandleFunc("POST /api/v1/billing/checkout", s.billingHandlers.HandleCheckout)
		s.mux.HandleFunc("POST /api/v1/billing/portal", s.billingHandlers.HandlePortal)
		s.mux.HandleFunc("GET /api/v1/billing/subscription", s.billingHandlers.HandleSubscription)
		s.mux.HandleFunc("POST /api/v1/billing/webhook", s.billingHandlers.HandleWebhook)
	}

	// Public: landing page at exact root path
	s.mux.HandleFunc("GET /{$}", handleLandingPage)

	// Public: serve install script for `curl -sSf https://fault.jabbey.io/install.sh | sh`
	s.mux.HandleFunc("GET /install.sh", handleInstallScript)

	// SPA: serve dashboard static files (catch-all for non-API paths)
	if s.cfg.WebDir != "" {
		s.mux.HandleFunc("GET /", s.spaHandler())
		s.logger.Info("SPA serving enabled", "web_dir", s.cfg.WebDir)
	}
}

func handleLandingPage(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Content-Type", "text/html; charset=utf-8")
	w.Header().Set("Cache-Control", "public, max-age=3600")
	w.Write(landingPage)
}

func handleInstallScript(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Content-Type", "text/plain; charset=utf-8")
	w.Write(installScript)
}

// spaHandler serves SvelteKit static files with SPA fallback.
// Static assets are served directly; all other paths serve index.html
// for client-side routing.
func (s *Server) spaHandler() http.HandlerFunc {
	webDir := s.cfg.WebDir

	return func(w http.ResponseWriter, r *http.Request) {
		urlPath := filepath.Clean(r.URL.Path)
		if urlPath == "." {
			urlPath = "/"
		}

		// Try to serve a static file
		filePath := filepath.Join(webDir, urlPath)
		if info, err := os.Stat(filePath); err == nil && !info.IsDir() {
			http.ServeFile(w, r, filePath)
			return
		}

		// SPA fallback: serve index.html for client-side routing
		http.ServeFile(w, r, filepath.Join(webDir, "index.html"))
	}
}

// Handler returns the fully wrapped HTTP handler with all middleware applied.
func (s *Server) Handler() http.Handler {
	var handler http.Handler = s.mux

	// Apply middleware in reverse order (outermost first)
	handler = UsageLimitsMiddleware(s.limitsEngine, s.logger)(handler)
	handler = AuthMiddleware(s.store, s.cfg.JWTSecret, s.logger)(handler)
	handler = CORSMiddleware(handler)
	handler = RequestLogger(s.logger)(handler)

	return handler
}

// Start runs the HTTP server and blocks until a shutdown signal is received.
func (s *Server) Start() error {
	addr := fmt.Sprintf(":%s", s.cfg.Port)

	httpServer := &http.Server{
		Addr:         addr,
		Handler:      s.Handler(),
		ReadTimeout:  30 * time.Second,
		WriteTimeout: 60 * time.Second,
		IdleTimeout:  120 * time.Second,
	}

	// Graceful shutdown on SIGINT/SIGTERM
	done := make(chan os.Signal, 1)
	signal.Notify(done, os.Interrupt, syscall.SIGTERM)

	go func() {
		s.logger.Info("server starting", "addr", addr)
		if err := httpServer.ListenAndServe(); err != nil && err != http.ErrServerClosed {
			s.logger.Error("server error", "error", err)
			os.Exit(1)
		}
	}()

	<-done
	s.logger.Info("shutting down server")

	ctx, cancel := context.WithTimeout(context.Background(), 10*time.Second)
	defer cancel()

	if err := httpServer.Shutdown(ctx); err != nil {
		return fmt.Errorf("server shutdown: %w", err)
	}

	s.logger.Info("server stopped")
	return nil
}
