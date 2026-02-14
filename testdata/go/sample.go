package sample

import (
	"fmt"
	"net/http"

	myalias "github.com/example/pkg"
)

// MaxRetries is the maximum number of retries.
const MaxRetries = 3

var defaultTimeout = 30

// Server represents an HTTP server.
type Server struct {
	Addr    string
	Handler http.Handler
}

// Handler is the interface for request handlers.
type Handler interface {
	Handle(w http.ResponseWriter, r *http.Request)
}

// NewServer creates a new Server.
func NewServer(addr string) *Server {
	return &Server{Addr: addr}
}

// Start starts the server.
func (s *Server) Start() error {
	fmt.Println("starting server on", s.Addr)
	return http.ListenAndServe(s.Addr, s.Handler)
}

func helperFunc(x, y int) (int, error) {
	_ = myalias.Something
	return x + y, nil
}
