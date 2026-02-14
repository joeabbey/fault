package parser

// Parser extracts structural information from source files.
type Parser interface {
	// Language returns the language this parser handles.
	Language() string
	// Parse extracts structural info from a file's content.
	Parse(filename string, content []byte) (*ParsedFile, error)
}

// ParsedFile holds the structural analysis of a single source file.
type ParsedFile struct {
	Path     string   `json:"path"`
	Language string   `json:"language"`
	Imports  []Import `json:"imports"`
	Exports  []Export `json:"exports"`
	Symbols  []Symbol `json:"symbols"`
}

// Import represents a single import statement.
type Import struct {
	Path   string   `json:"path"`
	Names  []string `json:"names,omitempty"`
	IsType bool     `json:"is_type,omitempty"`
	Line   int      `json:"line"`
}

// Export represents an exported declaration.
type Export struct {
	Name string `json:"name"`
	Kind string `json:"kind"` // function, type, variable, class
	Line int    `json:"line"`
}

// Symbol represents a named declaration in the file.
type Symbol struct {
	Name      string `json:"name"`
	Kind      string `json:"kind"` // function, type, variable, method, class
	Exported  bool   `json:"exported"`
	Line      int    `json:"line"`
	EndLine   int    `json:"end_line,omitempty"`
	Signature string `json:"signature,omitempty"`
}
