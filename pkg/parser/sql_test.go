package parser

import (
	"os"
	"testing"
)

func TestSQLParserLanguage(t *testing.T) {
	p := NewSQLParser()
	if p.Language() != "sql" {
		t.Errorf("expected 'sql', got %q", p.Language())
	}
}

func TestSQLCreateTable(t *testing.T) {
	src := `CREATE TABLE users (
    id SERIAL PRIMARY KEY,
    email VARCHAR(255) NOT NULL
);

CREATE TABLE IF NOT EXISTS orders (
    id SERIAL PRIMARY KEY,
    user_id INTEGER
);
`
	p := NewSQLParser()
	pf, err := p.Parse("schema.sql", []byte(src))
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	tableNames := make(map[string]bool)
	for _, s := range pf.Symbols {
		if s.Kind == "table" {
			tableNames[s.Name] = true
		}
	}

	if !tableNames["users"] {
		t.Error("expected to find users table")
	}
	if !tableNames["orders"] {
		t.Error("expected to find orders table")
	}
}

func TestSQLCreateView(t *testing.T) {
	src := `CREATE VIEW active_users AS
SELECT * FROM users;

CREATE OR REPLACE VIEW order_summary AS
SELECT * FROM orders;
`
	p := NewSQLParser()
	pf, err := p.Parse("views.sql", []byte(src))
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	viewNames := make(map[string]bool)
	for _, s := range pf.Symbols {
		if s.Kind == "view" {
			viewNames[s.Name] = true
		}
	}

	if !viewNames["active_users"] {
		t.Error("expected to find active_users view")
	}
	if !viewNames["order_summary"] {
		t.Error("expected to find order_summary view")
	}
}

func TestSQLCreateFunction(t *testing.T) {
	src := `CREATE FUNCTION calculate_total(order_id INTEGER)
RETURNS DECIMAL AS $$
BEGIN
    RETURN 0;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION update_timestamp()
RETURNS TRIGGER AS $$
BEGIN
    RETURN NEW;
END;
$$ LANGUAGE plpgsql;
`
	p := NewSQLParser()
	pf, err := p.Parse("functions.sql", []byte(src))
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	funcNames := make(map[string]bool)
	for _, s := range pf.Symbols {
		if s.Kind == "function" {
			funcNames[s.Name] = true
		}
	}

	if !funcNames["calculate_total"] {
		t.Error("expected to find calculate_total function")
	}
	if !funcNames["update_timestamp"] {
		t.Error("expected to find update_timestamp function")
	}
}

func TestSQLCreateProcedure(t *testing.T) {
	src := `CREATE PROCEDURE archive_old_orders(cutoff_date DATE)
LANGUAGE plpgsql AS $$
BEGIN
    DELETE FROM orders WHERE created_at < cutoff_date;
END;
$$;
`
	p := NewSQLParser()
	pf, err := p.Parse("procs.sql", []byte(src))
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	var found bool
	for _, s := range pf.Symbols {
		if s.Name == "archive_old_orders" && s.Kind == "procedure" {
			found = true
		}
	}
	if !found {
		t.Error("expected to find archive_old_orders procedure")
	}
}

func TestSQLCreateTrigger(t *testing.T) {
	src := `CREATE TRIGGER users_updated_at
BEFORE UPDATE ON users
FOR EACH ROW
EXECUTE FUNCTION update_timestamp();
`
	p := NewSQLParser()
	pf, err := p.Parse("triggers.sql", []byte(src))
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	var found bool
	for _, s := range pf.Symbols {
		if s.Name == "users_updated_at" && s.Kind == "trigger" {
			found = true
		}
	}
	if !found {
		t.Error("expected to find users_updated_at trigger")
	}
}

func TestSQLCreateIndex(t *testing.T) {
	src := `CREATE INDEX idx_users_email ON users(email);
CREATE UNIQUE INDEX idx_orders_user_status ON orders(user_id, status);
`
	p := NewSQLParser()
	pf, err := p.Parse("indexes.sql", []byte(src))
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	indexNames := make(map[string]bool)
	for _, s := range pf.Symbols {
		if s.Kind == "index" {
			indexNames[s.Name] = true
		}
	}

	if !indexNames["idx_users_email"] {
		t.Error("expected to find idx_users_email index")
	}
	if !indexNames["idx_orders_user_status"] {
		t.Error("expected to find idx_orders_user_status index")
	}
}

func TestSQLNoImports(t *testing.T) {
	src := `CREATE TABLE test (id INTEGER);
SELECT * FROM test;
`
	p := NewSQLParser()
	pf, err := p.Parse("test.sql", []byte(src))
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	if len(pf.Imports) != 0 {
		t.Errorf("SQL should have no imports, got %d", len(pf.Imports))
	}
}

func TestSQLEmptyFile(t *testing.T) {
	p := NewSQLParser()
	pf, err := p.Parse("empty.sql", []byte(""))
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	if len(pf.Imports) != 0 {
		t.Errorf("expected 0 imports, got %d", len(pf.Imports))
	}
	if len(pf.Exports) != 0 {
		t.Errorf("expected 0 exports, got %d", len(pf.Exports))
	}
	if len(pf.Symbols) != 0 {
		t.Errorf("expected 0 symbols, got %d", len(pf.Symbols))
	}
}

func TestSQLCommentSkipped(t *testing.T) {
	src := `-- CREATE TABLE not_real (id INTEGER);
/* CREATE VIEW also_not_real AS SELECT 1; */
`
	p := NewSQLParser()
	pf, err := p.Parse("comments.sql", []byte(src))
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	if len(pf.Symbols) != 0 {
		t.Errorf("expected 0 symbols from comments, got %d", len(pf.Symbols))
	}
}

func TestSQLDMLIgnored(t *testing.T) {
	src := `INSERT INTO users (email) VALUES ('test@test.com');
SELECT * FROM users WHERE id = 1;
UPDATE users SET name = 'Bob' WHERE id = 2;
DELETE FROM users WHERE id = 3;
`
	p := NewSQLParser()
	pf, err := p.Parse("queries.sql", []byte(src))
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	if len(pf.Symbols) != 0 {
		t.Errorf("expected 0 symbols from DML, got %d", len(pf.Symbols))
	}
}

func TestSQLSampleFile(t *testing.T) {
	content, err := os.ReadFile("../../testdata/sql/sample.sql")
	if err != nil {
		t.Fatalf("failed to read sample file: %v", err)
	}

	p := NewSQLParser()
	pf, err := p.Parse("sample.sql", content)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	// No imports for SQL.
	if len(pf.Imports) != 0 {
		t.Errorf("expected 0 imports, got %d", len(pf.Imports))
	}

	// Count by kind.
	kindCounts := make(map[string]int)
	for _, s := range pf.Symbols {
		kindCounts[s.Kind]++
	}

	// 3 tables: users, orders, order_items
	if kindCounts["table"] != 3 {
		t.Errorf("expected 3 tables, got %d", kindCounts["table"])
	}

	// 2 views: active_users, order_summary
	if kindCounts["view"] != 2 {
		t.Errorf("expected 2 views, got %d", kindCounts["view"])
	}

	// 2 functions: calculate_total, update_timestamp
	if kindCounts["function"] != 2 {
		t.Errorf("expected 2 functions, got %d", kindCounts["function"])
	}

	// 1 procedure: archive_old_orders
	if kindCounts["procedure"] != 1 {
		t.Errorf("expected 1 procedure, got %d", kindCounts["procedure"])
	}

	// 2 triggers: users_updated_at, orders_audit
	if kindCounts["trigger"] != 2 {
		t.Errorf("expected 2 triggers, got %d", kindCounts["trigger"])
	}

	// 2 indexes: idx_users_email, idx_orders_user_status
	if kindCounts["index"] != 2 {
		t.Errorf("expected 2 indexes, got %d", kindCounts["index"])
	}

	// All symbols should be exported.
	for _, s := range pf.Symbols {
		if !s.Exported {
			t.Errorf("symbol %q should be exported", s.Name)
		}
	}

	// Exports should match symbols count.
	if len(pf.Exports) != len(pf.Symbols) {
		t.Errorf("expected exports (%d) to match symbols (%d)", len(pf.Exports), len(pf.Symbols))
	}
}
