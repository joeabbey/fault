package parser

import (
	"os"
	"testing"
)

func TestBashParserLanguage(t *testing.T) {
	p := NewBashParser()
	if p.Language() != "bash" {
		t.Errorf("expected 'bash', got %q", p.Language())
	}
}

func TestBashSource(t *testing.T) {
	src := `source ./lib/utils.sh
source /etc/profile.d/env.sh
`
	p := NewBashParser()
	pf, err := p.Parse("script.sh", []byte(src))
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	if len(pf.Imports) != 2 {
		t.Fatalf("expected 2 imports, got %d", len(pf.Imports))
	}

	expected := []string{"./lib/utils.sh", "/etc/profile.d/env.sh"}
	for i, exp := range expected {
		if pf.Imports[i].Path != exp {
			t.Errorf("import[%d]: expected %q, got %q", i, exp, pf.Imports[i].Path)
		}
	}
}

func TestBashDotSource(t *testing.T) {
	src := `. ./helpers.sh
. /opt/scripts/common.sh
`
	p := NewBashParser()
	pf, err := p.Parse("script.sh", []byte(src))
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	if len(pf.Imports) != 2 {
		t.Fatalf("expected 2 imports, got %d", len(pf.Imports))
	}

	expected := []string{"./helpers.sh", "/opt/scripts/common.sh"}
	for i, exp := range expected {
		if pf.Imports[i].Path != exp {
			t.Errorf("import[%d]: expected %q, got %q", i, exp, pf.Imports[i].Path)
		}
	}
}

func TestBashExportVar(t *testing.T) {
	src := `export DATABASE_URL="postgres://localhost:5432/mydb"
export API_KEY="secret"
export MAX_RETRIES=3
`
	p := NewBashParser()
	pf, err := p.Parse("env.sh", []byte(src))
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	if len(pf.Exports) != 3 {
		t.Fatalf("expected 3 exports, got %d", len(pf.Exports))
	}

	expectedNames := []string{"DATABASE_URL", "API_KEY", "MAX_RETRIES"}
	for i, exp := range expectedNames {
		if pf.Exports[i].Name != exp {
			t.Errorf("export[%d]: expected %q, got %q", i, exp, pf.Exports[i].Name)
		}
		if pf.Exports[i].Kind != "variable" {
			t.Errorf("export[%d]: expected kind 'variable', got %q", i, pf.Exports[i].Kind)
		}
	}
}

func TestBashFunctionParens(t *testing.T) {
	src := `setup_database() {
    echo "Setting up..."
}

cleanup_temp_files() {
    rm -rf /tmp/*
}
`
	p := NewBashParser()
	pf, err := p.Parse("funcs.sh", []byte(src))
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	funcNames := make(map[string]bool)
	for _, s := range pf.Symbols {
		if s.Kind == "function" {
			funcNames[s.Name] = true
		}
	}

	if !funcNames["setup_database"] {
		t.Error("expected to find setup_database function")
	}
	if !funcNames["cleanup_temp_files"] {
		t.Error("expected to find cleanup_temp_files function")
	}
}

func TestBashFunctionKeyword(t *testing.T) {
	src := `function run_migrations {
    echo "Running..."
}

function deploy_application {
    echo "Deploying..."
}
`
	p := NewBashParser()
	pf, err := p.Parse("funcs.sh", []byte(src))
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	funcNames := make(map[string]bool)
	for _, s := range pf.Symbols {
		if s.Kind == "function" {
			funcNames[s.Name] = true
		}
	}

	if !funcNames["run_migrations"] {
		t.Error("expected to find run_migrations function")
	}
	if !funcNames["deploy_application"] {
		t.Error("expected to find deploy_application function")
	}
}

func TestBashDeclare(t *testing.T) {
	src := `declare -r CONFIG_FILE="/etc/app/config.yml"
declare -i RETRY_COUNT=0
`
	p := NewBashParser()
	pf, err := p.Parse("vars.sh", []byte(src))
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	varNames := make(map[string]bool)
	for _, s := range pf.Symbols {
		if s.Kind == "variable" && !s.Exported {
			varNames[s.Name] = true
		}
	}

	if !varNames["CONFIG_FILE"] {
		t.Error("expected to find CONFIG_FILE declare variable")
	}
	if !varNames["RETRY_COUNT"] {
		t.Error("expected to find RETRY_COUNT declare variable")
	}
}

func TestBashEmptyFile(t *testing.T) {
	p := NewBashParser()
	pf, err := p.Parse("empty.sh", []byte(""))
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

func TestBashCommentSkipped(t *testing.T) {
	src := `# source ./not_real.sh
# function not_a_func {
# export NOT_REAL=1
`
	p := NewBashParser()
	pf, err := p.Parse("comments.sh", []byte(src))
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	if len(pf.Imports) != 0 {
		t.Errorf("expected 0 imports from comments, got %d", len(pf.Imports))
	}
	if len(pf.Symbols) != 0 {
		t.Errorf("expected 0 symbols from comments, got %d", len(pf.Symbols))
	}
}

func TestBashSampleFile(t *testing.T) {
	content, err := os.ReadFile("../../testdata/bash/sample.sh")
	if err != nil {
		t.Fatalf("failed to read sample file: %v", err)
	}

	p := NewBashParser()
	pf, err := p.Parse("sample.sh", content)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	// Should find 4 imports: source ./lib/utils.sh, source /etc/profile.d/env.sh,
	// . ./helpers.sh, . /opt/scripts/common.sh
	if len(pf.Imports) != 4 {
		t.Errorf("expected 4 imports, got %d", len(pf.Imports))
		for i, imp := range pf.Imports {
			t.Logf("  import[%d]: %q (line %d)", i, imp.Path, imp.Line)
		}
	}

	// Count exported variables: DATABASE_URL, API_KEY, MAX_RETRIES
	exportedVars := 0
	for _, e := range pf.Exports {
		if e.Kind == "variable" {
			exportedVars++
		}
	}
	if exportedVars != 3 {
		t.Errorf("expected 3 exported variables, got %d", exportedVars)
	}

	// Count functions.
	funcNames := make(map[string]bool)
	for _, s := range pf.Symbols {
		if s.Kind == "function" {
			funcNames[s.Name] = true
		}
	}

	expectedFuncs := []string{"setup_database", "cleanup_temp_files", "run_migrations",
		"deploy_application", "process_batch", "main", "inner_helper"}
	for _, name := range expectedFuncs {
		if !funcNames[name] {
			t.Errorf("expected to find function %q", name)
		}
	}
}
