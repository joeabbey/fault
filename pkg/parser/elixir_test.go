package parser

import (
	"os"
	"testing"
)

func TestElixirParserLanguage(t *testing.T) {
	p := NewElixirParser()
	if p.Language() != "elixir" {
		t.Errorf("expected elixir, got %q", p.Language())
	}
}

func TestElixirParserFixture(t *testing.T) {
	content, err := os.ReadFile("../../testdata/elixir/sample.ex")
	if err != nil {
		t.Fatalf("reading fixture: %v", err)
	}

	p := NewElixirParser()
	pf, err := p.Parse("sample.ex", content)
	if err != nil {
		t.Fatalf("parsing: %v", err)
	}

	if pf.Language != "elixir" {
		t.Errorf("expected language elixir, got %q", pf.Language)
	}

	// Check imports
	importPaths := make(map[string]bool)
	for _, imp := range pf.Imports {
		importPaths[imp.Path] = true
	}

	expectedImports := []string{
		"GenServer",
		"MyApp.Telemetry",
		"MyApp.Accounts.User",
		"MyApp.Repo",
		"MyApp.Cache",
		"Ecto.Query",
		"Ecto.Changeset",
		"Logger",
	}
	for _, expected := range expectedImports {
		if !importPaths[expected] {
			t.Errorf("expected import %q not found (have: %v)", expected, importPaths)
		}
	}

	// Check alias with as: option
	for _, imp := range pf.Imports {
		if imp.Path == "MyApp.Cache" {
			found := false
			for _, n := range imp.Names {
				if n == "AppCache" {
					found = true
				}
			}
			if !found {
				t.Errorf("expected alias 'AppCache' for MyApp.Cache, got %v", imp.Names)
			}
		}
	}

	// Check exports
	exportNames := make(map[string]string)
	for _, exp := range pf.Exports {
		exportNames[exp.Name] = exp.Kind
	}

	// Modules should be exported
	if kind, ok := exportNames["MyApp.Accounts.UserServer"]; !ok {
		t.Error("expected export MyApp.Accounts.UserServer")
	} else if kind != "module" {
		t.Errorf("expected kind module, got %q", kind)
	}

	if kind, ok := exportNames["MyApp.Accounts.UserServer.Helpers"]; !ok {
		t.Error("expected export MyApp.Accounts.UserServer.Helpers")
	} else if kind != "module" {
		t.Errorf("expected kind module, got %q", kind)
	}

	// Public functions should be exported
	expectedFuncExports := []string{
		"start_link", "get_user", "update_user",
		"list_active_sessions", "broadcast_message",
		"init", "handle_call", "handle_cast",
		"format_session",
	}
	for _, name := range expectedFuncExports {
		if _, ok := exportNames[name]; !ok {
			t.Errorf("expected function export %q not found", name)
		}
	}

	// Macro should be exported
	if kind, ok := exportNames["define_handler"]; !ok {
		t.Error("expected export define_handler")
	} else if kind != "macro" {
		t.Errorf("expected kind macro for define_handler, got %q", kind)
	}

	// Check symbols
	symbolMap := make(map[string]Symbol)
	for _, sym := range pf.Symbols {
		symbolMap[sym.Name] = sym
	}

	// Private functions should not be exported
	privateFuncs := []string{"fetch_from_cache", "validate_attrs", "build_query"}
	for _, name := range privateFuncs {
		if s, ok := symbolMap[name]; ok {
			if s.Exported {
				t.Errorf("expected %q to not be exported", name)
			}
			if s.Kind != "function" {
				t.Errorf("expected %q to be function, got %q", name, s.Kind)
			}
		} else {
			t.Errorf("expected symbol %q not found", name)
		}
	}

	// defstruct should be detected
	if s, ok := symbolMap["__struct__"]; ok {
		if s.Kind != "type" {
			t.Errorf("expected __struct__ to be type, got %q", s.Kind)
		}
	} else {
		t.Error("expected __struct__ symbol for defstruct")
	}

	// @callback should be detected
	callbackNames := []string{"handle_event", "on_connect"}
	for _, name := range callbackNames {
		found := false
		for _, sym := range pf.Symbols {
			if sym.Name == name && sym.Kind == "callback" {
				found = true
				break
			}
		}
		if !found {
			t.Errorf("expected callback symbol %q not found", name)
		}
	}
}

func TestElixirParserEmptyFile(t *testing.T) {
	p := NewElixirParser()
	pf, err := p.Parse("empty.ex", []byte(""))
	if err != nil {
		t.Fatal(err)
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

func TestElixirParserUseImport(t *testing.T) {
	src := `defmodule MyApp.Worker do
  use GenServer
  use MyApp.Macros

  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts)
  end
end
`

	p := NewElixirParser()
	pf, err := p.Parse("test.ex", []byte(src))
	if err != nil {
		t.Fatal(err)
	}

	importPaths := make(map[string]bool)
	for _, imp := range pf.Imports {
		importPaths[imp.Path] = true
	}

	if !importPaths["GenServer"] {
		t.Error("expected use GenServer import")
	}
	if !importPaths["MyApp.Macros"] {
		t.Error("expected use MyApp.Macros import")
	}
}

func TestElixirParserPrivateFunctions(t *testing.T) {
	src := `defmodule MyApp.Example do
  def public_func(x), do: private_func(x)
  defp private_func(x), do: x * 2
end
`

	p := NewElixirParser()
	pf, err := p.Parse("test.ex", []byte(src))
	if err != nil {
		t.Fatal(err)
	}

	symbolMap := make(map[string]Symbol)
	for _, sym := range pf.Symbols {
		if sym.Kind == "function" || sym.Kind == "module" {
			symbolMap[sym.Name] = sym
		}
	}

	if s, ok := symbolMap["public_func"]; !ok {
		t.Error("expected public_func symbol")
	} else if !s.Exported {
		t.Error("expected public_func to be exported")
	}

	if s, ok := symbolMap["private_func"]; !ok {
		t.Error("expected private_func symbol")
	} else if s.Exported {
		t.Error("expected private_func to not be exported")
	}

	// Check exports list
	exportNames := make(map[string]bool)
	for _, exp := range pf.Exports {
		exportNames[exp.Name] = true
	}

	if !exportNames["public_func"] {
		t.Error("expected public_func in exports")
	}
	if exportNames["private_func"] {
		t.Error("unexpected private_func in exports")
	}
}
