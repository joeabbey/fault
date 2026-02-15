package parser

import (
	"testing"
)

func TestRustParserLanguage(t *testing.T) {
	p := NewRustParser()
	if p.Language() != "rust" {
		t.Errorf("expected rust, got %q", p.Language())
	}
}

func TestRustParserUseStatements(t *testing.T) {
	src := `use std::collections::HashMap;
use std::io;
use crate::module;
use super::*;
use std::io as stdio;
`

	p := NewRustParser()
	pf, err := p.Parse("main.rs", []byte(src))
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}

	if len(pf.Imports) != 5 {
		t.Fatalf("expected 5 imports, got %d", len(pf.Imports))
	}

	// use std::collections::HashMap -> name "HashMap"
	assertImportName(t, pf.Imports[0], "std::collections::HashMap", "HashMap")
	// use std::io -> name "io"
	assertImportName(t, pf.Imports[1], "std::io", "io")
	// use crate::module -> name "module"
	assertImportName(t, pf.Imports[2], "crate::module", "module")
	// use super::* -> name "*"
	assertImportName(t, pf.Imports[3], "super::*", "*")
	// use std::io as stdio -> name "stdio"
	assertImportName(t, pf.Imports[4], "std::io", "stdio")
}

func TestRustParserGroupedUse(t *testing.T) {
	src := `use std::{io, fs};
use std::collections::{HashMap, BTreeMap as BMap};
`

	p := NewRustParser()
	pf, err := p.Parse("main.rs", []byte(src))
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}

	if len(pf.Imports) != 2 {
		t.Fatalf("expected 2 imports, got %d", len(pf.Imports))
	}

	// use std::{io, fs}
	if pf.Imports[0].Path != "std" {
		t.Errorf("expected path 'std', got %q", pf.Imports[0].Path)
	}
	if len(pf.Imports[0].Names) != 2 {
		t.Fatalf("expected 2 names, got %d", len(pf.Imports[0].Names))
	}
	if pf.Imports[0].Names[0] != "io" || pf.Imports[0].Names[1] != "fs" {
		t.Errorf("expected [io, fs], got %v", pf.Imports[0].Names)
	}

	// use std::collections::{HashMap, BTreeMap as BMap}
	if pf.Imports[1].Path != "std::collections" {
		t.Errorf("expected path 'std::collections', got %q", pf.Imports[1].Path)
	}
	if len(pf.Imports[1].Names) != 2 {
		t.Fatalf("expected 2 names, got %d", len(pf.Imports[1].Names))
	}
	if pf.Imports[1].Names[0] != "HashMap" {
		t.Errorf("expected first name 'HashMap', got %q", pf.Imports[1].Names[0])
	}
	if pf.Imports[1].Names[1] != "BMap" {
		t.Errorf("expected second name 'BMap' (alias), got %q", pf.Imports[1].Names[1])
	}
}

func TestRustParserPubVsPrivate(t *testing.T) {
	src := `pub fn public_func() {}
fn private_func() {}
pub struct PublicStruct {}
struct PrivateStruct {}
pub enum PublicEnum {}
enum PrivateEnum {}
pub trait PublicTrait {}
trait PrivateTrait {}
`

	p := NewRustParser()
	pf, err := p.Parse("lib.rs", []byte(src))
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}

	// Check exports: should only have pub items
	exportNames := make(map[string]bool)
	for _, exp := range pf.Exports {
		exportNames[exp.Name] = true
	}

	if !exportNames["public_func"] {
		t.Error("expected public_func to be exported")
	}
	if exportNames["private_func"] {
		t.Error("private_func should not be exported")
	}
	if !exportNames["PublicStruct"] {
		t.Error("expected PublicStruct to be exported")
	}
	if exportNames["PrivateStruct"] {
		t.Error("PrivateStruct should not be exported")
	}
	if !exportNames["PublicEnum"] {
		t.Error("expected PublicEnum to be exported")
	}
	if exportNames["PrivateEnum"] {
		t.Error("PrivateEnum should not be exported")
	}
	if !exportNames["PublicTrait"] {
		t.Error("expected PublicTrait to be exported")
	}
	if exportNames["PrivateTrait"] {
		t.Error("PrivateTrait should not be exported")
	}
}

func TestRustParserSymbols(t *testing.T) {
	src := `pub fn process(data: &[u8]) -> Result<(), Error> {}
struct Config {
    name: String,
}
enum Status {
    Active,
    Inactive,
}
trait Processor {
    fn process(&self);
}
impl Processor for Config {
    fn process(&self) {}
}
pub type Alias = HashMap<String, Vec<u8>>;
pub const MAX_SIZE: usize = 1024;
pub static INSTANCE: Mutex<Option<Config>> = Mutex::new(None);
pub mod utils;
`

	p := NewRustParser()
	pf, err := p.Parse("lib.rs", []byte(src))
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}

	// Build a map of name -> list of kinds (a name can appear multiple times,
	// e.g. struct Config and impl Config).
	symKinds := make(map[string][]string)
	for _, sym := range pf.Symbols {
		symKinds[sym.Name] = append(symKinds[sym.Name], sym.Kind)
	}

	expectedSymbols := map[string]string{
		"process":   "function",
		"Config":    "struct",
		"Status":    "type",
		"Processor": "interface",
		"Alias":     "type",
		"MAX_SIZE":  "variable",
		"INSTANCE":  "variable",
		"utils":     "variable",
	}

	for name, kind := range expectedSymbols {
		kinds, ok := symKinds[name]
		if !ok {
			t.Errorf("expected symbol %q not found", name)
			continue
		}
		found := false
		for _, k := range kinds {
			if k == kind {
				found = true
				break
			}
		}
		if !found {
			t.Errorf("symbol %q: expected kind %q in %v", name, kind, kinds)
		}
	}
}

func TestRustParserGenericsFunctions(t *testing.T) {
	src := `pub fn parse<T: FromStr>(input: &str) -> T {}
pub fn process<'a>(data: &'a [u8]) -> &'a str {}
`

	p := NewRustParser()
	pf, err := p.Parse("lib.rs", []byte(src))
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}

	if len(pf.Symbols) < 2 {
		t.Fatalf("expected at least 2 symbols, got %d", len(pf.Symbols))
	}

	// Check that both are functions and exported
	for _, sym := range pf.Symbols {
		if sym.Kind != "function" {
			t.Errorf("expected function kind for %q, got %q", sym.Name, sym.Kind)
		}
		if !sym.Exported {
			t.Errorf("expected %q to be exported", sym.Name)
		}
	}
}

func TestRustParserPubCrate(t *testing.T) {
	src := `pub(crate) fn internal_func() {}
pub(super) fn parent_func() {}
pub fn fully_public() {}
`

	p := NewRustParser()
	pf, err := p.Parse("lib.rs", []byte(src))
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}

	// All three should be treated as exported (they have pub prefix)
	for _, sym := range pf.Symbols {
		if !sym.Exported {
			t.Errorf("expected %q to be exported (pub prefix present)", sym.Name)
		}
	}
}

func TestRustParserAsyncFn(t *testing.T) {
	src := `pub async fn fetch_data(url: &str) -> Result<Vec<u8>, Error> {}
async fn helper() {}
`

	p := NewRustParser()
	pf, err := p.Parse("lib.rs", []byte(src))
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}

	if len(pf.Symbols) != 2 {
		t.Fatalf("expected 2 symbols, got %d", len(pf.Symbols))
	}

	if pf.Symbols[0].Name != "fetch_data" || !pf.Symbols[0].Exported {
		t.Errorf("expected fetch_data to be exported, got name=%q exported=%v", pf.Symbols[0].Name, pf.Symbols[0].Exported)
	}
	if pf.Symbols[1].Name != "helper" || pf.Symbols[1].Exported {
		t.Errorf("expected helper to be private, got name=%q exported=%v", pf.Symbols[1].Name, pf.Symbols[1].Exported)
	}
}

func TestRustParserImplBlock(t *testing.T) {
	src := `impl Config {
    pub fn new() -> Self {}
    fn validate(&self) -> bool {}
}

impl Display for Config {
    fn fmt(&self, f: &mut Formatter) -> Result {}
}
`

	p := NewRustParser()
	pf, err := p.Parse("lib.rs", []byte(src))
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}

	// Should find: impl Config (symbol), new (pub fn), validate (fn),
	// impl Display for Config (symbol), fmt (fn)
	fnNames := make(map[string]bool)
	for _, sym := range pf.Symbols {
		if sym.Kind == "function" {
			fnNames[sym.Name] = true
		}
	}

	for _, name := range []string{"new", "validate", "fmt"} {
		if !fnNames[name] {
			t.Errorf("expected function %q not found", name)
		}
	}
}

// assertImportName checks a single import's path and first name.
func assertImportName(t *testing.T, imp Import, path, name string) {
	t.Helper()
	if imp.Path != path {
		t.Errorf("expected path %q, got %q", path, imp.Path)
	}
	if len(imp.Names) == 0 {
		t.Errorf("expected at least one name for import %q", path)
		return
	}
	if imp.Names[0] != name {
		t.Errorf("import %q: expected name %q, got %q", path, name, imp.Names[0])
	}
}
