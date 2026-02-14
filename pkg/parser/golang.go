package parser

import (
	"fmt"
	"go/ast"
	goparser "go/parser"
	"go/token"
	"strings"
	"unicode"
)

// GoParser extracts structural information from Go source files.
type GoParser struct{}

// NewGoParser creates a new Go parser.
func NewGoParser() *GoParser {
	return &GoParser{}
}

// Language returns "go".
func (p *GoParser) Language() string {
	return "go"
}

// Parse parses a Go source file and extracts imports, exports, and symbols.
func (p *GoParser) Parse(filename string, content []byte) (*ParsedFile, error) {
	fset := token.NewFileSet()
	file, err := goparser.ParseFile(fset, filename, content, goparser.ParseComments)
	if err != nil {
		return nil, fmt.Errorf("parsing Go file %s: %w", filename, err)
	}

	pf := &ParsedFile{
		Path:     filename,
		Language: "go",
		Imports:  make([]Import, 0),
		Exports:  make([]Export, 0),
		Symbols:  make([]Symbol, 0),
	}

	// Extract imports
	for _, imp := range file.Imports {
		pf.Imports = append(pf.Imports, extractGoImport(fset, imp))
	}

	// Extract declarations
	for _, decl := range file.Decls {
		switch d := decl.(type) {
		case *ast.FuncDecl:
			sym := extractFuncSymbol(fset, d)
			pf.Symbols = append(pf.Symbols, sym)
			if sym.Exported {
				pf.Exports = append(pf.Exports, Export{
					Name: sym.Name,
					Kind: sym.Kind,
					Line: sym.Line,
				})
			}
		case *ast.GenDecl:
			syms := extractGenDeclSymbols(fset, d)
			for _, sym := range syms {
				pf.Symbols = append(pf.Symbols, sym)
				if sym.Exported {
					pf.Exports = append(pf.Exports, Export{
						Name: sym.Name,
						Kind: sym.Kind,
						Line: sym.Line,
					})
				}
			}
		}
	}

	return pf, nil
}

// extractGoImport converts an ast.ImportSpec to our Import type.
func extractGoImport(fset *token.FileSet, imp *ast.ImportSpec) Import {
	path := strings.Trim(imp.Path.Value, `"`)
	i := Import{
		Path:  path,
		Names: make([]string, 0),
		Line:  fset.Position(imp.Pos()).Line,
	}
	if imp.Name != nil {
		i.Names = append(i.Names, imp.Name.Name)
	}
	return i
}

// extractFuncSymbol extracts a Symbol from a function declaration.
func extractFuncSymbol(fset *token.FileSet, fn *ast.FuncDecl) Symbol {
	name := fn.Name.Name
	kind := "function"

	// Methods have a receiver
	if fn.Recv != nil && fn.Recv.NumFields() > 0 {
		kind = "method"
	}

	sig := buildFuncSignature(fn)

	startLine := fset.Position(fn.Pos()).Line
	endLine := fset.Position(fn.End()).Line

	return Symbol{
		Name:      name,
		Kind:      kind,
		Exported:  isExported(name),
		Line:      startLine,
		EndLine:   endLine,
		Signature: sig,
	}
}

// extractGenDeclSymbols extracts symbols from a general declaration (type, var, const).
func extractGenDeclSymbols(fset *token.FileSet, decl *ast.GenDecl) []Symbol {
	symbols := make([]Symbol, 0)

	for _, spec := range decl.Specs {
		switch s := spec.(type) {
		case *ast.TypeSpec:
			kind := "type"
			if _, ok := s.Type.(*ast.InterfaceType); ok {
				kind = "interface"
			} else if _, ok := s.Type.(*ast.StructType); ok {
				kind = "struct"
			}

			sym := Symbol{
				Name:     s.Name.Name,
				Kind:     kind,
				Exported: isExported(s.Name.Name),
				Line:     fset.Position(s.Pos()).Line,
				EndLine:  fset.Position(s.End()).Line,
			}
			symbols = append(symbols, sym)

		case *ast.ValueSpec:
			kind := "variable"
			if decl.Tok == token.CONST {
				kind = "constant"
			}

			for _, name := range s.Names {
				sym := Symbol{
					Name:     name.Name,
					Kind:     kind,
					Exported: isExported(name.Name),
					Line:     fset.Position(name.Pos()).Line,
				}
				symbols = append(symbols, sym)
			}
		}
	}

	return symbols
}

// buildFuncSignature creates a human-readable function signature.
func buildFuncSignature(fn *ast.FuncDecl) string {
	var b strings.Builder

	b.WriteString("func ")

	// Receiver
	if fn.Recv != nil && fn.Recv.NumFields() > 0 {
		b.WriteString("(")
		for i, field := range fn.Recv.List {
			if i > 0 {
				b.WriteString(", ")
			}
			b.WriteString(typeString(field.Type))
		}
		b.WriteString(") ")
	}

	b.WriteString(fn.Name.Name)
	b.WriteString("(")

	// Parameters
	if fn.Type.Params != nil {
		params := make([]string, 0)
		for _, field := range fn.Type.Params.List {
			typeStr := typeString(field.Type)
			if len(field.Names) == 0 {
				params = append(params, typeStr)
			} else {
				for _, name := range field.Names {
					params = append(params, name.Name+" "+typeStr)
				}
			}
		}
		b.WriteString(strings.Join(params, ", "))
	}

	b.WriteString(")")

	// Return types
	if fn.Type.Results != nil && fn.Type.Results.NumFields() > 0 {
		results := make([]string, 0)
		for _, field := range fn.Type.Results.List {
			typeStr := typeString(field.Type)
			if len(field.Names) > 0 {
				for _, name := range field.Names {
					results = append(results, name.Name+" "+typeStr)
				}
			} else {
				results = append(results, typeStr)
			}
		}
		if len(results) == 1 {
			b.WriteString(" " + results[0])
		} else {
			b.WriteString(" (" + strings.Join(results, ", ") + ")")
		}
	}

	return b.String()
}

// typeString returns a simplified string representation of a type expression.
func typeString(expr ast.Expr) string {
	switch t := expr.(type) {
	case *ast.Ident:
		return t.Name
	case *ast.StarExpr:
		return "*" + typeString(t.X)
	case *ast.ArrayType:
		return "[]" + typeString(t.Elt)
	case *ast.MapType:
		return "map[" + typeString(t.Key) + "]" + typeString(t.Value)
	case *ast.SelectorExpr:
		return typeString(t.X) + "." + t.Sel.Name
	case *ast.InterfaceType:
		return "interface{}"
	case *ast.FuncType:
		return "func(...)"
	case *ast.ChanType:
		return "chan " + typeString(t.Value)
	case *ast.Ellipsis:
		return "..." + typeString(t.Elt)
	default:
		return "any"
	}
}

// isExported returns true if the name starts with an uppercase letter.
func isExported(name string) bool {
	if name == "" {
		return false
	}
	return unicode.IsUpper(rune(name[0]))
}
