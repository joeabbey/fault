package parser

import (
	"regexp"
	"strings"
)

type ProtobufParser struct{}

func NewProtobufParser() *ProtobufParser { return &ProtobufParser{} }

func (p *ProtobufParser) Language() string { return "protobuf" }

var (
	protoImport  = regexp.MustCompile(`(?m)^\s*import\s+"([^"]+)"`)
	protoMessage = regexp.MustCompile(`(?m)^\s*message\s+(\w+)`)
	protoService = regexp.MustCompile(`(?m)^\s*service\s+(\w+)`)
	protoEnum    = regexp.MustCompile(`(?m)^\s*enum\s+(\w+)`)
	protoRpc     = regexp.MustCompile(`(?m)^\s*rpc\s+(\w+)\s*\(`)
	protoPackage = regexp.MustCompile(`(?m)^\s*package\s+([\w.]+)`)
)

func (p *ProtobufParser) Parse(filename string, content []byte) (*ParsedFile, error) {
	pf := &ParsedFile{
		Path:     filename,
		Language: "protobuf",
		Imports:  make([]Import, 0),
		Exports:  make([]Export, 0),
		Symbols:  make([]Symbol, 0),
	}

	lines := strings.Split(string(content), "\n")
	inBlockComment := false

	for lineNum, line := range lines {
		lineNo := lineNum + 1
		trimmed := strings.TrimSpace(line)

		if inBlockComment {
			if strings.Contains(trimmed, "*/") {
				inBlockComment = false
			}
			continue
		}
		if strings.HasPrefix(trimmed, "/*") {
			if !strings.Contains(trimmed, "*/") {
				inBlockComment = true
			}
			continue
		}
		if trimmed == "" || strings.HasPrefix(trimmed, "//") {
			continue
		}

		// Import
		if m := protoImport.FindStringSubmatch(line); m != nil {
			pf.Imports = append(pf.Imports, Import{Path: m[1], Line: lineNo})
			continue
		}
		// Package (treat as module symbol)
		if m := protoPackage.FindStringSubmatch(line); m != nil {
			pf.Symbols = append(pf.Symbols, Symbol{Name: m[1], Kind: "module", Exported: true, Line: lineNo})
			continue
		}
		// Message
		if m := protoMessage.FindStringSubmatch(line); m != nil {
			pf.Symbols = append(pf.Symbols, Symbol{Name: m[1], Kind: "type", Exported: true, Line: lineNo})
			pf.Exports = append(pf.Exports, Export{Name: m[1], Kind: "type", Line: lineNo})
			continue
		}
		// Service
		if m := protoService.FindStringSubmatch(line); m != nil {
			pf.Symbols = append(pf.Symbols, Symbol{Name: m[1], Kind: "interface", Exported: true, Line: lineNo})
			pf.Exports = append(pf.Exports, Export{Name: m[1], Kind: "interface", Line: lineNo})
			continue
		}
		// Enum
		if m := protoEnum.FindStringSubmatch(line); m != nil {
			pf.Symbols = append(pf.Symbols, Symbol{Name: m[1], Kind: "type", Exported: true, Line: lineNo})
			pf.Exports = append(pf.Exports, Export{Name: m[1], Kind: "type", Line: lineNo})
			continue
		}
		// RPC
		if m := protoRpc.FindStringSubmatch(line); m != nil {
			pf.Symbols = append(pf.Symbols, Symbol{Name: m[1], Kind: "function", Exported: true, Line: lineNo})
			pf.Exports = append(pf.Exports, Export{Name: m[1], Kind: "function", Line: lineNo})
			continue
		}
	}

	return pf, nil
}
