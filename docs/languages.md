# Language Support

Fault supports 5 programming languages with language-specific parsing, analysis, and auto-fix capabilities.

## Supported Languages

| Language | Parser | Imports | Exports | Symbols | Analyzer Rules | Auto-fix |
|----------|--------|---------|---------|---------|----------------|----------|
| Go | Yes | Yes | Yes | Yes | Full | Yes |
| TypeScript/JavaScript | Yes | Yes | Yes | Yes | Full | Yes |
| Python | Yes | Yes | Yes | Yes | Full | Yes |
| Java | Yes | Yes | Yes | Yes | Partial | No |
| Rust | Yes | Yes | Yes | Yes | Partial | No |

## Go

**Parser capabilities:**
- `import` blocks (single and grouped)
- Function, method, struct, interface, type declarations
- Exported vs unexported symbols (capitalization)
- Package names

**Analyzer rules:**
- Broken imports (cross-file references)
- `fmt.Println` / `print` debug statements
- SQL injection via `db.Query` / `fmt.Sprintf`
- Path traversal via `os.Open` with unsanitized paths
- Hardcoded secrets
- Insecure crypto (MD5, SHA1)
- TODO/placeholder detection
- Unreachable code after return/panic

**Auto-fix:**
- Remove debug prints
- Replace hardcoded secrets with `os.Getenv()`
- Fix broken import paths

## TypeScript / JavaScript

**Parser capabilities:**
- ES module imports (`import { } from`, `import default`, `import *`, `import type`)
- CommonJS require (`const x = require()`)
- Dynamic imports (`import()`)
- Exports (named, default, re-exports, type re-exports)
- Functions, classes, interfaces, enums (including `const enum`)
- Arrow functions and function expressions
- Decorators (`@Component`, `@Injectable`)
- Generic interfaces (`interface Foo<T>`)
- Type-only re-exports (`export type { Foo } from`)

**Analyzer rules:**
- Broken imports (cross-file references)
- `console.log` debug statements
- `any` type usage (warns to use `unknown`)
- Non-null assertion abuse (`!.`)
- Empty interfaces (suggests `Record<string, never>`)
- `@ts-ignore` / `@ts-nocheck` directives
- Triple-slash directives (should be imports)
- `dangerouslySetInnerHTML` / `innerHTML` XSS
- `eval()`, `new Function()`, `document.write()` code execution
- `setTimeout`/`setInterval` with string arguments
- SQL injection via template literals
- Hardcoded secrets
- Insecure crypto (Math.random, MD5, SHA1)

**Auto-fix:**
- Replace `any` with `unknown`
- Remove `@ts-ignore` comments
- Remove unused type-only imports
- Replace hardcoded secrets with `process.env`
- Replace `Math.random()` with `crypto.randomUUID()`
- Remove `console.log` statements

## Python

**Parser capabilities:**
- `import module` and `from module import name` statements
- Relative imports (`from . import`, `from ..pkg import`)
- Function and class declarations
- Decorated functions/classes
- Global variable assignments

**Analyzer rules:**
- Broken imports (cross-file references)
- `print()` debug statements
- `raise NotImplementedError` placeholders
- Standalone `pass` statements
- SQL injection via f-strings and %-formatting
- Path traversal via `open()` with request params
- Hardcoded secrets
- Insecure crypto (hashlib.md5, hashlib.sha1)

**Auto-fix:**
- Remove debug prints
- Replace hardcoded secrets with `os.environ.get()`

## Java

**Parser capabilities:**
- Package declarations
- Import statements (including static imports)
- Class, interface, enum, record, and annotation declarations
- Method declarations with modifiers and generics
- Field declarations
- Constructor declarations
- Nested class detection

**Analyzer rules:**
- Broken imports (cross-file references)
- SQL injection via string concatenation in JDBC
- Hardcoded secrets
- `System.out.println` debug statements

**Known limitations:**
- No auto-fix support yet
- Generic type parameters in complex nested declarations may not be fully parsed
- Annotation processing is limited to declaration detection

## Rust

**Parser capabilities:**
- `use` statements (single, grouped, glob)
- `mod` declarations
- Function declarations (`fn`, `pub fn`, `async fn`)
- Struct, enum, trait, impl declarations
- Type aliases
- Const and static declarations
- Macro definitions (`macro_rules!`)
- Attribute macros (`#[derive]`, `#[cfg]`)

**Analyzer rules:**
- Broken imports (cross-file references)
- Hardcoded secrets
- `println!` / `dbg!` debug macros
- `unsafe` block detection

**Known limitations:**
- No auto-fix support yet
- Complex macro invocations may not be fully parsed
- Trait impl method signatures are detected but not fully resolved

## Adding Language Support

To add a new language to Fault:

1. Create a parser in `pkg/parser/` implementing the `Parser` interface
2. Register the parser in `cmd/fault/main.go`
3. Add language-specific rules to `pkg/analyzer/patterns.go` and `security.go`
4. Optionally add a fixer in `pkg/fixer/`
5. Add tests for all new functionality
