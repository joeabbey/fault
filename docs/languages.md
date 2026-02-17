# Language Support

Fault supports **42 programming languages**. Every language gets parsing, import/export analysis, and anti-pattern detection. Languages with deeper support also get auto-fix and language-specific security rules.

## All languages

| Language | Imports | Exports | Symbols | Security rules | Auto-fix |
|----------|:--:|:--:|:--:|:--:|:--:|
| **Go** | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: |
| **TypeScript / JavaScript** | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: |
| **Python** | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: |
| Java | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | |
| Rust | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | |
| Ruby | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | |
| Kotlin | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | |
| C# | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | |
| PHP | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | |
| Swift | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | |
| C | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | |
| C++ | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | |
| Objective-C | :white_check_mark: | :white_check_mark: | :white_check_mark: | | |
| Dart | :white_check_mark: | :white_check_mark: | :white_check_mark: | | |
| Scala | :white_check_mark: | :white_check_mark: | :white_check_mark: | | |
| R | :white_check_mark: | :white_check_mark: | :white_check_mark: | | |
| Elixir | :white_check_mark: | :white_check_mark: | :white_check_mark: | | |
| Lua | :white_check_mark: | :white_check_mark: | :white_check_mark: | | |
| Perl | :white_check_mark: | :white_check_mark: | :white_check_mark: | | |
| PowerShell | :white_check_mark: | :white_check_mark: | :white_check_mark: | | |
| Groovy | :white_check_mark: | :white_check_mark: | :white_check_mark: | | |
| Bash / Shell | :white_check_mark: | :white_check_mark: | :white_check_mark: | | |
| SQL | :white_check_mark: | | | | |
| Zig | :white_check_mark: | :white_check_mark: | :white_check_mark: | | |
| Nim | :white_check_mark: | :white_check_mark: | :white_check_mark: | | |
| Crystal | :white_check_mark: | :white_check_mark: | :white_check_mark: | | |
| V | :white_check_mark: | :white_check_mark: | :white_check_mark: | | |
| D | :white_check_mark: | :white_check_mark: | :white_check_mark: | | |
| Haskell | :white_check_mark: | :white_check_mark: | :white_check_mark: | | |
| Clojure | :white_check_mark: | :white_check_mark: | :white_check_mark: | | |
| Erlang | :white_check_mark: | :white_check_mark: | :white_check_mark: | | |
| F# | :white_check_mark: | :white_check_mark: | :white_check_mark: | | |
| OCaml | :white_check_mark: | :white_check_mark: | :white_check_mark: | | |
| Julia | :white_check_mark: | :white_check_mark: | :white_check_mark: | | |
| Fortran | :white_check_mark: | :white_check_mark: | :white_check_mark: | | |
| Solidity | :white_check_mark: | :white_check_mark: | :white_check_mark: | | |
| Terraform | :white_check_mark: | :white_check_mark: | :white_check_mark: | | |
| Protobuf | :white_check_mark: | :white_check_mark: | :white_check_mark: | | |
| Visual Basic | :white_check_mark: | :white_check_mark: | :white_check_mark: | | |
| COBOL | :white_check_mark: | :white_check_mark: | :white_check_mark: | | |
| Ada | :white_check_mark: | :white_check_mark: | :white_check_mark: | | |
| Pascal | :white_check_mark: | :white_check_mark: | :white_check_mark: | | |

> **Bold** languages have full support including auto-fix. All languages get the core analyzers: imports, consistency, references, tests, patterns, hallucination, complexity, concurrency, resource, migration, and doc drift.

---

## Language details

### Go

**Parser:**
- `import` blocks (single and grouped)
- Function, method, struct, interface, type declarations
- Exported vs unexported symbols (capitalization)
- Package names

**Security rules:**
- SQL injection via `db.Query` / `fmt.Sprintf`
- Path traversal via `os.Open` with unsanitized paths
- Hardcoded secrets
- Insecure crypto (MD5, SHA1)
- `fmt.Println` / `print` debug statements
- TODO/placeholder detection
- Unreachable code after return/panic

**Auto-fix:**
- Remove debug prints
- Replace hardcoded secrets with `os.Getenv()`
- Fix broken import paths

### TypeScript / JavaScript

**Parser:**
- ES module imports (`import { } from`, `import default`, `import *`, `import type`)
- CommonJS require (`const x = require()`)
- Dynamic imports (`import()`)
- Exports (named, default, re-exports, type re-exports)
- Functions, classes, interfaces, enums (including `const enum`)
- Arrow functions and function expressions
- Decorators (`@Component`, `@Injectable`)
- Generic interfaces (`interface Foo<T>`)
- Type-only re-exports (`export type { Foo } from`)

**Security rules:**
- `dangerouslySetInnerHTML` / `innerHTML` XSS
- `eval()`, `new Function()`, `document.write()` code execution
- `setTimeout`/`setInterval` with string arguments
- SQL injection via template literals
- Hardcoded secrets
- Insecure crypto (Math.random, MD5, SHA1)
- `any` type usage, non-null assertion abuse
- `@ts-ignore` / `@ts-nocheck` directives
- `console.log` debug statements

**Auto-fix:**
- Replace `any` with `unknown`
- Remove `@ts-ignore` comments
- Remove unused type-only imports
- Replace hardcoded secrets with `process.env`
- Replace `Math.random()` with `crypto.randomUUID()`
- Remove `console.log` statements

### Python

**Parser:**
- `import module` and `from module import name` statements
- Relative imports (`from . import`, `from ..pkg import`)
- Function and class declarations
- Decorated functions/classes
- Global variable assignments

**Security rules:**
- SQL injection via f-strings and %-formatting
- Path traversal via `open()` with request params
- Hardcoded secrets
- Insecure crypto (hashlib.md5, hashlib.sha1)
- `print()` debug statements
- `raise NotImplementedError` placeholders
- Standalone `pass` statements

**Auto-fix:**
- Remove debug prints
- Replace hardcoded secrets with `os.environ.get()`

---

## Adding language support

To add a new language to Fault:

1. Create a parser in `pkg/parser/` implementing the `Parser` interface
2. Register the parser in `cmd/fault/main.go`
3. Add language-specific rules to `pkg/analyzer/patterns.go` and `security.go`
4. Optionally add a fixer in `pkg/fixer/`
5. Add tests for all new functionality
