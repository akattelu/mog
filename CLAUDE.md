# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

**mog** is a Lua-based language interpreter written in Zig. The project implements a lexer, parser, AST, and pretty printer for a Lua-like language. This is a compiler/interpreter project following traditional parsing architecture with support for Lua 5.4 syntax including loops, functions, tables, and operators.

## Build Commands

```bash
# Build the project
zig build

# Build with detailed output (recommended - shows compilation progress)
zig build --summary all

# Run all tests
zig build test

# Run tests with detailed output (recommended - shows which tests pass/fail)
zig build test --summary all

# Run the REPL in lexer mode
zig build run -- repl --lex

# Run the REPL in parser mode
zig build run -- repl --parse

# Parse a Lua file and output the AST
zig build run -- parse <file>

# Format a Lua file with pretty printing
zig build run -- fmt <file>

# Show help message
zig build run -- help

# Format all Zig source files
zig fmt .

# Install git hooks (runs format check, build, and tests before commits)
zig build install-git-hooks

# Build release binaries for multiple targets
zig build -Drelease=true
```

**Note**: By default, `zig build` and `zig build test` produce minimal output and only show exit codes on success. Use `--summary all` to see detailed compilation progress, test results, and timing information. This is especially helpful when debugging build issues or verifying that tests are actually running.

## Architecture

### Core Components

1. **Lexer** (`src/lexer.zig`): Tokenizes input source code
   - Uses arena allocator for token memory management
   - Tracks token positions (start_pos, end_pos) for error reporting
   - Supports strings, identifiers, integers, floats, keywords, operators, and C builtins
   - Handles multi-character operators (==, ~=, //, .., ..., <<, >>, etc.)
   - Recognizes C builtin tokens starting with `$` prefix (e.g., `$puts`, `$malloc`)

2. **Token** (`src/token.zig`): Token types and definitions
   - Comprehensive TokenType enum covering all Lua operators, keywords, and C builtins
   - Keyword map for reserved words: `function`, `local`, `if`, `else`, `elseif`, `return`, `not`, `and`, `or`, `nil`, `while`, `repeat`, `for`, `break`, `do`, `end`, `then`, `until`, `in`, `goto`, `type`
   - Static string maps for efficient O(1) keyword lookup
   - Includes `builtin` token type for C function calls

3. **Parser** (`src/parser.zig`): Pratt parser implementing Lua 5.4 grammar
   - Implements precedence-based parsing for infix/prefix operators
   - Uses prefix/infix function maps for extensibility
   - Arena allocator for AST node allocation
   - **Precedence levels** (lowest to highest): lowest, logical_or, logical_and, comparison, bitwise_or, bitwise_xor, bitwise_and, bitwise_shift, concat, sum, product, unary, exponent, call
   - Supports right-associative operators (^ and ..)
   - Comprehensive error reporting with token position information

4. **AST** (`src/ast.zig`): Abstract Syntax Tree node definitions
   - **Statement types**: Assignment, Return, Expression, FunctionDeclaration, Do, While, Repeat, ForNumeric, ForGeneric, Break
   - **Expression types**: Identifier, Number (int/float), String, Prefix, Infix, Conditional, Boolean, Nil, Varargs, FunctionDef, TableConstructor, Index, Member, FunctionCall, MethodCall, CBuiltin
   - All nodes implement `tokenLiteral()`, `write(Writer)`, and `pretty(PrettyPrinter)` methods
   - Uses tagged unions for type-safe polymorphic node types
   - **CBuiltinExpression**: Represents C builtin function calls with `$` prefix for FFI (Foreign Function Interface) capabilities

5. **Pretty Printer** (`src/pretty_printer.zig`): Formats AST back to readable Lua code
   - Manages indentation levels for nested structures
   - Handles proper newline placement for blocks
   - Used by the `fmt` command to format Lua source files

6. **Main** (`src/main.zig`): CLI entry point with command routing
   - Command map routes to: `repl`, `parse`, `fmt`, `help`
   - REPL supports `--lex` mode (tokenization only) and `--parse` mode (full parsing)
   - Uses custom `std.Io.Writer` interface for buffered output

### Memory Management

- Arena allocators used throughout for simplified memory management
- Lexer and Parser each maintain their own arena allocator
- All allocations cleaned up via `deinit()` methods
- AST nodes allocated in parser's arena and freed when parser is deinitialized

### Key Design Patterns

- **Writer Interface**: Custom `std.Io.Writer` abstraction used for all output (tokens, AST nodes)
- **Tagged Unions**: Statement and Expression types use Zig's tagged unions for type-safe polymorphism
- **Static String Maps**: Efficient O(1) keyword and precedence lookups using compile-time string maps
- **Pratt Parsing**: Operator precedence handled through precedence climbing algorithm
- **Visitor Pattern**: `write()` and `pretty()` methods traverse AST for output generation

### Supported Lua Features

- **Variables**: local and global assignments, multiple assignment
- **Control Flow**: if-then-else-end, while-do-end, repeat-until, do-end blocks, break
- **Loops**: numeric for loops (`for i = 1, 10, 2 do`), generic for loops (`for k, v in pairs(t) do`)
- **Functions**: function declarations (local and global), function expressions, varargs (...)
- **Operators**:
  - Arithmetic: +, -, *, /, //, %, ^
  - Comparison: ==, ~=, <, >, <=, >=
  - Logical: and, or, not
  - Bitwise: &, |, ~, <<, >>
  - String: .. (concatenation)
  - Length: #
- **Tables**: table constructors with array-style, record-style, and computed-key fields
- **Calls**: function calls, method calls with : syntax
- **Access**: member access (.), index access ([])

### C Builtin Functions (FFI)

**CBuiltin** is a language extension in mog that enables Foreign Function Interface (FFI) capabilities by allowing direct calls to C functions from mog code. This feature bridges the gap between mog's Lua-like syntax and native C libraries.

#### Syntax

C builtin functions are prefixed with a dollar sign (`$`) followed by the C function name:

```lua
$puts("Hello from C!")
local ptr = $malloc(256)
$printf("Value: %d\n", 42)
$free(ptr)
```

#### Implementation Details

1. **Lexer** (`src/lexer.zig:100-115`):
   - The `readBuiltin()` function recognizes tokens starting with `$`
   - Validates that `$` is followed by a valid identifier (letter, then letters/digits/underscores)
   - Returns the complete builtin name including the `$` prefix
   - Invalid syntax (e.g., `$123`) produces a `LexError.Illegal` error

2. **Token** (`src/token.zig:9`):
   - Defines `builtin` as a distinct `TokenType` alongside identifiers and keywords
   - Builtin tokens are not in the keyword map as they're identified by lexical pattern

3. **Parser** (`src/parser.zig:119`):
   - Maps the `"builtin"` token type to the `parseCBuiltinExpression` prefix parser
   - Creates a `CBuiltinExpression` AST node containing the token and full name

4. **AST** (`src/ast.zig:675-697`):
   - `CBuiltinExpression` struct stores:
     - `token`: The original token with position information
     - `name`: The full builtin name including `$` prefix (e.g., `"$puts"`)
   - Implements standard AST methods: `tokenLiteral()`, `write()`, `pretty()`
   - Added to the `ExpressionTypes` enum and `Expression` tagged union

5. **Compilation** (`src/ast.zig:887-890`):
   - Function calls check if the callee is a `CBuiltin` expression
   - QBE backend handles dynamic linking to C libraries at compile time
   - Currently marked with FIXME for instruction generation

#### Usage Examples

```lua
-- Simple C function call
$puts("Hello, World!")

-- Assignment from C function
local memory = $malloc(1024)
local name = $getenv("USER")

-- C functions with multiple arguments
$printf("Name: %s, Age: %d\n", name, age)
$memcpy(dest, src, size)

-- Using in expressions
if $strcmp(str1, str2) == 0 then
    print("Strings are equal")
end

-- Freeing resources
$free(memory)
```

#### Testing

CBuiltin functionality is tested in:
- `src/tests/lexer.test.zig`: Tokenization of `$` prefix, position tracking, invalid cases
- `src/tests/parser.test.zig`: Parsing builtin expressions standalone, in function calls, in assignments, multiple builtins

#### QBE Integration

The mog compiler uses QBE (Quick Backend) to generate native code with dynamic linking to C libraries. C builtins are compiled as external function calls that QBE resolves at link time, allowing seamless integration with the C standard library and other C-compatible libraries.

### Testing

Tests are organized in `src/tests/` directory with separate files for each module:
- `src/tests/lexer.test.zig`: Token generation, position tracking
- `src/tests/parser.test.zig`: Statement parsing, expression parsing, operator precedence
- `src/tests/ast.test.zig`: String representation via `write()` methods
- `src/tests/token.test.zig`: Token type and keyword mapping tests
- `src/tests/pretty_printer.test.zig`: Pretty printing functionality

**Important**: To prevent Zig's dead code elimination from removing test files, they are explicitly referenced in `src/main.zig` (lines 219-233) as public declarations in a test block:
```zig
test {
    const parser_tests = @import("tests/parser.test.zig");
    const lexer_tests = @import("tests/lexer.test.zig");
    const token_tests = @import("tests/token.test.zig");
    const ast_tests = @import("tests/ast.test.zig");
    const pretty_printer_tests = @import("tests/pretty_printer.test.zig");

    std.testing.refAllDecls(token_tests);
    std.testing.refAllDecls(lexer_tests);
    std.testing.refAllDecls(parser_tests);
    std.testing.refAllDecls(ast_tests);
    std.testing.refAllDecls(pretty_printer_tests);
}
```

## Development Notes

- The parser uses `expectAndPeek()` pattern for token consumption with error handling
- Error messages include token position information for debugging via `setErrExpected()` and `setErrExpectedAny()` methods
- The `Block` type is aliased to `Program` (both are statement collections)
- In Zig 0.15+ there are new interfaces for `std.Io.Writer` and `std.Io.Reader` (note the capital `Io` instead of lowercase `io`). Use MCP tools to understand the new interface when revising I/O code.
- The pre-commit hook (`.githooks/pre-commit`) runs formatting check, build, and tests before allowing commits

## Zig Documentation

When working with Zig standard library APIs or answering questions about Zig language features, **always consult the official Zig documentation** using the available MCP tools before writing code or providing answers:

- `mcp__zig-docs__search_std_lib` - Search for standard library functions, types, and namespaces
- `mcp__zig-docs__get_std_lib_item` - Get detailed documentation for specific items (e.g., `std.io.Writer`, `std.ArrayList`)
- `mcp__zig-docs__list_builtin_functions` - List all available builtin functions (e.g., `@intCast`, `@as`)
- `mcp__zig-docs__get_builtin_function` - Get documentation for specific builtin functions

This ensures accurate usage of Zig APIs and prevents outdated or incorrect information from being provided.

## Lua Reference Grammar

Here is the grammar of Lua 5.4 - this can be useful when making updates to the mog parser:

```bnf
	chunk ::= block

	block ::= {stat} [retstat]

	stat ::=  ';' |
		 varlist '=' explist |
		 functioncall |
		 label |
		 break |
		 goto Name |
		 do block end |
		 while exp do block end |
		 repeat block until exp |
		 if exp then block {elseif exp then block} [else block] end |
		 for Name '=' exp ',' exp [',' exp] do block end |
		 for namelist in explist do block end |
		 function funcname funcbody |
		 local function Name funcbody |
		 local attnamelist ['=' explist]

	attnamelist ::=  Name attrib {',' Name attrib}

	attrib ::= ['<' Name '>']

	retstat ::= return [explist] [';']

	label ::= '::' Name '::'

	funcname ::= Name {'.' Name} [':' Name]

	varlist ::= var {',' var}

	var ::=  Name | prefixexp '[' exp ']' | prefixexp '.' Name

	namelist ::= Name {',' Name}

	explist ::= exp {',' exp}

	exp ::=  nil | false | true | Numeral | LiteralString | '...' | functiondef |
		 prefixexp | tableconstructor | exp binop exp | unop exp

	prefixexp ::= var | functioncall | '(' exp ')'

	functioncall ::=  prefixexp args | prefixexp ':' Name args

	args ::=  '(' [explist] ')' | tableconstructor | LiteralString

	functiondef ::= function funcbody

	funcbody ::= '(' [parlist] ')' block end

	parlist ::= namelist [',' '...'] | '...'

	tableconstructor ::= '{' [fieldlist] '}'

	fieldlist ::= field {fieldsep field} [fieldsep]

	field ::= '[' exp ']' '=' exp | Name '=' exp | exp

	fieldsep ::= ',' | ';'

	binop ::=  '+' | '-' | '*' | '/' | '//' | '^' | '%' |
		 '&' | '~' | '|' | '>>' | '<<' | '..' |
		 '<' | '<=' | '>' | '>=' | '==' | '~=' |
		 and | or

	unop ::= '-' | not | '#' | '~'
```

## QBE

The mog compiler uses QBE (binary stored in vendor/qbe/qbe) to output .s instruction files.
The QBE IR documentation can be found at: https://c9x.me/compile/doc/il.html read the contents of the page before answering questions or writing qbe related code.
