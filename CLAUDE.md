# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

**mog** is a Lua-based language interpreter written in Zig. The project implements a lexer, parser, and AST for a Lua-like language with slight modifications to support type declarations. This is a compiler/interpreter project following traditional parsing architecture.

## Language Grammar

The language grammar is defined in AGENTS.md and follows Lua's BNF grammar with adaptations for type annotations. Key syntax includes:
- Statements: `let`, `return`, `if-then-else-end`, function definitions
- Expressions: literals (integers, booleans, strings), infix/prefix operations, conditionals
- Type annotations: `let name: type = value`

## Build Commands

```bash
# Build the project
zig build

# Run the REPL in lexer mode
zig build run -- repl --lex

# Run the REPL in parser mode
zig build run -- repl --parse

# Run all tests
zig build test

# Type-check without building
zig build check

# Install git hooks
zig build install-git-hooks
```

## Architecture

### Core Components

1. **Lexer** (`src/lexer.zig`): Tokenizes input source code
   - Uses arena allocator for token memory management
   - Tracks token positions (start_pos, end_pos) for error reporting
   - Supports strings, identifiers, integers, keywords, operators

2. **Token** (`src/token.zig`): Token types and definitions
   - Keyword map for reserved words (`function`, `let`, `if`, `else`, `return`, etc.)
   - Static string maps for efficient keyword lookup

3. **Parser** (`src/parser.zig`): Pratt parser for expression parsing
   - Implements precedence-based parsing for infix/prefix operators
   - Uses prefix/infix function maps for extensibility
   - Arena allocator for AST node allocation
   - Precedence levels: lowest, equals, lessgreater, sum, product, prefix, call

4. **AST** (`src/ast.zig`): Abstract Syntax Tree node definitions
   - Statement types: Let, Return, Expression
   - Expression types: Identifier, Integer, Boolean, Prefix, Infix, Conditional
   - All nodes implement `tokenLiteral()` and `write(Writer)` methods
   - Uses tagged unions for polymorphic node types

5. **Main** (`src/main.zig`): REPL entry point
   - Supports `--lex` mode (tokenization only) and `--parse` mode (full parsing)
   - Uses custom `std.Io.Writer` interface for output buffering

### Memory Management

- Arena allocators used throughout for simplified memory management
- Lexer and Parser each maintain their own arena
- All allocations cleaned up via `deinit()` methods

### Key Design Patterns

- **Writer Interface**: Custom `std.Io.Writer` abstraction used for all output (tokens, AST nodes)
- **Tagged Unions**: Statement and Expression types use Zig's tagged unions for type-safe polymorphism
- **Static String Maps**: Efficient O(1) keyword and precedence lookups
- **Pratt Parsing**: Operator precedence handled through precedence climbing

### Testing

All core modules include comprehensive unit tests:
- Lexer: Token generation, position tracking
- Parser: Statement parsing, expression parsing, operator precedence
- AST: String representation via `write()` methods

Run individual module tests by examining test blocks in each file.

## Development Notes

- The parser uses `expectAndPeek()` pattern for token consumption with error handling
- Error messages include token position information for debugging
- The `Block` type is aliased to `Program` (both are statement collections)
- Boolean literal parsing includes logging (see `parseBooleanExpression` in parser.zig:326)
- In zig 0.15 there are new interfaces for std.Io.Writer and std.Io.Reader (note the capital Io instead of lowercase io). Use tools to understand the new interface when you want to use or revise these interfaces.

## Zig Documentation

When working with Zig standard library APIs or answering questions about Zig language features, **always consult the official Zig documentation** using the available MCP tools before writing code or providing answers:

- `mcp__zig-docs__search_std_lib` - Search for standard library functions, types, and namespaces
- `mcp__zig-docs__get_std_lib_item` - Get detailed documentation for specific items (e.g., `std.io.Writer`, `std.ArrayList`)
- `mcp__zig-docs__list_builtin_functions` - List all available builtin functions (e.g., `@intCast`, `@as`)
- `mcp__zig-docs__get_builtin_function` - Get documentation for specific builtin functions

This ensures accurate usage of Zig APIs and prevents outdated or incorrect information from being provided.

## Lua Reference Grammar

Here is the grammar of lua 5.4 - this can be useful when making updates to the mog parser

```bnf
	chunk ::= block

	block ::= {stat} [retstat]

	stat ::=  ‘;’ | 
		 varlist ‘=’ explist | 
		 functioncall | 
		 label | 
		 break | 
		 goto Name | 
		 do block end | 
		 while exp do block end | 
		 repeat block until exp | 
		 if exp then block {elseif exp then block} [else block] end | 
		 for Name ‘=’ exp ‘,’ exp [‘,’ exp] do block end | 
		 for namelist in explist do block end | 
		 function funcname funcbody | 
		 local function Name funcbody | 
		 local attnamelist [‘=’ explist] 

	attnamelist ::=  Name attrib {‘,’ Name attrib}

	attrib ::= [‘<’ Name ‘>’]

	retstat ::= return [explist] [‘;’]

	label ::= ‘::’ Name ‘::’

	funcname ::= Name {‘.’ Name} [‘:’ Name]

	varlist ::= var {‘,’ var}

	var ::=  Name | prefixexp ‘[’ exp ‘]’ | prefixexp ‘.’ Name 

	namelist ::= Name {‘,’ Name}

	explist ::= exp {‘,’ exp}

	exp ::=  nil | false | true | Numeral | LiteralString | ‘...’ | functiondef | 
		 prefixexp | tableconstructor | exp binop exp | unop exp 

	prefixexp ::= var | functioncall | ‘(’ exp ‘)’

	functioncall ::=  prefixexp args | prefixexp ‘:’ Name args 

	args ::=  ‘(’ [explist] ‘)’ | tableconstructor | LiteralString 

	functiondef ::= function funcbody

	funcbody ::= ‘(’ [parlist] ‘)’ block end

	parlist ::= namelist [‘,’ ‘...’] | ‘...’

	tableconstructor ::= ‘{’ [fieldlist] ‘}’

	fieldlist ::= field {fieldsep field} [fieldsep]

	field ::= ‘[’ exp ‘]’ ‘=’ exp | Name ‘=’ exp | exp

	fieldsep ::= ‘,’ | ‘;’

	binop ::=  ‘+’ | ‘-’ | ‘*’ | ‘/’ | ‘//’ | ‘^’ | ‘%’ | 
		 ‘&’ | ‘~’ | ‘|’ | ‘>>’ | ‘<<’ | ‘..’ | 
		 ‘<’ | ‘<=’ | ‘>’ | ‘>=’ | ‘==’ | ‘~=’ | 
		 and | or

	unop ::= ‘-’ | not | ‘#’ | ‘~’

```
