const std = @import("std");
const token = @import("token.zig");
const lex = @import("lexer.zig");
const ast = @import("ast.zig");
const TokenType = token.TokenType;
const AllocError = std.mem.Allocator.Error;
const LexError = lex.Lexer.LexError;

/// Parser struct that handles recursive descent pratt parsing of moglang
pub const Parser = struct {
    lexer: *lex.Lexer,
    current_token: *token.Token,
    peek_token: *token.Token,
    parser_error: ?*const []u8,
    alloc: std.heap.ArenaAllocator,
    err: ?ParserErrorInfo,

    // Parser error
    pub const ParserError = error{ fail, unexpected_token };
    const ParserErrorType = enum { unexpected_token, unexpected_token_any };
    const SuperError = (LexError || ParserError || AllocError || std.fmt.ParseIntError);
    const PrefixParseFn = *const fn (*Parser) SuperError!*ast.Expression;
    const InfixParseFn = *const fn (*Parser, *ast.Expression) SuperError!*ast.Expression;

    // Precedence defines precedence for Pratt parsing infix operators
    // Ordered from lowest to highest precedence (matching Lua 5.4 spec)
    const Precedence = enum(u8) {
        lowest,
        logical_or, // or
        logical_and, // and
        comparison, // <, >, <=, >=, ~=, ==
        bitwise_or, // |
        bitwise_xor, // ~
        bitwise_and, // &
        bitwise_shift, // <<, >>
        concat, // .. (right-associative)
        sum, // +, -
        product, // *, /, //, %
        unary, // not, #, -, ~ (unary)
        exponent, // ^ (right-associative)
        call, // function calls

        const TokenPrecedenceMap = std.StaticStringMap(Precedence).initComptime(.{
            .{ "t_or", Precedence.logical_or },
            .{ "t_and", Precedence.logical_and },
            .{ "eq", Precedence.comparison },
            .{ "neq", Precedence.comparison },
            .{ "lt", Precedence.comparison },
            .{ "gt", Precedence.comparison },
            .{ "lte", Precedence.comparison },
            .{ "gte", Precedence.comparison },
            .{ "pipe", Precedence.bitwise_or },
            .{ "tilde", Precedence.bitwise_xor },
            .{ "ampersand", Precedence.bitwise_and },
            .{ "lshift", Precedence.bitwise_shift },
            .{ "rshift", Precedence.bitwise_shift },
            .{ "dotdot", Precedence.concat },
            .{ "plus", Precedence.sum },
            .{ "minus", Precedence.sum },
            .{ "asterisk", Precedence.product },
            .{ "slash", Precedence.product },
            .{ "doubleslash", Precedence.product },
            .{ "percent", Precedence.product },
            .{ "caret", Precedence.exponent },
            .{ "lparen", Precedence.call },
            .{ "lbracket", Precedence.call },
            .{ "dot", Precedence.call },
            .{ "colon", Precedence.call },
            .{ "lbrace", Precedence.call },
            .{ "string", Precedence.call },
        });

        fn ofToken(t: TokenType) Precedence {
            return TokenPrecedenceMap.get(@tagName(t)) orelse Precedence.lowest;
        }
    };

    const ParserErrorInfo = union(ParserErrorType) { unexpected_token: struct { expected_token_type: TokenType, actual_token: token.Token }, unexpected_token_any: struct { expected_token_types: []const TokenType, actual_token: token.Token } };

    const InfixMap = std.StaticStringMap(InfixParseFn).initComptime(.{
        // Arithmetic operators
        .{ "plus", &parseInfixExpression },
        .{ "minus", &parseInfixExpression },
        .{ "asterisk", &parseInfixExpression },
        .{ "slash", &parseInfixExpression },
        .{ "doubleslash", &parseInfixExpression },
        .{ "percent", &parseInfixExpression },
        .{ "caret", &parseInfixExpression },
        // Comparison operators
        .{ "eq", &parseInfixExpression },
        .{ "neq", &parseInfixExpression },
        .{ "lt", &parseInfixExpression },
        .{ "gt", &parseInfixExpression },
        .{ "lte", &parseInfixExpression },
        .{ "gte", &parseInfixExpression },
        // Logical operators
        .{ "t_and", &parseInfixExpression },
        .{ "t_or", &parseInfixExpression },
        // Bitwise operators
        .{ "ampersand", &parseInfixExpression },
        .{ "pipe", &parseInfixExpression },
        .{ "tilde", &parseInfixExpression },
        .{ "lshift", &parseInfixExpression },
        .{ "rshift", &parseInfixExpression },
        // String concatenation
        .{ "dotdot", &parseInfixExpression },
        // Index and member access
        .{ "lbracket", &parseIndexExpression },
        .{ "dot", &parseMemberExpression },
        // Function calls
        .{ "lparen", &parseFunctionCall },
        .{ "colon", &parseMethodCall },
        .{ "lbrace", &parseFunctionCall }, // Table constructor as call argument
        .{ "string", &parseFunctionCall }, // String literal as call argument
    });
    const PrefixMap = std.StaticStringMap(PrefixParseFn).initComptime(.{
        // Literals
        .{ "ident", &parseIdentifierExpression },
        .{ "int", &parseIntegerExpression },
        .{ "float", &parseFloatExpression },
        .{ "string", &parseStringExpression },
        .{ "true", &parseBooleanExpression },
        .{ "false", &parseBooleanExpression },
        .{ "nil", &parseNilExpression },
        .{ "dotdotdot", &parseVarargsExpression },
        // Unary operators
        .{ "not", &parsePrefixExpression },
        .{ "minus", &parsePrefixExpression },
        .{ "hash", &parsePrefixExpression },
        .{ "tilde", &parsePrefixExpression },
        // Control flow
        .{ "t_if", &parseConditionalExpression },
        // Function definitions
        .{ "function", &parseFunctionDefExpression },
        // Table constructors
        .{ "lbrace", &parseTableConstructor },
        // Grouping
        .{ "lparen", &parseParenthesizedExpression },
    });

    /// Initialize the Parser
    /// This creates an arena allocator to create AST nodes
    pub fn init(allocator: std.mem.Allocator, lexer: *lex.Lexer) !Parser {
        const alloc = std.heap.ArenaAllocator.init(allocator);

        // Read first two tokens to initialize stat
        const first = (try lexer.nextToken());
        const second = (try lexer.nextToken());

        const parser = Parser{
            .lexer = lexer,
            .current_token = first,
            .peek_token = second,
            .parser_error = null,
            .alloc = alloc,
            .err = null,
        };
        return parser;
    }

    /// Deinits the parser
    /// Frees all AST nodes allocated by the parser
    pub fn deinit(self: *Parser) void {
        self.alloc.deinit();
    }

    /// Prints the error stored in the parser struct
    pub fn printError(self: *Parser) void {
        if (self.err == null) {
            std.log.err("Encountered unknown error while parsing", .{});
        }
        switch (self.err.?) {
            .unexpected_token => |e| {
                std.log.err("Expected token type: {s} but got {s} at position {d}..{d}", .{ @tagName(e.expected_token_type), @tagName(self.current_token.type), self.current_token.start_pos, self.current_token.end_pos });
            },
            .unexpected_token_any => |e| {
                std.log.err("Expected any of tokens: {any} but got \"{s}\" at position {d}..{d}", .{ e.expected_token_types, @tagName(self.current_token.type), self.current_token.start_pos, self.current_token.end_pos });
            },
        }
    }

    /// Read another token
    fn nextToken(self: *Parser) !void {
        self.current_token = self.peek_token;
        const new_token = try self.alloc.allocator().create(token.Token);
        new_token.* = (try self.lexer.nextToken()).*;
        self.peek_token = new_token;
    }

    fn setPeekError(self: *Parser, t: TokenType) !void {
        const msg = try std.fmt.allocPrint(
            self.alloc.allocator(),
            "expected next token to be {s}, got {s} instead at position {d}..{d}",
            .{
                @tagName(t),
                @tagName(self.peek_token.type),
                self.peek_token.start_pos,
                self.peek_token.end_pos,
            },
        );
        self.parser_error = &msg;
    }

    fn currentTokenIs(self: *Parser, t: TokenType) bool {
        return self.current_token.type == t;
    }

    fn peekTokenIs(self: *Parser, t: TokenType) bool {
        return self.peek_token.type == t;
    }

    // TODO: change this to just try with error, not returning boolean
    fn expectAndPeek(self: *Parser, t: TokenType) bool {
        if (self.peekTokenIs(t)) {
            self.nextToken() catch return false;
            return true;
        }
        self.setPeekError(t) catch {};
        return false;
    }

    fn currentPrecedence(self: *Parser) Precedence {
        return Precedence.ofToken(self.current_token.*.type);
    }
    fn peekPrecedence(self: *Parser) Precedence {
        return Precedence.ofToken(self.peek_token.*.type);
    }

    // parseBlock parses until it reaches an `else` or `end`
    fn parseBlock(self: *Parser) !*ast.Block {
        var block = try self.alloc.allocator().create(ast.Block);
        var statements = try std.ArrayList(*ast.Statement).initCapacity(self.alloc.allocator(), 8);
        while (!self.currentTokenIs(.t_else) and !self.currentTokenIs(.end)) {
            const stmt = try self.parseStatement();
            try statements.append(self.alloc.allocator(), stmt);
            try self.nextToken();
        }
        block.statements = statements.items;

        return block;
    }

    /// Parses a mog program, which is a collection of statements
    /// Recursively descends to return a Program AST node
    pub fn parseProgram(self: *Parser) !*ast.Program {
        var program = try self.alloc.allocator().create(ast.Program);
        var statements = try std.ArrayList(*ast.Statement).initCapacity(self.alloc.allocator(), 64);
        while (self.current_token.type != .eof) {
            const stmt = self.parseStatement() catch |err| {
                if (err == ParserError.fail) {
                    self.printError();
                }
                return err;
            };
            try statements.append(self.alloc.allocator(), stmt);
            try self.nextToken();
        }
        program.statements = statements.items;

        return program;
    }

    fn parseStatement(self: *Parser) SuperError!*ast.Statement {
        const stmt = try self.alloc.allocator().create(ast.Statement);
        switch (self.current_token.type) {
            .local => {
                if (self.peekTokenIs(.ident)) {
                    stmt.* = .{ .Assignment = try self.parseAssignmentStatement(true) };
                } else if (self.peekTokenIs(.function)) {
                    stmt.* = .{ .FunctionDeclaration = try self.parseFunctionDeclaration(true) };
                } else {
                    self.setErrExpectedAny(&[_]TokenType{ .ident, .function });
                    return ParserError.fail;
                }
            },
            .function => {
                // Check if next token is identifier (function declaration) or '(' (function expression)
                if (self.peekTokenIs(.ident)) {
                    stmt.* = .{ .FunctionDeclaration = try self.parseFunctionDeclaration(false) };
                } else {
                    stmt.* = .{ .Expression = try self.parseExpressionStatement() };
                }
            },
            .ident => {
                // Look ahead to determine if this is an assignment or expression
                if (self.peekTokenIs(.assign) or self.peekTokenIs(.comma)) {
                    stmt.* = .{ .Assignment = try self.parseAssignmentStatement(false) };
                } else {
                    stmt.* = .{ .Expression = try self.parseExpressionStatement() };
                }
            },
            .t_return => {
                stmt.* = .{ .Return = try self.parseReturnStatement() };
            },
            .do => {
                stmt.* = .{ .Do = try self.parseDoStatement() };
            },
            .t_while => {
                stmt.* = .{ .While = try self.parseWhileStatement() };
            },
            .repeat => {
                stmt.* = .{ .Repeat = try self.parseRepeatStatement() };
            },
            .t_for => {
                return try self.parseForStatement();
            },
            .t_break => {
                const brk = try self.alloc.allocator().create(ast.BreakStatement);
                brk.* = .{ .token = self.current_token.* };
                stmt.* = .{ .Break = brk };
            },
            else => {
                stmt.* = .{ .Expression = try self.parseExpressionStatement() };
            },
        }
        return stmt;
    }

    fn parseReturnStatement(self: *Parser) !*ast.ReturnStatement {
        const rs = try self.alloc.allocator().create(ast.ReturnStatement);
        rs.token = self.current_token.*;
        if (self.peekTokenIs(.semicolon) or self.peekTokenIs(.end)) {
            rs.expr = null;
            try self.nextToken();
            return rs;
        }
        try self.nextToken();
        // TODO: handle multiple return values
        rs.expr = try self.parseExpression(Precedence.lowest);

        // TODO: support bare return without semicolon ending
        if (self.peekTokenIs(TokenType.semicolon)) {
            try self.nextToken();
        }
        return rs;
    }

    fn parseDoStatement(self: *Parser) !*ast.DoStatement {
        const ds = try self.alloc.allocator().create(ast.DoStatement);
        ds.token = self.current_token.*; // 'do' token
        try self.nextToken(); // move past 'do'
        ds.block = try self.parseBlock();
        // parseBlock leaves us at 'end'
        if (!self.currentTokenIs(.end)) {
            self.setErrExpected(.end);
            return ParserError.fail;
        }
        return ds;
    }

    fn parseWhileStatement(self: *Parser) !*ast.WhileStatement {
        const ws = try self.alloc.allocator().create(ast.WhileStatement);
        ws.token = self.current_token.*; // 'while' token
        try self.nextToken(); // move past 'while'
        ws.condition = try self.parseExpression(Precedence.lowest);
        // Expect 'do' after condition
        if (!self.expectAndPeek(.do)) {
            self.setErrExpected(.do);
            return ParserError.fail;
        }
        try self.nextToken(); // move past 'do'
        ws.block = try self.parseBlock();
        // parseBlock leaves us at 'end'
        if (!self.currentTokenIs(.end)) {
            self.setErrExpected(.end);
            return ParserError.fail;
        }
        return ws;
    }

    fn parseRepeatStatement(self: *Parser) !*ast.RepeatStatement {
        const rs = try self.alloc.allocator().create(ast.RepeatStatement);
        rs.token = self.current_token.*; // 'repeat' token
        try self.nextToken(); // move past 'repeat'

        // Parse block until we hit 'until'
        var block = try self.alloc.allocator().create(ast.Block);
        var statements = try std.ArrayList(*ast.Statement).initCapacity(self.alloc.allocator(), 8);
        while (!self.currentTokenIs(.until)) {
            const stmt = try self.parseStatement();
            try statements.append(self.alloc.allocator(), stmt);
            try self.nextToken();
        }
        block.statements = statements.items;
        rs.block = block;

        // We should be at 'until' now
        if (!self.currentTokenIs(.until)) {
            self.setErrExpected(.until);
            return ParserError.fail;
        }
        try self.nextToken(); // move past 'until'
        rs.condition = try self.parseExpression(Precedence.lowest);
        return rs;
    }

    /// Parse for statement - dispatches to numeric or generic based on lookahead
    /// Grammar: for Name '=' exp ',' exp [',' exp] do block end (numeric)
    ///       OR for namelist in explist do block end (generic)
    fn parseForStatement(self: *Parser) !*ast.Statement {
        const for_token = self.current_token.*; // 'for' token

        // Expect identifier after 'for'
        if (!self.expectAndPeek(.ident)) {
            self.setErrExpected(.ident);
            return ParserError.fail;
        }
        // Now current token is the identifier

        const first_ident = try self.parseIdentifier();

        // Disambiguate: check next token
        // If '=' -> numeric for
        // If ',' or 'in' -> generic for
        if (self.peekTokenIs(.assign)) {
            return try self.parseForNumericStatement(for_token, first_ident);
        } else if (self.peekTokenIs(.comma) or self.peekTokenIs(.in)) {
            return try self.parseForGenericStatement(for_token, first_ident);
        } else {
            self.setErrExpectedAny(&[_]TokenType{ .assign, .comma, .in });
            return ParserError.fail;
        }
    }

    /// Parse numeric for statement
    /// Grammar: for Name '=' exp ',' exp [',' exp] do block end
    /// first_ident is already parsed
    fn parseForNumericStatement(self: *Parser, for_token: token.Token, first_ident: *ast.Identifier) !*ast.Statement {
        const stmt = try self.alloc.allocator().create(ast.Statement);
        const fns = try self.alloc.allocator().create(ast.ForNumericStatement);
        fns.token = for_token;
        fns.var_name = first_ident;

        // Expect '=' after variable name
        if (!self.expectAndPeek(.assign)) {
            self.setErrExpected(.assign);
            return ParserError.fail;
        }
        try self.nextToken(); // move past '='

        // Parse start expression
        fns.start = try self.parseExpression(Precedence.lowest);

        // Expect comma after start
        if (!self.expectAndPeek(.comma)) {
            self.setErrExpected(.comma);
            return ParserError.fail;
        }
        try self.nextToken(); // move past comma

        // Parse end expression
        fns.end = try self.parseExpression(Precedence.lowest);

        // Check for optional step expression
        if (self.peekTokenIs(.comma)) {
            try self.nextToken(); // consume comma
            try self.nextToken(); // move to step expression
            fns.step = try self.parseExpression(Precedence.lowest);
        } else {
            fns.step = null;
        }

        // Expect 'do' keyword
        if (!self.expectAndPeek(.do)) {
            self.setErrExpected(.do);
            return ParserError.fail;
        }
        try self.nextToken(); // move past 'do'

        // Parse block
        fns.block = try self.parseBlock();

        // Should be at 'end'
        if (!self.currentTokenIs(.end)) {
            self.setErrExpected(.end);
            return ParserError.fail;
        }

        stmt.* = .{ .ForNumeric = fns };
        return stmt;
    }

    /// Parse generic for statement
    /// Grammar: for namelist in explist do block end
    /// first_ident is already parsed
    fn parseForGenericStatement(self: *Parser, for_token: token.Token, first_ident: *ast.Identifier) !*ast.Statement {
        const stmt = try self.alloc.allocator().create(ast.Statement);
        const fgs = try self.alloc.allocator().create(ast.ForGenericStatement);
        fgs.token = for_token;
        fgs.allocator = self.alloc.allocator();

        // Create name list starting with first_ident
        const name_list = try self.alloc.allocator().create(ast.NameList);
        name_list.* = try ast.NameList.init(self.alloc.allocator(), first_ident);

        // Parse additional identifiers if present (comma-separated)
        while (self.peekTokenIs(.comma)) {
            try self.nextToken(); // consume comma

            // Check if next token is 'in' - would be an error
            if (self.peekTokenIs(.in)) {
                break; // Will fail on expectAndPeek below
            }

            if (!self.expectAndPeek(.ident)) {
                self.setErrExpected(.ident);
                return ParserError.fail;
            }
            const next_ident = try self.parseIdentifier();
            try name_list.add(next_ident);
        }

        fgs.names = name_list;

        // Expect 'in' keyword
        if (!self.expectAndPeek(.in)) {
            self.setErrExpected(.in);
            return ParserError.fail;
        }
        try self.nextToken(); // move past 'in'

        // Parse expression list
        fgs.expressions = try self.parseExpressionList();

        // Expect 'do' keyword
        if (!self.expectAndPeek(.do)) {
            self.setErrExpected(.do);
            return ParserError.fail;
        }
        try self.nextToken(); // move past 'do'

        // Parse block
        fgs.block = try self.parseBlock();

        // Should be at 'end'
        if (!self.currentTokenIs(.end)) {
            self.setErrExpected(.end);
            return ParserError.fail;
        }

        stmt.* = .{ .ForGeneric = fgs };
        return stmt;
    }

    fn parseAssignmentStatement(self: *Parser, is_local: bool) !*ast.AssignmentStatement {
        const as = try self.alloc.allocator().create(ast.AssignmentStatement);
        as.is_local = is_local;

        if (is_local) {
            as.token = self.current_token.*; // 'local' token
            if (!self.expectAndPeek(.ident)) {
                self.setErrExpected(.ident);
                return ParserError.fail;
            }
        } else {
            as.token = self.current_token.*; // first identifier token
        }

        // Parse the first identifier
        const first_ident = try self.parseIdentifier();
        const name_list = try self.alloc.allocator().create(ast.NameList);
        name_list.* = try ast.NameList.init(self.alloc.allocator(), first_ident);

        // Parse additional identifiers if present (comma-separated)
        while (self.peekTokenIs(.comma)) {
            try self.nextToken(); // consume comma
            if (!self.expectAndPeek(.ident)) {
                self.setErrExpected(.ident);
                return ParserError.fail;
            }
            const next_ident = try self.parseIdentifier();
            try name_list.add(next_ident);
        }

        as.names = name_list;

        // Expect assignment operator
        if (!self.expectAndPeek(.assign)) {
            self.setErrExpected(.assign);
            return ParserError.fail;
        }
        try self.nextToken();

        // TODO: parse multiple expressions on RHS
        const expr = try self.parseExpression(Precedence.lowest);
        as.expr = expr;

        if (self.peekTokenIs(TokenType.semicolon)) {
            try self.nextToken();
        }

        return as;
    }

    fn parseExpressionStatement(self: *Parser) !*ast.ExpressionStatement {
        const es = try self.alloc.allocator().create(ast.ExpressionStatement);
        es.expr = try self.parseExpression(Precedence.lowest);
        if (self.peekTokenIs(TokenType.semicolon)) {
            try self.nextToken();
        }
        return es;
    }

    fn parsePrefixExpression(self: *Parser) SuperError!*ast.Expression {
        const expr = try self.alloc.allocator().create(ast.Expression);
        const prefix_expr = try self.alloc.allocator().create(ast.PrefixExpression);

        prefix_expr.token = self.current_token.*;
        prefix_expr.operator = prefix_expr.token.literal;
        try self.nextToken();
        prefix_expr.expression = try self.parseExpression(Precedence.lowest);

        expr.* = .{ .Prefix = prefix_expr };
        return expr;
    }

    fn parseInfixExpression(self: *Parser, left: *ast.Expression) SuperError!*ast.Expression {
        const expr = try self.alloc.allocator().create(ast.Expression);
        const infix_expr = try self.alloc.allocator().create(ast.InfixExpression);

        infix_expr.token = self.current_token.*;
        infix_expr.operator = infix_expr.token.literal;
        infix_expr.left = left;
        const prec = (self.currentPrecedence());
        try self.nextToken();

        // Right-associative operators: ^, ..
        // For right-associativity, we parse the right side with one less precedence
        const is_right_assoc = std.mem.eql(u8, infix_expr.operator, "^") or
            std.mem.eql(u8, infix_expr.operator, "..");
        const right_prec = if (is_right_assoc)
            @as(Precedence, @enumFromInt(@intFromEnum(prec) - 1))
        else
            prec;

        infix_expr.right = try self.parseExpression(right_prec);

        expr.* = .{ .Infix = infix_expr };
        return expr;
    }

    fn parseIndexExpression(self: *Parser, left: *ast.Expression) SuperError!*ast.Expression {
        const expr = try self.alloc.allocator().create(ast.Expression);
        const index_expr = try self.alloc.allocator().create(ast.IndexExpression);

        index_expr.token = self.current_token.*;
        index_expr.object = left;

        // Move past the '['
        try self.nextToken();

        // Parse the index expression
        index_expr.index = try self.parseExpression(.lowest);

        // Expect closing ']'
        if (!self.expectAndPeek(.rbracket)) {
            self.setErrExpected(.rbracket);
            return ParserError.fail;
        }

        expr.* = .{ .Index = index_expr };
        return expr;
    }

    fn parseMemberExpression(self: *Parser, left: *ast.Expression) SuperError!*ast.Expression {
        const expr = try self.alloc.allocator().create(ast.Expression);
        const member_expr = try self.alloc.allocator().create(ast.MemberExpression);

        member_expr.token = self.current_token.*;
        member_expr.object = left;

        // Expect identifier after dot
        if (!self.expectAndPeek(.ident)) {
            self.setErrExpected(.ident);
            return ParserError.fail;
        }

        // Parse the field name as an identifier
        const field = try self.alloc.allocator().create(ast.Identifier);
        field.token = self.current_token.*;
        field.value = self.current_token.literal;
        member_expr.field = field;

        expr.* = .{ .Member = member_expr };
        return expr;
    }

    fn parseConditionalExpression(self: *Parser) SuperError!*ast.Expression {
        const expr = try self.alloc.allocator().create(ast.Expression);
        const cond_expr = try self.alloc.allocator().create(ast.ConditionalExpression);
        cond_expr.token = self.current_token.*;
        try self.nextToken();

        // on condition
        cond_expr.condition = try self.parseExpression(.lowest);
        if (!self.expectAndPeek(.then)) {
            self.setErrExpected(.then);
            return ParserError.fail;
        }
        try self.nextToken();
        cond_expr.then_block = try self.parseBlock();
        if (self.currentTokenIs(.t_else)) {
            try self.nextToken();
            cond_expr.else_block = try self.parseBlock();
            if (self.currentTokenIs(.t_else)) {
                self.setErrExpected(.t_else);
                return ParserError.fail;
            }
        } else {
            cond_expr.else_block = null;
        }
        expr.* = .{ .Conditional = cond_expr };
        return expr;
    }

    fn parseParenthesizedExpression(self: *Parser) SuperError!*ast.Expression {
        // drop references to the parens
        try self.nextToken();
        const expr = try self.parseExpression(.lowest);
        if (!self.expectAndPeek(.rparen)) {
            self.setErrExpected(.rparen);
            return ParserError.fail;
        }
        return expr;
    }

    fn parseExpression(self: *Parser, precedence: Precedence) SuperError!*ast.Expression {
        var left = try self.alloc.allocator().create(ast.Expression);
        const prefix = PrefixMap.get(@tagName(self.current_token.type));
        if (prefix == null) {
            const err = try std.fmt.allocPrint(self.alloc.allocator(), "no prefix function found for token {s}", .{@tagName(self.current_token.type)});
            self.parser_error = &err;
            return ParserError.fail;
        }
        left = try prefix.?(self);

        while (!self.peekTokenIs(TokenType.semicolon) and @intFromEnum(precedence) < @intFromEnum(self.peekPrecedence())) {
            const infix = InfixMap.get(@tagName(self.peek_token.type)) orelse return left;
            try self.nextToken();
            left = try infix(self, left);
        }
        return left;
    }

    fn parseIdentifier(self: *Parser) !*ast.Identifier {
        const ident = try self.alloc.allocator().create(ast.Identifier);
        ident.token = self.current_token.*;
        ident.value = self.current_token.*.literal;

        return ident;
    }

    fn parseIdentifierExpression(self: *Parser) !*ast.Expression {
        const expr = try self.alloc.allocator().create(ast.Expression);
        const ident = try self.parseIdentifier();
        expr.* = .{ .Identifier = ident };
        return expr;
    }

    fn parseIntegerExpression(self: *Parser) !*ast.Expression {
        const expr = try self.alloc.allocator().create(ast.Expression);
        const int = try self.alloc.allocator().create(ast.NumberLiteral);
        int.value = .{ .Integer = try std.fmt.parseInt(i32, self.current_token.literal, 10) };
        expr.* = .{ .Number = int };
        return expr;
    }

    fn parseFloatExpression(self: *Parser) !*ast.Expression {
        const expr = try self.alloc.allocator().create(ast.Expression);
        const float = try self.alloc.allocator().create(ast.NumberLiteral);
        float.value = .{ .Float = try std.fmt.parseFloat(f32, self.current_token.literal) };
        expr.* = .{ .Number = float };
        return expr;
    }

    fn parseStringExpression(self: *Parser) !*ast.Expression {
        const expr = try self.alloc.allocator().create(ast.Expression);
        const str = try self.alloc.allocator().create(ast.StringLiteral);
        str.token = self.current_token.*;

        // cut out the string literal quotations to keep the value only
        const len = self.current_token.end_pos - self.current_token.start_pos;
        str.value = self.current_token.literal[1..len];
        expr.* = .{ .String = str };
        return expr;
    }

    fn parseNilExpression(self: *Parser) !*ast.Expression {
        const expr = try self.alloc.allocator().create(ast.Expression);
        const nil = try self.alloc.allocator().create(ast.Nil);
        nil.token = self.current_token.*;
        expr.* = .{ .Nil = nil };
        return expr;
    }

    fn parseVarargsExpression(self: *Parser) !*ast.Expression {
        const expr = try self.alloc.allocator().create(ast.Expression);
        const varargs = try self.alloc.allocator().create(ast.Varargs);
        varargs.token = self.current_token.*;
        expr.* = .{ .Varargs = varargs };
        return expr;
    }

    /// Parse a boolean expression - errors if value is not true or false
    fn parseBooleanExpression(self: *Parser) !*ast.Expression {
        const expr = try self.alloc.allocator().create(ast.Expression);
        const boolean = try self.alloc.allocator().create(ast.BooleanLiteral);
        boolean.token = self.current_token.*;
        if (std.mem.eql(u8, self.current_token.literal, "true")) {
            boolean.value = true;
        } else if (std.mem.eql(u8, self.current_token.literal, "false")) {
            boolean.value = false;
        } else {
            self.setErrExpectedAny(&[_]TokenType{ .true, .false });
            return ParserError.fail;
        }
        expr.* = .{ .Boolean = boolean };
        return expr;
    }

    /// Parse a parameter list for function definitions
    /// Grammar: namelist [',' '...'] | '...'
    fn parseParamList(self: *Parser) !*ast.ParamList {
        const param_list = try self.alloc.allocator().create(ast.ParamList);

        // Check if it's just varargs
        if (self.currentTokenIs(.dotdotdot)) {
            param_list.* = try ast.ParamList.initVarargs(self.alloc.allocator(), self.current_token.*);
            return param_list;
        }

        // Parse first parameter (must be identifier)
        if (!self.currentTokenIs(.ident)) {
            self.setErrExpected(.ident);
            return ParserError.fail;
        }
        const first_param = try self.parseIdentifier();
        param_list.* = try ast.ParamList.init(self.alloc.allocator(), first_param);

        // Parse additional parameters
        while (self.peekTokenIs(.comma)) {
            try self.nextToken(); // consume comma
            try self.nextToken(); // move to next param

            // Check if this is trailing varargs
            if (self.currentTokenIs(.dotdotdot)) {
                param_list.setVarargs();
                return param_list;
            }

            // Must be an identifier
            if (!self.currentTokenIs(.ident)) {
                self.setErrExpected(.ident);
                return ParserError.fail;
            }
            const param = try self.parseIdentifier();
            try param_list.add(param);
        }

        return param_list;
    }

    /// Parse a comma-separated list of expressions
    /// Grammar: exp {',' exp}
    /// Returns an ArrayList of expressions
    fn parseExpressionList(self: *Parser) !std.ArrayList(*ast.Expression) {
        var expressions = try std.ArrayList(*ast.Expression).initCapacity(self.alloc.allocator(), 4);

        // Parse first expression
        const first_expr = try self.parseExpression(Precedence.lowest);
        try expressions.append(self.alloc.allocator(), first_expr);

        // Parse additional comma-separated expressions
        while (self.peekTokenIs(.comma)) {
            try self.nextToken(); // consume comma
            try self.nextToken(); // move to next expression
            const expr = try self.parseExpression(Precedence.lowest);
            try expressions.append(self.alloc.allocator(), expr);
        }

        return expressions;
    }

    /// Parse a function body
    /// Grammar: '(' [parlist] ')' block end
    fn parseFunctionBody(self: *Parser) !*ast.FunctionBody {
        const body = try self.alloc.allocator().create(ast.FunctionBody);

        // Expect opening paren
        if (!self.expectAndPeek(.lparen)) {
            self.setErrExpected(.lparen);
            return ParserError.fail;
        }
        body.token = self.current_token.*; // lparen token

        // Check if there are parameters
        try self.nextToken(); // move past lparen
        if (self.currentTokenIs(.rparen)) {
            // No parameters
            body.params = null;
        } else {
            // Parse parameter list
            body.params = try self.parseParamList();
            // Move to rparen
            if (!self.expectAndPeek(.rparen)) {
                self.setErrExpected(.rparen);
                return ParserError.fail;
            }
        }

        // Move past rparen and parse block
        try self.nextToken();
        body.block = try self.parseBlock();

        // Current token should be 'end'
        if (!self.currentTokenIs(.end)) {
            self.setErrExpected(.end);
            return ParserError.fail;
        }

        return body;
    }

    /// Parse a function definition expression
    /// Grammar: function funcbody
    fn parseFunctionDefExpression(self: *Parser) !*ast.Expression {
        const expr = try self.alloc.allocator().create(ast.Expression);
        const func_def = try self.alloc.allocator().create(ast.FunctionDefExpression);

        func_def.token = self.current_token.*; // 'function' token
        func_def.body = try self.parseFunctionBody();

        expr.* = .{ .FunctionDef = func_def };
        return expr;
    }

    /// Parse a function decl
    fn parseFunctionDeclaration(self: *Parser, is_local: bool) !*ast.FunctionDeclaration {
        const func = try self.alloc.allocator().create(ast.FunctionDeclaration);

        func.is_local = is_local;

        // If local, current is 'local', need to advance to 'function'
        if (is_local) {
            try self.nextToken();
        }

        func.token = self.current_token.*; // 'function' token

        // Advance to identifier
        try self.nextToken();

        // parse name
        func.name = try self.parseIdentifier();

        // parse body
        func.body = try self.parseFunctionBody();

        return func;
    }

    /// Parse a single field in a table constructor
    /// Grammar: '[' exp ']' '=' exp | Name '=' exp | exp
    /// Disambiguates based on current token:
    /// - '[' -> ComputedKey
    /// - ident + '=' -> RecordStyle
    /// - otherwise -> ArrayStyle
    fn parseField(self: *Parser) !*ast.Field {
        const field = try self.alloc.allocator().create(ast.Field);

        // Check for computed key: [expr] = expr
        if (self.currentTokenIs(.lbracket)) {
            try self.nextToken(); // move past '['
            const key_expr = try self.parseExpression(Precedence.lowest);

            // Expect ']'
            if (!self.expectAndPeek(.rbracket)) {
                self.setErrExpected(.rbracket);
                return ParserError.fail;
            }

            // Expect '='
            if (!self.expectAndPeek(.assign)) {
                self.setErrExpected(.assign);
                return ParserError.fail;
            }
            try self.nextToken(); // move past '='

            const value_expr = try self.parseExpression(Precedence.lowest);
            field.* = .{ .ComputedKey = .{ .key = key_expr, .value = value_expr } };
            return field;
        }

        // Check for record style: Name = exp
        // Need to peek ahead to see if there's an '=' after an identifier
        if (self.currentTokenIs(.ident) and self.peekTokenIs(.assign)) {
            const name = try self.parseIdentifier();
            try self.nextToken(); // move to '='
            try self.nextToken(); // move past '='

            const value_expr = try self.parseExpression(Precedence.lowest);
            field.* = .{ .RecordStyle = .{ .name = name, .value = value_expr } };
            return field;
        }

        // Otherwise it's array style: just an expression
        const expr = try self.parseExpression(Precedence.lowest);
        field.* = .{ .ArrayStyle = expr };
        return field;
    }

    /// Parse a field list (comma or semicolon separated fields with optional trailing separator)
    /// Grammar: field {fieldsep field} [fieldsep]
    /// Returns an ArrayList of fields
    fn parseFieldList(self: *Parser) !std.ArrayList(*ast.Field) {
        var fields = try std.ArrayList(*ast.Field).initCapacity(self.alloc.allocator(), 4);

        // Parse first field
        const first_field = try self.parseField();
        try fields.append(self.alloc.allocator(), first_field);

        // Parse additional fields separated by comma or semicolon
        while (self.peekTokenIs(.comma) or self.peekTokenIs(.semicolon)) {
            try self.nextToken(); // consume separator

            // Check for closing brace (trailing separator case)
            if (self.peekTokenIs(.rbrace)) {
                break;
            }

            try self.nextToken(); // move to next field
            const field = try self.parseField();
            try fields.append(self.alloc.allocator(), field);
        }

        return fields;
    }

    /// Parse a table constructor expression
    /// Grammar: '{' [fieldlist] '}'
    fn parseTableConstructor(self: *Parser) !*ast.Expression {
        const expr = try self.alloc.allocator().create(ast.Expression);
        const table = try self.alloc.allocator().create(ast.TableConstructor);

        table.token = self.current_token.*; // '{' token
        table.allocator = self.alloc.allocator();

        // Check for empty table
        if (self.peekTokenIs(.rbrace)) {
            table.fields = try std.ArrayList(*ast.Field).initCapacity(self.alloc.allocator(), 0);
            try self.nextToken(); // move to '}'
            expr.* = .{ .TableConstructor = table };
            return expr;
        }

        // Parse field list
        try self.nextToken(); // move past '{'
        table.fields = try self.parseFieldList();

        // Expect closing brace
        if (!self.expectAndPeek(.rbrace)) {
            self.setErrExpected(.rbrace);
            return ParserError.fail;
        }

        expr.* = .{ .TableConstructor = table };
        return expr;
    }

    /// Parse function call arguments
    /// Grammar: '(' [explist] ')' | tableconstructor | LiteralString
    /// Current token should be the opening '(', '{', or string
    fn parseArgs(self: *Parser) !ast.CallArgs {
        if (self.currentTokenIs(.lparen)) {
            // Parse expression list in parentheses
            try self.nextToken(); // move past '('

            // Check for empty argument list
            if (self.currentTokenIs(.rparen)) {
                return ast.CallArgs{ .ExpressionList = try std.ArrayList(*ast.Expression).initCapacity(self.alloc.allocator(), 0) };
            }

            // Parse expression list
            const exprs = try self.parseExpressionList();

            // Expect closing paren
            if (!self.expectAndPeek(.rparen)) {
                self.setErrExpected(.rparen);
                return ParserError.fail;
            }

            return ast.CallArgs{ .ExpressionList = exprs };
        } else if (self.currentTokenIs(.lbrace)) {
            // Parse table constructor
            const table_expr = try self.parseTableConstructor();
            // parseTableConstructor returns an Expression, extract the TableConstructor
            return ast.CallArgs{ .TableConstructor = table_expr.TableConstructor };
        } else if (self.currentTokenIs(.string)) {
            // Parse string literal
            const str = try self.alloc.allocator().create(ast.StringLiteral);
            str.token = self.current_token.*;
            const len = self.current_token.end_pos - self.current_token.start_pos;
            str.value = self.current_token.literal[1..len];
            return ast.CallArgs{ .StringLiteral = str };
        } else {
            self.setErrExpectedAny(&[_]TokenType{ .lparen, .lbrace, .string });
            return ParserError.fail;
        }
    }

    /// Parse a function call expression (infix operator)
    /// Grammar: prefixexp args
    /// Current token is '(' when this is called
    fn parseFunctionCall(self: *Parser, left: *ast.Expression) !*ast.Expression {
        const expr = try self.alloc.allocator().create(ast.Expression);
        const call = try self.alloc.allocator().create(ast.FunctionCallExpression);

        call.token = self.current_token.*; // '(', '{', or string token
        call.function = left;
        call.args = try self.parseArgs();

        expr.* = .{ .FunctionCall = call };
        return expr;
    }

    /// Parse a method call expression (infix operator)
    /// Grammar: prefixexp ':' Name args
    /// Current token is ':' when this is called
    fn parseMethodCall(self: *Parser, left: *ast.Expression) !*ast.Expression {
        const expr = try self.alloc.allocator().create(ast.Expression);
        const call = try self.alloc.allocator().create(ast.MethodCallExpression);

        call.token = self.current_token.*; // ':' token
        call.object = left;

        // Expect identifier after ':'
        if (!self.expectAndPeek(.ident)) {
            self.setErrExpected(.ident);
            return ParserError.fail;
        }

        // Parse method name
        const method = try self.alloc.allocator().create(ast.Identifier);
        method.token = self.current_token.*;
        method.value = self.current_token.literal;
        call.method = method;

        // Expect args after method name (can be '(', '{', or string)
        if (!self.peekTokenIs(.lparen) and !self.peekTokenIs(.lbrace) and !self.peekTokenIs(.string)) {
            self.setErrExpectedAny(&[_]TokenType{ .lparen, .lbrace, .string });
            return ParserError.fail;
        }
        try self.nextToken(); // move to args token

        call.args = try self.parseArgs();

        expr.* = .{ .MethodCall = call };
        return expr;
    }

    // Set an "expected x but got y" error into the parser state
    // Uses the current parser token as "actual"
    fn setErrExpected(self: *Parser, expected: TokenType) void {
        self.err = .{ .unexpected_token = .{ .expected_token_type = expected, .actual_token = self.current_token.* } };
    }

    // Set an "expected any of x,y but got z" error into the parser state
    // Uses the current parser token as "actual"
    fn setErrExpectedAny(self: *Parser, expected_token_Types: []const TokenType) void {
        self.err = .{ .unexpected_token_any = .{ .expected_token_types = expected_token_Types, .actual_token = self.current_token.* } };
    }
};
