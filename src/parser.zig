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

    // Parser error
    pub const ParserError = error{fail};
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
        });

        fn ofToken(t: TokenType) Precedence {
            return TokenPrecedenceMap.get(@tagName(t)) orelse Precedence.lowest;
        }
    };

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
        };
        return parser;
    }

    /// Deinits the parser
    /// Frees all AST nodes allocated by the parser
    pub fn deinit(self: *Parser) void {
        self.alloc.deinit();
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
            const stmt = try self.parseStatement();
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
            else => {
                stmt.* = .{ .Expression = try self.parseExpressionStatement() };
            },
        }
        return stmt;
    }

    fn parseReturnStatement(self: *Parser) !*ast.ReturnStatement {
        const rs = try self.alloc.allocator().create(ast.ReturnStatement);
        rs.token = self.current_token.*;
        if (self.peekTokenIs(TokenType.semicolon)) {
            rs.expr = null;
            try self.nextToken();
            return rs;
        }
        try self.nextToken();
        rs.expr = try self.parseExpression(Precedence.lowest);
        if (self.peekTokenIs(TokenType.semicolon)) {
            try self.nextToken();
        }
        return rs;
    }

    fn parseAssignmentStatement(self: *Parser, is_local: bool) !*ast.AssignmentStatement {
        const as = try self.alloc.allocator().create(ast.AssignmentStatement);
        as.is_local = is_local;

        if (is_local) {
            as.token = self.current_token.*; // 'local' token
            if (!self.expectAndPeek(.ident)) {
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
                return ParserError.fail;
            }
            const next_ident = try self.parseIdentifier();
            try name_list.add(next_ident);
        }

        as.names = name_list;

        // Expect assignment operator
        if (!self.expectAndPeek(.assign)) {
            return ParserError.fail;
        }
        try self.nextToken();

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

    fn parseConditionalExpression(self: *Parser) SuperError!*ast.Expression {
        const expr = try self.alloc.allocator().create(ast.Expression);
        const cond_expr = try self.alloc.allocator().create(ast.ConditionalExpression);
        cond_expr.token = self.current_token.*;
        try self.nextToken();

        // on condition
        cond_expr.condition = try self.parseExpression(.lowest);
        if (!self.expectAndPeek(.then)) {
            return ParserError.fail;
        }
        try self.nextToken();
        cond_expr.then_block = try self.parseBlock();
        if (self.currentTokenIs(.t_else)) {
            try self.nextToken();
            cond_expr.else_block = try self.parseBlock();
            if (self.currentTokenIs(.t_else)) {
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
                return ParserError.fail;
            }
            const param = try self.parseIdentifier();
            try param_list.add(param);
        }

        return param_list;
    }

    /// Parse a function body
    /// Grammar: '(' [parlist] ')' block end
    fn parseFunctionBody(self: *Parser) !*ast.FunctionBody {
        const body = try self.alloc.allocator().create(ast.FunctionBody);

        // Expect opening paren
        if (!self.expectAndPeek(.lparen)) {
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
                return ParserError.fail;
            }
        }

        // Move past rparen and parse block
        try self.nextToken();
        body.block = try self.parseBlock();

        // Current token should be 'end'
        if (!self.currentTokenIs(.end)) {
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
};
