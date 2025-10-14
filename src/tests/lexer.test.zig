const std = @import("std");
const token = @import("../token.zig");
const TokenType = token.TokenType;
const Token = token.Token;
const Lexer = @import("../lexer.zig").Lexer;

test "comprehensive token coverage" {
    // Test all currently implemented token types - doesn't need to be valid syntax
    const input =
        \\function local if else elseif then end do while repeat until for in break goto
        \\return type not and or nil true false
        \\identifier ident_w_underscore ident3 42 "string literal"
        \\= + - * / # ~ < > == ~= . >= <=
        \\( ) { } [ ] , ; :
        \\ .. ... % ^ // & | << >> ::
        \\ --
    ;

    const tests = [_]struct {
        expectedType: TokenType,
        expectedLiteral: []const u8,
    }{
        // Keywords
        .{ .expectedType = TokenType.function, .expectedLiteral = "function" },
        .{ .expectedType = TokenType.local, .expectedLiteral = "local" },
        .{ .expectedType = TokenType.t_if, .expectedLiteral = "if" },
        .{ .expectedType = TokenType.t_else, .expectedLiteral = "else" },
        .{ .expectedType = TokenType.elseif, .expectedLiteral = "elseif" },
        .{ .expectedType = TokenType.then, .expectedLiteral = "then" },
        .{ .expectedType = TokenType.end, .expectedLiteral = "end" },
        .{ .expectedType = TokenType.do, .expectedLiteral = "do" },
        .{ .expectedType = TokenType.t_while, .expectedLiteral = "while" },
        .{ .expectedType = TokenType.repeat, .expectedLiteral = "repeat" },
        .{ .expectedType = TokenType.until, .expectedLiteral = "until" },
        .{ .expectedType = TokenType.t_for, .expectedLiteral = "for" },
        .{ .expectedType = TokenType.in, .expectedLiteral = "in" },
        .{ .expectedType = TokenType.t_break, .expectedLiteral = "break" },
        .{ .expectedType = TokenType.goto, .expectedLiteral = "goto" },
        .{ .expectedType = TokenType.t_return, .expectedLiteral = "return" },
        .{ .expectedType = TokenType.t_type, .expectedLiteral = "type" },
        .{ .expectedType = TokenType.not, .expectedLiteral = "not" },
        .{ .expectedType = TokenType.t_and, .expectedLiteral = "and" },
        .{ .expectedType = TokenType.t_or, .expectedLiteral = "or" },
        .{ .expectedType = TokenType.nil, .expectedLiteral = "nil" },
        .{ .expectedType = TokenType.true, .expectedLiteral = "true" },
        .{ .expectedType = TokenType.false, .expectedLiteral = "false" },
        // Literals
        .{ .expectedType = TokenType.ident, .expectedLiteral = "identifier" },
        .{ .expectedType = TokenType.ident, .expectedLiteral = "ident_w_underscore" },
        .{ .expectedType = TokenType.ident, .expectedLiteral = "ident3" },
        .{ .expectedType = TokenType.int, .expectedLiteral = "42" },
        .{ .expectedType = TokenType.string, .expectedLiteral = "\"string literal\"" },
        // Operators
        .{ .expectedType = TokenType.assign, .expectedLiteral = "=" },
        .{ .expectedType = TokenType.plus, .expectedLiteral = "+" },
        .{ .expectedType = TokenType.minus, .expectedLiteral = "-" },
        .{ .expectedType = TokenType.asterisk, .expectedLiteral = "*" },
        .{ .expectedType = TokenType.slash, .expectedLiteral = "/" },
        .{ .expectedType = TokenType.hash, .expectedLiteral = "#" },
        .{ .expectedType = TokenType.tilde, .expectedLiteral = "~" },
        .{ .expectedType = TokenType.lt, .expectedLiteral = "<" },
        .{ .expectedType = TokenType.gt, .expectedLiteral = ">" },
        .{ .expectedType = TokenType.eq, .expectedLiteral = "==" },
        .{ .expectedType = TokenType.neq, .expectedLiteral = "~=" },
        .{ .expectedType = TokenType.dot, .expectedLiteral = "." },
        .{ .expectedType = TokenType.gte, .expectedLiteral = ">=" },
        .{ .expectedType = TokenType.lte, .expectedLiteral = "<=" },
        // Delimiters (currently implemented)
        .{ .expectedType = TokenType.lparen, .expectedLiteral = "(" },
        .{ .expectedType = TokenType.rparen, .expectedLiteral = ")" },
        .{ .expectedType = TokenType.lbrace, .expectedLiteral = "{" },
        .{ .expectedType = TokenType.rbrace, .expectedLiteral = "}" },
        .{ .expectedType = TokenType.lbracket, .expectedLiteral = "[" },
        .{ .expectedType = TokenType.rbracket, .expectedLiteral = "]" },
        .{ .expectedType = TokenType.comma, .expectedLiteral = "," },
        .{ .expectedType = TokenType.semicolon, .expectedLiteral = ";" },
        .{ .expectedType = TokenType.colon, .expectedLiteral = ":" },
        // Other operators
        .{ .expectedType = TokenType.dotdot, .expectedLiteral = ".." },
        .{ .expectedType = TokenType.dotdotdot, .expectedLiteral = "..." },
        .{ .expectedType = TokenType.percent, .expectedLiteral = "%" },
        .{ .expectedType = TokenType.caret, .expectedLiteral = "^" },
        .{ .expectedType = TokenType.doubleslash, .expectedLiteral = "//" },
        .{ .expectedType = TokenType.ampersand, .expectedLiteral = "&" },
        .{ .expectedType = TokenType.pipe, .expectedLiteral = "|" },
        .{ .expectedType = TokenType.lshift, .expectedLiteral = "<<" },
        .{ .expectedType = TokenType.rshift, .expectedLiteral = ">>" },
        .{ .expectedType = TokenType.doublecolon, .expectedLiteral = "::" },
        // Comments
        .{ .expectedType = TokenType.doublehyphen, .expectedLiteral = "--" },
    };

    const allocator = std.testing.allocator;
    var lexer = try Lexer.init(allocator, input);
    defer lexer.deinit();
    for (tests) |t| {
        const tok = (try lexer.nextToken()).*;
        try std.testing.expectEqualStrings(t.expectedLiteral, tok.literal);
        try std.testing.expectEqual(t.expectedType, tok.type);
    }
}

test "token position tracking" {
    // Simple test focused on verifying start_pos and end_pos accuracy
    const input = "local x = 42 + y";

    const tests = [_]struct {
        expectedType: TokenType,
        expectedLiteral: []const u8,
        expectedStart: u32,
        expectedEnd: u32,
    }{
        .{ .expectedType = TokenType.local, .expectedLiteral = "local", .expectedStart = 0, .expectedEnd = 4 },
        .{ .expectedType = TokenType.ident, .expectedLiteral = "x", .expectedStart = 6, .expectedEnd = 6 },
        .{ .expectedType = TokenType.assign, .expectedLiteral = "=", .expectedStart = 8, .expectedEnd = 8 },
        .{ .expectedType = TokenType.int, .expectedLiteral = "42", .expectedStart = 10, .expectedEnd = 11 },
        .{ .expectedType = TokenType.plus, .expectedLiteral = "+", .expectedStart = 13, .expectedEnd = 13 },
        .{ .expectedType = TokenType.ident, .expectedLiteral = "y", .expectedStart = 15, .expectedEnd = 15 },
    };

    const allocator = std.testing.allocator;
    var lexer = try Lexer.init(allocator, input);
    defer lexer.deinit();
    for (tests) |t| {
        const tok = (try lexer.nextToken()).*;
        try std.testing.expectEqualStrings(t.expectedLiteral, tok.literal);
        try std.testing.expectEqual(t.expectedType, tok.type);
        try std.testing.expectEqual(t.expectedStart, tok.start_pos);
        try std.testing.expectEqual(t.expectedEnd, tok.end_pos);
    }
}

test "float number parsing" {
    // Test float parsing and disambiguation from dot operators
    const input = "3.14 42 1.0 99.999 123 .. 456";

    const tests = [_]struct {
        expectedType: TokenType,
        expectedLiteral: []const u8,
    }{
        .{ .expectedType = TokenType.float, .expectedLiteral = "3.14" },
        .{ .expectedType = TokenType.int, .expectedLiteral = "42" },
        .{ .expectedType = TokenType.float, .expectedLiteral = "1.0" },
        .{ .expectedType = TokenType.float, .expectedLiteral = "99.999" },
        .{ .expectedType = TokenType.int, .expectedLiteral = "123" },
        .{ .expectedType = TokenType.dotdot, .expectedLiteral = ".." },
        .{ .expectedType = TokenType.int, .expectedLiteral = "456" },
    };

    const allocator = std.testing.allocator;
    var lexer = try Lexer.init(allocator, input);
    defer lexer.deinit();
    for (tests) |t| {
        const tok = (try lexer.nextToken()).*;
        try std.testing.expectEqualStrings(t.expectedLiteral, tok.literal);
        try std.testing.expectEqual(t.expectedType, tok.type);
    }
}

test "builtin tokens" {
    // Test C builtin function tokens with $ prefix
    const input = "$puts $malloc $printf $free_memory $fn123";

    const tests = [_]struct {
        expectedType: TokenType,
        expectedLiteral: []const u8,
    }{
        .{ .expectedType = TokenType.builtin, .expectedLiteral = "$puts" },
        .{ .expectedType = TokenType.builtin, .expectedLiteral = "$malloc" },
        .{ .expectedType = TokenType.builtin, .expectedLiteral = "$printf" },
        .{ .expectedType = TokenType.builtin, .expectedLiteral = "$free_memory" },
        .{ .expectedType = TokenType.builtin, .expectedLiteral = "$fn123" },
    };

    const allocator = std.testing.allocator;
    var lexer = try Lexer.init(allocator, input);
    defer lexer.deinit();
    for (tests) |t| {
        const tok = (try lexer.nextToken()).*;
        try std.testing.expectEqualStrings(t.expectedLiteral, tok.literal);
        try std.testing.expectEqual(t.expectedType, tok.type);
    }
}

test "builtin token position tracking" {
    // Test that builtin tokens have correct position information
    const input = "$puts $malloc";

    const tests = [_]struct {
        expectedType: TokenType,
        expectedLiteral: []const u8,
        expectedStart: u32,
        expectedEnd: u32,
    }{
        .{ .expectedType = TokenType.builtin, .expectedLiteral = "$puts", .expectedStart = 0, .expectedEnd = 4 },
        .{ .expectedType = TokenType.builtin, .expectedLiteral = "$malloc", .expectedStart = 6, .expectedEnd = 12 },
    };

    const allocator = std.testing.allocator;
    var lexer = try Lexer.init(allocator, input);
    defer lexer.deinit();
    for (tests) |t| {
        const tok = (try lexer.nextToken()).*;
        try std.testing.expectEqualStrings(t.expectedLiteral, tok.literal);
        try std.testing.expectEqual(t.expectedType, tok.type);
        try std.testing.expectEqual(t.expectedStart, tok.start_pos);
        try std.testing.expectEqual(t.expectedEnd, tok.end_pos);
    }
}

test "builtin token invalid cases" {
    // Test that $ followed by non-letter produces error
    const allocator = std.testing.allocator;

    // $ followed by digit should be illegal
    var lexer1 = try Lexer.init(allocator, "$123");
    defer lexer1.deinit();
    const result1 = lexer1.nextToken();
    try std.testing.expectError(Lexer.LexError.Illegal, result1);

    // $ alone should be illegal when at end
    var lexer2 = try Lexer.init(allocator, "$");
    defer lexer2.deinit();
    const result2 = lexer2.nextToken();
    try std.testing.expectError(Lexer.LexError.Illegal, result2);
}
