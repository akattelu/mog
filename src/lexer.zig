const std = @import("std");
const token = @import("token.zig");
const TokenType = token.TokenType;
const Token = token.Token;
fn isDigit(ch: u8) bool {
    return std.ascii.isDigit(ch);
}
fn isLetter(ch: u8) bool {
    return std.ascii.isAlphabetic(ch);
}

pub const Lexer = struct {
    alloc: std.heap.ArenaAllocator,
    input: []const u8,
    position: u32,
    read_position: u32,
    ch: u8,

    pub const LexError = error{Illegal};

    pub fn init(allocator: std.mem.Allocator, input: []const u8) Lexer {
        const arena = std.heap.ArenaAllocator.init(allocator);
        var l = Lexer{ .input = input, .position = 0, .read_position = 0, .ch = 0, .alloc = arena };
        l.readChar();
        return l;
    }

    pub fn deinit(self: *Lexer) void {
        self.alloc.deinit();
    }

    fn readChar(self: *Lexer) void {
        if (self.read_position >= self.input.len) {
            self.ch = 0;
        } else {
            self.ch = self.input[self.read_position];
        }
        self.position = self.read_position;
        self.read_position += 1;
    }

    fn peekChar(self: *Lexer) u8 {
        if (self.read_position >= self.input.len) {
            return 0;
        } else {
            return self.input[self.read_position];
        }
    }

    fn readNumber(self: *Lexer) []const u8 {
        const start_pos = self.position;
        while (isDigit(self.ch)) {
            self.readChar();
        }
        const number_as_string = self.input[start_pos..self.position];
        return number_as_string;
    }

    fn readIdent(self: *Lexer) []const u8 {
        const start_pos = self.position;
        while (isLetter(self.ch)) {
            self.readChar();
        }
        return self.input[start_pos..self.position];
    }

    fn skipWhitespace(self: *Lexer) void {
        while (self.ch == '\r' or self.ch == '\n' or self.ch == ' ' or self.ch == '\t') {
            self.readChar();
        }
    }

    pub fn nextToken(self: *Lexer) !*Token {
        var tokType: TokenType = TokenType.illegal;
        var literal: []const u8 = "";
        self.skipWhitespace();
        if (self.read_position > self.input.len) {
            const tok = try self.alloc.allocator().create(Token);
            tok.literal = "";
            tok.type = TokenType.eof;
            tok.start_pos = self.position;
            tok.end_pos = self.position;
            return tok;
        }

        switch (self.ch) {
            '=' => {
                if (self.peekChar() == '=') {
                    self.readChar();
                    tokType = TokenType.eq;
                    literal = "==";
                } else {
                    tokType = TokenType.assign;
                    literal = "=";
                }
            },
            '+' => {
                tokType = TokenType.plus;
                literal = "+";
            },
            '(' => {
                tokType = TokenType.lparen;
                literal = "(";
            },
            ')' => {
                tokType = TokenType.rparen;
                literal = ")";
            },
            '{' => {
                tokType = TokenType.lbrace;
                literal = "{";
            },
            '}' => {
                tokType = TokenType.rbrace;
                literal = "}";
            },
            ',' => {
                tokType = TokenType.comma;
                literal = ",";
            },
            ';' => {
                tokType = TokenType.semicolon;
                literal = ";";
            },
            '!' => {
                if (self.peekChar() == '=') {
                    self.readChar();
                    tokType = TokenType.neq;
                    literal = "!=";
                } else {
                    tokType = TokenType.bang;
                    literal = "!";
                }
            },
            '-' => {
                tokType = TokenType.minus;
                literal = "-";
            },
            '/' => {
                tokType = TokenType.slash;
                literal = "/";
            },
            '*' => {
                tokType = TokenType.asterisk;
                literal = "*";
            },
            '<' => {
                tokType = TokenType.lt;
                literal = "<";
            },
            '>' => {
                tokType = TokenType.gt;
                literal = ">";
            },
            0 => {
                tokType = TokenType.eof;
                literal = "";
            },
            else => {
                const tok = try self.alloc.allocator().create(Token);
                if (isLetter(self.ch)) {
                    tok.literal = self.readIdent();
                    tok.type = TokenType.fromIdent(tok.literal);
                    tok.start_pos = @intCast(self.position - tok.literal.len);
                    tok.end_pos = self.position - 1;
                    return tok;
                } else if (isDigit(self.ch)) {
                    tok.literal = self.readNumber();
                    tok.type = TokenType.int;
                    tok.start_pos = @intCast(self.position - tok.literal.len);
                    tok.end_pos = self.position - 1;
                    return tok;
                } else {
                    return LexError.Illegal;
                }
            },
        }

        self.readChar();
        const tok = try self.alloc.allocator().create(Token);
        tok.literal = literal;
        tok.type = tokType;
        tok.start_pos = @intCast(self.position - tok.literal.len);
        tok.end_pos = self.position - 1;
        return tok;
    }
};

test "next token test" {
    const input =
        \\let five = 5;
        \\let ten = 10;
        \\let add = fn(x, y) {
        \\x + y;
        \\};
        \\let result = add(five, ten);
        \\!-/*5;
        \\5 < 10 > 5;
        \\if (5 < 10) {
        \\return true;
        \\} else {
        \\return false;
        \\}
        \\5==10;
        \\5!=10;
    ;
    const tests = [_]struct {
        expectedType: TokenType,
        expectedLiteral: []const u8,
        expectedStart: u32,
        expectedEnd: u32,
    }{
        .{ .expectedType = TokenType.let, .expectedLiteral = "let", .expectedStart = 0, .expectedEnd = 2 },
        .{ .expectedType = TokenType.ident, .expectedLiteral = "five", .expectedStart = 4, .expectedEnd = 7 },
        .{ .expectedType = TokenType.assign, .expectedLiteral = "=", .expectedStart = 9, .expectedEnd = 9 },
        .{ .expectedType = TokenType.int, .expectedLiteral = "5", .expectedStart = 11, .expectedEnd = 11 },
        .{ .expectedType = TokenType.semicolon, .expectedLiteral = ";", .expectedStart = 12, .expectedEnd = 12 },
        .{ .expectedType = TokenType.let, .expectedLiteral = "let", .expectedStart = 14, .expectedEnd = 16 },
        .{ .expectedType = TokenType.ident, .expectedLiteral = "ten", .expectedStart = 18, .expectedEnd = 20 },
        .{ .expectedType = TokenType.assign, .expectedLiteral = "=", .expectedStart = 22, .expectedEnd = 22 },
        .{ .expectedType = TokenType.int, .expectedLiteral = "10", .expectedStart = 24, .expectedEnd = 25 },
        .{ .expectedType = TokenType.semicolon, .expectedLiteral = ";", .expectedStart = 26, .expectedEnd = 26 },
        .{ .expectedType = TokenType.let, .expectedLiteral = "let", .expectedStart = 28, .expectedEnd = 30 },
        .{ .expectedType = TokenType.ident, .expectedLiteral = "add", .expectedStart = 32, .expectedEnd = 34 },
        .{ .expectedType = TokenType.assign, .expectedLiteral = "=", .expectedStart = 36, .expectedEnd = 36 },
        .{ .expectedType = TokenType.function, .expectedLiteral = "fn", .expectedStart = 38, .expectedEnd = 39 },
        .{ .expectedType = TokenType.lparen, .expectedLiteral = "(", .expectedStart = 40, .expectedEnd = 40 },
        .{ .expectedType = TokenType.ident, .expectedLiteral = "x", .expectedStart = 41, .expectedEnd = 41 },
        .{ .expectedType = TokenType.comma, .expectedLiteral = ",", .expectedStart = 42, .expectedEnd = 42 },
        .{ .expectedType = TokenType.ident, .expectedLiteral = "y", .expectedStart = 44, .expectedEnd = 44 },
        .{ .expectedType = TokenType.rparen, .expectedLiteral = ")", .expectedStart = 45, .expectedEnd = 45 },
        .{ .expectedType = TokenType.lbrace, .expectedLiteral = "{", .expectedStart = 47, .expectedEnd = 47 },
        .{ .expectedType = TokenType.ident, .expectedLiteral = "x", .expectedStart = 49, .expectedEnd = 49 },
        .{ .expectedType = TokenType.plus, .expectedLiteral = "+", .expectedStart = 51, .expectedEnd = 51 },
        .{ .expectedType = TokenType.ident, .expectedLiteral = "y", .expectedStart = 53, .expectedEnd = 53 },
        .{ .expectedType = TokenType.semicolon, .expectedLiteral = ";", .expectedStart = 54, .expectedEnd = 54 },
        .{ .expectedType = TokenType.rbrace, .expectedLiteral = "}", .expectedStart = 56, .expectedEnd = 56 },
        .{ .expectedType = TokenType.semicolon, .expectedLiteral = ";", .expectedStart = 57, .expectedEnd = 57 },
        .{ .expectedType = TokenType.let, .expectedLiteral = "let", .expectedStart = 59, .expectedEnd = 61 },
        .{ .expectedType = TokenType.ident, .expectedLiteral = "result", .expectedStart = 63, .expectedEnd = 68 },
        .{ .expectedType = TokenType.assign, .expectedLiteral = "=", .expectedStart = 70, .expectedEnd = 70 },
        .{ .expectedType = TokenType.ident, .expectedLiteral = "add", .expectedStart = 72, .expectedEnd = 74 },
        .{ .expectedType = TokenType.lparen, .expectedLiteral = "(", .expectedStart = 75, .expectedEnd = 75 },
        .{ .expectedType = TokenType.ident, .expectedLiteral = "five", .expectedStart = 76, .expectedEnd = 79 },
        .{ .expectedType = TokenType.comma, .expectedLiteral = ",", .expectedStart = 80, .expectedEnd = 80 },
        .{ .expectedType = TokenType.ident, .expectedLiteral = "ten", .expectedStart = 82, .expectedEnd = 84 },
        .{ .expectedType = TokenType.rparen, .expectedLiteral = ")", .expectedStart = 85, .expectedEnd = 85 },
        .{ .expectedType = TokenType.semicolon, .expectedLiteral = ";", .expectedStart = 86, .expectedEnd = 86 },
        .{ .expectedType = TokenType.bang, .expectedLiteral = "!", .expectedStart = 88, .expectedEnd = 88 },
        .{ .expectedType = TokenType.minus, .expectedLiteral = "-", .expectedStart = 89, .expectedEnd = 89 },
        .{ .expectedType = TokenType.slash, .expectedLiteral = "/", .expectedStart = 90, .expectedEnd = 90 },
        .{ .expectedType = TokenType.asterisk, .expectedLiteral = "*", .expectedStart = 91, .expectedEnd = 91 },
        .{ .expectedType = TokenType.int, .expectedLiteral = "5", .expectedStart = 92, .expectedEnd = 92 },
        .{ .expectedType = TokenType.semicolon, .expectedLiteral = ";", .expectedStart = 93, .expectedEnd = 93 },
        .{ .expectedType = TokenType.int, .expectedLiteral = "5", .expectedStart = 95, .expectedEnd = 95 },
        .{ .expectedType = TokenType.lt, .expectedLiteral = "<", .expectedStart = 97, .expectedEnd = 97 },
        .{ .expectedType = TokenType.int, .expectedLiteral = "10", .expectedStart = 99, .expectedEnd = 100 },
        .{ .expectedType = TokenType.gt, .expectedLiteral = ">", .expectedStart = 102, .expectedEnd = 102 },
        .{ .expectedType = TokenType.int, .expectedLiteral = "5", .expectedStart = 104, .expectedEnd = 104 },
        .{ .expectedType = TokenType.semicolon, .expectedLiteral = ";", .expectedStart = 105, .expectedEnd = 105 },
        .{ .expectedType = TokenType.t_if, .expectedLiteral = "if", .expectedStart = 107, .expectedEnd = 108 },
        .{ .expectedType = TokenType.lparen, .expectedLiteral = "(", .expectedStart = 110, .expectedEnd = 110 },
        .{ .expectedType = TokenType.int, .expectedLiteral = "5", .expectedStart = 111, .expectedEnd = 111 },
        .{ .expectedType = TokenType.lt, .expectedLiteral = "<", .expectedStart = 113, .expectedEnd = 113 },
        .{ .expectedType = TokenType.int, .expectedLiteral = "10", .expectedStart = 115, .expectedEnd = 116 },
        .{ .expectedType = TokenType.rparen, .expectedLiteral = ")", .expectedStart = 117, .expectedEnd = 117 },
        .{ .expectedType = TokenType.lbrace, .expectedLiteral = "{", .expectedStart = 119, .expectedEnd = 119 },
        .{ .expectedType = TokenType.t_return, .expectedLiteral = "return", .expectedStart = 121, .expectedEnd = 126 },
        .{ .expectedType = TokenType.true, .expectedLiteral = "true", .expectedStart = 128, .expectedEnd = 131 },
        .{ .expectedType = TokenType.semicolon, .expectedLiteral = ";", .expectedStart = 132, .expectedEnd = 132 },
        .{ .expectedType = TokenType.rbrace, .expectedLiteral = "}", .expectedStart = 134, .expectedEnd = 134 },
        .{ .expectedType = TokenType.t_else, .expectedLiteral = "else", .expectedStart = 136, .expectedEnd = 139 },
        .{ .expectedType = TokenType.lbrace, .expectedLiteral = "{", .expectedStart = 141, .expectedEnd = 141 },
        .{ .expectedType = TokenType.t_return, .expectedLiteral = "return", .expectedStart = 143, .expectedEnd = 148 },
        .{ .expectedType = TokenType.false, .expectedLiteral = "false", .expectedStart = 150, .expectedEnd = 154 },
        .{ .expectedType = TokenType.semicolon, .expectedLiteral = ";", .expectedStart = 155, .expectedEnd = 155 },
        .{ .expectedType = TokenType.rbrace, .expectedLiteral = "}", .expectedStart = 157, .expectedEnd = 157 },
        .{ .expectedType = TokenType.int, .expectedLiteral = "5", .expectedStart = 159, .expectedEnd = 159 },
        .{ .expectedType = TokenType.eq, .expectedLiteral = "==", .expectedStart = 160, .expectedEnd = 161 },
        .{ .expectedType = TokenType.int, .expectedLiteral = "10", .expectedStart = 162, .expectedEnd = 163 },
        .{ .expectedType = TokenType.semicolon, .expectedLiteral = ";", .expectedStart = 164, .expectedEnd = 164 },
        .{ .expectedType = TokenType.int, .expectedLiteral = "5", .expectedStart = 166, .expectedEnd = 166 },
        .{ .expectedType = TokenType.neq, .expectedLiteral = "!=", .expectedStart = 167, .expectedEnd = 168 },
        .{ .expectedType = TokenType.int, .expectedLiteral = "10", .expectedStart = 169, .expectedEnd = 170 },
        .{ .expectedType = TokenType.semicolon, .expectedLiteral = ";", .expectedStart = 171, .expectedEnd = 171 },
    };

    const allocator = std.testing.allocator;
    var lexer = Lexer.init(allocator, input);
    defer lexer.deinit();
    for (tests) |t| {
        const tok = (try lexer.nextToken()).*;
        try std.testing.expectEqualStrings(t.expectedLiteral, tok.literal);
        try std.testing.expectEqual(t.expectedType, tok.type);
        try std.testing.expectEqual(t.expectedStart, tok.start_pos);
        try std.testing.expectEqual(t.expectedEnd, tok.end_pos);
    }
}
