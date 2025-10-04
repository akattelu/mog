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

/// Allocates tokens from an input buffer string
/// Initialize with .`init` function
pub const Lexer = struct {
    alloc: std.heap.ArenaAllocator,
    input: []const u8,
    position: u32,
    read_position: u32,
    ch: u8,

    pub const LexError = error{Illegal};

    /// Returns a Lexer struct value after initializing arena allocator
    /// This method initializes the lexer with the correct internal state pointers to the string buffer
    /// So, this function must be used to initialize the struct
    pub fn init(allocator: std.mem.Allocator, input: []const u8) !Lexer {
        const arena = std.heap.ArenaAllocator.init(allocator);
        var lex: Lexer = .{
            .input = input,
            .position = 0,
            .read_position = 0,
            .ch = 0,
            .alloc = arena,
        };
        lex.readChar();
        return lex;
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

    fn readString(self: *Lexer) []const u8 {
        self.readChar();
        const start = self.position;
        while (self.ch != '\"') {
            self.readChar();
        }

        // include literal and quotes
        const literal = self.input[start - 1 .. self.position + 1];

        // go past "
        self.readChar();
        return literal;
    }

    pub fn nextToken(self: *Lexer) !*Token {
        var tokType: TokenType = TokenType.illegal;
        var literal: []const u8 = "";
        self.skipWhitespace();
        if (self.read_position > self.input.len) {
            const tok = try self.alloc.allocator().create(Token);
            tok.* = .{
                .literal = "",
                .type = TokenType.eof,
                .start_pos = self.position,
                .end_pos = self.position,
            };
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
            ':' => {
                tokType = TokenType.colon;
                literal = ":";
            },
            ',' => {
                tokType = TokenType.comma;
                literal = ",";
            },
            ';' => {
                tokType = TokenType.semicolon;
                literal = ";";
            },
            '~' => {
                if (self.peekChar() == '=') {
                    self.readChar();
                    tokType = TokenType.neq;
                    literal = "~=";
                } else {
                    tokType = TokenType.tilde;
                    literal = "~";
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
                if (self.ch == '\"') {
                    tok.* = .{ .literal = self.readString(), .type = TokenType.string, .start_pos = @intCast(self.position - tok.literal.len), .end_pos = self.position - 1 };
                    return tok;
                } else if (isLetter(self.ch)) {
                    tok.literal = self.readIdent();
                    tok.type = TokenType.fromWord(tok.literal);
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
        \\local five = 3;
        \\local ten = 8;
        \\local add = function(x, y)
        \\return x + y;
        \\end;
        \\local result = add(five, ten);
        \\~-/*3;
        \\3 < 10 > 5;
        \\if (3 < 10) then
        \\return true;
        \\ else
        \\return false;
        \\end
        \\3==10;
        \\3~=10;
        \\local five: string = "hello world";
    ;
    const tests = [_]struct {
        expectedType: TokenType,
        expectedLiteral: []const u8,
        expectedStart: u32,
        expectedEnd: u32,
    }{
        .{ .expectedType = TokenType.local, .expectedLiteral = "local", .expectedStart = 0, .expectedEnd = 4 },
        .{ .expectedType = TokenType.ident, .expectedLiteral = "five", .expectedStart = 6, .expectedEnd = 9 },
        .{ .expectedType = TokenType.assign, .expectedLiteral = "=", .expectedStart = 11, .expectedEnd = 11 },
        .{ .expectedType = TokenType.int, .expectedLiteral = "3", .expectedStart = 13, .expectedEnd = 13 },
        .{ .expectedType = TokenType.semicolon, .expectedLiteral = ";", .expectedStart = 14, .expectedEnd = 14 },
        .{ .expectedType = TokenType.local, .expectedLiteral = "local", .expectedStart = 16, .expectedEnd = 20 },
        .{ .expectedType = TokenType.ident, .expectedLiteral = "ten", .expectedStart = 22, .expectedEnd = 24 },
        .{ .expectedType = TokenType.assign, .expectedLiteral = "=", .expectedStart = 26, .expectedEnd = 26 },
        .{ .expectedType = TokenType.int, .expectedLiteral = "8", .expectedStart = 28, .expectedEnd = 28 },
        .{ .expectedType = TokenType.semicolon, .expectedLiteral = ";", .expectedStart = 29, .expectedEnd = 29 },
        .{ .expectedType = TokenType.local, .expectedLiteral = "local", .expectedStart = 31, .expectedEnd = 35 },
        .{ .expectedType = TokenType.ident, .expectedLiteral = "add", .expectedStart = 37, .expectedEnd = 39 },
        .{ .expectedType = TokenType.assign, .expectedLiteral = "=", .expectedStart = 41, .expectedEnd = 41 },
        .{ .expectedType = TokenType.function, .expectedLiteral = "function", .expectedStart = 43, .expectedEnd = 50 },
        .{ .expectedType = TokenType.lparen, .expectedLiteral = "(", .expectedStart = 51, .expectedEnd = 51 },
        .{ .expectedType = TokenType.ident, .expectedLiteral = "x", .expectedStart = 52, .expectedEnd = 52 },
        .{ .expectedType = TokenType.comma, .expectedLiteral = ",", .expectedStart = 53, .expectedEnd = 53 },
        .{ .expectedType = TokenType.ident, .expectedLiteral = "y", .expectedStart = 55, .expectedEnd = 55 },
        .{ .expectedType = TokenType.rparen, .expectedLiteral = ")", .expectedStart = 56, .expectedEnd = 56 },
        .{ .expectedType = TokenType.t_return, .expectedLiteral = "return", .expectedStart = 58, .expectedEnd = 63 },
        .{ .expectedType = TokenType.ident, .expectedLiteral = "x", .expectedStart = 65, .expectedEnd = 65 },
        .{ .expectedType = TokenType.plus, .expectedLiteral = "+", .expectedStart = 67, .expectedEnd = 67 },
        .{ .expectedType = TokenType.ident, .expectedLiteral = "y", .expectedStart = 69, .expectedEnd = 69 },
        .{ .expectedType = TokenType.semicolon, .expectedLiteral = ";", .expectedStart = 70, .expectedEnd = 70 },
        .{ .expectedType = TokenType.end, .expectedLiteral = "end", .expectedStart = 72, .expectedEnd = 74 },
        .{ .expectedType = TokenType.semicolon, .expectedLiteral = ";", .expectedStart = 75, .expectedEnd = 75 },
        .{ .expectedType = TokenType.local, .expectedLiteral = "local", .expectedStart = 77, .expectedEnd = 81 },
        .{ .expectedType = TokenType.ident, .expectedLiteral = "result", .expectedStart = 83, .expectedEnd = 88 },
        .{ .expectedType = TokenType.assign, .expectedLiteral = "=", .expectedStart = 90, .expectedEnd = 90 },
        .{ .expectedType = TokenType.ident, .expectedLiteral = "add", .expectedStart = 92, .expectedEnd = 94 },
        .{ .expectedType = TokenType.lparen, .expectedLiteral = "(", .expectedStart = 95, .expectedEnd = 95 },
        .{ .expectedType = TokenType.ident, .expectedLiteral = "five", .expectedStart = 96, .expectedEnd = 99 },
        .{ .expectedType = TokenType.comma, .expectedLiteral = ",", .expectedStart = 100, .expectedEnd = 100 },
        .{ .expectedType = TokenType.ident, .expectedLiteral = "ten", .expectedStart = 102, .expectedEnd = 104 },
        .{ .expectedType = TokenType.rparen, .expectedLiteral = ")", .expectedStart = 105, .expectedEnd = 105 },
        .{ .expectedType = TokenType.semicolon, .expectedLiteral = ";", .expectedStart = 106, .expectedEnd = 106 },
        .{ .expectedType = TokenType.tilde, .expectedLiteral = "~", .expectedStart = 108, .expectedEnd = 108 },
        .{ .expectedType = TokenType.minus, .expectedLiteral = "-", .expectedStart = 109, .expectedEnd = 109 },
        .{ .expectedType = TokenType.slash, .expectedLiteral = "/", .expectedStart = 110, .expectedEnd = 110 },
        .{ .expectedType = TokenType.asterisk, .expectedLiteral = "*", .expectedStart = 111, .expectedEnd = 111 },
        .{ .expectedType = TokenType.int, .expectedLiteral = "3", .expectedStart = 112, .expectedEnd = 112 },
        .{ .expectedType = TokenType.semicolon, .expectedLiteral = ";", .expectedStart = 113, .expectedEnd = 113 },
        .{ .expectedType = TokenType.int, .expectedLiteral = "3", .expectedStart = 115, .expectedEnd = 115 },
        .{ .expectedType = TokenType.lt, .expectedLiteral = "<", .expectedStart = 117, .expectedEnd = 117 },
        .{ .expectedType = TokenType.int, .expectedLiteral = "10", .expectedStart = 119, .expectedEnd = 120 },
        .{ .expectedType = TokenType.gt, .expectedLiteral = ">", .expectedStart = 122, .expectedEnd = 122 },
        .{ .expectedType = TokenType.int, .expectedLiteral = "5", .expectedStart = 124, .expectedEnd = 124 },
        .{ .expectedType = TokenType.semicolon, .expectedLiteral = ";", .expectedStart = 125, .expectedEnd = 125 },
        .{ .expectedType = TokenType.t_if, .expectedLiteral = "if", .expectedStart = 127, .expectedEnd = 128 },
        .{ .expectedType = TokenType.lparen, .expectedLiteral = "(", .expectedStart = 130, .expectedEnd = 130 },
        .{ .expectedType = TokenType.int, .expectedLiteral = "3", .expectedStart = 131, .expectedEnd = 131 },
        .{ .expectedType = TokenType.lt, .expectedLiteral = "<", .expectedStart = 133, .expectedEnd = 133 },
        .{ .expectedType = TokenType.int, .expectedLiteral = "10", .expectedStart = 135, .expectedEnd = 136 },
        .{ .expectedType = TokenType.rparen, .expectedLiteral = ")", .expectedStart = 137, .expectedEnd = 137 },
        .{ .expectedType = TokenType.then, .expectedLiteral = "then", .expectedStart = 139, .expectedEnd = 142 },
        .{ .expectedType = TokenType.t_return, .expectedLiteral = "return", .expectedStart = 144, .expectedEnd = 149 },
        .{ .expectedType = TokenType.true, .expectedLiteral = "true", .expectedStart = 151, .expectedEnd = 154 },
        .{ .expectedType = TokenType.semicolon, .expectedLiteral = ";", .expectedStart = 155, .expectedEnd = 155 },
        .{ .expectedType = TokenType.t_else, .expectedLiteral = "else", .expectedStart = 158, .expectedEnd = 161 },
        .{ .expectedType = TokenType.t_return, .expectedLiteral = "return", .expectedStart = 163, .expectedEnd = 168 },
        .{ .expectedType = TokenType.false, .expectedLiteral = "false", .expectedStart = 170, .expectedEnd = 174 },
        .{ .expectedType = TokenType.semicolon, .expectedLiteral = ";", .expectedStart = 175, .expectedEnd = 175 },
        .{ .expectedType = TokenType.end, .expectedLiteral = "end", .expectedStart = 177, .expectedEnd = 179 },
        .{ .expectedType = TokenType.int, .expectedLiteral = "3", .expectedStart = 181, .expectedEnd = 181 },
        .{ .expectedType = TokenType.eq, .expectedLiteral = "==", .expectedStart = 182, .expectedEnd = 183 },
        .{ .expectedType = TokenType.int, .expectedLiteral = "10", .expectedStart = 184, .expectedEnd = 185 },
        .{ .expectedType = TokenType.semicolon, .expectedLiteral = ";", .expectedStart = 186, .expectedEnd = 186 },
        .{ .expectedType = TokenType.int, .expectedLiteral = "3", .expectedStart = 188, .expectedEnd = 188 },
        .{ .expectedType = TokenType.neq, .expectedLiteral = "~=", .expectedStart = 189, .expectedEnd = 190 },
        .{ .expectedType = TokenType.int, .expectedLiteral = "10", .expectedStart = 191, .expectedEnd = 192 },
        .{ .expectedType = TokenType.semicolon, .expectedLiteral = ";", .expectedStart = 193, .expectedEnd = 193 },
        .{ .expectedType = TokenType.local, .expectedLiteral = "local", .expectedStart = 195, .expectedEnd = 199 },
        .{ .expectedType = TokenType.ident, .expectedLiteral = "five", .expectedStart = 201, .expectedEnd = 204 },
        .{ .expectedType = TokenType.colon, .expectedLiteral = ":", .expectedStart = 205, .expectedEnd = 205 },
        .{ .expectedType = TokenType.ident, .expectedLiteral = "string", .expectedStart = 207, .expectedEnd = 212 },
        .{ .expectedType = TokenType.assign, .expectedLiteral = "=", .expectedStart = 214, .expectedEnd = 214 },
        .{ .expectedType = TokenType.string, .expectedLiteral = "\"hello world\"", .expectedStart = 216, .expectedEnd = 228 },
        .{ .expectedType = TokenType.semicolon, .expectedLiteral = ";", .expectedStart = 229, .expectedEnd = 229 },
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
