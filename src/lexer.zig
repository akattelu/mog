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

    pub fn init(allocator: std.mem.Allocator, input: []const u8) !*Lexer {
        var arena = std.heap.ArenaAllocator.init(allocator);
        var l = try arena.allocator().create(Lexer);
        l.input = input;
        l.position = 0;
        l.read_position = 0;
        l.ch = 0;
        l.alloc = arena;
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
        \\let five = 3;
        \\let ten = 8;
        \\let add = function(x, y)
        \\return x + y;
        \\end;
        \\let result = add(five, ten);
        \\!-/*3;
        \\3 < 10 > 5;
        \\if (3 < 10) then
        \\return true;
        \\ else
        \\return false;
        \\end
        \\3==10;
        \\3!=10;
        \\let five: string = "hello world";
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
        .{ .expectedType = TokenType.int, .expectedLiteral = "3", .expectedStart = 11, .expectedEnd = 11 },
        .{ .expectedType = TokenType.semicolon, .expectedLiteral = ";", .expectedStart = 12, .expectedEnd = 12 },
        .{ .expectedType = TokenType.let, .expectedLiteral = "let", .expectedStart = 14, .expectedEnd = 16 },
        .{ .expectedType = TokenType.ident, .expectedLiteral = "ten", .expectedStart = 18, .expectedEnd = 20 },
        .{ .expectedType = TokenType.assign, .expectedLiteral = "=", .expectedStart = 22, .expectedEnd = 22 },
        .{ .expectedType = TokenType.int, .expectedLiteral = "8", .expectedStart = 24, .expectedEnd = 24 },
        .{ .expectedType = TokenType.semicolon, .expectedLiteral = ";", .expectedStart = 25, .expectedEnd = 25 },
        .{ .expectedType = TokenType.let, .expectedLiteral = "let", .expectedStart = 27, .expectedEnd = 29 },
        .{ .expectedType = TokenType.ident, .expectedLiteral = "add", .expectedStart = 31, .expectedEnd = 33 },
        .{ .expectedType = TokenType.assign, .expectedLiteral = "=", .expectedStart = 35, .expectedEnd = 35 },
        .{ .expectedType = TokenType.function, .expectedLiteral = "function", .expectedStart = 37, .expectedEnd = 44 },
        .{ .expectedType = TokenType.lparen, .expectedLiteral = "(", .expectedStart = 45, .expectedEnd = 45 },
        .{ .expectedType = TokenType.ident, .expectedLiteral = "x", .expectedStart = 46, .expectedEnd = 46 },
        .{ .expectedType = TokenType.comma, .expectedLiteral = ",", .expectedStart = 47, .expectedEnd = 47 },
        .{ .expectedType = TokenType.ident, .expectedLiteral = "y", .expectedStart = 49, .expectedEnd = 49 },
        .{ .expectedType = TokenType.rparen, .expectedLiteral = ")", .expectedStart = 50, .expectedEnd = 50 },
        .{ .expectedType = TokenType.t_return, .expectedLiteral = "return", .expectedStart = 52, .expectedEnd = 57 },
        .{ .expectedType = TokenType.ident, .expectedLiteral = "x", .expectedStart = 59, .expectedEnd = 59 },
        .{ .expectedType = TokenType.plus, .expectedLiteral = "+", .expectedStart = 61, .expectedEnd = 61 },
        .{ .expectedType = TokenType.ident, .expectedLiteral = "y", .expectedStart = 63, .expectedEnd = 63 },
        .{ .expectedType = TokenType.semicolon, .expectedLiteral = ";", .expectedStart = 64, .expectedEnd = 64 },
        .{ .expectedType = TokenType.end, .expectedLiteral = "end", .expectedStart = 66, .expectedEnd = 68 },
        .{ .expectedType = TokenType.semicolon, .expectedLiteral = ";", .expectedStart = 69, .expectedEnd = 69 },
        .{ .expectedType = TokenType.let, .expectedLiteral = "let", .expectedStart = 71, .expectedEnd = 73 },
        .{ .expectedType = TokenType.ident, .expectedLiteral = "result", .expectedStart = 75, .expectedEnd = 80 },
        .{ .expectedType = TokenType.assign, .expectedLiteral = "=", .expectedStart = 82, .expectedEnd = 82 },
        .{ .expectedType = TokenType.ident, .expectedLiteral = "add", .expectedStart = 84, .expectedEnd = 86 },
        .{ .expectedType = TokenType.lparen, .expectedLiteral = "(", .expectedStart = 87, .expectedEnd = 87 },
        .{ .expectedType = TokenType.ident, .expectedLiteral = "five", .expectedStart = 88, .expectedEnd = 91 },
        .{ .expectedType = TokenType.comma, .expectedLiteral = ",", .expectedStart = 92, .expectedEnd = 92 },
        .{ .expectedType = TokenType.ident, .expectedLiteral = "ten", .expectedStart = 94, .expectedEnd = 96 },
        .{ .expectedType = TokenType.rparen, .expectedLiteral = ")", .expectedStart = 97, .expectedEnd = 97 },
        .{ .expectedType = TokenType.semicolon, .expectedLiteral = ";", .expectedStart = 98, .expectedEnd = 98 },
        .{ .expectedType = TokenType.bang, .expectedLiteral = "!", .expectedStart = 100, .expectedEnd = 100 },
        .{ .expectedType = TokenType.minus, .expectedLiteral = "-", .expectedStart = 101, .expectedEnd = 101 },
        .{ .expectedType = TokenType.slash, .expectedLiteral = "/", .expectedStart = 102, .expectedEnd = 102 },
        .{ .expectedType = TokenType.asterisk, .expectedLiteral = "*", .expectedStart = 103, .expectedEnd = 103 },
        .{ .expectedType = TokenType.int, .expectedLiteral = "3", .expectedStart = 104, .expectedEnd = 104 },
        .{ .expectedType = TokenType.semicolon, .expectedLiteral = ";", .expectedStart = 105, .expectedEnd = 105 },
        .{ .expectedType = TokenType.int, .expectedLiteral = "3", .expectedStart = 107, .expectedEnd = 107 },
        .{ .expectedType = TokenType.lt, .expectedLiteral = "<", .expectedStart = 109, .expectedEnd = 109 },
        .{ .expectedType = TokenType.int, .expectedLiteral = "10", .expectedStart = 111, .expectedEnd = 112 },
        .{ .expectedType = TokenType.gt, .expectedLiteral = ">", .expectedStart = 114, .expectedEnd = 114 },
        .{ .expectedType = TokenType.int, .expectedLiteral = "5", .expectedStart = 116, .expectedEnd = 116 },
        .{ .expectedType = TokenType.semicolon, .expectedLiteral = ";", .expectedStart = 117, .expectedEnd = 117 },
        .{ .expectedType = TokenType.t_if, .expectedLiteral = "if", .expectedStart = 119, .expectedEnd = 120 },
        .{ .expectedType = TokenType.lparen, .expectedLiteral = "(", .expectedStart = 122, .expectedEnd = 122 },
        .{ .expectedType = TokenType.int, .expectedLiteral = "3", .expectedStart = 123, .expectedEnd = 123 },
        .{ .expectedType = TokenType.lt, .expectedLiteral = "<", .expectedStart = 125, .expectedEnd = 125 },
        .{ .expectedType = TokenType.int, .expectedLiteral = "10", .expectedStart = 127, .expectedEnd = 128 },
        .{ .expectedType = TokenType.rparen, .expectedLiteral = ")", .expectedStart = 129, .expectedEnd = 129 },
        .{ .expectedType = TokenType.then, .expectedLiteral = "then", .expectedStart = 131, .expectedEnd = 134 },
        .{ .expectedType = TokenType.t_return, .expectedLiteral = "return", .expectedStart = 136, .expectedEnd = 141 },
        .{ .expectedType = TokenType.true, .expectedLiteral = "true", .expectedStart = 143, .expectedEnd = 146 },
        .{ .expectedType = TokenType.semicolon, .expectedLiteral = ";", .expectedStart = 147, .expectedEnd = 147 },
        .{ .expectedType = TokenType.t_else, .expectedLiteral = "else", .expectedStart = 150, .expectedEnd = 153 },
        .{ .expectedType = TokenType.t_return, .expectedLiteral = "return", .expectedStart = 155, .expectedEnd = 160 },
        .{ .expectedType = TokenType.false, .expectedLiteral = "false", .expectedStart = 162, .expectedEnd = 166 },
        .{ .expectedType = TokenType.semicolon, .expectedLiteral = ";", .expectedStart = 167, .expectedEnd = 167 },
        .{ .expectedType = TokenType.end, .expectedLiteral = "end", .expectedStart = 169, .expectedEnd = 171 },
        .{ .expectedType = TokenType.int, .expectedLiteral = "3", .expectedStart = 173, .expectedEnd = 173 },
        .{ .expectedType = TokenType.eq, .expectedLiteral = "==", .expectedStart = 174, .expectedEnd = 175 },
        .{ .expectedType = TokenType.int, .expectedLiteral = "10", .expectedStart = 176, .expectedEnd = 177 },
        .{ .expectedType = TokenType.semicolon, .expectedLiteral = ";", .expectedStart = 178, .expectedEnd = 178 },
        .{ .expectedType = TokenType.int, .expectedLiteral = "3", .expectedStart = 180, .expectedEnd = 180 },
        .{ .expectedType = TokenType.neq, .expectedLiteral = "!=", .expectedStart = 181, .expectedEnd = 182 },
        .{ .expectedType = TokenType.int, .expectedLiteral = "10", .expectedStart = 183, .expectedEnd = 184 },
        .{ .expectedType = TokenType.semicolon, .expectedLiteral = ";", .expectedStart = 185, .expectedEnd = 185 },
        .{ .expectedType = TokenType.let, .expectedLiteral = "let", .expectedStart = 187, .expectedEnd = 189 },
        .{ .expectedType = TokenType.ident, .expectedLiteral = "five", .expectedStart = 191, .expectedEnd = 194 },
        .{ .expectedType = TokenType.colon, .expectedLiteral = ":", .expectedStart = 195, .expectedEnd = 195 },
        .{ .expectedType = TokenType.string, .expectedLiteral = "string", .expectedStart = 197, .expectedEnd = 202 },
        .{ .expectedType = TokenType.assign, .expectedLiteral = "=", .expectedStart = 204, .expectedEnd = 204 },
        .{ .expectedType = TokenType.string, .expectedLiteral = "\"hello world\"", .expectedStart = 206, .expectedEnd = 218 },
        .{ .expectedType = TokenType.semicolon, .expectedLiteral = ";", .expectedStart = 219, .expectedEnd = 219 },
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
