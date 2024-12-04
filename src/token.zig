const std = @import("std");
pub const TokenType = enum { illegal, eof, ident, int, assign, plus, comma, semicolon, lparen, lbrace, rparen, rbrace, function, let };
pub const Token = struct {
    type: TokenType,
    literal: []const u8,

    fn init(t: TokenType, literal: []const u8) Token {
        return Token{ .type = t, .literal = literal };
    }
};

pub const Lexer = struct {
    input: []const u8,
    position: usize,
    read_position: usize,
    ch: u8,

    fn Init(input: []const u8) Lexer {
        var l = Lexer{ .input = input, .position = 0, .read_position = 0, .ch = 0 };
        l.read_char();
        return l;
    }

    fn read_char(self: *Lexer) void {
        if (self.read_position >= self.input.len) {
            self.ch = 0;
        } else {
            self.ch = self.input[self.read_position];
        }
        self.position = self.read_position;
        self.read_position += 1;
    }

    fn next_token(self: *Lexer) Token {
        var tokType: TokenType = TokenType.illegal;
        var literal: []const u8 = "";

        switch (self.ch) {
            '=' => {
                tokType = TokenType.assign;
                literal = "=";
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
            0 => {
                tokType = TokenType.eof;
                literal = "";
            },
            else => {
                tokType = TokenType.illegal;
                literal = "";
            },
        }

        self.read_char();
        return Token.init(tokType, literal);
    }
};

test "token test" {
    const tok = Token.init(TokenType.plus, "+");
    try std.testing.expectEqualStrings(tok.literal, "+");
    try std.testing.expectEqual(tok.type, TokenType.plus);
}

test "next token test" {
    const input = "=+(){},;";
    const tests = [_]struct { expectedType: TokenType, expectedLiteral: []const u8 }{
        .{ .expectedType = TokenType.assign, .expectedLiteral = "=" },
        .{ .expectedType = TokenType.plus, .expectedLiteral = "+" },
        .{ .expectedType = TokenType.lparen, .expectedLiteral = "(" },
        .{ .expectedType = TokenType.rparen, .expectedLiteral = ")" },
        .{ .expectedType = TokenType.lbrace, .expectedLiteral = "{" },
        .{ .expectedType = TokenType.rbrace, .expectedLiteral = "}" },
        .{ .expectedType = TokenType.comma, .expectedLiteral = "," },
        .{ .expectedType = TokenType.semicolon, .expectedLiteral = ";" },
    };

    var lexer = Lexer.Init(input);
    for (tests) |t| {
        const tok = lexer.next_token();
        try std.testing.expectEqualStrings(tok.literal, t.expectedLiteral);
        try std.testing.expectEqual(tok.type, t.expectedType);
    }
}
