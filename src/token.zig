const std = @import("std");
pub const TokenType = enum {
    illegal,
    eof,
    ident,
    int,
    assign,
    plus,
    comma,
    semicolon,
    lparen,
    lbrace,
    rparen,
    rbrace,
    function,
    let,
    bang,
    minus,
    slash,
    asterisk,
    lt,
    gt,
    t_if,
    true,
    false,
    t_else,
    t_return,
    eq,
    neq,

    fn fromIdent(lit: []const u8) TokenType {
        const keywords = std.StaticStringMap(TokenType);
        const map = keywords.initComptime(.{
            .{ "fn", .function },
            .{ "let", .let },
            .{ "if", .t_if },
            .{ "else", .t_else },
            .{ "true", .true },
            .{ "false", .false },
            .{ "return", .t_return },
        });
        return map.get(lit) orelse .ident;
    }
};

pub const Token = struct {
    type: TokenType,
    literal: []const u8,

    fn init(t: TokenType, literal: []const u8) Token {
        return Token{ .type = t, .literal = literal };
    }

    pub fn toString(self: *const Token) []const u8 {
        return self.literal;
    }
};

fn isDigit(ch: u8) bool {
    return std.ascii.isDigit(ch);
}
fn isLetter(ch: u8) bool {
    return std.ascii.isAlphabetic(ch);
}

pub const Lexer = struct {
    input: []const u8,
    position: usize,
    read_position: usize,
    ch: u8,

    pub fn init(input: []const u8) Lexer {
        var l = Lexer{ .input = input, .position = 0, .read_position = 0, .ch = 0 };
        l.readChar();
        return l;
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

    pub fn nextToken(self: *Lexer) ?Token {
        var tokType: TokenType = TokenType.illegal;
        var literal: []const u8 = "";
        self.skipWhitespace();
        if (self.read_position > self.input.len) {
            return null;
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
                if (isLetter(self.ch)) {
                    literal = self.readIdent();
                    tokType = TokenType.fromIdent(literal);
                    return Token.init(tokType, literal);
                } else if (isDigit(self.ch)) {
                    literal = self.readNumber();
                    tokType = TokenType.int;
                    return Token.init(tokType, literal);
                } else {
                    return null;
                }
            },
        }

        self.readChar();
        return Token.init(tokType, literal);
    }
};

test "token test" {
    const tok = Token.init(TokenType.plus, "+");
    try std.testing.expectEqualStrings(tok.literal, "+");
    try std.testing.expectEqual(tok.type, TokenType.plus);
}

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
    const tests = [_]struct { expectedType: TokenType, expectedLiteral: []const u8 }{
        .{ .expectedType = TokenType.let, .expectedLiteral = "let" },
        .{ .expectedType = TokenType.ident, .expectedLiteral = "five" },
        .{ .expectedType = TokenType.assign, .expectedLiteral = "=" },
        .{ .expectedType = TokenType.int, .expectedLiteral = "5" },
        .{ .expectedType = TokenType.semicolon, .expectedLiteral = ";" },
        .{ .expectedType = TokenType.let, .expectedLiteral = "let" },
        .{ .expectedType = TokenType.ident, .expectedLiteral = "ten" },
        .{ .expectedType = TokenType.assign, .expectedLiteral = "=" },
        .{ .expectedType = TokenType.int, .expectedLiteral = "10" },
        .{ .expectedType = TokenType.semicolon, .expectedLiteral = ";" },
        .{ .expectedType = TokenType.let, .expectedLiteral = "let" },
        .{ .expectedType = TokenType.ident, .expectedLiteral = "add" },
        .{ .expectedType = TokenType.assign, .expectedLiteral = "=" },
        .{ .expectedType = TokenType.function, .expectedLiteral = "fn" },
        .{ .expectedType = TokenType.lparen, .expectedLiteral = "(" },
        .{ .expectedType = TokenType.ident, .expectedLiteral = "x" },
        .{ .expectedType = TokenType.comma, .expectedLiteral = "," },
        .{ .expectedType = TokenType.ident, .expectedLiteral = "y" },
        .{ .expectedType = TokenType.rparen, .expectedLiteral = ")" },
        .{ .expectedType = TokenType.lbrace, .expectedLiteral = "{" },
        .{ .expectedType = TokenType.ident, .expectedLiteral = "x" },
        .{ .expectedType = TokenType.plus, .expectedLiteral = "+" },
        .{ .expectedType = TokenType.ident, .expectedLiteral = "y" },
        .{ .expectedType = TokenType.semicolon, .expectedLiteral = ";" },
        .{ .expectedType = TokenType.rbrace, .expectedLiteral = "}" },
        .{ .expectedType = TokenType.semicolon, .expectedLiteral = ";" },
        .{ .expectedType = TokenType.let, .expectedLiteral = "let" },
        .{ .expectedType = TokenType.ident, .expectedLiteral = "result" },
        .{ .expectedType = TokenType.assign, .expectedLiteral = "=" },
        .{ .expectedType = TokenType.ident, .expectedLiteral = "add" },
        .{ .expectedType = TokenType.lparen, .expectedLiteral = "(" },
        .{ .expectedType = TokenType.ident, .expectedLiteral = "five" },
        .{ .expectedType = TokenType.comma, .expectedLiteral = "," },
        .{ .expectedType = TokenType.ident, .expectedLiteral = "ten" },
        .{ .expectedType = TokenType.rparen, .expectedLiteral = ")" },
        .{ .expectedType = TokenType.semicolon, .expectedLiteral = ";" },
        .{ .expectedType = TokenType.bang, .expectedLiteral = "!" },
        .{ .expectedType = TokenType.minus, .expectedLiteral = "-" },
        .{ .expectedType = TokenType.slash, .expectedLiteral = "/" },
        .{ .expectedType = TokenType.asterisk, .expectedLiteral = "*" },
        .{ .expectedType = TokenType.int, .expectedLiteral = "5" },
        .{ .expectedType = TokenType.semicolon, .expectedLiteral = ";" },
        .{ .expectedType = TokenType.int, .expectedLiteral = "5" },
        .{ .expectedType = TokenType.lt, .expectedLiteral = "<" },
        .{ .expectedType = TokenType.int, .expectedLiteral = "10" },
        .{ .expectedType = TokenType.gt, .expectedLiteral = ">" },
        .{ .expectedType = TokenType.int, .expectedLiteral = "5" },
        .{ .expectedType = TokenType.semicolon, .expectedLiteral = ";" },
        .{ .expectedType = TokenType.t_if, .expectedLiteral = "if" },
        .{ .expectedType = TokenType.lparen, .expectedLiteral = "(" },
        .{ .expectedType = TokenType.int, .expectedLiteral = "5" },
        .{ .expectedType = TokenType.lt, .expectedLiteral = "<" },
        .{ .expectedType = TokenType.int, .expectedLiteral = "10" },
        .{ .expectedType = TokenType.rparen, .expectedLiteral = ")" },
        .{ .expectedType = TokenType.lbrace, .expectedLiteral = "{" },
        .{ .expectedType = TokenType.t_return, .expectedLiteral = "return" },
        .{ .expectedType = TokenType.true, .expectedLiteral = "true" },
        .{ .expectedType = TokenType.semicolon, .expectedLiteral = ";" },
        .{ .expectedType = TokenType.rbrace, .expectedLiteral = "}" },
        .{ .expectedType = TokenType.t_else, .expectedLiteral = "else" },
        .{ .expectedType = TokenType.lbrace, .expectedLiteral = "{" },
        .{ .expectedType = TokenType.t_return, .expectedLiteral = "return" },
        .{ .expectedType = TokenType.false, .expectedLiteral = "false" },
        .{ .expectedType = TokenType.semicolon, .expectedLiteral = ";" },
        .{ .expectedType = TokenType.rbrace, .expectedLiteral = "}" },
        .{ .expectedType = TokenType.int, .expectedLiteral = "5" },
        .{ .expectedType = TokenType.eq, .expectedLiteral = "==" },
        .{ .expectedType = TokenType.int, .expectedLiteral = "10" },
        .{ .expectedType = TokenType.semicolon, .expectedLiteral = ";" },
        .{ .expectedType = TokenType.int, .expectedLiteral = "5" },
        .{ .expectedType = TokenType.neq, .expectedLiteral = "!=" },
        .{ .expectedType = TokenType.int, .expectedLiteral = "10" },
        .{ .expectedType = TokenType.semicolon, .expectedLiteral = ";" },
    };

    var lexer = Lexer.init(input);
    for (tests) |t| {
        const tok = lexer.nextToken();
        try std.testing.expect(tok != null);
        try std.testing.expectEqualStrings(tok.?.literal, t.expectedLiteral);
        try std.testing.expectEqual(tok.?.type, t.expectedType);
    }
}
