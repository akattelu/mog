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

    fn from_ident(lit: []const u8) TokenType {
        if (std.mem.eql(u8, lit, "fn")) {
            return TokenType.function;
        } else if (std.mem.eql(u8, lit, "let")) {
            return TokenType.let;
        } else if (std.mem.eql(u8, lit, "if")) {
            return TokenType.t_if;
        } else if (std.mem.eql(u8, lit, "else")) {
            return TokenType.t_else;
        } else if (std.mem.eql(u8, lit, "true")) {
            return TokenType.true;
        } else if (std.mem.eql(u8, lit, "false")) {
            return TokenType.false;
        } else if (std.mem.eql(u8, lit, "return")) {
            return TokenType.t_return;
        } else {
            return TokenType.ident;
        }
    }
};
pub const Token = struct {
    type: TokenType,
    literal: []const u8,

    fn init(t: TokenType, literal: []const u8) Token {
        return Token{ .type = t, .literal = literal };
    }

    pub fn to_string(self: *Token) []const u8 {
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

    fn peek_char(self: *Lexer) u8 {
        if (self.read_position >= self.input.len) {
            return 0;
        } else {
            return self.input[self.read_position];
        }
    }

    fn read_number(self: *Lexer) []const u8 {
        const start_pos = self.position;
        while (isDigit(self.ch)) {
            self.read_char();
        }
        const number_as_string = self.input[start_pos..self.position];
        return number_as_string;
    }

    fn read_ident(self: *Lexer) []const u8 {
        const start_pos = self.position;
        while (isLetter(self.ch)) {
            self.read_char();
        }
        return self.input[start_pos..self.position];
    }

    fn skip_whitespace(self: *Lexer) void {
        while (self.ch == '\r' or self.ch == '\n' or self.ch == ' ' or self.ch == '\t') {
            self.read_char();
        }
    }

    pub fn next_token(self: *Lexer) ?Token {
        var tokType: TokenType = TokenType.illegal;
        var literal: []const u8 = "";
        self.skip_whitespace();
        if (self.read_position > self.input.len) {
            return null;
        }

        switch (self.ch) {
            '=' => {
                if (self.peek_char() == '=') {
                    self.read_char();
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
                if (self.peek_char() == '=') {
                    self.read_char();
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
                    literal = self.read_ident();
                    tokType = TokenType.from_ident(literal);
                    return Token.init(tokType, literal);
                } else if (isDigit(self.ch)) {
                    literal = self.read_number();
                    tokType = TokenType.int;
                    return Token.init(tokType, literal);
                } else {
                    return null;
                }
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
        const tok = lexer.next_token();
        try std.testing.expect(tok != null);
        try std.testing.expectEqualStrings(tok.?.literal, t.expectedLiteral);
        try std.testing.expectEqual(tok.?.type, t.expectedType);
    }
}
