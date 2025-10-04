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

    /// Deallocates all tokens created by the lexer
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

    /// Retrieves the next valid token in the input buffer
    /// Returns LexError.Illegal for illegal characters
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
            '[' => {
                tokType = TokenType.lbracket;
                literal = "[";
            },
            ']' => {
                tokType = TokenType.rbracket;
                literal = "]";
            },
            ':' => {
                if (self.peekChar() == ':') {
                    self.readChar();
                    tokType = TokenType.doublecolon;
                    literal = "::";
                } else {
                    tokType = TokenType.colon;
                    literal = ":";
                }
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
                if (self.peekChar() == '-') {
                    self.readChar();
                    tokType = TokenType.doublehyphen;
                    literal = "--";
                } else {
                    tokType = TokenType.minus;
                    literal = "-";
                }
            },
            '/' => {
                if (self.peekChar() == '/') {
                    self.readChar();
                    tokType = TokenType.doubleslash;
                    literal = "//";
                } else {
                    tokType = TokenType.slash;
                    literal = "/";
                }
            },
            '*' => {
                tokType = TokenType.asterisk;
                literal = "*";
            },
            '#' => {
                tokType = TokenType.hash;
                literal = "#";
            },
            '%' => {
                tokType = TokenType.percent;
                literal = "%";
            },
            '^' => {
                tokType = TokenType.caret;
                literal = "^";
            },
            '&' => {
                tokType = TokenType.ampersand;
                literal = "&";
            },
            '|' => {
                tokType = TokenType.pipe;
                literal = "|";
            },
            '<' => {
                if (self.peekChar() == '=') {
                    self.readChar();
                    tokType = TokenType.lte;
                    literal = "<=";
                } else if (self.peekChar() == '<') {
                    self.readChar();
                    tokType = TokenType.lshift;
                    literal = "<<";
                } else {
                    tokType = TokenType.lt;
                    literal = "<";
                }
            },
            '>' => {
                if (self.peekChar() == '=') {
                    self.readChar();
                    tokType = TokenType.gte;
                    literal = ">=";
                } else if (self.peekChar() == '>') {
                    self.readChar();
                    tokType = TokenType.rshift;
                    literal = ">>";
                } else {
                    tokType = TokenType.gt;
                    literal = ">";
                }
            },
            '.' => {
                if (self.peekChar() == '.') {
                    self.readChar();
                    if (self.peekChar() == '.') {
                        self.readChar();
                        tokType = TokenType.dotdotdot;
                        literal = "...";
                    } else {
                        tokType = TokenType.dotdot;
                        literal = "..";
                    }
                } else {
                    tokType = TokenType.dot;
                    literal = ".";
                }
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

test "comprehensive token coverage" {
    // Test all currently implemented token types - doesn't need to be valid syntax
    const input =
        \\function local if else elseif then end do while repeat until for in break goto
        \\return type not and or nil true false
        \\identifier 42 "string literal"
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
