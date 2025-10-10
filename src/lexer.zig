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

    fn readNumber(self: *Lexer) struct { literal: []const u8, is_float: bool } {
        const start_pos = self.position;
        var is_float = false;

        // Read initial digits
        while (isDigit(self.ch)) {
            self.readChar();
        }

        // Check for decimal point followed by digit (to distinguish from .. operator)
        if (self.ch == '.' and isDigit(self.peekChar())) {
            is_float = true;
            self.readChar(); // consume '.'

            // Read fractional part
            while (isDigit(self.ch)) {
                self.readChar();
            }
        }

        const number_as_string = self.input[start_pos..self.position];
        return .{ .literal = number_as_string, .is_float = is_float };
    }

    fn readIdent(self: *Lexer) []const u8 {
        const start_pos = self.position;
        while (isLetter(self.ch) or self.ch == '_') {
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
    pub fn nextToken(self: *Lexer) (std.mem.Allocator.Error || LexError)!*Token {
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
                    const num_result = self.readNumber();
                    tok.literal = num_result.literal;
                    tok.type = if (num_result.is_float) TokenType.float else TokenType.int;
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
