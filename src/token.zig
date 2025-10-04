const std = @import("std");
const Writer = std.Io.Writer;
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
    lbracket,
    rbracket,
    function,
    local,
    not,
    minus,
    slash,
    asterisk,
    hash,
    lt,
    gt,
    lte,
    gte,
    t_if,
    true,
    false,
    t_else,
    elseif,
    t_return,
    eq,
    neq,
    t_type,
    colon,
    number,
    string,
    end,
    then,
    do,
    t_while,
    repeat,
    until,
    t_for,
    in,
    t_break,
    goto,
    t_and,
    t_or,
    nil,
    dotdot,
    dotdotdot,
    percent,
    caret,
    doubleslash,
    ampersand,
    pipe,
    tilde,
    lshift,
    rshift,
    dot,
    doublecolon,
    doublehyphen,

    const KeywordMap = std.StaticStringMap(TokenType).initComptime(.{
        .{ "function", .function },
        .{ "local", .local },
        .{ "if", .t_if },
        .{ "else", .t_else },
        .{ "elseif", .elseif },
        .{ "true", .true },
        .{ "false", .false },
        .{ "return", .t_return },
        .{ "type", .t_type },
        .{ "end", .end },
        .{ "then", .then },
        .{ "do", .do },
        .{ "while", .t_while },
        .{ "repeat", .repeat },
        .{ "until", .until },
        .{ "for", .t_for },
        .{ "in", .in },
        .{ "break", .t_break },
        .{ "goto", .goto },
        .{ "and", .t_and },
        .{ "or", .t_or },
        .{ "not", .not },
        .{ "nil", .nil },
    });

    /// Returns a matching TokenType from a string literal
    pub fn fromWord(lit: []const u8) TokenType {
        return KeywordMap.get(lit) orelse .ident;
    }
};

pub const Token = struct {
    type: TokenType,
    literal: []const u8,
    start_pos: u32,
    end_pos: u32,

    /// Write a string representation of the token to the specified writer
    pub fn write(self: *const Token, writer: *Writer) std.Io.Writer.Error!void {
        try writer.print("{s}[{any}]@{d}..{d}", .{ self.literal, self.type, self.start_pos, self.end_pos });
    }
};

test "write" {
    // Create testing allocator
    const alloc = std.testing.allocator;

    // Create allocating writer interface
    var writer = Writer.Allocating.init(alloc);
    defer writer.deinit();

    // Write token
    const tok = Token{ .type = .ident, .literal = "hello", .start_pos = 0, .end_pos = 5 };
    try tok.write(&(writer.writer));

    const actual = writer.written();
    try std.testing.expectEqualStrings("hello[.ident]@0..5", actual);
}
