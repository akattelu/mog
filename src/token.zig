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
    t_type,
    colon,
    number,
    string,
    end,
    then,
    quote,

    const KeywordMap = std.StaticStringMap(TokenType).initComptime(.{ .{ "function", .function }, .{ "let", .let }, .{ "if", .t_if }, .{ "else", .t_else }, .{ "true", .true }, .{ "false", .false }, .{ "return", .t_return }, .{ "type", .t_type }, .{ "number", .number }, .{ "string", .string }, .{ "end", .end }, .{ "then", .then } });

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
    pub fn write(self: *const Token, writer: *std.Io.Writer) std.Io.Writer.Error!void {
        try writer.print("{s}[{any}]@{d}..{d}", .{ self.literal, self.type, self.start_pos, self.end_pos });
        try writer.flush();
    }
};

test "write" {
    // Create testing allocator
    const alloc = std.testing.allocator;

    // Create allocating writer interface
    var writer = std.Io.Writer.Allocating.init(alloc);
    defer writer.deinit();

    // Write token
    const tok = Token{ .type = .ident, .literal = "hello", .start_pos = 0, .end_pos = 5 };
    try tok.write(&(writer.writer));

    const actual = writer.written();
    try std.testing.expectEqualStrings("hello[.ident]@0..5", actual);
}
