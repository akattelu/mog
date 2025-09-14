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

    // FIXME: Convert this to using a Writer implementation?
    pub fn toString(self: *const Token, buf: *const []u8) []const u8 {
        return std.fmt.bufPrint(buf.*, "{s}[{any}]@{d}..{d}", .{ self.literal, self.type, self.start_pos, self.end_pos }) catch {
            return self.literal;
        };
    }
};
