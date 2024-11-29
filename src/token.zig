const std = @import("std");
pub const TokenType = enum { illegal, eof, ident, int, assign, plus, comma, semicolon, lparen, lbrace, rparen, rbrace, function, let };
pub const Token = struct { type: TokenType, literal: []const u8 };

test "token test" {
    const tok = Token{ .literal = "+", .type = TokenType.plus };
    try std.testing.expectEqualStrings(tok.literal, "+");
    try std.testing.expectEqual(tok.type, TokenType.plus);
}
