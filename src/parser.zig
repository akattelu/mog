const std = @import("std");
const token = @import("token.zig");
const lex = @import("lexer.zig");

pub const Parser = struct {
    lexer: *lex.Lexer,
    current_token: *token.Token,
    peek_token: *token.Token,

    pub fn init(lexer: *lex.Lexer) !Parser {
        const first = try lexer.nextToken();
        const second = try lexer.nextToken();
        return Parser{
            .lexer = lexer,
            .current_token = first,
            .peek_token = second,
        };
    }

    pub fn nextToken(self: *Parser) !void {
        self.current_token = self.peek_token;
        self.peek_token = try self.lexer.nextToken();
    }
};

test "create parser over string" {
    const input = "let x = 3";
    const allocator = std.testing.allocator;
    var lexer = lex.Lexer.init(allocator, input);
    defer lexer.deinit();

    const parser = try Parser.init(&lexer);
    try std.testing.expectEqualStrings(parser.current_token.literal, "let");
    try std.testing.expectEqualStrings(parser.peek_token.literal, "x");
}
