const std = @import("std");
const token = @import("token.zig");
const lex = @import("lexer.zig");
const ast = @import("ast.zig");

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

    pub fn parseProgram(self: *Parser) !ast.Program {}
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

fn testParser(input: []u8) !Parser {
    const allocator = std.testing.allocator;
    var lexer = lex.Lexer.init(allocator, input);
    defer lexer.deinit();

    return try Parser.init(&lexer);
}

test "let statements" {
    const input =
        \\let x = 3;
        \\let y = 5;
        \\let foo = 400;
    ;

    const parser = testParser(input);
    const program = parser.parseProgram();
    std.testing.expectEqual(3, program.statements.len);

    try testLetStatement(program.statements[0], "x");
    try testLetStatement(program.statements[1], "y");
    try testLetStatement(program.statements[2], "foo");
}

const TestError = error{IncorrectType};

fn testLetStatement(s: ast.Statement, name: []u8) !void {
    std.testing.expectEqualString("let", s.tokenLiteral());
    std.testing.expectEqualString("let", s.tokenLiteral());

    switch (s) {
        .Let => |ls| {
            std.testing.expectEqualStrings(name, ls.name.value);
            std.testing.expectEqualStrings(name, ls.name.tokenLiteral());
        },

        else => {
            return TestError.IncorrectType;
        },
    }
}
