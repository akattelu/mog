const std = @import("std");
pub const token = @import("token.zig");
pub const lex = @import("lexer.zig");
pub const ast = @import("ast.zig");
pub const parser = @import("parser.zig");

pub fn main() !void {
    const in = std.io.getStdIn().reader();
    const output = std.io.getStdOut().writer();
    var input: [1024]u8 = undefined;

    // repl
    try output.print(">> ", .{});
    var read_value = try in.readUntilDelimiterOrEof(&input, '\n');

    while (read_value) |prog| {
        var lexer = lex.Lexer.init(std.heap.page_allocator, prog);
        defer lexer.deinit();
        var tok = try lexer.nextToken();
        while (tok.type != .eof) {
            var buf: [256]u8 = undefined;
            const str = tok.toString(&buf[0..]);
            try output.print("{s}\n", .{str});
            tok = try lexer.nextToken();
        }
        try output.print(">> ", .{});
        read_value = try in.readUntilDelimiterOrEof(&input, '\n');
    }
}

test {
    std.testing.refAllDecls(@This());
}
