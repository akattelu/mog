const std = @import("std");
pub const token = @import("token.zig");

pub fn main() !void {
    const in = std.io.getStdIn().reader();
    const output = std.io.getStdOut().writer();
    var input: [1024]u8 = undefined;

    // repl
    try output.print(">> ", .{});
    var read_value = try in.readUntilDelimiterOrEof(&input, '\n');

    while (read_value) |prog| {
        var lexer = token.Lexer.init(prog);
        var tok = lexer.next_token();
        while (tok) |t| {
            try output.print("token: {s}\n", .{t.to_string()});
            tok = lexer.next_token();
        }
        try output.print(">> ", .{});
        read_value = try in.readUntilDelimiterOrEof(&input, '\n');
    }
}

test {
    std.testing.refAllDecls(@This());
}
