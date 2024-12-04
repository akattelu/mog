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
        var tok = lexer.nextToken();
        while (tok) |t| {
            var buf: [256]u8 = undefined;
            const str = t.toString(&buf[0..]);
            try output.print("token: {s}\n", .{str});
            tok = lexer.nextToken();
        }
        try output.print(">> ", .{});
        read_value = try in.readUntilDelimiterOrEof(&input, '\n');
    }
}

test {
    std.testing.refAllDecls(@This());
}
