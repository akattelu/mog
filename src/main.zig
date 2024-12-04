const std = @import("std");
pub const token = @import("token.zig");

pub fn main() !void {
    const in = std.io.getStdIn().reader();
    const output = std.io.getStdOut().writer();
    var input: [1024]u8 = undefined;

    // repl
    try output.print(">> ", .{});
    var read_value = try in.readUntilDelimiterOrEof(&input, '\n');

    while (read_value != null) {
        var lexer = token.Lexer.init(read_value.?);
        var tok = lexer.next_token();
        while (tok != null) {
            try output.print("token: {s}\n", .{tok.?.to_string()});
            tok = lexer.next_token();
        }
        try output.print(">> ", .{});
        read_value = try in.readUntilDelimiterOrEof(&input, '\n');
    }
}

test {
    std.testing.refAllDecls(@This());
}
