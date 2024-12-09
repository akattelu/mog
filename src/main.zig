const std = @import("std");
pub const token = @import("token.zig");
pub const lex = @import("lexer.zig");
pub const Parser = @import("parser.zig").Parser;
pub const ParserError = @import("parser.zig").Parser.ParserError;
pub const ast = @import("ast.zig");

const Reader = std.fs.File.Reader;
const Writer = std.fs.File.Writer;

const ReplFunctions = std.StaticStringMap(*const fn (Reader, Writer) anyerror!void).initComptime(.{
    .{ "--parser", &parser_repl },
    .{ "--lexer", &lex_repl },
});

pub fn main() !void {
    var args = std.process.args();
    const in = std.io.getStdIn().reader();
    const output = std.io.getStdOut().writer();
    _ = args.skip();
    const command = args.next() orelse "--parser";
    const repl_fn = ReplFunctions.get(command);
    if (repl_fn == null) {
        try output.print("unknown flag {s}", .{command});
        std.process.exit(0);
    }
    try repl_fn.?(in, output);
}

fn lex_repl(
    in: Reader,
    output: Writer,
) !void {
    // repl
    var input: [1024]u8 = undefined;
    try output.print(">> ", .{});
    var read_value = try in.readUntilDelimiterOrEof(&input, '\n');

    while (read_value) |prog| {
        var lexer = try lex.Lexer.init(std.heap.page_allocator, prog);
        defer lexer.deinit();
        var tok = try lexer.nextToken();
        while (tok.type != .eof) : (tok = try lexer.nextToken()) {
            var buf: [256]u8 = undefined;
            const str = tok.toString(&buf[0..]);
            try output.print("{s}\n", .{str});
        }
        try output.print(">> ", .{});
        read_value = try in.readUntilDelimiterOrEof(&input, '\n');
    }
}

fn parser_repl(
    in: Reader,
    output: Writer,
) !void {

    // repl
    var input: [1024]u8 = undefined;
    try output.print(">> ", .{});
    var read_value = try in.readUntilDelimiterOrEof(&input, '\n');

    while (read_value) |prog| {
        var lexer = try lex.Lexer.init(std.heap.page_allocator, prog);
        var parser = try Parser.init(std.heap.page_allocator, lexer);
        defer lexer.deinit();
        defer parser.deinit();
        const program = parser.parseProgram() catch |err| switch (err) {
            ParserError.fail => {
                try output.print("ERROR: {s}\n", .{parser.parser_error.?.*});
                try output.print(">> ", .{});
                read_value = try in.readUntilDelimiterOrEof(&input, '\n');
                continue;
            },
            else => {
                return err;
            },
        };

        try program.write(output);
        _ = try output.write("\n");

        try output.print(">> ", .{});
        read_value = try in.readUntilDelimiterOrEof(&input, '\n');
    }
}

test {
    std.testing.refAllDecls(@This());
}
