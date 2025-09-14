const std = @import("std");
const Reader = std.fs.File.Reader;
const Writer = std.fs.File.Writer;

pub const ast = @import("ast.zig");
pub const lex = @import("lexer.zig");
pub const Parser = @import("parser.zig").Parser;
pub const ParserError = @import("parser.zig").Parser.ParserError;
pub const token = @import("token.zig");

// const ReplFunctions = std.StaticStringMap(*const fn (Reader, Writer) anyerror!void).initComptime(.{
//     .{ "--parser", &parser_repl },
//     .{ "--lexer", &lex_repl },
// });

pub fn main() !void {
    // var args = std.process.args();
    // const in = std.fs.File.stdin().reader(&.{});
    // const output = std.fs.File.stdout().writer(&.{});
    // var writer = output.interface;
    // _ = args.skip();
    // const command = args.next() orelse "--parser";

    // Read command line arguments and skip the first one
    var args = try std.process.argsWithAllocator(std.heap.page_allocator);
    _ = args.skip();
    const first_arg = args.next();

    // Ignore non-"repl" arguments
    if (first_arg == null or !(std.mem.eql(u8, first_arg.?, "repl"))) {
        std.process.exit(0);
    }

    // Create a buffered stdin reader
    var input_buffer: [10]u8 = undefined;
    var stdin = std.fs.File.stdin();
    var stdin_reader = stdin.reader(&input_buffer);

    // Create an allocating writer
    //
    // This is required because we need to allocate dynamically
    // in order to read arbitrary amounts of text per line
    var allocating_writer = std.Io.Writer.Allocating.init(std.heap.page_allocator);

    while (stdin_reader.interface.streamDelimiter(&allocating_writer.writer, '\n')) |bytesRead| {
        const line = allocating_writer.written();
        std.debug.print("line contents: {s}, bytes read: {d}\n", .{ line, bytesRead });

        // Clear writer so written() doesn't have previous iter content
        allocating_writer.clearRetainingCapacity();

        // Toss the newline character so it doesn't get picked up in the next iter
        stdin_reader.interface.toss(1);
    } else |err| switch (err) {
        std.Io.Reader.StreamError.EndOfStream => {
            std.process.exit(0);
        },
        else => {
            std.debug.print("error: {any}", .{err});
        },
    }

    // while (stdin_reader.interface.streamDelimiter(&allocating_writer.writer, '\n')) |numBytes| {
    //     const chunk = allocating_writer.written();
    //     std.debug.print("chunk: {s} numBytes: {d}", .{ chunk, numBytes });
    //     // Reset array list in dynamic allocator
    //     allocating_writer.clearRetainingCapacity();
    // } else |err| switch (err) {
    //     std.Io.Reader.StreamError.EndOfStream => {
    //         const unused = allocating_writer.writer.unusedCapacityLen();
    //         std.debug.print("Discarding unused bytes: {d}\n", .{unused});

    //         // Discard byte for \n
    //         stdin_reader.interface.toss(1);
    //     },
    //     else => {
    //         std.debug.print("error: {any}", .{err});
    //     },
    // }

    // const repl_fn = ReplFunctions.get(command);
    // if (repl_fn == null) {
    //     try writer.print("unknown flag {s}", .{command});
    //     std.process.exit(0);
    // }
    // try repl_fn.?(in, output);
}

// fn lex_repl(
//     in: Reader,
//     output: Writer,
// ) !void {
//     // repl
//     var input: [1024]u8 = undefined;
//     var reader = in.interface;
//     var writer = output.interface;
//     try writer.print(">> ", .{});
//     var read_value = try reader.streamDelimiterEnding(&input, '\n');

//     while (read_value) |prog| {
//         var lexer = try lex.Lexer.init(std.heap.page_allocator, prog);
//         defer lexer.deinit();
//         var tok = try lexer.nextToken();
//         while (tok.type != .eof) : (tok = try lexer.nextToken()) {
//             var buf: [256]u8 = undefined;
//             const str = tok.toString(&buf[0..]);
//             try output.print("{s}\n", .{str});
//         }
//         try output.print(">> ", .{});
//         read_value = try in.readUntilDelimiterOrEof(&input, '\n');
//     }
// }

// fn parser_repl(
//     in: Reader,
//     output: Writer,
// ) !void {

//     // repl
//     var input: [1024]u8 = undefined;
//     try output.print(">> ", .{});
//     var read_value = try in.readUntilDelimiterOrEof(&input, '\n');

//     while (read_value) |prog| {
//         var lexer = try lex.Lexer.init(std.heap.page_allocator, prog);
//         var parser = try Parser.init(std.heap.page_allocator, lexer);
//         defer lexer.deinit();
//         defer parser.deinit();
//         const program = parser.parseProgram() catch |err| switch (err) {
//             ParserError.fail => {
//                 try output.print("ERROR: {s}\n", .{parser.parser_error.?.*});
//                 try output.print(">> ", .{});
//                 read_value = try in.readUntilDelimiterOrEof(&input, '\n');
//                 continue;
//             },
//             else => {
//                 return err;
//             },
//         };

//         try program.write(output);
//         _ = try output.write("\n");

//         try output.print(">> ", .{});
//         read_value = try in.readUntilDelimiterOrEof(&input, '\n');
//     }
// }

test {
    std.testing.refAllDecls(@This());
}
