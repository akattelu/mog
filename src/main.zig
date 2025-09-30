const std = @import("std");
const Writer = std.Io.Writer;
const File = std.fs.File;
const print = std.debug.print;
const process = std.process;
const StreamError = std.Io.Reader.StreamError;

pub const ast = @import("ast.zig");
pub const Lexer = @import("lexer.zig").Lexer;
const Parser = @import("parser.zig").Parser;
const ParserError = @import("parser.zig").Parser.ParserError;
pub const token = @import("token.zig");

pub fn main() !void {
    // Read command line arguments and skip the first one
    var args = try process.argsWithAllocator(std.heap.page_allocator);
    _ = args.skip();
    const first_arg = args.next();

    // Ignore non-"repl" arguments
    if (first_arg == null or !(std.mem.eql(u8, first_arg.?, "repl"))) {
        process.exit(1);
    }

    const mode = args.next();
    if (mode == null or (!(std.mem.eql(u8, mode.?, "--lex")) and !(std.mem.eql(u8, mode.?, "--parse")))) {
        process.exit(1);
    }

    // Create a buffered stdin reader
    var input_buffer: [10]u8 = undefined;
    var stdin = File.stdin();
    var stdin_reader = stdin.reader(&input_buffer);

    // Create stdout writer
    var output_buffer: [32]u8 = undefined;
    var stdout = File.stdout();
    var stdout_writer = stdout.writer(&output_buffer);

    // Create an allocating writer
    //
    // This is required because we need to allocate dynamically
    // in order to read arbitrary amounts of text per line
    var allocating_writer = Writer.Allocating.init(std.heap.page_allocator);

    print(">> ", .{});
    while (stdin_reader.interface.streamDelimiter(&allocating_writer.writer, '\n')) |bytesRead| {
        const line = allocating_writer.written();
        print("line contents: {s}, bytes read: {d}\n", .{ line, bytesRead });

        // Handle lexer mode
        if (std.mem.eql(u8, mode.?, "--lex")) {
            var lexer = try Lexer.init(std.heap.page_allocator, line);
            defer lexer.deinit();
            var tok = try lexer.nextToken();
            while (tok.type != .eof) : (tok = try lexer.nextToken()) {
                try tok.write(&stdout_writer.interface);
                try stdout_writer.interface.printAsciiChar('\n', .{});
                try stdout_writer.interface.flush();
            }
        }

        // Clear writer so written() doesn't have previous iter content
        allocating_writer.clearRetainingCapacity();

        // Toss the newline character so it doesn't get picked up in the next iter
        stdin_reader.interface.toss(1);

        // Print prompt for next line
        print(">> ", .{});
    } else |err| switch (err) {

        // Error is thrown by `streamDelimiter` when end of stream is reached (Ctrl+D)
        StreamError.EndOfStream => {
            process.cleanExit();
        },
        else => {
            print("error: {any}", .{err});
        },
    }
}

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
