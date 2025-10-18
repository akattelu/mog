const std = @import("std");
const Writer = std.Io.Writer;
const File = std.fs.File;
const print = std.debug.print;
const process = std.process;
const StreamError = std.Io.Reader.StreamError;

pub const ast = @import("ast.zig");
pub const Lexer = @import("lexer.zig").Lexer;
const p = @import("parser.zig");
const Parser = p.Parser;
const ParserError = p.Parser.ParserError;
pub const token = @import("token.zig");
const PrettyPrinter = @import("pretty_printer.zig").PrettyPrinter;
const QBECompiler = @import("qbe/compiler.zig").QBECompiler;

const CommandErrUnion = (std.mem.Allocator.Error || std.Io.Writer.Error || Lexer.LexError || ParserError || std.fmt.ParseIntError || std.fs.File.OpenError || std.Io.Reader.Error);
const Command = *const fn (args: *std.process.ArgIterator) CommandErrUnion!u8;
const command_map = std.StaticStringMap(Command).initComptime(.{
    .{ "repl", &repl },
    .{ "parse", &parse_file },
    .{ "fmt", &fmt_file },
    .{ "help", &help },
    .{ "build", &build },
});

pub fn main() !void {
    // Read command line arguments and skip the first one
    var args = try process.argsWithAllocator(std.heap.page_allocator);
    _ = args.skip();
    const first_arg = args.next();

    // Ignore non-"repl" arguments
    if (first_arg == null) {
        process.exit(1);
    }

    const command = command_map.get(first_arg.?);
    if (command == null) {
        process.exit(1);
    }
    const ret = try command.?(&args);

    if (ret == 0) process.cleanExit();
    process.exit(ret);
}

pub fn help(args: *std.process.ArgIterator) CommandErrUnion!u8 {
    _ = args;

    const help_text =
        \\mog - A Lua-based language utility toolkit
        \\
        \\USAGE:
        \\  mog <command> [options]
        \\
        \\COMMANDS:
        \\  help                                     Show this help message
        \\  repl --lex                               Start REPL in lexer mode (tokenization only)
        \\  repl --parse                             Start REPL in parser mode (full parsing)
        \\  parse <file>                             Parse a Lua file and output the AST
        \\  fmt <file>                               Format a Lua file with pretty printing
        \\  build <input_file> -o <output_file>k     Compile a lua file to an output QBE SSA file
        \\
        \\EXAMPLES:
        \\  mog help
        \\  mog repl --lex
        \\  mog repl --parse
        \\  mog parse script.lua
        \\  mog fmt script.lua
        \\  mog build script.lua -o out.ssa
    ;

    print("{s}", .{help_text});
    return 0;
}

pub fn repl(args: *std.process.ArgIterator) CommandErrUnion!u8 {
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
    const out_iface: *std.Io.Writer = &stdout_writer.interface;

    // Create an allocating writer
    // This is required because we need to allocate dynamically
    // in order to read arbitrary amounts of text per line
    var allocating_writer = Writer.Allocating.init(std.heap.page_allocator);

    print(">> ", .{});
    while (stdin_reader.interface.streamDelimiter(&allocating_writer.writer, '\n')) |_| {
        const line = allocating_writer.written();

        // Handle lexer mode
        if (std.mem.eql(u8, mode.?, "--lex")) {
            var lexer = try Lexer.init(std.heap.page_allocator, line);
            defer lexer.deinit();
            var tok = try lexer.nextToken();
            while (tok.type != .eof) : (tok = try lexer.nextToken()) {
                try tok.write(out_iface);
                try out_iface.printAsciiChar('\n', .{});
                try out_iface.flush();
            }
        }

        // Handle parse mode
        if (std.mem.eql(u8, mode.?, "--parse")) {
            var lexer = try Lexer.init(std.heap.page_allocator, line);
            defer lexer.deinit();

            var parser = try Parser.init(std.heap.page_allocator, &lexer);
            defer parser.deinit();

            const program = try parser.parseProgram();
            try program.write(out_iface);
            try out_iface.printAsciiChar('\n', .{});
            try out_iface.flush();
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
            return 0;
        },
        else => {
            print("error: {any}", .{err});
        },
    }
    return 0;
}

pub fn parse_file(args: *std.process.ArgIterator) CommandErrUnion!u8 {
    const file_name = args.next();

    if (file_name == null) {
        return 1;
    }

    // Open file
    const file_handle = try std.fs.cwd().openFile(file_name.?, .{});
    const stat = try file_handle.stat();
    const file_size = stat.size;

    // Read contents
    var input_buffer: [2048]u8 = undefined;
    var reader = file_handle.reader(&input_buffer);
    const alloc = std.heap.smp_allocator;
    const file_data = try reader.interface.readAlloc(alloc, file_size);

    // Create and run parser
    var lexer = try Lexer.init(alloc, file_data);
    var parser = try Parser.init(alloc, &lexer);
    const program = try parser.parseProgram();

    // Create stdout writer
    var output_buffer: [1028]u8 = undefined;
    const stdout = std.fs.File.stdout();
    var writer = stdout.writer(&output_buffer);

    // Write to stdodut
    try program.write(&writer.interface);
    try writer.interface.flush();

    return 0;
}

pub fn fmt_file(args: *std.process.ArgIterator) CommandErrUnion!u8 {
    const file_name = args.next();

    if (file_name == null) {
        return 1;
    }

    // Open file
    const file_handle = try std.fs.cwd().openFile(file_name.?, .{});
    const stat = try file_handle.stat();
    const file_size = stat.size;

    // Read contents
    var input_buffer: [2048]u8 = undefined;
    var reader = file_handle.reader(&input_buffer);
    const alloc = std.heap.smp_allocator;
    const file_data = try reader.interface.readAlloc(alloc, file_size);

    // Create and run parser
    var lexer = try Lexer.init(alloc, file_data);
    var parser = try Parser.init(alloc, &lexer);
    const program = try parser.parseProgram();

    // Create stdout writer
    var output_buffer: [1028]u8 = undefined;
    const stdout = std.fs.File.stdout();
    var writer = stdout.writer(&output_buffer);

    // Pretty print to stdout
    var pp = PrettyPrinter.init(&writer.interface);
    try program.pretty(&pp);
    try writer.interface.flush();

    return 0;
}

pub fn build(args: *std.process.ArgIterator) CommandErrUnion!u8 {
    const input_file = args.next();
    const o_flag = args.next();
    const output_file = args.next();

    if (input_file == null or o_flag == null or output_file == null or (!std.mem.eql(u8, "-o", o_flag.?))) {
        std.log.err("unexpected arguments for build", .{});
        return try help(args);
    }

    // Open file
    const file_handle = try std.fs.cwd().openFile(input_file.?, .{});
    const stat = try file_handle.stat();
    const file_size = stat.size;

    // Read contents
    var input_buffer: [2048]u8 = undefined;
    var reader = file_handle.reader(&input_buffer);
    const alloc = std.heap.page_allocator;
    const file_data = try reader.interface.readAlloc(alloc, file_size);

    // Create and run parser
    var lexer = try Lexer.init(alloc, file_data);
    var parser = try Parser.init(alloc, &lexer);
    const program = try parser.parseProgram();

    // Create a compiler and compile
    var compiler = try QBECompiler.init(alloc);
    try program.compile(&compiler);

    // Output the final ssa file
    try compiler.emitFile(output_file.?);

    return 0;
}

test {
    // Test declaration references
    const parser_tests = @import("tests/parser.test.zig");
    const lexer_tests = @import("tests/lexer.test.zig");
    const token_tests = @import("tests/token.test.zig");
    const ast_tests = @import("tests/ast.test.zig");
    const pretty_printer_tests = @import("tests/pretty_printer.test.zig");
    const qbe_data_tests = @import("qbe/tests/data.test.zig");
    const qbe_function_tests = @import("qbe/tests/function.test.zig");

    std.testing.refAllDecls(token_tests);
    std.testing.refAllDecls(lexer_tests);
    std.testing.refAllDecls(parser_tests);
    std.testing.refAllDecls(ast_tests);
    std.testing.refAllDecls(pretty_printer_tests);
    std.testing.refAllDecls(qbe_data_tests);
    std.testing.refAllDecls(qbe_function_tests);
}
