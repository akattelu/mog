const std = @import("std");
const data = @import("data.zig");
pub const Data = data;
const function = @import("function.zig");
const Writer = std.Io.Writer;
const FunctionSection = function.FunctionSection;
const Function = function.Function;

/// QBECompiler updates and emits a stored state for a QBE program
pub const QBECompiler = struct {
    arena: std.heap.ArenaAllocator,
    data: data.Data,
    functions: FunctionSection,
    current_function: *Function,

    /// Returns an initialized QBECompiler
    pub fn init(alloc: std.mem.Allocator) !QBECompiler {
        var arena = std.heap.ArenaAllocator.init(alloc);
        const arena_allocator = arena.allocator();

        var functions = FunctionSection.init(arena_allocator);

        // Add main function
        var main = try Function.init(arena_allocator, "main", .w, .@"export");
        try functions.add(&main);

        const start_block = try function.Block.init(arena_allocator, "start");
        // TODO: consider updating this functions signature to take a reference
        try main.addBlock(start_block);

        return .{
            .arena = arena,
            .data = data.Data.init(),
            .functions = functions,
            .current_function = &main,
        };
    }

    /// Emit QBE IR to the writer
    pub fn emit(self: *QBECompiler, writer: *Writer) !void {
        try self.data.emit(writer);
        try self.functions.emit(writer);
        try writer.flush();
        return;
    }

    /// Emit QBE IR to a file specified by subpath
    pub fn emitFile(self: *QBECompiler, subpath: []const u8) !void {
        const file_handle = try std.fs.cwd().createFile(subpath, .{});
        defer file_handle.close();

        var file_writer_buffer: [2048]u8 = undefined;
        var writer = file_handle.writer(&file_writer_buffer);

        try self.emit(&writer.interface);
    }
};
