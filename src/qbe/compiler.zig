const std = @import("std");
const data = @import("data.zig");
pub const Data = data;
const function = @import("function.zig");
pub const Function = function;
const Writer = std.Io.Writer;
const FunctionSection = function.FunctionSection;

/// QBECompiler updates and emits a stored state for a QBE program
pub const QBECompiler = struct {
    arena: std.heap.ArenaAllocator,
    data: *data.Data,
    functions: *FunctionSection,
    current_function: *function.Function,

    /// Returns an initialized QBECompiler
    pub fn init(alloc: std.mem.Allocator) !QBECompiler {
        var arena = std.heap.ArenaAllocator.init(alloc);
        const arena_allocator = arena.allocator();

        // Create data section
        const data_section: *data.Data = try arena_allocator.create(data.Data);
        data_section.* = data.Data.init(arena_allocator);
        // data_section.* = .{ .items = .{}, .alloc = arena_allocator };

        // Create function section
        var functions = try arena_allocator.create(FunctionSection);
        functions.* = FunctionSection.init(arena_allocator);

        // Add main function
        const main = try arena_allocator.create(function.Function);
        main.* = try function.Function.init(arena_allocator, "main", .w, .@"export");
        try functions.add(main);

        return .{
            .arena = arena,
            .data = data_section,
            .functions = functions,
            .current_function = main,
        };
    }

    pub fn deinit(self: *QBECompiler) void {
        self.arena.deinit();
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
        std.log.info("emitting file {s} with compiler state: {any}", .{ subpath, self });
        const file_handle = try std.fs.cwd().createFile(subpath, .{});
        defer file_handle.close();

        var file_writer_buffer: [2048]u8 = undefined;
        var writer = file_handle.writer(&file_writer_buffer);

        try self.emit(&writer.interface);
    }
};
