const std = @import("std");
const data = @import("data.zig");
pub const Data = data;
const function = @import("function.zig");
pub const Function = function;
const Writer = std.Io.Writer;
const FunctionSection = function.FunctionSection;

/// QBECompiler updates and emits a stored state for a QBE program
pub const QBECompiler = struct {
    alloc: std.mem.Allocator,
    data: *data.Data,
    functions: *FunctionSection,
    current_function: *function.Function,

    /// Returns an initialized QBECompiler
    pub fn init(alloc: std.mem.Allocator) !QBECompiler {

        // Create data section
        const data_section: *data.Data = try alloc.create(data.Data);
        data_section.* = data.Data.init(alloc);
        // data_section.* = .{ .items = .{}, .alloc = arena_allocator };

        // Create function section
        var functions = try alloc.create(FunctionSection);
        functions.* = FunctionSection.init(alloc);

        // Add main function
        const main = try alloc.create(function.Function);
        main.* = try function.Function.init(alloc, "main", .w, .@"export");
        try functions.add(main);

        return .{
            .alloc = alloc,
            .data = data_section,
            .functions = functions,
            .current_function = main,
        };
    }

    pub fn deinit(self: *QBECompiler) void {
        self.data.deinit();
        self.functions.deinit();
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
