const std = @import("std");
const Writer = std.Io.Writer;

/// QBECompiler updates and emits a stored state for a QBE program
pub const QBECompiler = struct {
    arena: std.heap.ArenaAllocator,

    /// Returns an initialized QBECompiler
    pub fn init(alloc: std.mem.Allocator) QBECompiler {
        const arena = std.heap.ArenaAllocator.init(alloc);
        return .{ .arena = arena };
    }

    /// Emit QBE IR to the writer
    pub fn emit(self: *QBECompiler, writer: *Writer) !void {
        _ = self;
        const ssa_content =
            \\ # Define the string constant.
            \\ data $str = { b "hello world", b 0 }
            \\ 
            \\ export function w $main() {
            \\ @start
            \\         # Call the puts function with $str as argument.
            \\         %r =w call $puts(l $str)
            \\         ret 0
            \\ }
        ;

        try writer.writeAll(ssa_content);
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
