const std = @import("std");
const data = @import("data.zig");
const symbol_table = @import("symbol_table.zig");
const function = @import("function.zig");
pub const Data = data;
pub const Function = function;
const Writer = std.Io.Writer;
const FunctionSection = function.FunctionSection;
const SymbolTable = symbol_table.SymbolTable;

/// QBECompiler updates and emits a stored state for a QBE program
pub const QBECompiler = struct {
    alloc: std.mem.Allocator,
    data: *data.Data,
    functions: *FunctionSection,
    current_function: *function.Function,
    symbol_table: *SymbolTable,

    /// Returns an initialized QBECompiler
    pub fn init(alloc: std.mem.Allocator) !QBECompiler {

        // Create data section
        const data_section: *data.Data = try alloc.create(data.Data);
        data_section.* = data.Data.init(alloc);

        // Create function section
        var functions = try alloc.create(FunctionSection);
        functions.* = FunctionSection.init(alloc);

        // Add main function
        const main = try alloc.create(function.Function);
        main.* = try function.Function.init(alloc, "main", .w, .@"export");
        try functions.add(main);

        const symtab = try alloc.create(SymbolTable);
        symtab.* = try SymbolTable.init(alloc);

        return .{
            .alloc = alloc,
            .data = data_section,
            .functions = functions,
            .current_function = main,
            .symbol_table = symtab,
        };
    }

    pub fn deinit(self: *QBECompiler) void {
        self.data.deinit();
        self.functions.deinit();
        self.symbol_table.deinit();
        self.alloc.destroy(self.symbol_table);
        // FIXME: need to deallocate the data and function as well
        // TODO: add a test that verifies no leaks with this
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
