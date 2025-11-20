const std = @import("std");
const data = @import("data.zig");
const symbol_table = @import("symbol_table.zig");
const function = @import("function.zig");
pub const Data = data;
pub const Function = function;
const Writer = std.Io.Writer;
const FunctionSection = function.FunctionSection;
const SymbolTable = symbol_table.SymbolTable;
const Temporary = symbol_table.Temporary;

/// QBECompiler updates and emits a stored state for a QBE program
pub const QBECompiler = struct {
    alloc: std.mem.Allocator,
    data: *data.Data,
    functions: *FunctionSection,
    current_function: *function.Function,
    symbol_table: *SymbolTable,
    error_string: ?[]const u8,

    pub const Error = error{Invalid};

    /// Returns an initialized QBECompiler
    pub fn init(alloc: std.mem.Allocator) !QBECompiler {

        // Create data section
        const data_section: *data.Data = try alloc.create(data.Data);
        data_section.* = data.Data.init(alloc);

        // Create function section
        var functions = try alloc.create(FunctionSection);
        functions.* = FunctionSection.init(alloc);
        const main = try functions.addMainFunction();

        const symtab = try alloc.create(SymbolTable);
        symtab.* = try SymbolTable.init(alloc);

        return .{
            .alloc = alloc,
            .data = data_section,
            .functions = functions,
            .current_function = main,
            .symbol_table = symtab,
            .error_string = null,
        };
    }

    pub fn deinit(self: *QBECompiler) void {
        self.data.deinit();
        self.alloc.destroy(self.data);
        self.functions.deinit();
        self.alloc.destroy(self.functions);
        self.symbol_table.deinit();
        self.alloc.destroy(self.symbol_table);
        if (self.error_string) |s| {
            self.alloc.free(s);
        }
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

    /// Add a structured instruction to the current function current block state
    pub fn emitAssignment(self: *QBECompiler, datatype: function.Type, rhs: []const u8) !*Temporary {
        const temp = try self.symbol_table.createTemporary(datatype);
        const instr = try std.fmt.allocPrint(self.alloc, "%{s} ={s} {s}", .{ temp.name, @tagName(datatype), rhs });
        defer self.alloc.free(instr);
        _ = try self.current_function.current_block.?.addInstruction(instr);

        // Return temp incase caller needs to use variable
        return temp;
    }

    /// Add string instruction without creating a temporary or assignment
    pub fn emitString(self: *QBECompiler, rhs: []const u8) !void {
        _ = try self.current_function.current_block.?.addInstruction(rhs);
    }

    /// Call $printf for debugging
    pub fn addDebugPrint(self: *QBECompiler, temp: *Temporary, fmt: []const u8) !void {
        const s = try self.data.addString(fmt);
        const s_copy_instr = try std.fmt.allocPrint(self.alloc, "copy {s}", .{s});
        defer self.alloc.free(s_copy_instr);
        const s_local = try self.emitAssignment(.l, s_copy_instr);

        const rhs = try std.fmt.allocPrint(self.alloc, "call $printf(l %{s}, ..., {s} %{s})", .{ s_local.name, @tagName(temp.datatype), temp.name });
        defer self.alloc.free(rhs);
        _ = try self.emitAssignment(.w, rhs);
    }

    /// Store current block and set current_block ptr
    pub fn pushBlock(self: *QBECompiler, block: *function.Block) !void {
        try self.current_function.putBlock(block);
    }

    /// Update error string and return Invalid
    pub fn withError(self: *QBECompiler, comptime fmt: []const u8, args: anytype) (std.mem.Allocator.Error || Error) {
        self.error_string = try std.fmt.allocPrint(self.alloc, fmt, args);
        return Error.Invalid;
    }
};
