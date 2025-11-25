const std = @import("std");
const data = @import("data.zig");
const symbol_table = @import("symbol_table.zig");
const function = @import("function.zig");
const boxed = @import("nan_box.zig");
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
    pub fn write(self: *QBECompiler, writer: *Writer) !void {
        try self.data.write(writer);
        try self.functions.write(writer);
        try writer.flush();
        return;
    }

    /// Write QBE IR to a file specified by subpath
    pub fn writeFile(self: *QBECompiler, subpath: []const u8) !void {
        const file_handle = try std.fs.cwd().createFile(subpath, .{});
        defer file_handle.close();

        var file_writer_buffer: [2048]u8 = undefined;
        var writer = file_handle.writer(&file_writer_buffer);

        try self.write(&writer.interface);
    }

    /// Add a structured instruction to the current function current block state
    pub fn emitAssignment(self: *QBECompiler, datatype: function.Type, comptime rhs_fmt: []const u8, args: anytype) !*Temporary {
        const temp = try self.symbol_table.createTemporary(datatype);
        const rhs = try std.fmt.allocPrint(self.alloc, rhs_fmt, args);
        defer self.alloc.free(rhs);

        const instr = try std.fmt.allocPrint(self.alloc, "%{s} ={s} {s}", .{ temp.name, @tagName(datatype), rhs });
        defer self.alloc.free(instr);
        _ = try self.current_function.current_block.?.addInstruction(instr);

        // Return temp incase caller needs to use variable
        return temp;
    }

    /// Add string instruction without creating a temporary or assignment
    pub fn emitRaw(self: *QBECompiler, comptime fmt: []const u8, args: anytype) !void {
        const instr = try std.fmt.allocPrint(self.alloc, fmt, args);
        defer self.alloc.free(instr);
        _ = try self.current_function.current_block.?.addInstruction(instr);
    }

    /// Prints an error message and exits immediately
    pub fn emitPanic(self: *QBECompiler, comptime fmt: []const u8) !void {
        const err_string = try self.data.addString(fmt);
        _ = try self.emitAssignment(.l, "call $puts(l {s})", .{err_string});
        _ = try self.emitAssignment(.l, "call $exit()", .{});
    }

    /// Emit code to unbox the value out of a nan boxed value
    pub fn emitUnboxing(self: *QBECompiler, temp: *Temporary) !*Temporary {
        // FIXME: I think this needs to use .d conditionally based on the boxed value
        const long_casted = try self.emitAssignment(.l, "cast %{s}", .{temp.name});
        const mask: u64 = 0x0000FFFFFFFFFFFF;
        const masked = try self.emitAssignment(.l, "and %{s}, {d}", .{ long_casted.name, mask });
        return masked;
    }

    pub fn emitBoxing(self: *QBECompiler, comptime box_type: boxed.BoxedValueType, unboxed: *Temporary) !*Temporary {
        const empty_box = boxed.BoxedValue.emptyBoxOf(box_type);
        const casted = try self.emitAssignment(.l, "cast %{s}", .{unboxed.name});
        return try self.emitAssignment(.d, "or %{s}, {d}", .{ casted.name, empty_box });
    }

    pub fn emitRuntimeBoxing(self: *QBECompiler, basis: *Temporary, unboxed: *Temporary) !*Temporary {
        // TODO: might be a bug where the .l and .d aren't right and i need to cast to d always
        // const casted = try self.emitAssignment(.l, "cast %{s}", .{unboxed.name});
        const mask: u64 = 0xFFFF000000000000;
        const casted_basis = try self.emitAssignment(.l, "cast %{s}", .{basis.name});
        const empty_box = try self.emitAssignment(.l, "and %{s}, {d}", .{ casted_basis.name, mask });
        // const casted_box = try self.emitAssignment(.d, "cast %{s}", .{empty_box.name});
        const result = try self.emitAssignment(.l, "or %{s}, %{s}", .{ unboxed.name, empty_box.name });
        const casted_result = try self.emitAssignment(.d, "cast %{s}", .{result.name});
        return casted_result;
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
