const std = @import("std");
/// QBE instruction data types
pub const Type = enum { w, l, s, d, sb, ub, sh, uh };

/// QBE Instruction with LHS, RHS (union), and type
pub const Instruction = struct {
    /// Left-hand side temporary variable (optional, without % sigil)
    lhs: ?[]const u8,
    /// Type of the assignment result (optional)
    assign_type: ?Type,
    /// The instruction operation and its operands
    rhs: union(InstructionType) {
        call: *Call,
        ret: Ret,
    },

    /// Emit instruction to writer
    /// Format: [%lhs =type] operation
    pub fn emit(self: *const Instruction, writer: *std.Io.Writer) !void {
        // Emit optional assignment (lhs and type)
        if (self.lhs) |lhs_name| {
            if (self.assign_type) |atype| {
                try writer.print("%{s} ={s} ", .{ lhs_name, @tagName(atype) });
            } else {
                try writer.print("%{s} = ", .{lhs_name});
            }
        }

        // Emit the operation (call or ret)
        switch (self.rhs) {
            .call => |call| try call.emit(writer),
            .ret => |ret| try ret.emit(writer),
        }
    }
};

/// QBE instruction operations
/// This enum will be expanded as more instruction types are implemented
pub const InstructionType = enum { call, ret };

/// QBE instruction
/// Represents a single instruction in a basic block
///
/// QBE call instruction argument
pub const Argument = struct {
    /// Argument type
    arg_type: Type,
    /// Argument value (temporary variable with % prefix, or literal)
    value: []const u8,
};

/// QBE call instruction
pub const Call = struct {
    /// Function name to call (without $ sigil)
    function_name: []const u8,
    /// Arguments passed to the function
    args: std.ArrayList(*Argument),
    /// Allocator
    alloc: std.mem.Allocator,

    pub fn init(alloc: std.mem.Allocator, name: []const u8) !Call {
        const name_copy = try alloc.dupe(u8, name);
        return .{
            .alloc = alloc,
            .args = try std.ArrayList(*Argument).initCapacity(alloc, 3),
            .function_name = name_copy,
        };
    }

    pub fn add_arg(self: *Call, arg_type: Type, arg: []const u8) !void {
        const arg_ptr = try self.alloc.create(Argument);
        // FIXME: make sure deinit() frees this name
        const arg_name_copy = try self.alloc.dupe(u8, arg);
        arg_ptr.* = .{
            .arg_type = arg_type,
            .value = arg_name_copy,
        };
        try self.args.append(self.alloc, arg_ptr);
    }

    pub fn deinit(self: *Call) void {
        self.args.deinit(self.alloc);
        self.alloc.free(self.function_name);
    }

    /// Emit call instruction to writer
    /// Format: call $function_name(type arg1, type arg2, ...)
    pub fn emit(self: *const Call, writer: *std.Io.Writer) !void {
        try writer.print("call ${s}(", .{self.function_name});
        for (self.args.items, 0..) |arg, i| {
            if (i > 0) try writer.print(", ", .{});
            try writer.print("{s} {s}", .{ @tagName(arg.arg_type), arg.value });
        }
        try writer.print(")", .{});
    }
};

/// QBE return instruction
pub const Ret = struct {
    /// Optional return value (null for void returns)
    value: ?[]const u8,

    /// Emit return instruction to writer
    /// Format: ret or ret %value
    pub fn emit(self: *const Ret, writer: *std.Io.Writer) !void {
        if (self.value) |val| {
            try writer.print("ret {s}", .{val});
        } else {
            try writer.print("ret", .{});
        }
    }
};
