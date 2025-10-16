const std = @import("std");

/// QBE instruction data types
pub const Type = enum { w, l, s, d, sb, ub, sh, uh };

/// QBE function linkage types
pub const Linkage = enum {
    none,
    @"export",
    thread,
    section,
};

/// QBE function parameter
/// Represents a single parameter in a function signature
pub const Parameter = struct {
    /// Parameter name (temporary variable without % sigil)
    name: []const u8,
    /// Parameter type
    param_type: Type,
};

/// QBE instruction operations
/// This enum will be expanded as more instruction types are implemented
pub const InstructionType = enum { call, ret };

/// QBE instruction
/// Represents a single instruction in a basic block
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

/// QBE basic block
/// A block is a sequence of instructions with a single entry point (the label)
/// and a single exit point (a terminating instruction like ret or jmp)
pub const Block = struct {
    /// Block label (without @ sigil, e.g., "start", "loop_body")
    label: []const u8,
    /// Instructions in this block, ending with a terminating instruction
    instructions: std.ArrayList(*Instruction),
    /// Allocator for memory management
    alloc: std.mem.Allocator,

    /// Initialize a new block with the given label
    pub fn init(allocator: std.mem.Allocator, label: []const u8) !Block {
        const copied_label = try allocator.dupe(u8, label);
        return .{
            .label = copied_label,
            .instructions = try std.ArrayList(*Instruction).initCapacity(allocator, 5),
            .alloc = allocator,
        };
    }

    /// Deinitialize the block and free instruction list
    pub fn deinit(self: *Block) void {
        self.instructions.deinit(self.alloc);
        self.alloc.free(self.label);
    }

    /// Add an instruction to this block
    pub fn addInstruction(self: *Block, instruction: *Instruction) !void {
        try self.instructions.append(self.alloc, instruction);
    }

    pub fn addNewCall(self: *Block, name: []const u8) !*Call {
        const instr: *Instruction = try self.alloc.create(Instruction);
        const call: *Call = try self.alloc.create(Call);
        call.* = try Call.init(self.alloc, name);
        instr.* = .{ .lhs = null, .assign_type = null, .rhs = .{ .call = call } };
        try self.addInstruction(instr);
        return call;
    }

    /// Emit block to writer
    /// Format: @label followed by tab-indented instructions
    pub fn emit(self: *const Block, writer: *std.Io.Writer) !void {
        try writer.print("@{s}\n", .{self.label});
        for (self.instructions.items) |instruction| {
            try writer.writeByte('\t');
            try instruction.emit(writer);
            try writer.writeByte('\n');
        }
    }
};

/// QBE function definition
pub const Function = struct {
    /// Function name (without $ sigil)
    name: []const u8,
    /// Optional return type (null for void functions)
    return_type: ?Type,
    /// Linkage specification
    linkage: Linkage,
    /// Function parameters
    params: std.ArrayList(*Parameter),
    /// Map of block labels to blocks (maintains insertion order for emission)
    blocks: std.StringArrayHashMap(*Block),
    /// Allocator for memory management
    allocator: std.mem.Allocator,
    /// Pointer to current block
    current_block: *Block,

    /// Initialize a new function with the given name and linkage
    /// name needs to outlive the struct lifetime
    pub fn init(
        allocator: std.mem.Allocator,
        name: []const u8,
        return_type: ?Type,
        linkage: Linkage,
    ) !Function {

        // Copy name
        const copied_name = try allocator.dupe(u8, name);
        // every function starts with a @start block
        const start_block = try allocator.create(Block);
        start_block.* = try Block.init(allocator, "start");
        var func: Function = .{
            .name = copied_name,
            .return_type = return_type,
            .linkage = linkage,
            .params = try std.ArrayList(*Parameter).initCapacity(allocator, 4),
            .blocks = std.StringArrayHashMap(*Block).init(allocator),
            .allocator = allocator,
            .current_block = start_block,
        };
        try func.addBlock(start_block);
        return func;
    }

    /// Deinitialize the function and free all resources
    pub fn deinit(self: *Function) void {
        self.params.deinit(self.allocator);
        var block_iter = self.blocks.iterator();
        while (block_iter.next()) |entry| {
            const block = entry.value_ptr.*;
            block.deinit();
            self.allocator.destroy(block);
        }
        self.blocks.deinit();
        self.allocator.free(self.name);
    }

    /// Add a parameter to the function signature
    pub fn addParameter(self: *Function, param: *Parameter) !void {
        try self.params.append(self.allocator, param);
    }

    /// Add a block to the function body
    /// The block is moved into the function's ownership
    pub fn addBlock(self: *Function, block: *Block) !void {
        try self.blocks.put(block.label, block);
    }

    /// Get a block by its label (returns null if not found)
    pub fn getBlock(self: *Function, label: []const u8) ?*Block {
        return self.blocks.getPtr(label);
    }

    /// Emit function to writer
    /// Format: [linkage] function [return_type] $name(params) { blocks }
    pub fn emit(self: *const Function, writer: *std.Io.Writer) !void {
        // Emit linkage if not none
        switch (self.linkage) {
            .none => {},
            .@"export" => try writer.print("export ", .{}),
            .thread => try writer.print("thread ", .{}),
            .section => try writer.print("section ", .{}),
        }

        // Emit function keyword
        try writer.print("function ", .{});

        // Emit return type if present
        if (self.return_type) |rtype| {
            try writer.print("{s} ", .{@tagName(rtype)});
        }

        // Emit function name
        try writer.print("${s}(", .{self.name});

        // Emit parameters
        for (self.params.items, 0..) |param, i| {
            if (i > 0) try writer.print(", ", .{});
            try writer.print("{s} %{s}", .{ @tagName(param.param_type), param.name });
        }

        try writer.print(") {{\n", .{});

        // Emit blocks in order
        var block_iter = self.blocks.iterator();
        while (block_iter.next()) |entry| {
            try entry.value_ptr.*.emit(writer);
        }

        try writer.print("}}\n", .{});
    }
};

/// Struct corresponding to function section of QBE IR (array of function definitions)
pub const FunctionSection = struct {
    functions: std.ArrayList(*Function),
    allocator: std.mem.Allocator,

    /// Create new function section
    pub fn init(allocator: std.mem.Allocator) FunctionSection {
        return .{
            .functions = std.ArrayList(*Function).empty,
            .allocator = allocator,
        };
    }

    /// Deinitialize the function section and free all functions
    pub fn deinit(self: *FunctionSection) void {
        for (self.functions.items) |func| {
            func.deinit();
        }
        self.functions.deinit(self.allocator);
    }

    /// Add a function to the section
    pub fn add(self: *FunctionSection, func: *Function) !void {
        try self.functions.append(self.allocator, func);
    }

    /// Emit IR for the function section
    pub fn emit(self: *const FunctionSection, writer: *std.Io.Writer) !void {
        for (self.functions.items, 0..) |func, i| {
            try func.emit(writer);
            // Add newline between functions (but not after the last one)
            if (i < self.functions.items.len - 1) {
                try writer.writeByte('\n');
            }
        }
    }
};
