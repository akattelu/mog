const std = @import("std");
const function = @import("../function.zig");
const Function = function.Function;
const FunctionSection = function.FunctionSection;
const Block = function.Block;
const instruction = @import("../instruction.zig");
const Instruction = instruction.Instruction;
const Call = instruction.Call;
const Ret = instruction.Ret;
const Parameter = function.Parameter;
const Type = instruction.Type;
const Linkage = function.Linkage;

test "Block.addNewCall creates and adds call instruction" {
    const alloc = std.testing.allocator;

    // Create a block
    var block = try Block.init(alloc, "test_block");
    defer block.deinit();

    // Add a new call instruction
    const call = try block.addNewCall("printf");

    // Verify the call was created
    try std.testing.expectEqualStrings("printf", call.function_name);
    try std.testing.expectEqual(@as(usize, 0), call.args.items.len);

    // Add arguments to the call
    try call.add_arg(.w, "%x");
    try call.add_arg(.l, "42");

    // Verify arguments were added
    try std.testing.expectEqual(@as(usize, 2), call.args.items.len);
    try std.testing.expectEqualStrings("%x", call.args.items[0].value);
    try std.testing.expectEqual(Type.w, call.args.items[0].arg_type);
    try std.testing.expectEqualStrings("42", call.args.items[1].value);
    try std.testing.expectEqual(Type.l, call.args.items[1].arg_type);

    // Verify the instruction was added to the block
    try std.testing.expectEqual(@as(usize, 1), block.instructions.items.len);

    // Verify the instruction's structure
    const instr = block.instructions.items[0];
    try std.testing.expectEqual(@as(?[]const u8, null), instr.lhs);
    try std.testing.expectEqual(@as(?Type, null), instr.assign_type);
}

test "FunctionSection allocation and deallocation" {
    const alloc = std.testing.allocator;

    // Create function section
    var section = FunctionSection.init(alloc);
    defer section.deinit();

    // Create first function with a block and instructions
    const func1 = try alloc.create(Function);
    func1.* = try Function.init(alloc, "main", .w, .@"export");
    try section.add(func1);

    // Add a call to the start block
    const call1 = try func1.current_block.?.addNewCall("puts");
    try call1.add_arg(.l, "%msg");

    // Add a return instruction
    const ret_instr = try alloc.create(Instruction);
    ret_instr.* = .{
        .lhs = null,
        .assign_type = null,
        .rhs = .{ .ret = .{ .value = "0" } },
    };
    try func1.current_block.?.instructions.append(alloc, ret_instr);

    // Create second function
    const func2 = try alloc.create(Function);
    func2.* = try Function.init(alloc, "helper", null, .none);
    try section.add(func2);

    // Add a call to the helper function's start block
    const call2 = try func2.current_block.?.addNewCall("exit");
    try call2.add_arg(.w, "1");

    // Verify functions were added
    try std.testing.expectEqual(@as(usize, 2), section.functions.items.len);
    try std.testing.expectEqualStrings("main", section.functions.items[0].name);
    try std.testing.expectEqualStrings("helper", section.functions.items[1].name);

    // deinit will be called automatically and should not leak
}

test "FunctionSection.emit outputs correct QBE IR" {
    const alloc = std.testing.allocator;

    // Initialize writer
    var writer = std.Io.Writer.Allocating.init(alloc);
    defer writer.deinit();

    // Create function section
    var section = FunctionSection.init(alloc);
    defer section.deinit();

    // Create a simple function: export function w $add(w %a, w %b)
    const func = try alloc.create(Function);
    func.* = try Function.init(alloc, "add", .w, .@"export");

    // Add parameters
    _ = try func.createParameter("a", .w);
    _ = try func.createParameter("b", .w);

    // Add instructions to start block
    // %sum =w add %a, %b (we'll use a call as placeholder since add isn't implemented yet)
    const call = try func.current_block.?.addNewCall("__internal_add");
    try call.add_arg(.w, "%a");
    try call.add_arg(.w, "%b");

    // Modify the instruction to have an assignment
    func.current_block.?.instructions.items[0].lhs = "sum";
    func.current_block.?.instructions.items[0].assign_type = .w;

    // Add return instruction
    const ret_instr = try alloc.create(Instruction);
    ret_instr.* = .{
        .lhs = null,
        .assign_type = null,
        .rhs = .{ .ret = .{ .value = "%sum" } },
    };
    try func.current_block.?.instructions.append(alloc, ret_instr);

    // Add function to section
    try section.add(func);

    // Expected output
    const expected =
        "export function w $add(w %a, w %b) {\n" ++
        "@start\n" ++
        "\t%sum =w call $__internal_add(w %a, w %b)\n" ++
        "\tret %sum\n" ++
        "}\n";

    // Emit and verify
    try section.emit(&writer.writer);
    try std.testing.expectEqualStrings(expected, writer.written());
}
