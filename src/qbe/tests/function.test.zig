const std = @import("std");
const function = @import("../function.zig");
const Function = function.Function;
const FunctionSection = function.FunctionSection;
const Block = function.Block;
const Instruction = function.Instruction;
const Call = function.Call;
const Ret = function.Ret;
const Parameter = function.Parameter;
const Argument = function.Argument;
const Type = function.Type;
const Linkage = function.Linkage;

// test "emit simple function with return" {
//     // Initialize writer
//     const alloc = std.testing.allocator;
//     var writer = std.Io.Writer.Allocating.init(alloc);
//     defer writer.deinit();

//     // Create function: function w $add(w %a, w %b) { @start ... }
//     var func = try Function.init(alloc, "add", .w, .none);
//     defer func.deinit();

//     // Add parameters
//     var param_a: Parameter = .{ .name = "a", .param_type = .w };
//     var param_b: Parameter = .{ .name = "b", .param_type = .w };
//     try func.addParameter(&param_a);
//     try func.addParameter(&param_b);

//     // Add instructions to the start block (automatically created by Function.init)
//     const args = [_]Argument{
//         .{ .arg_type = .w, .value = "%a" },
//         .{ .arg_type = .w, .value = "%b" },
//     };
//     const result_call: Instruction = .{
//         .lhs = "result",
//         .assign_type = .w,
//         .rhs = .{ .call = .{ .function_name = "internal_add", .args = &args } },
//     };
//     try func.current_block.addInstruction(&result_call);

//     // Add return instruction: ret %result
//     const empty_ret = .{
//         .lhs = null,
//         .assign_type = null,
//         .rhs = .{ .ret = .{ .value = "%result" } },
//     };
//     try func.current_block.addInstruction(&empty_ret);

//     // Emit and verify
//     const expected = "function w $add(w %a, w %b) {\n" ++
//         "@start\n" ++
//         "\t%result =w call $internal_add(w %a, w %b)\n" ++
//         "\tret %result\n" ++
//         "}\n";

//     try func.emit(&writer.writer);
//     try std.testing.expectEqualStrings(expected, writer.written());
// }

// test "emit exported function with multiple blocks" {
//     // Initialize writer
//     const alloc = std.testing.allocator;
//     var writer = std.Io.Writer.Allocating.init(alloc);
//     defer writer.deinit();

//     // Create exported function: export function w $main() { ... }
//     var func = try Function.init(alloc, "main", .w, .@"export");
//     defer func.deinit();

//     // Add instructions to the start block (automatically created by Function.init)
//     const args = [_]Argument{
//         .{ .arg_type = .l, .value = "$str" },
//     };
//     try func.current_block.addInstruction(.{
//         .lhs = "r",
//         .assign_type = .w,
//         .rhs = .{ .call = .{ .function_name = "puts", .args = &args } },
//     });

//     // Add return instruction: ret 0
//     try func.current_block.addInstruction(.{
//         .lhs = null,
//         .assign_type = null,
//         .rhs = .{ .ret = .{ .value = "0" } },
//     });

//     // Emit and verify
//     const expected = "export function w $main() {\n" ++
//         "@start\n" ++
//         "\t%r =w call $puts(l $str)\n" ++
//         "\tret 0\n" ++
//         "}\n";

//     try func.emit(&writer.writer);
//     try std.testing.expectEqualStrings(expected, writer.written());
// }

// test "emit void function with no parameters" {
//     // Initialize writer
//     const alloc = std.testing.allocator;
//     var writer = std.Io.Writer.Allocating.init(alloc);
//     defer writer.deinit();

//     // Create void function: function $cleanup() { ... }
//     var func = try Function.init(alloc, "cleanup", null, .none);
//     defer func.deinit();

//     // Add instructions to the start block (automatically created by Function.init)
//     const no_args = [_]Argument{};
//     try func.current_block.addInstruction(.{
//         .lhs = null,
//         .assign_type = null,
//         .rhs = .{ .call = .{ .function_name = "free", .args = &no_args } },
//     });

//     // Add void return: ret
//     try func.current_block.addInstruction(.{
//         .lhs = null,
//         .assign_type = null,
//         .rhs = .{ .ret = .{ .value = null } },
//     });

//     // Emit and verify
//     const expected = "function $cleanup() {\n" ++
//         "@start\n" ++
//         "\tcall $free()\n" ++
//         "\tret\n" ++
//         "}\n";

//     try func.emit(&writer.writer);
//     try std.testing.expectEqualStrings(expected, writer.written());
// }

// test "emit function section with multiple functions" {
//     // Initialize writer
//     const alloc = std.testing.allocator;
//     var writer = std.Io.Writer.Allocating.init(alloc);
//     defer writer.deinit();

//     // Create function section
//     var section = FunctionSection.init(alloc);
//     defer section.deinit();

//     // Create first function: function w $add(w %a, w %b)
//     var func1 = try Function.init(alloc, "add", .w, .none);
//     var param_a: Parameter = .{ .name = "a", .param_type = .w };
//     var param_b: Parameter = .{ .name = "b", .param_type = .w };
//     try func1.addParameter(&param_a);
//     try func1.addParameter(&param_b);

//     const args1 = [_]Argument{
//         .{ .arg_type = .w, .value = "%a" },
//         .{ .arg_type = .w, .value = "%b" },
//     };
//     try func1.current_block.addInstruction(.{
//         .lhs = "result",
//         .assign_type = .w,
//         .rhs = .{ .call = .{ .function_name = "internal_add", .args = &args1 } },
//     });
//     try func1.current_block.addInstruction(.{
//         .lhs = null,
//         .assign_type = null,
//         .rhs = .{ .ret = .{ .value = "%result" } },
//     });

//     // Create second function: export function w $main()
//     var func2 = try Function.init(alloc, "main", .w, .@"export");

//     const args2 = [_]Argument{
//         .{ .arg_type = .l, .value = "$str" },
//     };
//     try func2.current_block.addInstruction(.{
//         .lhs = "r",
//         .assign_type = .w,
//         .rhs = .{ .call = .{ .function_name = "puts", .args = &args2 } },
//     });
//     try func2.current_block.addInstruction(.{
//         .lhs = null,
//         .assign_type = null,
//         .rhs = .{ .ret = .{ .value = "0" } },
//     });

//     // Add functions to section
//     try section.add(&func1);
//     try section.add(&func2);

//     // Emit and verify
//     const expected = "function w $add(w %a, w %b) {\n" ++
//         "@start\n" ++
//         "\t%result =w call $internal_add(w %a, w %b)\n" ++
//         "\tret %result\n" ++
//         "}\n" ++
//         "\n" ++
//         "export function w $main() {\n" ++
//         "@start\n" ++
//         "\t%r =w call $puts(l $str)\n" ++
//         "\tret 0\n" ++
//         "}\n";

//     try section.emit(&writer.writer);
//     try std.testing.expectEqualStrings(expected, writer.written());
// }
