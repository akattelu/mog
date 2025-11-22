const allocPrint = @import("std").fmt.allocPrint;
const Compiler = @import("compiler.zig").QBECompiler;
const Temporary = @import("symbol_table.zig").Temporary;

// NOTE: sign bit is technically unused for now but its set to 1
const NAN_TAG_BASE: u64 = 0xFFF8_0000_0000_0000;

pub const BoxedValueType = enum(u3) { nil, bool, int, double, string, table, function, builtin };
pub const BoxedValue = union(BoxedValueType) {
    nil,
    bool,
    int,
    double,
    string,
    table,
    function,
    builtin,

    pub const nil_value = NAN_TAG_BASE; // this works cause tag is 0b000 and theres no value

    inline fn maskFor(t: BoxedValueType) u64 {
        return @as(u64, @intFromEnum(t)) << 48;
    }

    /// Emit instructions that unbox the nan value and return the temporary its stored in
    pub fn emitValue(comptime t: BoxedValueType, c: *Compiler, boxed: *Temporary) !*Temporary {
        return switch (t) {
            .bool => {
                const mask: u32 = @as(u32, 0b1);

                // Cast the d into w to drop bits and get the right type
                const cast_instr = try allocPrint(c.alloc, "cast %{s}", .{boxed.name});
                defer c.alloc.free(cast_instr);
                const cast_result_temp = try c.emitAssignment(.l, cast_instr);

                // Mask all but last bit and return result
                const mask_instr = try allocPrint(c.alloc, "and %{s}, {d}", .{ cast_result_temp.name, mask });
                defer c.alloc.free(mask_instr);
                const masked_temp = try c.emitAssignment(.l, mask_instr);

                return masked_temp;
            },
            else => {
                unreachable;
            },
        };
    }

    /// Extract the 3 type bits
    pub fn emitType(c: *Compiler, boxed: *Temporary) !*Temporary {
        // The type tag is stored in bits 48-50 (3 bits)
        // We need to shift right by 48 bits to get those bits into the lower position
        const shift_instr = try allocPrint(c.alloc, "shr %{s}, 48", .{boxed.name});
        defer c.alloc.free(shift_instr);
        const shifted_temp = try c.emitAssignment(.l, shift_instr);

        // Mask to keep only the lower 3 bits (0b111 = 7)
        const mask: u32 = 0b111;
        const mask_instr = try allocPrint(c.alloc, "and %{s}, {d}", .{ shifted_temp.name, mask });
        defer c.alloc.free(mask_instr);
        const masked_temp = try c.emitAssignment(.l, mask_instr);

        return masked_temp;
    }

    /// Check the 2 temporaries for matching types and fail with an error if they don't matchA
    pub fn emitTypeValidation(c: *Compiler) !void {
        _ = c;
    }

    /// Turns an i32 into a nan boxed u64
    pub fn fromInt(i: i32) u64 {
        const int64 = @as(u32, @bitCast(i));
        const typemask: u64 = maskFor(.int);
        return @bitCast(NAN_TAG_BASE | typemask | int64);
    }

    /// Turns boolean into nan boxed u64
    pub fn fromBoolean(b: bool) u64 {
        const lsb = @as(u64, @intFromBool(b));
        const typemask = maskFor(.bool);
        return @bitCast(NAN_TAG_BASE | typemask | lsb);
    }
};
