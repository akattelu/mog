const Compiler = @import("compiler.zig").QBECompiler;
const Temporary = @import("symbol_table.zig").Temporary;

// NOTE: sign bit is technically unused for now but its set to 1
const NAN_TAG_BASE: f64 = 0xFFF8_0000_0000_0000;

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

    pub fn maskFor(t: BoxedValueType) u64 {
        return @as(f64, @intFromEnum(t)) << 48;
    }

    pub fn getValueInstruction(comptime t: BoxedValueType, c: *Compiler) !*Temporary {
        return switch (t) {
            .bool => {
                _ = try c.symbol_table.createTemporary(.function, .d);
            },
            else => {
                return try c.symbol_table.createTemporary(.function, .d);
            },
        };
    }

    pub fn getTruthinessValue(comptime t: BoxedValueType, c: *Compiler) !*Temporary {
        if (t == .bool) {
            _ = try c.symbol_table.createTemporary(.function, .l);
        }
        return try c.symbol_table.createTemporary(.function, .d);
    }

    pub fn toCValue(t: *Temporary, c: *Compiler) !*Temporary {
        _ = t;
        return try c.symbol_table.createTemporary(.function, .d);
    }

    pub fn fromInt(i: i32) f64 {
        const int64 = @as(i64, i);
        return NAN_TAG_BASE | maskFor(.int) | int64;
    }
};
