const std = @import("std");
const testing = std.testing;

pub fn Id3FieldOffset(comptime T: type, comptime field_name: []const u8) comptime_int {
    return @offsetOf(T, field_name);
}

pub fn Id3FieldType(comptime T: type, comptime field_name: []const u8) type {
    const fields = @typeInfo(T).Struct.fields;
    inline for (fields) |fld| {
        if (std.mem.eql(u8, fld.name, field_name)) {
            return fld.type;
        }
    }
    unreachable;
}

pub fn Id3FieldContext(comptime T: type, comptime field_name: []const u8) type {
    return struct {
        const Self = @This();

        field_name: []const u8,
        offset: usize,

        pub const offset2: usize = @offsetOf(T, field_name);

        pub fn lessThan(self: Self, a: T, b: T) bool {
            const T2: type = Id3FieldType(T, field_name); // TODO: Rename T2
            const a_fld_ptr: *T2 = @ptrFromInt(@intFromPtr(&a) + self.offset);
            const b_fld_ptr: *T2 = @ptrFromInt(@intFromPtr(&b) + self.offset);
            // const b_fld_ptr: *T2 = @ptrFromInt(@intFromPtr(&b) + .offset2);
            switch (@typeInfo(T2)) {
                .Enum => {
                    return @intFromEnum(a_fld_ptr.*) < @intFromEnum(b_fld_ptr.*);
                },
                .Int => {
                    return a_fld_ptr.* < b_fld_ptr.*;
                },
                else => {
                    return false;
                },
            }
        }

        // TODO: This probably doesn't work in this form
        // fn compareFn(self: Self, context: GolfConditions, item: GolfConditions) std.math.Order {
        //     const T2: type = GolfFieldType(field_name);
        //     const a_fld_ptr: *T2 = @ptrFromInt(@intFromPtr(&context) + self.offset);
        //     const b_fld_ptr: *T2 = @ptrFromInt(@intFromPtr(&item) + self.offset);
        //     switch (@typeInfo(T2)) {
        //         .Enum => {
        //             return std.math.order(@intFromEnum(a_fld_ptr.*), @intFromEnum(b_fld_ptr.*));
        //         },
        //         .Int => {
        //             return std.math.order(a_fld_ptr.*, b_fld_ptr.*);
        //         },
        //         else => {
        //             return false;
        //         },
        //     }
        // }

        pub fn getValueAsInt(self: Self, a: T) u64 {
            const T2: type = Id3FieldType(T, field_name);
            const a_fld_ptr: *T2 = @ptrFromInt(@intFromPtr(&a) + self.offset);
            switch (@typeInfo(T2)) {
                .Enum => {
                    return @intFromEnum(a_fld_ptr.*);
                },
                .Int => {
                    return @as(u64, a_fld_ptr.*);
                },
                else => {
                    return 0;
                },
            }
        }

        pub fn init() Self {
            return Self{ .field_name = field_name, .offset = @offsetOf(T, field_name) };
        }

        // This seems to work
        pub fn sort(self: Self, train: []T) void {
            std.sort.insertion(T, train, self, Self.lessThan);
        }

        // TODO: This probably doesn't work in this form
        // pub fn upperBound(self: Self, items: []T) void {
        //     return std.sort.upperBound(T, items, self, Self.lessThan);
        // }
    };
}

pub fn Id3SorterStruct(comptime T: type, comptime field_names: []const [*:0]const u8) type {
    var fields: [field_names.len]std.builtin.Type.StructField = undefined;
    for (field_names, 0..) |field_name, i| {
        // std.fmt.comptimePrint("Making field {s} at index {d}\n", .{ field_name, i });
        // const fieldName: [:0]const u8 = field_name[0.. :0];
        const fieldName: [:0]const u8 = std.mem.span(field_name);
        const fieldType: type = Id3FieldContext(T, fieldName); // Note the coercion here
        const defaultFieldValue: fieldType = fieldType.init();
        fields[i] = .{
            .name = fieldName, // need sentinel termination for the field name
            .type = fieldType,
            .default_value = &defaultFieldValue,
            .is_comptime = false,
            .alignment = 0,
        };
    }

    return @Type(.{
        .Struct = .{
            .layout = .auto,
            .fields = fields[0..],
            .decls = &[_]std.builtin.Type.Declaration{},
            .is_tuple = false,
        },
    });
}

// TODO: Not sure if the following will work as needed to replace sort_records and get_value_as_int
pub fn Id3FieldProcessors(comptime T: type, comptime attribute_field_names: [][]const u8) type {
    return struct {
        const Self = @This();

        pub fn init() Self {
            return Self{};
        }

        pub fn sortRecords(field_name: []const u8, records: []T) void {
            inline for (attribute_field_names) |attr_fld| {
                if (std.mem.eql(u8, attr_fld, field_name)) {
                    // @field(sorting_struct, attr_fld).sort(records);
                    Id3FieldContext(T, attr_fld).init().sort(records);
                    return;
                }
            }
            unreachable;
        }
        pub fn getValueAsInt(field_name: []const u8, record: T) u64 {
            inline for (attribute_field_names) |attr_fld| {
                if (std.mem.eql(u8, attr_fld, field_name)) {
                    // return @field(sorting_struct, attr_fld).getValueAsInt(record);
                    return Id3FieldContext(T, attr_fld).init().getValueAsInt(record);
                }
            }
            unreachable;
        }
    };
}

export fn add(a: i32, b: i32) i32 {
    return a + b;
}

test "basic add functionality" {
    try testing.expect(add(3, 7) == 10);
}
