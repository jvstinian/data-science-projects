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

        pub fn writeValueAsStringToBuffer(out_buf: []u8, options: std.fmt.FormatOptions, int_value: u64) usize {
            const FIELD_TYPE: type = Id3FieldType(T, field_name);
            switch (@typeInfo(FIELD_TYPE)) {
                .Enum => {
                    const enum_value: FIELD_TYPE = @enumFromInt(int_value);
                    return formatTextBuf(out_buf, @tagName(enum_value), options);
                },
                .Int => {
                    const field_value: FIELD_TYPE = @truncate(int_value);
                    return std.fmt.formatIntBuf(out_buf, field_value, 10, std.fmt.Case.lower, options);
                },
                else => {
                    // No formatting, resulting length is 0
                    return 0;
                },
            }
        }

        pub fn getPossibleValueCount() usize {
            const FIELD_TYPE: type = Id3FieldType(T, field_name);
            switch (@typeInfo(FIELD_TYPE)) {
                .Enum => {
                    return @typeInfo(FIELD_TYPE).Enum.fields.len;
                },
                .Int => {
                    return std.math.pow(usize, 2, @typeInfo(FIELD_TYPE).Int.bits);
                },
                else => {
                    return 1; // Shouldn't happen, but return 1 for safety
                },
            }
        }

        pub fn getMaxValueStringLength() comptime_int {
            const FIELD_TYPE: type = Id3FieldType(T, field_name);
            switch (@typeInfo(FIELD_TYPE)) {
                .Enum => {
                    var ret: comptime_int = 0;
                    for (@typeInfo(FIELD_TYPE).Enum.fields) |fld| {
                        const fld_len: usize = fld.name.len;
                        if (fld_len > ret) {
                            ret = fld_len;
                        }
                    }
                    return ret;
                },
                .Int => {
                    const max_int: FIELD_TYPE = std.math.maxInt(FIELD_TYPE);
                    var ret: comptime_int = @intFromFloat(@ceil(@log10(@max(@as(f64, @floatFromInt(max_int)), 1.0))));
                    if (@typeInfo(FIELD_TYPE).Int.signedness) {
                        ret += 1; // For the sign
                    }
                    return ret;
                },
                else => {
                    return 1; // Shouldn't happen, but return 1 for safety
                },
            }
        }

        pub fn isNotEnumType() bool {
            const FIELD_TYPE: type = Id3FieldType(T, field_name);
            switch (@typeInfo(FIELD_TYPE)) {
                .Enum => {
                    return false;
                },
                else => { // Include .Int
                    return true;
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

// We introduce a simple function for matching a run time field name against a comptime array of field names.
// This function is only used in the test that immediately follows below.
// TODO: When ready, remove this function and the test.
fn field_name_match_function(comptime field_names: []const [*:0]const u8, current_field: []const u8) bool {
    inline for (field_names) |fld| {
        const adj_fld: [:0]const u8 = std.mem.span(fld);
        if (std.mem.eql(u8, adj_fld, current_field)) {
            std.debug.print("Found field {s}\n", .{fld});
            return true;
        }
    }
    // unreachable;
    return false;
}

test "testing field matching function" {
    const noncat_fields: [4][*:0]const u8 = .{ "outlook", "temperature", "windy", "play" };
    try std.testing.expect(field_name_match_function(&noncat_fields, "windy"));
    // try std.testing.expect(field_name_match_function(&noncat_fields, "windy0"));
}

// TODO: Not sure if the following will work as needed to replace sort_records and get_value_as_int
pub fn Id3FieldProcessors(comptime T: type, comptime attribute_field_names: []const []const u8) type {
    return struct {
        // const Self = @This();

        // pub fn init() Self {
        //     return Self{};
        // }

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
        pub fn writeValueAsStringToBuffer(field_name: []const u8, out_buf: []u8, options: std.fmt.FormatOptions, int_value: u64) usize {
            inline for (attribute_field_names) |attr_fld| {
                if (std.mem.eql(u8, attr_fld, field_name)) {
                    return Id3FieldContext(T, attr_fld).writeValueAsStringToBuffer(out_buf, options, int_value);
                }
            }
            unreachable;
        }
        pub fn getMaxValueStringLength(field_name: []const u8) usize {
            inline for (attribute_field_names) |attr_fld| {
                if (std.mem.eql(u8, attr_fld, field_name)) {
                    return Id3FieldContext(T, attr_fld).getMaxValueStringLength();
                }
            }
            unreachable;
        }
        pub fn isNotEnumType(field_name: []const u8) bool {
            inline for (attribute_field_names) |attr_fld| {
                if (std.mem.eql(u8, attr_fld, field_name)) {
                    return Id3FieldContext(T, attr_fld).isNotEnumType();
                }
            }
            unreachable;
        }
    };
}

// Alternate approach to Id3FieldProcessors that uses a sorting struct
pub fn AltId3FieldProcessors(comptime T: type, comptime attribute_field_names: []const [*:0]const u8) type {
    return struct {
        const Self = @This();

        sorting_struct: Id3SorterStruct(T, attribute_field_names),

        pub fn init() Self {
            return Self{ .sorting_struct = .{} };
        }

        pub fn sortRecords(self: Self, field_name: []const u8, records: []T) void {
            inline for (attribute_field_names) |attr_fld| {
                if (std.mem.eql(u8, std.mem.span(attr_fld), field_name)) {
                    @field(self.sorting_struct, std.mem.span(attr_fld)).sort(records);
                    // Id3FieldContext(T, attr_fld).init().sort(records);
                    return;
                }
            }
            unreachable;
        }
        pub fn getValueAsInt(self: Self, field_name: []const u8, record: T) u64 {
            inline for (attribute_field_names) |attr_fld| {
                if (std.mem.eql(u8, std.mem.span(attr_fld), field_name)) {
                    return @field(self.sorting_struct, std.mem.span(attr_fld)).getValueAsInt(record);
                    // return Id3FieldContext(T, attr_fld).init().getValueAsInt(record);
                }
            }
            unreachable;
        }
    };
}

pub fn Id3Entropy(comptime T: type, comptime target_field_name: []const u8) type {
    return struct {
        pub fn calculate_entropy(records: []T) f64 {
            const FC = Id3FieldContext(T, target_field_name);
            const target_field_context = FC.init();
            target_field_context.sort(records);

            const FIELD_TYPE = Id3FieldType(T, target_field_name);
            const N = @typeInfo(FIELD_TYPE).Enum.fields.len;
            var counts: [N]usize = undefined;

            if (records.len > 0) {
                counts[0] = 1; // first record always counts
            }
            var idx: usize = 1;
            var val_idx: usize = 0;
            while (idx < records.len) : (idx += 1) {
                if (@field(records[idx], target_field_name) != @field(records[idx - 1], target_field_name)) {
                    val_idx += 1;
                    counts[val_idx] = 1;
                } else {
                    counts[val_idx] += 1;
                }
            }

            const val_len = val_idx + 1;
            const counts_slice: []usize = counts[0..val_len];
            var total_count: usize = 0;
            for (counts_slice) |count| {
                total_count += count;
            }

            var entropy: f64 = 0.0;
            for (counts_slice) |count| {
                if (count > 0) {
                    const p: f64 = @as(f64, @floatFromInt(count)) / @as(f64, @floatFromInt(total_count));
                    entropy -= p * std.math.log2(p);
                }
            }

            return entropy;
        }
        pub fn calculateEntropyUsingHashMap(records: []T) std.mem.Allocator.Error!f64 {
            const MEM_SIZE = calculateMemorySizeForEntropy(T, target_field_name);
            var buffer: [MEM_SIZE]u8 = undefined;
            var fba = std.heap.FixedBufferAllocator.init(&buffer);
            const allocator = fba.allocator();
            // const memory = try allocator.alloc(u8, 100);
            // defer allocator.free(memory);

            var hm = std.hash_map.AutoHashMap(u64, usize).init(allocator);
            defer hm.deinit();

            for (records) |record| {
                const k: u8 = @intFromEnum(@field(record, target_field_name));
                const v_ptr_maybe: ?*u64 = hm.getPtr(k);
                if (v_ptr_maybe) |v_ptr| {
                    v_ptr.* += 1;
                } else {
                    try hm.put(k, 1);
                }
            }

            const total_count: usize = records.len;
            const entropy: f64 = calculateEntropyFromHashMap(total_count, hm);
            // std.debug.print("In entropy calculation, total count is {d}\n", .{total_count});
            // var entropy: f64 = 0.0;
            // var iterator = hm.iterator();
            // while (iterator.next()) |entry| {
            //     const count: u64 = entry.value_ptr.*;
            //     std.debug.print("In entropy calculation, key {d} has count {d}\n", .{ entry.key_ptr.*, entry.value_ptr.* });
            //     if (count > 0) {
            //         const p: f64 = @as(f64, @floatFromInt(count)) / @as(f64, @floatFromInt(total_count));
            //         std.debug.print("In entropy calculation, key {d} has probability {d} ({d} / {d})\n", .{ entry.key_ptr.*, p, @as(f64, @floatFromInt(count)), @as(f64, @floatFromInt(total_count)) });
            //         entropy -= p * std.math.log2(p);
            //     }
            // }

            return entropy;
        }
    };
}

fn calculateEntropyFromHashMap(total_count: usize, hm: std.hash_map.AutoHashMap(u64, usize)) f64 {
    // std.debug.print("In entropy calculation, total count is {d}\n", .{total_count}); // TODO
    var entropy: f64 = 0.0;
    var iterator = hm.iterator();
    while (iterator.next()) |entry| {
        const count: u64 = entry.value_ptr.*;
        // std.debug.print("In entropy calculation, key {d} has count {d}\n", .{ entry.key_ptr.*, entry.value_ptr.* }); // TODO
        if (count > 0) {
            const p: f64 = @as(f64, @floatFromInt(count)) / @as(f64, @floatFromInt(total_count));
            // std.debug.print("In entropy calculation, key {d} has probability {d} ({d} / {d})\n", .{ entry.key_ptr.*, p, @as(f64, @floatFromInt(count)), @as(f64, @floatFromInt(total_count)) }); // TODO
            entropy -= p * std.math.log2(p);
        }
    }
    return entropy;
}

pub fn Id3Gain(comptime T: type, comptime attribute_field_name: []const u8, comptime target_field_name: []const u8) type {
    return struct {
        pub fn calculateGainUsingHashMap(records: []T) std.mem.Allocator.Error!f64 {
            const GAIN_MEM_SIZE = calculateMemorySizeForGain(T, &.{attribute_field_name}, target_field_name);
            var buffer: [GAIN_MEM_SIZE]u8 = undefined;
            var fba = std.heap.FixedBufferAllocator.init(&buffer);
            const allocator = fba.allocator();
            // const memory = try allocator.alloc(u8, 100);
            // defer allocator.free(memory);

            const HMT = std.hash_map.AutoHashMap(u64, usize);
            var hm = std.hash_map.AutoHashMap(u64, HMT).init(allocator);
            defer hm.deinit();

            for (records) |record| {
                // const k: u64 = @intFromEnum(@field(record, attribute_field_name));
                // const k: u64 = @field(sorting_struct, attribute_field_name).getValueAsInt(record); // TODO: This approach needs improvement
                const k: u64 = Id3FieldContext(T, attribute_field_name).init().getValueAsInt(record);
                const gpresult: std.hash_map.AutoHashMap(u64, HMT).GetOrPutResult = try hm.getOrPut(k);
                const resk: u64 = @intFromEnum(@field(record, target_field_name));
                if (gpresult.found_existing) {
                    const v_ptr_maybe: ?*u64 = gpresult.value_ptr.*.getPtr(resk);
                    if (v_ptr_maybe) |v_ptr| {
                        v_ptr.* += 1;
                    } else {
                        try gpresult.value_ptr.*.put(resk, 1);
                    }
                } else {
                    gpresult.value_ptr.* = HMT.init(allocator);
                    try gpresult.value_ptr.*.put(resk, 1);
                }
            }
            defer {
                var valiterator = hm.valueIterator();
                while (valiterator.next()) |val| {
                    std.debug.print("In gain calculation, calling deinit on value of hash map\n", .{});
                    val.*.deinit();
                }
            }

            const entropy: f64 = try Id3Entropy(T, target_field_name).calculateEntropyUsingHashMap(records);
            const total_count: usize = records.len;
            // std.debug.print("In gain calculation, total count is {d}\n", .{total_count}); // TODO
            var condinfo: f64 = 0.0;
            var iterator = hm.iterator();
            while (iterator.next()) |entry| {
                const condhm: HMT = entry.value_ptr.*;
                var condcount: usize = 0;
                var condvit = condhm.valueIterator();
                while (condvit.next()) |val| {
                    condcount += val.*;
                }
                const localinfo: f64 = calculateEntropyFromHashMap(condcount, condhm);
                condinfo += localinfo * @as(f64, @floatFromInt(condcount)) / @as(f64, @floatFromInt(total_count));
            }

            return entropy - condinfo;
        }
    };
}

const ID3NodeTag = enum {
    node,
    most_frequent,
    constant_value,
    empty,
};

fn MostFrequentValueLeaf(comptime T: type, comptime target_field_name: []const u8) type {
    return struct {
        const Self = @This();

        value: ReturnType,
        empirical_probability: f64,

        const ReturnType: type = Id3FieldType(T, target_field_name);

        pub fn init(val: ReturnType, emp_prob: f64) Self {
            return Self{ .value = val, .empirical_probability = emp_prob };
        }
    };
}

pub fn ConstantValueLeaf(comptime T: type, comptime target_field_name: []const u8) type {
    return struct {
        const Self = @This();

        value: ReturnType,

        const ReturnType: type = Id3FieldType(T, target_field_name);

        pub fn init(val: ReturnType) Self {
            return Self{ .value = val };
        }
    };
}

// fn ID3Node(comptime T: type, comptime target_field_name: []const u8) type {
//     return struct {
//         const Self = @This();
//
//         field_name: []const u8,
//         values: std.ArrayList(u64), // This will hold the values of the attribute field
//         nodes: std.ArrayList(ID3NodeType), // This will hold the child nodes
//
//         pub fn init(gpa: std.mem.Allocator, attribute_field_name: []const u8) Self {
//             // TODO: Is target_field_name needed here?
//             _ = target_field_name; // to avoid unused variable warning
//             return Self{ .field_name = attribute_field_name, .values = std.ArrayList(u64).init(gpa), .nodes = std.ArrayList(ID3NodeType).init(gpa) };
//         }
//
//         pub fn deinit(self: Self) void {
//             self.values.deinit();
//             for (self.nodes.items) |node| {
//                 node.deinit();
//             }
//             self.nodes.deinit();
//         }
//
//         pub fn appendValue(self: *Self, value: u64, node: ID3NodeType) std.mem.Allocator.Error!void {
//             try self.values.append(value);
//             try self.nodes.append(node);
//         }
//     };
// }

pub fn ID3Node(comptime T: type, comptime attribute_field_names: []const []const u8, comptime target_field_name: []const u8) type {
    return struct {
        const Self = @This();

        const TreeType = ID3NodeType(T, attribute_field_names, target_field_name);

        field_name: []const u8,
        values: std.ArrayList(u64), // This will hold the values of the attribute field
        nodes: std.ArrayList(TreeType), // This will hold the child nodes

        pub fn init(gpa: std.mem.Allocator, attribute_field_name: []const u8) Self {
            return Self{ .field_name = attribute_field_name, .values = std.ArrayList(u64).init(gpa), .nodes = std.ArrayList(TreeType).init(gpa) };
        }

        pub fn deinit(self: Self) void {
            self.values.deinit();
            for (self.nodes.items) |node| {
                node.deinit();
            }
            self.nodes.deinit();
        }

        pub fn appendValue(self: *Self, value: u64, node: TreeType) std.mem.Allocator.Error!void {
            try self.values.append(value);
            try self.nodes.append(node);
        }
    };
}

// TODO: Might make sense to improve the following definition of MEM_SIZE and GAIN_MEM_SIZE
const KEY_SIZE = @sizeOf(u64);
const VAL_SIZE = @sizeOf(usize);
const EXTRA_SIZE = 100;
const EXTRA_MULTIPLIER = 2;
// const MEM_SIZE = std.math.pow(usize, 2, 8) * (KEY_SIZE + VAL_SIZE) + EXTRA_SIZE;
// const GAIN_MEM_SIZE = std.math.pow(usize, 2, 8) * (KEY_SIZE + MEM_SIZE) + EXTRA_SIZE;

pub fn calculateMemorySizeForEntropy(comptime T: type, comptime target_field_name: []const u8) comptime_int {
    const max_field_values: usize = Id3FieldContext(T, target_field_name).getPossibleValueCount();
    return max_field_values * EXTRA_MULTIPLIER * (KEY_SIZE + VAL_SIZE) + EXTRA_SIZE;
}

pub fn calculateMemorySizeForGain(comptime T: type, comptime attribute_field_names: []const []const u8, comptime target_field_name: []const u8) comptime_int {
    const MEM_SIZE = calculateMemorySizeForEntropy(T, target_field_name);
    var max_field_values: usize = 0;
    inline for (attribute_field_names) |fld| {
        const field_values: usize = Id3FieldContext(T, fld).getPossibleValueCount();

        if (field_values > max_field_values) {
            max_field_values = field_values;
        }
    }
    return max_field_values * EXTRA_MULTIPLIER * (KEY_SIZE + MEM_SIZE) + EXTRA_SIZE;
}

// const ID3NodeType =
pub fn ID3NodeType(comptime T: type, comptime attribute_field_names: []const []const u8, comptime target_field_name: []const u8) type {
    return union(ID3NodeTag) {
        const Self = @This();

        node: ID3Node(T, attribute_field_names, target_field_name),
        most_frequent: MostFrequentValueLeaf(T, target_field_name),
        constant_value: ConstantValueLeaf(T, target_field_name),
        empty: void,

        // TODO: Use a pointer in the following instead?
        pub fn deinit(self: Self) void {
            switch (self) {
                .node => |node| node.deinit(),
                .most_frequent => {},
                .constant_value => {},
                .empty => {},
            }
        }

        fn maxAttributeFieldValueLength() comptime_int {
            comptime var ret = 0;
            inline for (attribute_field_names) |attr_fld| {
                const max_field_len = Id3FieldContext(T, attr_fld).getMaxValueStringLength();
                if (max_field_len > ret) {
                    ret = max_field_len;
                }
            }
            return ret;
        }

        pub fn print(self: Self) !void {
            const stdout_file = std.io.getStdOut().writer();
            var bw = std.io.bufferedWriter(stdout_file);
            const stdout = bw.writer();

            // try ID3NodeType.printNext(self, 0, stdout); // TODO: Remove
            try self.printNext(0, stdout);

            try bw.flush(); // don't forget to flush!
        }

        pub fn printNext(self: Self, initial_spaces: usize, stdout: std.io.BufferedWriter(4096, std.fs.File.Writer).Writer) !void {
            switch (self) {
                .node => |node| {
                    // std.debug.print("Node with values: {any}\n", .{node.values.items});
                    const BUFFER_LENGTH = Self.maxAttributeFieldValueLength();
                    const FP: type = Id3FieldProcessors(T, attribute_field_names);
                    var required_value_chars: usize = FP.getMaxValueStringLength(node.field_name);
                    // var max_value_chars: usize = 0;
                    // For integer (not Enum) types we determine the number of characters needed to print the maximum value
                    // seen in the slice of record values for the node.
                    if (FP.isNotEnumType(node.field_name)) {
                        var max_value_chars: usize = 0;
                        for (node.values.items) |val| {
                            const temp_chars: usize = @intFromFloat(@ceil(@log10(@max(@as(f64, @floatFromInt(val)), 1.0))));
                            if (temp_chars > max_value_chars) {
                                max_value_chars = temp_chars;
                            }
                        }
                        if (max_value_chars < required_value_chars) {
                            required_value_chars = max_value_chars;
                        }
                    }
                    // Max 8-byte integer value value: 18,446,744,073,709,551,615
                    // We defined a buffer large enough to hold the string
                    // "{s} - {: ^N} -> ", where N is the number of characters needed to print the maximum value.
                    // The buffer is taken to be up to 35 characters long,
                    // which allows for the 15 known characters (everything except for N),
                    // and up to 20 characters which is the number of characters
                    // needed to print the maximum value of usize.
                    // var fmt_buf: [35]u8 = undefined;
                    // const fmt_slice: []u8 = std.fmt.bufPrint(&fmt_buf, "{{s}} - {{: ^{d}}}", .{required_value_chars});

                    for (node.values.items, node.nodes.items, 0..) |value, next_node, idx| {
                        // try stdout.print("- '{:>{}}' -> ", .{ value, max_value_chars });
                        if (idx > 0) {
                            // for (0..initial_spaces) |_| {
                            //     try stdout.print(" ", .{});
                            // }
                            try stdout.writeByteNTimes(' ', initial_spaces);
                        }
                        // if (FP.isNotEnumType(node.field_name)) {
                        //     // For integer types, we print the value right-aligned
                        //     try stdout.print("{s} - {:^3} -> ", .{ node.field_name, value });
                        //     // const test_value = 3;
                        //     // try stdout.print("{s} - {:>{d} -> ", .{ node.field_name, value, test_value });
                        // } else {
                        //     // For enum types, we print the value centered
                        //     var out_buf: [100]u8 = undefined; // TODO: Settle on alternative length
                        //     const buflen = FP.writeValueAsStringToBuffer(node.field_name, &out_buf, .{ .width = required_value_chars, .fill = ' ', .alignment = .center }, value);
                        //     try stdout.print("{s} - {s} -> ", .{ node.field_name, out_buf[0..buflen] });
                        // }
                        var out_buf: [BUFFER_LENGTH]u8 = undefined; // TODO: Settle on alternative length
                        const buflen = FP.writeValueAsStringToBuffer(node.field_name, &out_buf, .{ .width = required_value_chars, .fill = ' ', .alignment = .center }, value);
                        try stdout.print("{s} - {s} -> ", .{ node.field_name, out_buf[0..buflen] });
                        // try stdout.print("{s} - {:^3} -> ", .{ node.field_name, value });
                        // try stdout.print(fmt_slice, .{ node.field_name, value });
                        const spaces: usize = initial_spaces + node.field_name.len + required_value_chars + 7; // 7 for the " -> "
                        try next_node.printNext(spaces, stdout);
                    }
                },
                .most_frequent => |mfv| {
                    // try stdout.print("{s: >{d}}{d} (freq {d})\n", .{ "", initial_spaces, mfv.value, mfv.empirical_probability });
                    try stdout.print("{s} (freq {d})\n", .{ @tagName(mfv.value), mfv.empirical_probability });
                },
                .constant_value => |cv| {
                    try stdout.print("{s} (constant)\n", .{@tagName(cv.value)});
                },
                .empty => {
                    try stdout.print("Failure (empty)\n", .{});
                },
            }
        }

        // TODO: Need to extend the return type to include the empirical probability of the most frequent value
        pub fn calculateMostFrequentValue(records: []T) std.mem.Allocator.Error!MostFrequentValueLeaf(T, target_field_name) {
            const MEM_SIZE = calculateMemorySizeForEntropy(T, target_field_name);
            var buffer: [MEM_SIZE]u8 = undefined;
            var fba = std.heap.FixedBufferAllocator.init(&buffer);
            const allocator = fba.allocator();

            var hm = std.hash_map.AutoHashMap(u64, usize).init(allocator);
            defer hm.deinit();

            for (records) |record| {
                const k: u64 = @intFromEnum(@field(record, target_field_name));
                const v_ptr_maybe: ?*u64 = hm.getPtr(k);
                if (v_ptr_maybe) |v_ptr| {
                    v_ptr.* += 1;
                } else {
                    try hm.put(k, 1);
                }
            }

            const total_count: usize = records.len;
            var max_count: usize = 0;
            var most_frequent_value: u64 = 0;
            var iterator = hm.iterator();
            while (iterator.next()) |entry| {
                const count: u64 = entry.value_ptr.*;
                if (count > max_count) {
                    max_count = count;
                    most_frequent_value = entry.key_ptr.*;
                }
            }
            const freq: f64 = @as(f64, @floatFromInt(max_count)) / @as(f64, @floatFromInt(total_count));
            // return freq;
            return MostFrequentValueLeaf(T, target_field_name).init(
                @enumFromInt(most_frequent_value),
                freq,
            );
        }

        const TargetFieldType: type = Id3FieldType(T, target_field_name);

        // The following predict function will traverse the ID3 tree.
        // We return a nullable GolfFieldType("play"), which we might improve later.
        pub fn predict(self: Self, record: T) ?TargetFieldType {
            switch (self) {
                .node => |node| {
                    // const lookupValue: u64 = get_value_as_int(node.field_name, record); // TODO: This was changed
                    const lookupValue: u64 = Id3FieldProcessors(T, attribute_field_names).getValueAsInt(node.field_name, record);
                    for (node.values.items, node.nodes.items) |value, next_node| {
                        if (value == lookupValue) {
                            // Found the matching value, return the prediction
                            return next_node.predict(record);
                        }
                    }
                    return null;
                },
                .most_frequent => |mfv| {
                    return mfv.value; // Return the most frequent value
                },
                .constant_value => |cv| {
                    return cv.value; // Return the constant value
                },
                .empty => {
                    return null; // No prediction available
                },
            }
        }

        fn allTargetValuesEqual(records: []T) bool {
            if (records.len == 0) {
                return false; // No records to compare
            }
            const first_value: Self.TargetFieldType = @field(records[0], target_field_name);
            for (records[1..]) |record| {
                if (@field(record, target_field_name) != first_value) {
                    return false; // Found a different value
                }
            }
            return true; // All values are equal
        }

        const attribute_count: usize = attribute_field_names.len;

        pub fn buildNode(remaining_field_names: []const []const u8, records: []T, allocator: std.mem.Allocator) std.mem.Allocator.Error!Self {
            // std.debug.print("Entering build_node\n", .{}); // TODO
            if (records.len == 0) {
                // If S is empty, return a single node with value Failure;
                // std.debug.print("In build_node, constructing empty leaf\n", .{}); // TODO
                return Self{ .empty = {} };
            } else if (allTargetValuesEqual(records)) {
                // If S consists of records all with the same value for
                // the categorical attribute,
                // return a single node with that value;
                // std.debug.print("In build_node, constructing constant value leaf\n", .{}); // TODO
                return Self{ .constant_value = ConstantValueLeaf(T, target_field_name).init(@field(records[0], target_field_name)) };
            } else if (remaining_field_names.len == 0) {
                const mfv: MostFrequentValueLeaf(T, target_field_name) = try calculateMostFrequentValue(records);
                // std.debug.print("In build_node, constructing most frequent value leaf\n", .{}); // TODO
                return Self{ .most_frequent = mfv };
            } else {
                var max_gain: f64 = undefined;
                var arg_max: usize = undefined;
                for (remaining_field_names, 0..) |attr, i| {
                    std.debug.print("In build_node, attr is {s}\n", .{attr});
                    const gain: f64 = try calculateGainForFieldUsingHashMap(attr, records);
                    if ((i == 0) or (gain > max_gain)) {
                        max_gain = gain;
                        arg_max = i;
                    }
                }
                const best_field_name: []const u8 = remaining_field_names[arg_max];
                std.debug.print("In build_node, best attribute is {s}\n", .{best_field_name});

                var updated_attributes: [attribute_count][]const u8 = undefined;
                for (remaining_field_names, 0..) |attr, idx| {
                    if (idx == arg_max) {
                        // Skip the attribute with the maximum gain
                        continue;
                    } else if (idx < arg_max) {
                        // If the index is less than arg_max, we can keep the attribute as is
                        updated_attributes[idx] = attr;
                    } else { // idx > arg_max
                        // If the index is greater than arg_max, we need to adjust the index
                        updated_attributes[idx - 1] = attr;
                    }
                }
                const updated_attributes_slice: []const []const u8 = updated_attributes[0..(remaining_field_names.len - 1)];

                // Sort the records
                //// sort_records(remaining_field_names[arg_max], records); // TODO: Replaced with the following
                // const field_processors: Id3FieldProcessors(T, attribute_field_names) = Id3FieldProcessors(T, attribute_field_names).init();
                // field_processors.sortRecords(remaining_field_names[arg_max], records);
                Id3FieldProcessors(T, attribute_field_names).sortRecords(remaining_field_names[arg_max], records);

                // Create a list of nodes
                var node: ID3Node(T, attribute_field_names, target_field_name) = ID3Node(T, attribute_field_names, target_field_name).init(allocator, best_field_name);
                // var node = ID3Node(T, attribute_field_names, target_field_name).init(allocator, best_field_name); // TODO: This works
                var start_idx: usize = 0;
                var end_idx: usize = 0;
                while (start_idx < records.len) : (end_idx += 1) {
                    // Find the end of the current value group
                    // TODO: best_field_name isn't comptime so we might need to adjust the value extraction
                    // const start_value: u64 = get_value_as_int(best_field_name, records[start_idx]); // TODO: Replaced with the following
                    // const start_value: u64 = field_processors.getValueAsInt(best_field_name, records[start_idx]);
                    const start_value: u64 = Id3FieldProcessors(T, attribute_field_names).getValueAsInt(best_field_name, records[start_idx]);
                    var append_flag: bool = false;
                    if (end_idx == records.len) {
                        append_flag = true;
                    } else {
                        // const end_value: u64 = get_value_as_int(best_field_name, records[end_idx]); // TODO: Replaced with the following
                        // const end_value: u64 = field_processors.getValueAsInt(best_field_name, records[end_idx]);
                        const end_value: u64 = Id3FieldProcessors(T, attribute_field_names).getValueAsInt(best_field_name, records[end_idx]);
                        if (end_value != start_value) {
                            append_flag = true;
                        }
                    }
                    if (append_flag) {
                        // Create a sub-node for the current value group
                        const sub_records = records[start_idx..end_idx];
                        const sub_node = try buildNode(updated_attributes_slice, sub_records, allocator);
                        try node.appendValue(start_value, sub_node);
                        start_idx = end_idx; // Move to the next group
                    } else {
                        continue; // Continue to find the end of the current value group
                    }
                }

                return Self{ .node = node };
            }
        }

        pub fn calculateGainForFieldUsingHashMap(attribute_field_name: []const u8, records: []T) std.mem.Allocator.Error!f64 {
            inline for (attribute_field_names) |fld| {
                // const adj_fld: [:0]const u8 = std.mem.span(fld);
                std.debug.print("In calculateGainForFieldUsingHashMap, checking to see if field matches {s}\n", .{fld});
                if (std.mem.eql(u8, fld, attribute_field_name)) {
                    std.debug.print("In calculate_gain_using_hash_map, Found field {s}\n", .{fld});
                    // return @field(sorting_struct, fld).getValueAsInt(records[0]);
                    // return calculate_gain_using_hash_map0(target_field_name, fld, records); // TODO: This was the original
                    return Id3Gain(T, fld, target_field_name).calculateGainUsingHashMap(records);
                }
            }
            unreachable;
        }
    };
}

test "simple array list test" {
    var list = std.ArrayList(i32).init(std.testing.allocator);
    defer list.deinit(); // try commenting this out and see if zig detects the memory leak!
    try list.append(42);
    try std.testing.expectEqual(@as(i32, 42), list.pop());
}

test "testing curly brace escape in format string" {
    var buf: [10]u8 = undefined;
    const fmt_str: []const u8 = "{{: >{d}}}";
    const result: []const u8 = try std.fmt.bufPrint(&buf, fmt_str, .{4});
    // std.debug.print("Formatted string: {s}\n", .{result});
    try std.testing.expectEqualStrings("{: >4}", result);
}

test "testing formatIntBuf" {
    var buf: [20]u8 = undefined;
    const resultlen: usize = std.fmt.formatIntBuf(&buf, 42, 10, std.fmt.Case.lower, .{ .width = 5, .fill = ' ', .alignment = .center });
    // _ = resultlen; // to avoid unused variable warning
    std.debug.print("Formatted integer: {s}\n", .{buf[0..resultlen]});
    try std.testing.expectEqualStrings(" 42  ", buf[0..resultlen]);
}

pub fn formatTextBuf(out_buf: []u8, bytes: []const u8, options: std.fmt.FormatOptions) usize {
    var fbs = std.io.fixedBufferStream(out_buf);
    // Not clear to me why the format string argument `fmt` is required here.
    // Looking through the std.fmt.formatText function, it seems that it is not used,
    // except for the `commptime checkTextFmt(fmt);` call.
    // Also, looking at the checkTextFmt function, it looks like a single character is expected.
    std.fmt.formatText(bytes, "s", options, fbs.writer()) catch unreachable;
    return fbs.pos;
}

test "testing formatTextBuf" {
    var buf: [20]u8 = undefined;
    const resultlen: usize = formatTextBuf(&buf, "Hello", .{ .width = 10, .fill = ' ', .alignment = .center });
    std.debug.print("Formatted text: {s}\n", .{buf[0..resultlen]});
    try std.testing.expectEqualStrings("  Hello   ", buf[0..resultlen]);
}

export fn add(a: i32, b: i32) i32 {
    return a + b;
}

test "basic add functionality" {
    try testing.expect(add(3, 7) == 10);
}
