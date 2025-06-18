const std = @import("std");
const id3 = @import("id3.zig");
const golf = @import("golf.zig");

// Golf Conditions and related types
const Outlook = golf.Outlook;
const WhetherToPlay = golf.WhetherToPlay;
const Windy = golf.Windy;
const HumidityBucket = golf.HumidityBucket;
const GolfConditions = golf.GolfConditions;

// const GolfFieldOffset = golf.GolfFieldOffset;
const GolfFieldType = golf.GolfFieldType;
const GolfFieldContext2 = golf.GolfFieldContext2;
const Id3SorterStruct = id3.Id3SorterStruct;

const enum_fields: [3][*:0]const u8 = .{ "outlook", "windy", "humidity" };
const enum_fields2: [3][]const u8 = .{ "outlook", "windy", "humidity_bucket" };

const enum_and_target_fields: [6][*:0]const u8 = enum_fields ++ [3][*:0]const u8{ "humidity_bucket", "temperature", "play" };
const sorting_struct = Id3SorterStruct(GolfConditions, &enum_and_target_fields){};

fn sort_records(attribute_field_name: []const u8, records: []GolfConditions) void {
    inline for (enum_fields2) |fld| {
        if (std.mem.eql(u8, fld, attribute_field_name)) {
            @field(sorting_struct, fld).sort(records);
            return;
        }
    }
    unreachable;
}

fn get_value_as_int(attribute_field_name: []const u8, record: GolfConditions) u64 {
    inline for (enum_fields2) |fld| {
        if (std.mem.eql(u8, fld, attribute_field_name)) {
            return @field(sorting_struct, fld).getValueAsInt(record);
        }
    }
    unreachable;
}

// fn PrintSorterConstruct(comptime field_names: []const [*:0]const u8) void {
//     @compileLog("Number of fields", field_names.len);
//     for (field_names) |field_name| {
//         @compileLog("Working with field ", field_name);
//     }
// }
//

fn calculate_entropy(comptime target_field_name: []const u8, records: []GolfConditions) f64 {
    const FC = GolfFieldContext2(target_field_name);
    const target_field_context = FC.init();
    target_field_context.sort(records);

    const T = GolfFieldType(target_field_name);
    const N = @typeInfo(T).Enum.fields.len;
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

// TODO: Probably have to consider implementing compareFn and upperBound in a similar way to what was done for sort.
// REFERENCES: https://ziglang.org/documentation/master/std/#std.sort.upperBound
// fn calculate_gain(comptime nbr_of_attrs: usize, attrs: []const u8, comptime target_field_name: []const u8, records: []GolfConditions) f64 {
//     const entropy: f64 = calculate_entropy(target_field_name, records);
//     // var attrcounts0: [nbr_of_attrs]usize = undefined;
//     // var attrcounts: []usize = attrcounts0[0..(attrs.len)];
//     var gains0: [nbr_of_attrs]f64 = undefined;
//     var gains: []f64 = gains0[0..(attrs.len)];
//     // for (attrs, 0..) |attr, i| {
//     //     @field(sorting_struct, attr).sort(records);
//     //     if (records.len > 0) {
//     //         attrcounts[0] = 1; // first record always counts
//     //     }
//     //     var idx: usize = 1;
//     //     var val_idx: usize = 0;
//     //     while (idx < records.len) : (idx += 1) {
//     //         if (@field(records[idx], attr) == @field(records[idx - 1], attr)) {
//     //             attrcounts[val_idx] += 1;
//     //         } else {
//     //             val_idx += 1;
//     //             attrcounts[val_idx] = 1;
//     //         }
//     //     }
//     //     const attr_entropy: f64 = calculate_entropy(attr, records);
//     //     gains[i] = entropy - attr_entropy;
//     // }
// }

test "testing entropy" {
    var single_value_test = [_]GolfConditions{ GolfConditions{ .id = 0, .outlook = .sunny, .temperature = 85, .humidity = 85, .humidity_bucket = .gt75, .windy = .no, .play = .dont }, GolfConditions{ .id = 1, .outlook = .sunny, .temperature = 80, .humidity = 90, .humidity_bucket = .gt75, .windy = .yes, .play = .dont }, GolfConditions{ .id = 2, .outlook = .overcast, .temperature = 83, .humidity = 78, .humidity_bucket = .gt75, .windy = .no, .play = .dont }, GolfConditions{ .id = 3, .outlook = .rain, .temperature = 70, .humidity = 96, .humidity_bucket = .gt75, .windy = .no, .play = .dont } };
    const actual_val1: f64 = calculate_entropy("play", &single_value_test);
    try std.testing.expectApproxEqAbs(@as(f64, 0.0), actual_val1, 1e-12);

    var max_entropy_test = [_]GolfConditions{ GolfConditions{ .id = 0, .outlook = .sunny, .temperature = 85, .humidity = 85, .humidity_bucket = .gt75, .windy = .no, .play = .dont }, GolfConditions{ .id = 1, .outlook = .sunny, .temperature = 80, .humidity = 90, .humidity_bucket = .gt75, .windy = .yes, .play = .do } };
    const actual_val2: f64 = calculate_entropy("play", &max_entropy_test);
    const exp_val2: f64 = std.math.log2(@as(f64, 2.0));
    std.debug.print("Expected value: {d}, Actual value: {d}\n", .{ exp_val2, actual_val2 });
    try std.testing.expectApproxEqAbs(exp_val2, actual_val2, 1e-12);
}

// function ID3 (R: a set of non-categorical attributes,
// 		 C: the categorical attribute,
// 		 S: a training set) returns a decision tree;
//    begin
// 	If S is empty, return a single node with value Failure;
// 	If S consists of records all with the same value for
// 	   the categorical attribute,
// 	   return a single node with that value;
// 	If R is empty, then return a single node with as value
// 	   the most frequent of the values of the categorical attribute
// 	   that are found in records of S; [note that then there
// 	   will be errors, that is, records that will be improperly
// 	   classified];
// 	Let D be the attribute with largest Gain(D,S)
// 	   among attributes in R;
// 	Let {dj| j=1,2, .., m} be the values of attribute D;
// 	Let {Sj| j=1,2, .., m} be the subsets of S consisting
// 	   respectively of records with value dj for attribute D;
// 	Return a tree with root labeled D and arcs labeled
// 	   d1, d2, .., dm going respectively to the trees
//
// 	     ID3(R-{D}, C, S1), ID3(R-{D}, C, S2), .., ID3(R-{D}, C, Sm);
//    end ID3;

const ID3NodeTag = enum {
    node,
    most_frequent,
    constant_value,
    empty,
};

fn MostFrequentValueLeaf(comptime target_field_name: []const u8) type {
    return struct {
        const Self = @This();

        value: ReturnType,
        empirical_probability: f64,

        const ReturnType: type = GolfFieldType(target_field_name);

        pub fn init(val: ReturnType, emp_prob: f64) Self {
            return Self{ .value = val, .empirical_probability = emp_prob };
        }
    };
}

fn ConstantValueLeaf(comptime target_field_name: []const u8) type {
    return struct {
        const Self = @This();

        value: ReturnType,

        const ReturnType: type = GolfFieldType(target_field_name);

        pub fn init(val: ReturnType) Self {
            return Self{ .value = val };
        }
    };
}

// TODO: Need to extend the return type to include the empirical probability of the most frequent value
fn calculate_most_frequent_value(comptime target_field_name: []const u8, records: []GolfConditions) std.mem.Allocator.Error!MostFrequentValueLeaf(target_field_name) {
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
    return MostFrequentValueLeaf(target_field_name).init(
        @enumFromInt(most_frequent_value),
        freq,
    );
}

test "constant value node" {
    const ConstantValueLeafType = ConstantValueLeaf("play");
    const cvf = ConstantValueLeafType.init(WhetherToPlay.dont);
    try std.testing.expect(cvf.value == WhetherToPlay.dont);
    try std.testing.expect(@TypeOf(cvf.value) == WhetherToPlay);
}

fn ID3Node(comptime target_field_name: []const u8) type {
    return struct {
        const Self = @This();

        field_name: []const u8,
        values: std.ArrayList(u64), // This will hold the values of the attribute field
        nodes: std.ArrayList(ID3NodeType), // This will hold the child nodes

        pub fn init(gpa: std.mem.Allocator, attribute_field_name: []const u8) Self {
            // TODO: Is target_field_name needed here?
            _ = target_field_name; // to avoid unused variable warning
            return Self{ .field_name = attribute_field_name, .values = std.ArrayList(u64).init(gpa), .nodes = std.ArrayList(ID3NodeType).init(gpa) };
        }

        pub fn deinit(self: Self) void {
            self.values.deinit();
            for (self.nodes.items) |node| {
                node.deinit();
            }
            self.nodes.deinit();
        }

        pub fn appendValue(self: *Self, value: u64, node: ID3NodeType) std.mem.Allocator.Error!void {
            try self.values.append(value);
            try self.nodes.append(node);
        }
    };
}

const ID3NodeType = union(ID3NodeTag) {
    node: ID3Node("play"),
    most_frequent: MostFrequentValueLeaf("play"),
    constant_value: ConstantValueLeaf("play"),
    empty: void,

    // TODO: Use a pointer in the following instead?
    pub fn deinit(self: ID3NodeType) void {
        switch (self) {
            .node => |node| node.deinit(),
            .most_frequent => {},
            .constant_value => {},
            .empty => {},
        }
    }

    pub fn print(self: ID3NodeType) !void {
        const stdout_file = std.io.getStdOut().writer();
        var bw = std.io.bufferedWriter(stdout_file);
        const stdout = bw.writer();

        // try ID3NodeType.printNext(self, 0, stdout);
        try self.printNext(0, stdout);

        try bw.flush(); // don't forget to flush!
    }

    pub fn printNext(self: ID3NodeType, initial_spaces: usize, stdout: std.io.BufferedWriter(4096, std.fs.File.Writer).Writer) !void {
        switch (self) {
            .node => |node| {
                // std.debug.print("Node with values: {any}\n", .{node.values.items});
                var max_value_chars: usize = 0;
                for (node.values.items) |val| {
                    const temp_chars: usize = @intFromFloat(@ceil(@log10(@max(@as(f64, @floatFromInt(val)), 1.0))));
                    if (temp_chars > max_value_chars) {
                        max_value_chars = temp_chars;
                    }
                }
                for (node.values.items, node.nodes.items, 0..) |value, next_node, idx| {
                    // try stdout.print("- '{:>{}}' -> ", .{ value, max_value_chars });
                    if (idx > 0) {
                        for (0..initial_spaces) |_| {
                            try stdout.print(" ", .{});
                        }
                    }
                    try stdout.print("{s} - {:^3} -> ", .{ node.field_name, value });
                    const spaces: usize = initial_spaces + node.field_name.len + 3 + 7; // 7 for the " -> "
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

    // The following predict function will traverse the ID3 tree.
    // We return a nullable GolfFieldType("play"), which we might improve later.
    pub fn predict(self: ID3NodeType, record: GolfConditions) ?GolfFieldType("play") {
        switch (self) {
            .node => |node| {
                const lookupValue: u64 = get_value_as_int(node.field_name, record);
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
};

fn all_target_values_equal(comptime target_field_name: []const u8, records: []GolfConditions) bool {
    if (records.len == 0) {
        return false; // No records to compare
    }
    const first_value: GolfFieldType(target_field_name) = @field(records[0], target_field_name);
    for (records[1..]) |record| {
        if (@field(record, target_field_name) != first_value) {
            return false; // Found a different value
        }
    }
    return true; // All values are equal
}

fn build_node(attribute_field_names: []const []const u8, comptime attribute_count: usize, comptime target_field_name: []const u8, records: []GolfConditions, allocator: std.mem.Allocator) std.mem.Allocator.Error!ID3NodeType {
    std.debug.print("Entering build_node\n", .{});
    if (records.len == 0) {
        // If S is empty, return a single node with value Failure;
        std.debug.print("In build_node, constructing empty leaf\n", .{});
        return ID3NodeType{ .empty = {} };
    } else if (all_target_values_equal(target_field_name, records)) {
        // If S consists of records all with the same value for
        // the categorical attribute,
        // return a single node with that value;
        std.debug.print("In build_node, constructing constant value leaf\n", .{});
        return ID3NodeType{ .constant_value = ConstantValueLeaf(target_field_name).init(@field(records[0], target_field_name)) };
    } else if (attribute_field_names.len == 0) {
        const mfv: MostFrequentValueLeaf(target_field_name) = try calculate_most_frequent_value(target_field_name, records);
        std.debug.print("In build_node, constructing most frequent value leaf\n", .{});
        return ID3NodeType{ .most_frequent = mfv };
    } else {
        var max_gain: f64 = undefined;
        var arg_max: usize = undefined;
        for (attribute_field_names, 0..) |attr, i| {
            std.debug.print("In build_node, attr is {s}\n", .{attr});
            const gain: f64 = try calculate_gain_using_hash_map(target_field_name, attr, records);
            if ((i == 0) or (gain > max_gain)) {
                max_gain = gain;
                arg_max = i;
            }
        }
        const best_field_name: []const u8 = attribute_field_names[arg_max];
        std.debug.print("In build_node, best attribute is {s}\n", .{best_field_name});

        var updated_attributes: [attribute_count][]const u8 = undefined;
        for (attribute_field_names, 0..) |attr, idx| {
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
        const updated_attributes_slice: []const []const u8 = updated_attributes[0..(attribute_field_names.len - 1)];

        // Sort the records
        sort_records(attribute_field_names[arg_max], records);

        // Create a list of nodes
        var node: ID3Node("play") = ID3Node("play").init(allocator, best_field_name);
        var start_idx: usize = 0;
        var end_idx: usize = 0;
        while (start_idx < records.len) : (end_idx += 1) {
            // Find the end of the current value group
            // TODO: best_field_name isn't comptime so we might need to adjust the value extraction
            const start_value: u64 = get_value_as_int(best_field_name, records[start_idx]);
            var append_flag: bool = false;
            if (end_idx == records.len) {
                append_flag = true;
            } else {
                const end_value: u64 = get_value_as_int(best_field_name, records[end_idx]);
                if (end_value != start_value) {
                    append_flag = true;
                }
            }
            if (append_flag) {
                // Create a sub-node for the current value group
                const sub_records = records[start_idx..end_idx];
                const sub_node = try build_node(updated_attributes_slice, attribute_count, target_field_name, sub_records, allocator);
                try node.appendValue(start_value, sub_node);
                start_idx = end_idx; // Move to the next group
            } else {
                continue; // Continue to find the end of the current value group
            }
        }

        return ID3NodeType{ .node = node };
    }
}

pub fn main() !void {
    // sunny   |      85     |    85    | false | Don't Play
    // sunny   |      80     |    90    | true  | Don't Play
    // overcast|      83     |    78    | false | Play
    // rain    |      70     |    96    | false | Play
    // rain    |      68     |    80    | false | Play
    // rain    |      65     |    70    | true  | Don't Play
    // overcast|      64     |    65    | true  | Play
    // sunny   |      72     |    95    | false | Don't Play
    // sunny   |      69     |    70    | false | Play
    // rain    |      75     |    80    | false | Play
    // sunny   |      75     |    70    | true  | Play
    // overcast|      72     |    90    | true  | Play
    // overcast|      81     |    75    | false | Play
    // rain    |      71     |    80    | true  | Don't Play

    var train = [_]GolfConditions{ GolfConditions{ .id = 0, .outlook = .sunny, .temperature = 85, .humidity = 85, .humidity_bucket = .gt75, .windy = .no, .play = .dont }, GolfConditions{ .id = 1, .outlook = .sunny, .temperature = 80, .humidity = 90, .humidity_bucket = .gt75, .windy = .yes, .play = .dont }, GolfConditions{ .id = 2, .outlook = .overcast, .temperature = 83, .humidity = 78, .humidity_bucket = .gt75, .windy = .no, .play = .do }, GolfConditions{ .id = 3, .outlook = .rain, .temperature = 70, .humidity = 96, .humidity_bucket = .gt75, .windy = .no, .play = .do }, GolfConditions{ .id = 4, .outlook = .rain, .temperature = 68, .humidity = 80, .humidity_bucket = .gt75, .windy = .no, .play = .do }, GolfConditions{ .id = 5, .outlook = .rain, .temperature = 65, .humidity = 70, .humidity_bucket = .le75, .windy = .yes, .play = .dont }, GolfConditions{ .id = 6, .outlook = .overcast, .temperature = 64, .humidity = 65, .humidity_bucket = .le75, .windy = .yes, .play = .do }, GolfConditions{ .id = 7, .outlook = .sunny, .temperature = 72, .humidity = 95, .humidity_bucket = .gt75, .windy = .no, .play = .dont }, GolfConditions{ .id = 8, .outlook = .sunny, .temperature = 69, .humidity = 70, .humidity_bucket = .le75, .windy = .no, .play = .do }, GolfConditions{ .id = 9, .outlook = .rain, .temperature = 75, .humidity = 80, .humidity_bucket = .gt75, .windy = .no, .play = .do }, GolfConditions{ .id = 10, .outlook = .sunny, .temperature = 75, .humidity = 70, .humidity_bucket = .le75, .windy = .yes, .play = .do }, GolfConditions{ .id = 11, .outlook = .overcast, .temperature = 72, .humidity = 90, .humidity_bucket = .gt75, .windy = .yes, .play = .do }, GolfConditions{ .id = 12, .outlook = .overcast, .temperature = 81, .humidity = 75, .humidity_bucket = .le75, .windy = .no, .play = .do }, GolfConditions{ .id = 13, .outlook = .rain, .temperature = 71, .humidity = 80, .humidity_bucket = .gt75, .windy = .yes, .play = .dont } };

    // stdout is for the actual output of your application, for example if you
    // are implementing gzip, then only the compressed bytes should be sent to
    // stdout, not any debugging messages.
    const stdout_file = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout_file);
    const stdout = bw.writer();

    // const fld: []const u8 = "temperature";
    // std.sort.insertion(GolfConditions, &train, {}, GolfConditions.lessThan); // This does work, just wanted to test the GolfFieldContext
    for (train) |rec| {
        try stdout.print("{d}: {d}\n", .{ rec.id, rec.temperature });
    }

    try stdout.print("Now using GolfContext2\n", .{});
    const WindyContext2 = GolfFieldContext2("windy");
    const windy_context2 = WindyContext2.init();
    const WhetherToPlay2 = GolfFieldContext2("play");
    const wtp2 = WhetherToPlay2.init();
    try stdout.print("Now using GolfContext2: {s}\n", .{windy_context2.field_name});
    std.sort.insertion(GolfConditions, &train, windy_context2, WindyContext2.lessThan);
    for (train) |rec| {
        try stdout.print("{d}: {s}\n", .{ rec.id, @tagName(rec.windy) });
    }
    try stdout.print("Now using GolfContext2: {s}\n", .{wtp2.field_name});
    std.sort.insertion(GolfConditions, &train, wtp2, WhetherToPlay2.lessThan);
    for (train) |rec| {
        try stdout.print("{d}: {s}\n", .{ rec.id, @tagName(rec.play) });
    }
    try stdout.print("Trying new sort method on GolfContext2 with field {s}\n", .{windy_context2.field_name});
    // WindyContext2.sort(windy_context2, &train);
    windy_context2.sort(&train);
    for (train) |rec| {
        try stdout.print("{d}: {s}\n", .{ rec.id, @tagName(rec.windy) });
    }
    try stdout.print("Now using the stuct sort methods\n", .{});
    try stdout.print("Trying the struct sort methods with field {s}\n", .{"play"});
    sorting_struct.play.sort(&train);
    for (train) |rec| {
        try stdout.print("{d}: {s}\n", .{ rec.id, @tagName(rec.play) });
    }
    try stdout.print("Trying the struct sort methods with field {s}\n", .{"windy"});
    sorting_struct.windy.sort(&train);
    for (train) |rec| {
        try stdout.print("{d}: {s}\n", .{ rec.id, @tagName(rec.windy) });
    }
    try stdout.print("Trying the struct sort methods with field {s}\n", .{"temperature"});
    sorting_struct.temperature.sort(&train);
    for (train) |rec| {
        try stdout.print("{d}: {d}\n", .{ rec.id, rec.temperature });
    }

    try bw.flush(); // don't forget to flush!

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer {
        const deinit_status = gpa.deinit();
        //fail test; can't try in defer as defer is executed after we return
        if (deinit_status == .leak) std.testing.expect(false) catch @panic("TEST FAIL");
    }

    const root: ID3NodeType = try build_node(&enum_fields2, enum_fields2.len, "play", &train, allocator);
    try root.print();
    defer root.deinit(); // deinitialize the root node to free memory
}

test "simple test" {
    var list = std.ArrayList(i32).init(std.testing.allocator);
    defer list.deinit(); // try commenting this out and see if zig detects the memory leak!
    try list.append(42);
    try std.testing.expectEqual(@as(i32, 42), list.pop());
}

// test "bool inequality" {
//     std.testing.expect(false < true);
// }

test "outlook values" {
    try std.testing.expect(@typeInfo(Outlook).Enum.fields.len == 3);
    try std.testing.expect(std.mem.eql(u8, @typeInfo(Outlook).Enum.fields[1].name, "overcast"));
}

test "windy values" {
    try std.testing.expect(@typeInfo(Windy).Enum.fields.len == 2);
    try std.testing.expect(std.mem.eql(u8, @typeInfo(Windy).Enum.fields[1].name, "yes"));
}

test "inline for for non-categorical variables" {
    const noncat_fields: [2][]const u8 = .{ "outlook", "temperature" };
    inline for (noncat_fields) |fld| {
        std.debug.print("Looking at field {s}\n", .{fld});
        if (std.mem.eql(u8, fld, "outlook")) {
            const T2 = GolfFieldType(fld);
            try std.testing.expect(T2 == Outlook);
        } else if (std.mem.eql(u8, fld, "temperature")) {
            const T2 = GolfFieldType(fld);
            try std.testing.expect(T2 == u8);
        } else {
            try std.testing.expect(false);
        }
    }
}

test "checking temperature type" {
    const fld: []const u8 = "temperature";
    const T2 = GolfFieldType(fld);
    try std.testing.expect(T2 == u8);
    try std.testing.expect(@typeInfo(T2).Int.signedness == .unsigned);
    try std.testing.expect(@typeInfo(T2).Int.bits == 8);
}

test "checking types of features are acceptable" {
    const noncat_fields: [4][]const u8 = .{ "outlook", "temperature", "windy", "play" };
    inline for (noncat_fields) |fld| {
        std.debug.print("Looking at field {s}\n", .{fld});
        const T = GolfFieldType(fld);
        switch (@typeInfo(T)) {
            .Enum => {
                try std.testing.expect(true);
            },
            .Int => {
                try std.testing.expect(true);
            },
            else => {
                try std.testing.expect(false);
            },
        }
    }
}

fn field_name_match_function(comptime field_names: []const [*:0]const u8, current_field: []const u8) bool {
    inline for (field_names) |fld| {
        const adj_fld: [:0]const u8 = std.mem.span(fld);
        if (std.mem.eql(u8, adj_fld, current_field)) {
            std.debug.print("Found field {s}\n", .{fld});
            // return current_field;
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

const KEY_SIZE = 8;
const VAL_SIZE = 8;
const EXTRA_SIZE = 100;
const MEM_SIZE = std.math.pow(usize, 2, 8) * (KEY_SIZE + VAL_SIZE) + EXTRA_SIZE;
const GAIN_MEM_SIZE = std.math.pow(usize, 2, 8) * (KEY_SIZE + MEM_SIZE) + EXTRA_SIZE;

fn calculate_entropy_from_hash_map(total_count: usize, hm: std.hash_map.AutoHashMap(u64, usize)) f64 {
    std.debug.print("In entropy calculation, total count is {d}\n", .{total_count});
    var entropy: f64 = 0.0;
    var iterator = hm.iterator();
    while (iterator.next()) |entry| {
        const count: u64 = entry.value_ptr.*;
        std.debug.print("In entropy calculation, key {d} has count {d}\n", .{ entry.key_ptr.*, entry.value_ptr.* });
        if (count > 0) {
            const p: f64 = @as(f64, @floatFromInt(count)) / @as(f64, @floatFromInt(total_count));
            std.debug.print("In entropy calculation, key {d} has probability {d} ({d} / {d})\n", .{ entry.key_ptr.*, p, @as(f64, @floatFromInt(count)), @as(f64, @floatFromInt(total_count)) });
            entropy -= p * std.math.log2(p);
        }
    }
    return entropy;
}

fn calculate_entropy_using_hash_map(comptime target_field_name: []const u8, records: []GolfConditions) std.mem.Allocator.Error!f64 {
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
    const entropy: f64 = calculate_entropy_from_hash_map(total_count, hm);
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

test "testing hash_map_example" {
    var single_value_test = [_]GolfConditions{ GolfConditions{ .id = 0, .outlook = .sunny, .temperature = 85, .humidity = 85, .humidity_bucket = .gt75, .windy = .no, .play = .dont }, GolfConditions{ .id = 1, .outlook = .sunny, .temperature = 80, .humidity = 90, .humidity_bucket = .gt75, .windy = .yes, .play = .dont }, GolfConditions{ .id = 2, .outlook = .overcast, .temperature = 83, .humidity = 78, .humidity_bucket = .gt75, .windy = .no, .play = .dont }, GolfConditions{ .id = 3, .outlook = .rain, .temperature = 70, .humidity = 96, .humidity_bucket = .gt75, .windy = .no, .play = .dont } };
    const actual_val1: f64 = try calculate_entropy_using_hash_map("play", &single_value_test);
    try std.testing.expectApproxEqAbs(@as(f64, 0.0), actual_val1, 1e-12);

    var max_entropy_test = [_]GolfConditions{ GolfConditions{ .id = 0, .outlook = .sunny, .temperature = 85, .humidity = 85, .humidity_bucket = .gt75, .windy = .no, .play = .dont }, GolfConditions{ .id = 1, .outlook = .sunny, .temperature = 80, .humidity = 90, .humidity_bucket = .gt75, .windy = .yes, .play = .do } };
    const actual_val2: f64 = try calculate_entropy_using_hash_map("play", &max_entropy_test);
    const exp_val2: f64 = std.math.log2(@as(f64, 2.0));
    std.debug.print("Expected value: {d}, Actual value: {d}\n", .{ exp_val2, actual_val2 });
    try std.testing.expectApproxEqAbs(exp_val2, actual_val2, 1e-12);
}

fn calculate_gain_using_hash_map(comptime target_field_name: []const u8, attribute_field_name: []const u8, records: []GolfConditions) std.mem.Allocator.Error!f64 {
    // _ = target_field_name; // to avoid unused variable warning
    inline for (enum_fields2) |fld| {
        // const adj_fld: [:0]const u8 = std.mem.span(fld);
        // std.debug.print("In calculate_gain_using_hash_map, checking to see if field {s} matches {s}\n", .{ attribute_field_name, fld });
        std.debug.print("In calculate_gain_using_hash_map, checking to see if field matches {s}\n", .{fld});
        if (std.mem.eql(u8, fld, attribute_field_name)) {
            std.debug.print("In calculate_gain_using_hash_map, Found field {s}\n", .{fld});
            // return @field(sorting_struct, fld).getValueAsInt(records[0]);
            return calculate_gain_using_hash_map0(target_field_name, fld, records);
        }
    }
    unreachable;
}

fn calculate_gain_using_hash_map0(comptime target_field_name: []const u8, comptime attribute_field_name: []const u8, records: []GolfConditions) std.mem.Allocator.Error!f64 {
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
        const k: u64 = @field(sorting_struct, attribute_field_name).getValueAsInt(record); // TODO: This approach needs improvement
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

    const entropy: f64 = try calculate_entropy_using_hash_map(target_field_name, records);
    const total_count: usize = records.len;
    std.debug.print("In gain calculation, total count is {d}\n", .{total_count});
    var condinfo: f64 = 0.0;
    var iterator = hm.iterator();
    while (iterator.next()) |entry| {
        const condhm: HMT = entry.value_ptr.*;
        var condcount: usize = 0;
        var condvit = condhm.valueIterator();
        while (condvit.next()) |val| {
            condcount += val.*;
        }
        const localinfo: f64 = calculate_entropy_from_hash_map(condcount, condhm);
        condinfo += localinfo * @as(f64, @floatFromInt(condcount)) / @as(f64, @floatFromInt(total_count));
    }

    return entropy - condinfo;
}

test "testing calculate_gain_using_hash_map0" {
    var single_value_test = [_]GolfConditions{ GolfConditions{ .id = 0, .outlook = .sunny, .temperature = 85, .humidity = 85, .humidity_bucket = .gt75, .windy = .no, .play = .dont }, GolfConditions{ .id = 1, .outlook = .sunny, .temperature = 80, .humidity = 90, .humidity_bucket = .gt75, .windy = .yes, .play = .dont }, GolfConditions{ .id = 2, .outlook = .overcast, .temperature = 83, .humidity = 78, .humidity_bucket = .gt75, .windy = .no, .play = .dont }, GolfConditions{ .id = 3, .outlook = .rain, .temperature = 70, .humidity = 96, .humidity_bucket = .gt75, .windy = .no, .play = .dont } };
    const actual_val1: f64 = try calculate_gain_using_hash_map0("play", "windy", &single_value_test);
    try std.testing.expectApproxEqAbs(@as(f64, 0.0), actual_val1, 1e-12);

    // var max_entropy_test = [_]GolfConditions{ GolfConditions{ .id = 0, .outlook = .sunny, .temperature = 85, .humidity = 85, .humidity_bucket = .gt75, .windy = .no, .play = .dont }, GolfConditions{ .id = 1, .outlook = .sunny, .temperature = 80, .humidity = 90, .humidity_bucket = .gt75, .windy = .yes, .play = .do } };
    // const actual_val2: f64 = try calculate_gain_using_hash_map0("play", "windy", &max_entropy_test);
    // const exp_val2: f64 = std.math.log2(@as(f64, 2.0));
    // std.debug.print("Expected value: {d}, Actual value: {d}\n", .{ exp_val2, actual_val2 });
    // try std.testing.expectApproxEqAbs(exp_val2, actual_val2, 1e-12);
}

test "testing calculate_gain_using_hash_map" {
    var recs = [_]GolfConditions{GolfConditions{ .id = 0, .outlook = .sunny, .temperature = 85, .humidity = 85, .humidity_bucket = .gt75, .windy = .no, .play = .dont }};
    const windy_val = try calculate_gain_using_hash_map("play", "windy", &recs);
    try std.testing.expect(windy_val == @intFromEnum(Windy.no));
}

test "testing gain from tutorial" {
    var train = [_]GolfConditions{ GolfConditions{ .id = 0, .outlook = .sunny, .temperature = 85, .humidity = 85, .humidity_bucket = .gt75, .windy = .no, .play = .dont }, GolfConditions{ .id = 1, .outlook = .sunny, .temperature = 80, .humidity = 90, .humidity_bucket = .gt75, .windy = .yes, .play = .dont }, GolfConditions{ .id = 2, .outlook = .overcast, .temperature = 83, .humidity = 78, .humidity_bucket = .gt75, .windy = .no, .play = .do }, GolfConditions{ .id = 3, .outlook = .rain, .temperature = 70, .humidity = 96, .humidity_bucket = .gt75, .windy = .no, .play = .do }, GolfConditions{ .id = 4, .outlook = .rain, .temperature = 68, .humidity = 80, .humidity_bucket = .gt75, .windy = .no, .play = .do }, GolfConditions{ .id = 5, .outlook = .rain, .temperature = 65, .humidity = 70, .humidity_bucket = .le75, .windy = .yes, .play = .dont }, GolfConditions{ .id = 6, .outlook = .overcast, .temperature = 64, .humidity = 65, .humidity_bucket = .le75, .windy = .yes, .play = .do }, GolfConditions{ .id = 7, .outlook = .sunny, .temperature = 72, .humidity = 95, .humidity_bucket = .gt75, .windy = .no, .play = .dont }, GolfConditions{ .id = 8, .outlook = .sunny, .temperature = 69, .humidity = 70, .humidity_bucket = .le75, .windy = .no, .play = .do }, GolfConditions{ .id = 9, .outlook = .rain, .temperature = 75, .humidity = 80, .humidity_bucket = .gt75, .windy = .no, .play = .do }, GolfConditions{ .id = 10, .outlook = .sunny, .temperature = 75, .humidity = 70, .humidity_bucket = .le75, .windy = .yes, .play = .do }, GolfConditions{ .id = 11, .outlook = .overcast, .temperature = 72, .humidity = 90, .humidity_bucket = .gt75, .windy = .yes, .play = .do }, GolfConditions{ .id = 12, .outlook = .overcast, .temperature = 81, .humidity = 75, .humidity_bucket = .le75, .windy = .no, .play = .do }, GolfConditions{ .id = 13, .outlook = .rain, .temperature = 71, .humidity = 80, .humidity_bucket = .gt75, .windy = .yes, .play = .dont } };

    for (train) |rec| {
        if (rec.humidity > 75) {
            try std.testing.expect(rec.humidity_bucket == HumidityBucket.gt75);
        } else {
            try std.testing.expect(rec.humidity_bucket == HumidityBucket.le75);
        }
    }

    const actual_entropy: f64 = try calculate_entropy_using_hash_map("play", &train);
    try std.testing.expectApproxEqAbs(@as(f64, 0.94), actual_entropy, 1e-3);

    const actual_gain_outlook: f64 = try calculate_gain_using_hash_map0("play", "outlook", &train);
    try std.testing.expectApproxEqAbs(@as(f64, 0.246), actual_gain_outlook, 1e-3);

    const actual_gain_windy: f64 = try calculate_gain_using_hash_map0("play", "windy", &train);
    try std.testing.expectApproxEqAbs(@as(f64, 0.048), actual_gain_windy, 1e-3);
}
