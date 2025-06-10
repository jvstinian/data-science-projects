const std = @import("std");

const Outlook = enum { sunny, overcast, rain };

const WhetherToPlay = enum { dont, do };

const Windy = enum(u1) { no = 0, yes = 1 };

const GolfConditions = struct {
    id: u8,
    outlook: Outlook,
    temperature: u8,
    humidity: u8,
    windy: Windy,
    play: WhetherToPlay,

    // fn lessThan(context: []const u8, a: GolfConditions, b: GolfConditions) bool {
    //     const fld = context;
    //     return @field(a, fld) < @field(b, fld);
    // }
    fn lessThan(context: void, a: GolfConditions, b: GolfConditions) bool {
        _ = context;
        return a.temperature < b.temperature;
    }
};

fn GolfFieldOffset(comptime field_name: []const u8) comptime_int {
    return @offsetOf(GolfConditions, field_name);
}

test "golf field offsets" {
    std.debug.print("Testing golf field offsets\n", .{});
    try std.testing.expect(GolfFieldOffset("id") == 0);
    try std.testing.expect(GolfFieldOffset("outlook") == 1);
    try std.testing.expect(GolfFieldOffset("windy") == 4);
}

fn GolfFieldType(comptime field_name: []const u8) type {
    const fields = @typeInfo(GolfConditions).Struct.fields;
    inline for (fields) |fld| {
        if (std.mem.eql(u8, fld.name, field_name)) {
            return fld.type;
        }
    }
    // return void;
    unreachable;
}

test "golf field types from name" {
    std.debug.print("Testing golf field types from name\n", .{});
    try std.testing.expect(GolfFieldType("id") == u8);
    try std.testing.expect(GolfFieldType("outlook") == Outlook);
    try std.testing.expect(GolfFieldType("humidity") == u8);
    try std.testing.expect(GolfFieldType("windy") == Windy);
}

fn GolfFieldContext(comptime T: type, comptime field_name: []const u8) type {
    return struct {
        const Self = @This();

        field_name: []const u8,
        offset: usize,
        // offset: u8 = @offsetOf(GolfConditions, field_name),

        pub const offset2: usize = @offsetOf(GolfConditions, field_name);

        pub fn lessThan(self: Self, a: GolfConditions, b: GolfConditions) bool {
            // const T2: type = GolfFieldType(field_name);
            // std.testing.expect(T == T2);
            const a_fld_ptr: *T = @ptrFromInt(@intFromPtr(&a) + self.offset);
            const b_fld_ptr: *T = @ptrFromInt(@intFromPtr(&b) + self.offset);
            return @intFromEnum(a_fld_ptr.*) < @intFromEnum(b_fld_ptr.*);
        }

        pub fn init() Self {
            return Self{ .field_name = field_name, .offset = @offsetOf(GolfConditions, field_name) };
        }
    };
}

fn GolfFieldContext2(comptime field_name: []const u8) type {
    return struct {
        const Self = @This();

        field_name: []const u8,
        offset: usize,
        // offset: u8 = @offsetOf(GolfConditions, field_name),

        pub const offset2: usize = @offsetOf(GolfConditions, field_name);

        pub fn lessThan(self: Self, a: GolfConditions, b: GolfConditions) bool {
            const T2: type = GolfFieldType(field_name);
            // std.testing.expect(T == T2);
            const a_fld_ptr: *T2 = @ptrFromInt(@intFromPtr(&a) + self.offset);
            const b_fld_ptr: *T2 = @ptrFromInt(@intFromPtr(&b) + self.offset);
            // const b_fld_ptr: *T2 = @ptrFromInt(@intFromPtr(&b) + .offset2);
            // return @intFromEnum(a_fld_ptr.*) < @intFromEnum(b_fld_ptr.*);
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

        pub fn init() Self {
            return Self{ .field_name = field_name, .offset = @offsetOf(GolfConditions, field_name) };
        }

        // This seems to work
        pub fn sort(self: Self, train: []GolfConditions) void {
            std.sort.insertion(GolfConditions, train, self, Self.lessThan);
        }

        // TODO: This probably doesn't work in this form
        // pub fn upperBound(self: Self, items: []GolfConditions) void {
        //     return std.sort.upperBound(GolfConditions, items, self, Self.lessThan);
        // }
    };
}

test "check GolfFieldContext2 type" {
    const fld: []const u8 = "play";
    const playType: type = GolfFieldContext2(fld);
    const playDefault: playType = playType.init();
    try comptime std.testing.expect(@TypeOf(playDefault) == playType);
}

// fn MakeSorterStruct(comptime field_names: []*const [:0]const u8) type
fn MakeSorterStruct(comptime field_names: []const [*:0]const u8) type {
    var fields: [field_names.len]std.builtin.Type.StructField = undefined;
    for (field_names, 0..) |field_name, i| {
        // std.fmt.comptimePrint("Making field {s} at index {d}\n", .{ field_name, i });
        // const fieldName: [:0]const u8 = field_name[0.. :0];
        const fieldName: [:0]const u8 = std.mem.span(field_name);
        const fieldType: type = GolfFieldContext2(fieldName); // Note the coercion here
        const defaultFieldValue: fieldType = fieldType.init();
        // if (fieldName[0] == '?') {
        //         //     fieldType = @Type(.{ .Optional = .{ .child = fieldType } });
        //     fieldName = fieldName[1..];
        // }
        fields[i] = .{
            .name = fieldName, // need sentinel termination for the field name
            .type = fieldType,
            .default_value = &defaultFieldValue,
            .is_comptime = false,
            .alignment = 0,
        };
    }
    // _ = field_names.len; // to avoid unused variable warning
    // const fld: [:0]const u8 = "play";
    // const fld2: []const u8 = "play";
    // const playType: type = GolfFieldContext2(fld2);
    // const playDefault: playType = playType.init();
    // var fields: [1]std.builtin.Type.StructField = .{
    //     .{
    //         .name = fld,
    //         .type = playType, // GolfFieldContext2("play"), // WhetherToPlay,
    //         .default_value = &playDefault, // GolfFieldContext2("play").init(), //  WhetherToPlay.dont,
    //         .is_comptime = false,
    //         .alignment = 0,
    //     },
    // };

    return @Type(.{
        .Struct = .{
            .layout = .auto,
            .fields = fields[0..],
            .decls = &[_]std.builtin.Type.Declaration{},
            .is_tuple = false,
        },
    });
}

const enum_fields: [4][*:0]const u8 = .{ "outlook", "windy", "play", "temperature" };
// const enum_fields: [3]*const [:0]u8 = .{ "outlook", "windy", "play" }; // does not work

const sorting_struct = MakeSorterStruct(&enum_fields){};

// fn PrintSorterConstruct(comptime field_names: []const [*:0]const u8) void {
//     @compileLog("Number of fields", field_names.len);
//     for (field_names) |field_name| {
//         @compileLog("Working with field ", field_name);
//     }
// }
//

test "golf context from field name" {
    std.debug.print("Testing golf context construction using field name\n", .{});
    const WindyContext = GolfFieldContext(Windy, "windy");
    const windy_context = WindyContext.init();
    const WindyContext2 = GolfFieldContext2("windy");
    const windy_context2 = WindyContext2.init();
    try std.testing.expect(windy_context.offset == windy_context2.offset);
    const gc1 = GolfConditions{ .id = 0, .outlook = .sunny, .temperature = 85, .humidity = 85, .windy = .no, .play = .dont };
    const gc2 = GolfConditions{ .id = 0, .outlook = .sunny, .temperature = 85, .humidity = 85, .windy = .yes, .play = .dont };
    // try std.testing.expect(WindyContext.lessThan(gc1, gc2));
    try std.testing.expect(windy_context.lessThan(gc1, gc2));
    // try std.testing.expect(WindyContext2.lessThan(gc1, gc2));
    try std.testing.expect(windy_context2.lessThan(gc1, gc2));
}

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
    var single_value_test = [_]GolfConditions{ GolfConditions{ .id = 0, .outlook = .sunny, .temperature = 85, .humidity = 85, .windy = .no, .play = .dont }, GolfConditions{ .id = 1, .outlook = .sunny, .temperature = 80, .humidity = 90, .windy = .yes, .play = .dont }, GolfConditions{ .id = 2, .outlook = .overcast, .temperature = 83, .humidity = 78, .windy = .no, .play = .dont }, GolfConditions{ .id = 3, .outlook = .rain, .temperature = 70, .humidity = 96, .windy = .no, .play = .dont } };
    const actual_val1: f64 = calculate_entropy("play", &single_value_test);
    try std.testing.expectApproxEqAbs(@as(f64, 0.0), actual_val1, 1e-12);

    var max_entropy_test = [_]GolfConditions{ GolfConditions{ .id = 0, .outlook = .sunny, .temperature = 85, .humidity = 85, .windy = .no, .play = .dont }, GolfConditions{ .id = 1, .outlook = .sunny, .temperature = 80, .humidity = 90, .windy = .yes, .play = .do } };
    const actual_val2: f64 = calculate_entropy("play", &max_entropy_test);
    const exp_val2: f64 = std.math.log2(@as(f64, 2.0));
    std.debug.print("Expected value: {d}, Actual value: {d}\n", .{ exp_val2, actual_val2 });
    try std.testing.expectApproxEqAbs(exp_val2, actual_val2, 1e-12);
}

// fn build_node(attribute_field_names: []const [*:0]const u8, comptime target_field_name: []const u8, records: []GolfConditions, lower_bound: usize, upper_bound: usize) type {
//     // TODO
// }

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

    var train = [_]GolfConditions{ GolfConditions{ .id = 0, .outlook = .sunny, .temperature = 85, .humidity = 85, .windy = .no, .play = .dont }, GolfConditions{ .id = 1, .outlook = .sunny, .temperature = 80, .humidity = 90, .windy = .yes, .play = .dont }, GolfConditions{ .id = 2, .outlook = .overcast, .temperature = 83, .humidity = 78, .windy = .no, .play = .do }, GolfConditions{ .id = 3, .outlook = .rain, .temperature = 70, .humidity = 96, .windy = .no, .play = .do }, GolfConditions{ .id = 4, .outlook = .rain, .temperature = 68, .humidity = 80, .windy = .no, .play = .do }, GolfConditions{ .id = 5, .outlook = .rain, .temperature = 65, .humidity = 70, .windy = .yes, .play = .dont }, GolfConditions{ .id = 6, .outlook = .overcast, .temperature = 64, .humidity = 65, .windy = .yes, .play = .do }, GolfConditions{ .id = 7, .outlook = .sunny, .temperature = 72, .humidity = 95, .windy = .no, .play = .dont }, GolfConditions{ .id = 8, .outlook = .sunny, .temperature = 69, .humidity = 70, .windy = .no, .play = .do }, GolfConditions{ .id = 9, .outlook = .rain, .temperature = 75, .humidity = 80, .windy = .no, .play = .do }, GolfConditions{ .id = 10, .outlook = .sunny, .temperature = 75, .humidity = 70, .windy = .yes, .play = .do }, GolfConditions{ .id = 11, .outlook = .overcast, .temperature = 72, .humidity = 90, .windy = .yes, .play = .do }, GolfConditions{ .id = 12, .outlook = .overcast, .temperature = 81, .humidity = 75, .windy = .no, .play = .do }, GolfConditions{ .id = 13, .outlook = .rain, .temperature = 71, .humidity = 80, .windy = .yes, .play = .dont } };

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

    const WindyContext = GolfFieldContext(Windy, "windy");
    const windy_context = WindyContext.init();
    try stdout.print("Windy offset using declaration: {d}\n", .{WindyContext.offset2});
    try stdout.print("Windy offset using member variable: {d}\n", .{windy_context.offset});
    std.sort.insertion(GolfConditions, &train, windy_context, WindyContext.lessThan);
    for (train) |rec| {
        try stdout.print("{d}: {s}\n", .{ rec.id, @tagName(rec.windy) });
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
