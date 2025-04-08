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
