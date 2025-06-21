const std = @import("std");
const id3 = @import("id3.zig");
const testing = std.testing;

pub const Outlook = enum { sunny, overcast, rain };

pub const WhetherToPlay = enum { dont, do };

pub const Windy = enum(u1) { no = 0, yes = 1 };

pub const HumidityBucket = enum(u1) { le75 = 0, gt75 = 1 };

pub const GolfConditions = struct {
    id: u8,
    outlook: Outlook,
    temperature: u8,
    humidity: u8,
    humidity_bucket: HumidityBucket,
    windy: Windy,
    play: WhetherToPlay,
};

test "outlook values" {
    try std.testing.expect(@typeInfo(Outlook).Enum.fields.len == 3);
    try std.testing.expect(std.mem.eql(u8, @typeInfo(Outlook).Enum.fields[1].name, "overcast"));
}

test "windy values" {
    try std.testing.expect(@typeInfo(Windy).Enum.fields.len == 2);
    try std.testing.expect(std.mem.eql(u8, @typeInfo(Windy).Enum.fields[1].name, "yes"));
}

fn GolfFieldOffset(comptime field_name: []const u8) comptime_int {
    return id3.Id3FieldOffset(GolfConditions, field_name);
}

test "golf field offsets" {
    try std.testing.expect(GolfFieldOffset("id") == 0);
    try std.testing.expect(GolfFieldOffset("outlook") == 1);
    try std.testing.expect(GolfFieldOffset("windy") == 5);
}

pub fn GolfFieldType(comptime field_name: []const u8) type {
    return id3.Id3FieldType(GolfConditions, field_name);
}

test "golf field types from name" {
    try std.testing.expect(GolfFieldType("id") == u8);
    try std.testing.expect(GolfFieldType("outlook") == Outlook);
    try std.testing.expect(GolfFieldType("humidity") == u8);
    try std.testing.expect(GolfFieldType("windy") == Windy);
}

pub fn GolfFieldContext2(comptime field_name: []const u8) type {
    return id3.Id3FieldContext(GolfConditions, field_name);
}

test "check GolfFieldContext2 type" {
    const fld: []const u8 = "play";
    const playType: type = GolfFieldContext2(fld);
    const playDefault: playType = playType.init();
    try comptime std.testing.expect(@TypeOf(playDefault) == playType);
}

test "check less than in golf context from field name" {
    const WindyContext2 = GolfFieldContext2("windy");
    const windy_context2 = WindyContext2.init();
    const gc1 = GolfConditions{ .id = 0, .outlook = .sunny, .temperature = 85, .humidity = 85, .humidity_bucket = .gt75, .windy = .no, .play = .dont };
    const gc2 = GolfConditions{ .id = 0, .outlook = .sunny, .temperature = 85, .humidity = 85, .humidity_bucket = .gt75, .windy = .yes, .play = .dont };
    try std.testing.expect(windy_context2.lessThan(gc1, gc2));
}

fn calculate_entropy(comptime target_field_name: []const u8, records: []GolfConditions) f64 {
    return id3.Id3Entropy(GolfConditions, target_field_name).calculate_entropy(records);
}

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

test "constant value node" {
    const ConstantValueLeafType = id3.ConstantValueLeaf(GolfConditions, "play");
    const cvf = ConstantValueLeafType.init(WhetherToPlay.dont);
    try std.testing.expect(cvf.value == WhetherToPlay.dont);
    try std.testing.expect(@TypeOf(cvf.value) == WhetherToPlay);
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

test "testing calculation of entropy using hash map" {
    const calculate_entropy_using_hash_map = id3.Id3Entropy(GolfConditions, "play").calculateEntropyUsingHashMap;

    var single_value_test = [_]GolfConditions{ GolfConditions{ .id = 0, .outlook = .sunny, .temperature = 85, .humidity = 85, .humidity_bucket = .gt75, .windy = .no, .play = .dont }, GolfConditions{ .id = 1, .outlook = .sunny, .temperature = 80, .humidity = 90, .humidity_bucket = .gt75, .windy = .yes, .play = .dont }, GolfConditions{ .id = 2, .outlook = .overcast, .temperature = 83, .humidity = 78, .humidity_bucket = .gt75, .windy = .no, .play = .dont }, GolfConditions{ .id = 3, .outlook = .rain, .temperature = 70, .humidity = 96, .humidity_bucket = .gt75, .windy = .no, .play = .dont } };
    const actual_val1: f64 = try calculate_entropy_using_hash_map(&single_value_test);
    try std.testing.expectApproxEqAbs(@as(f64, 0.0), actual_val1, 1e-12);

    var max_entropy_test = [_]GolfConditions{ GolfConditions{ .id = 0, .outlook = .sunny, .temperature = 85, .humidity = 85, .humidity_bucket = .gt75, .windy = .no, .play = .dont }, GolfConditions{ .id = 1, .outlook = .sunny, .temperature = 80, .humidity = 90, .humidity_bucket = .gt75, .windy = .yes, .play = .do } };
    const actual_val2: f64 = try calculate_entropy_using_hash_map(&max_entropy_test);
    const exp_val2: f64 = std.math.log2(@as(f64, 2.0));
    std.debug.print("Expected value: {d}, Actual value: {d}\n", .{ exp_val2, actual_val2 });
    try std.testing.expectApproxEqAbs(exp_val2, actual_val2, 1e-12);
}

test "testing calculation of gain using hash map with comptime field name" {
    const calculate_gain_using_hash_map0 = id3.Id3Gain(GolfConditions, "windy", "play").calculateGainUsingHashMap;

    var single_value_test = [_]GolfConditions{ GolfConditions{ .id = 0, .outlook = .sunny, .temperature = 85, .humidity = 85, .humidity_bucket = .gt75, .windy = .no, .play = .dont }, GolfConditions{ .id = 1, .outlook = .sunny, .temperature = 80, .humidity = 90, .humidity_bucket = .gt75, .windy = .yes, .play = .dont }, GolfConditions{ .id = 2, .outlook = .overcast, .temperature = 83, .humidity = 78, .humidity_bucket = .gt75, .windy = .no, .play = .dont }, GolfConditions{ .id = 3, .outlook = .rain, .temperature = 70, .humidity = 96, .humidity_bucket = .gt75, .windy = .no, .play = .dont } };
    const actual_val1: f64 = try calculate_gain_using_hash_map0(&single_value_test);
    try std.testing.expectApproxEqAbs(@as(f64, 0.0), actual_val1, 1e-12);

    // var max_entropy_test = [_]GolfConditions{ GolfConditions{ .id = 0, .outlook = .sunny, .temperature = 85, .humidity = 85, .humidity_bucket = .gt75, .windy = .no, .play = .dont }, GolfConditions{ .id = 1, .outlook = .sunny, .temperature = 80, .humidity = 90, .humidity_bucket = .gt75, .windy = .yes, .play = .do } };
    // const actual_val2: f64 = try calculate_gain_using_hash_map0("play", "windy", &max_entropy_test);
    // const exp_val2: f64 = std.math.log2(@as(f64, 2.0));
    // std.debug.print("Expected value: {d}, Actual value: {d}\n", .{ exp_val2, actual_val2 });
    // try std.testing.expectApproxEqAbs(exp_val2, actual_val2, 1e-12);
}

test "testing calculation of gain for field using hash map" {
    const attribute_field_names: [3][]const u8 = .{ "outlook", "humidity_bucket", "windy" };
    const calculate_gain_using_hash_map = id3.ID3NodeType(GolfConditions, &attribute_field_names, "play").calculateGainForFieldUsingHashMap;

    var recs = [_]GolfConditions{GolfConditions{ .id = 0, .outlook = .sunny, .temperature = 85, .humidity = 85, .humidity_bucket = .gt75, .windy = .no, .play = .dont }};
    const windy_val = try calculate_gain_using_hash_map("windy", &recs);
    try std.testing.expect(windy_val == @intFromEnum(Windy.no));
}

test "testing gain from tutorial" {
    const calculate_entropy_using_hash_map = id3.Id3Entropy(GolfConditions, "play").calculateEntropyUsingHashMap;

    const calculate_gain_using_hash_map0_windy = id3.Id3Gain(GolfConditions, "windy", "play").calculateGainUsingHashMap;
    const calculate_gain_using_hash_map0_outlook = id3.Id3Gain(GolfConditions, "outlook", "play").calculateGainUsingHashMap;

    const attribute_field_names: [3][]const u8 = .{ "outlook", "humidity_bucket", "windy" };
    const calculate_gain_using_hash_map = id3.ID3NodeType(GolfConditions, &attribute_field_names, "play").calculateGainForFieldUsingHashMap;

    var train = [_]GolfConditions{ GolfConditions{ .id = 0, .outlook = .sunny, .temperature = 85, .humidity = 85, .humidity_bucket = .gt75, .windy = .no, .play = .dont }, GolfConditions{ .id = 1, .outlook = .sunny, .temperature = 80, .humidity = 90, .humidity_bucket = .gt75, .windy = .yes, .play = .dont }, GolfConditions{ .id = 2, .outlook = .overcast, .temperature = 83, .humidity = 78, .humidity_bucket = .gt75, .windy = .no, .play = .do }, GolfConditions{ .id = 3, .outlook = .rain, .temperature = 70, .humidity = 96, .humidity_bucket = .gt75, .windy = .no, .play = .do }, GolfConditions{ .id = 4, .outlook = .rain, .temperature = 68, .humidity = 80, .humidity_bucket = .gt75, .windy = .no, .play = .do }, GolfConditions{ .id = 5, .outlook = .rain, .temperature = 65, .humidity = 70, .humidity_bucket = .le75, .windy = .yes, .play = .dont }, GolfConditions{ .id = 6, .outlook = .overcast, .temperature = 64, .humidity = 65, .humidity_bucket = .le75, .windy = .yes, .play = .do }, GolfConditions{ .id = 7, .outlook = .sunny, .temperature = 72, .humidity = 95, .humidity_bucket = .gt75, .windy = .no, .play = .dont }, GolfConditions{ .id = 8, .outlook = .sunny, .temperature = 69, .humidity = 70, .humidity_bucket = .le75, .windy = .no, .play = .do }, GolfConditions{ .id = 9, .outlook = .rain, .temperature = 75, .humidity = 80, .humidity_bucket = .gt75, .windy = .no, .play = .do }, GolfConditions{ .id = 10, .outlook = .sunny, .temperature = 75, .humidity = 70, .humidity_bucket = .le75, .windy = .yes, .play = .do }, GolfConditions{ .id = 11, .outlook = .overcast, .temperature = 72, .humidity = 90, .humidity_bucket = .gt75, .windy = .yes, .play = .do }, GolfConditions{ .id = 12, .outlook = .overcast, .temperature = 81, .humidity = 75, .humidity_bucket = .le75, .windy = .no, .play = .do }, GolfConditions{ .id = 13, .outlook = .rain, .temperature = 71, .humidity = 80, .humidity_bucket = .gt75, .windy = .yes, .play = .dont } };

    for (train) |rec| {
        if (rec.humidity > 75) {
            try std.testing.expect(rec.humidity_bucket == HumidityBucket.gt75);
        } else {
            try std.testing.expect(rec.humidity_bucket == HumidityBucket.le75);
        }
    }

    const actual_entropy: f64 = try calculate_entropy_using_hash_map(&train);
    try std.testing.expectApproxEqAbs(@as(f64, 0.94), actual_entropy, 1e-3);

    const actual_gain_outlook: f64 = try calculate_gain_using_hash_map0_outlook(&train);
    try std.testing.expectApproxEqAbs(@as(f64, 0.246), actual_gain_outlook, 1e-3);

    const actual_gain_windy: f64 = try calculate_gain_using_hash_map0_windy(&train);
    try std.testing.expectApproxEqAbs(@as(f64, 0.048), actual_gain_windy, 1e-3);

    const actual_gain_outlook2: f64 = try calculate_gain_using_hash_map("outlook", &train);
    try std.testing.expectApproxEqAbs(@as(f64, 0.246), actual_gain_outlook2, 1e-3);

    const actual_gain_windy2: f64 = try calculate_gain_using_hash_map("windy", &train);
    try std.testing.expectApproxEqAbs(@as(f64, 0.048), actual_gain_windy2, 1e-3);
}

export fn multiply(a: i32, b: i32) i32 {
    return a * b;
}

test "basic multiply functionality" {
    try testing.expect(multiply(3, 7) == 21);
}
