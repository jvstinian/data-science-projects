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

pub const golfRecords = [_]GolfConditions{ GolfConditions{ .id = 0, .outlook = .sunny, .temperature = 85, .humidity = 85, .humidity_bucket = .gt75, .windy = .no, .play = .dont }, GolfConditions{ .id = 1, .outlook = .sunny, .temperature = 80, .humidity = 90, .humidity_bucket = .gt75, .windy = .yes, .play = .dont }, GolfConditions{ .id = 2, .outlook = .overcast, .temperature = 83, .humidity = 78, .humidity_bucket = .gt75, .windy = .no, .play = .do }, GolfConditions{ .id = 3, .outlook = .rain, .temperature = 70, .humidity = 96, .humidity_bucket = .gt75, .windy = .no, .play = .do }, GolfConditions{ .id = 4, .outlook = .rain, .temperature = 68, .humidity = 80, .humidity_bucket = .gt75, .windy = .no, .play = .do }, GolfConditions{ .id = 5, .outlook = .rain, .temperature = 65, .humidity = 70, .humidity_bucket = .le75, .windy = .yes, .play = .dont }, GolfConditions{ .id = 6, .outlook = .overcast, .temperature = 64, .humidity = 65, .humidity_bucket = .le75, .windy = .yes, .play = .do }, GolfConditions{ .id = 7, .outlook = .sunny, .temperature = 72, .humidity = 95, .humidity_bucket = .gt75, .windy = .no, .play = .dont }, GolfConditions{ .id = 8, .outlook = .sunny, .temperature = 69, .humidity = 70, .humidity_bucket = .le75, .windy = .no, .play = .do }, GolfConditions{ .id = 9, .outlook = .rain, .temperature = 75, .humidity = 80, .humidity_bucket = .gt75, .windy = .no, .play = .do }, GolfConditions{ .id = 10, .outlook = .sunny, .temperature = 75, .humidity = 70, .humidity_bucket = .le75, .windy = .yes, .play = .do }, GolfConditions{ .id = 11, .outlook = .overcast, .temperature = 72, .humidity = 90, .humidity_bucket = .gt75, .windy = .yes, .play = .do }, GolfConditions{ .id = 12, .outlook = .overcast, .temperature = 81, .humidity = 75, .humidity_bucket = .le75, .windy = .no, .play = .do }, GolfConditions{ .id = 13, .outlook = .rain, .temperature = 71, .humidity = 80, .humidity_bucket = .gt75, .windy = .yes, .play = .dont } };

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

pub fn GolfFieldContext(comptime field_name: []const u8) type {
    return id3.Id3FieldContext(GolfConditions, field_name);
}

test "check GolfFieldContext type" {
    const fld: []const u8 = "play";
    const playType: type = GolfFieldContext(fld);
    const playDefault: playType = playType.init();
    try comptime std.testing.expect(@TypeOf(playDefault) == playType);
}

test "check less than in golf context from field name" {
    const WindyContext2 = GolfFieldContext("windy");
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

test "testing gain from tutorial" {
    const calculate_entropy_using_hash_map = id3.Id3Entropy(GolfConditions, "play").calculateEntropyUsingHashMap;

    const calculate_gain_using_hash_map0_windy = id3.Id3Gain(GolfConditions, "windy", "play").calculateGainUsingHashMap;
    const calculate_gain_using_hash_map0_outlook = id3.Id3Gain(GolfConditions, "outlook", "play").calculateGainUsingHashMap;

    const attribute_field_names: [3][]const u8 = .{ "outlook", "humidity_bucket", "windy" };
    const calculate_gain_using_hash_map = id3.ID3NodeType(GolfConditions, &attribute_field_names, "play").calculateGainForFieldUsingHashMap;

    var train: [golfRecords.len]GolfConditions = undefined;
    @memcpy(&train, &golfRecords);

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

test "testing sort of windy field using context" {
    var train: [golfRecords.len]GolfConditions = undefined;
    @memcpy(&train, &golfRecords);

    const WindyContext2 = GolfFieldContext("windy");
    const windy_context2 = WindyContext2.init();
    std.sort.insertion(GolfConditions, &train, windy_context2, WindyContext2.lessThan);

    try std.testing.expect(std.mem.eql(u8, windy_context2.field_name, "windy"));

    var idx: usize = 1;
    while (idx < train.len) : (idx += 1) {
        try std.testing.expect(windy_context2.getValueAsInt(train[idx - 1]) <= windy_context2.getValueAsInt(train[idx]));
    }
}

test "testing sort method of context for windy field" {
    var train: [golfRecords.len]GolfConditions = undefined;
    @memcpy(&train, &golfRecords);

    const WindyContext2 = GolfFieldContext("windy");
    const windy_context2 = WindyContext2.init();
    windy_context2.sort(&train);

    var idx: usize = 1;
    while (idx < train.len) : (idx += 1) {
        try std.testing.expect(windy_context2.getValueAsInt(train[idx - 1]) <= windy_context2.getValueAsInt(train[idx]));
    }
}

test "testing sort of play field using context" {
    var train: [golfRecords.len]GolfConditions = undefined;
    @memcpy(&train, &golfRecords);

    const WhetherToPlayContext = GolfFieldContext("play");
    const wtp = WhetherToPlayContext.init();
    std.sort.insertion(GolfConditions, &train, wtp, WhetherToPlayContext.lessThan);

    try std.testing.expect(std.mem.eql(u8, wtp.field_name, "play"));

    var idx: usize = 1;
    while (idx < train.len) : (idx += 1) {
        try std.testing.expect(wtp.getValueAsInt(train[idx - 1]) <= wtp.getValueAsInt(train[idx]));
    }
}

test "testing sorting of fields using sorting structure" {
    var train: [golfRecords.len]GolfConditions = undefined;
    @memcpy(&train, &golfRecords);

    const golf_fields: [5][*:0]const u8 = .{ "outlook", "windy", "temperature", "humidity_bucket", "play" };
    const sorting_struct = id3.Id3SorterStruct(GolfConditions, &golf_fields){};
    sorting_struct.play.sort(&train);

    var idx: usize = 1;
    while (idx < train.len) : (idx += 1) {
        try std.testing.expect(sorting_struct.play.getValueAsInt(train[idx - 1]) <= sorting_struct.play.getValueAsInt(train[idx]));
    }

    sorting_struct.windy.sort(&train);
    idx = 1;
    while (idx < train.len) : (idx += 1) {
        try std.testing.expect(sorting_struct.windy.getValueAsInt(train[idx - 1]) <= sorting_struct.windy.getValueAsInt(train[idx]));
    }

    sorting_struct.temperature.sort(&train);
    idx = 1;
    while (idx < train.len) : (idx += 1) {
        try std.testing.expect(sorting_struct.temperature.getValueAsInt(train[idx - 1]) <= sorting_struct.temperature.getValueAsInt(train[idx]));
    }
}

test "testing sorting of play field using sorting processor" {
    var train: [golfRecords.len]GolfConditions = undefined;
    @memcpy(&train, &golfRecords);

    const golf_fields: [4][*:0]const u8 = .{ "outlook", "windy", "humidity_bucket", "play" };
    const sorting_processor = id3.AltId3FieldProcessors(GolfConditions, &golf_fields).init();
    sorting_processor.sortRecords("play", &train);

    var idx: usize = 1;
    while (idx < train.len) : (idx += 1) {
        try std.testing.expect(sorting_processor.getValueAsInt("play", train[idx - 1]) <= sorting_processor.getValueAsInt("play", train[idx]));
    }
}

test "check type sizes used for entropy calculations including humidity" {
    const KEY_SIZE = @sizeOf(u64);
    const VAL_SIZE = @sizeOf(usize);
    const EXTRA_SIZE = 100;
    const EXTRA_MULTIPLIER = 2;
    // play only has two values, hence the multiplication by 2 in the following
    const EXPECTED_MEM_SIZE = 2 * EXTRA_MULTIPLIER * (KEY_SIZE + VAL_SIZE) + EXTRA_SIZE;
    // Humidity is of type u8, so it can assume 256 values.  This is a larger number of values than for the other fields considered.
    const EXPECTED_GAIN_SIZE = std.math.pow(usize, 2, 8) * EXTRA_MULTIPLIER * (KEY_SIZE + EXPECTED_MEM_SIZE) + EXTRA_SIZE;
    const ACTUAL_MEM_SIZE: usize = id3.calculateMemorySizeForEntropy(GolfConditions, "play");
    try std.testing.expectEqual(ACTUAL_MEM_SIZE, EXPECTED_MEM_SIZE);
    const ACTUAL_GAIN_SIZE: usize = id3.calculateMemorySizeForGain(GolfConditions, &[_][]const u8{ "outlook", "windy", "humidity" }, "play");
    try std.testing.expectEqual(ACTUAL_GAIN_SIZE, EXPECTED_GAIN_SIZE);
}

test "check type sizes used for entropy calculations with humidity_bucket" {
    const KEY_SIZE = @sizeOf(u64);
    const VAL_SIZE = @sizeOf(usize);
    const EXTRA_SIZE = 100;
    const EXTRA_MULTIPLIER = 2;
    // play only has two values, hence the multiplication by 2 in the following
    const EXPECTED_MEM_SIZE = 2 * EXTRA_MULTIPLIER * (KEY_SIZE + VAL_SIZE) + EXTRA_SIZE;
    // Among the three fields, the field that can assume the most values is "outlook", which can assume 3 values.
    const EXPECTED_GAIN_SIZE = 3 * EXTRA_MULTIPLIER * (KEY_SIZE + EXPECTED_MEM_SIZE) + EXTRA_SIZE;
    const ACTUAL_MEM_SIZE: usize = id3.calculateMemorySizeForEntropy(GolfConditions, "play");
    try std.testing.expectEqual(ACTUAL_MEM_SIZE, EXPECTED_MEM_SIZE);
    const ACTUAL_GAIN_SIZE: usize = id3.calculateMemorySizeForGain(GolfConditions, &[_][]const u8{ "outlook", "windy", "humidity_bucket" }, "play");
    try std.testing.expectEqual(ACTUAL_GAIN_SIZE, EXPECTED_GAIN_SIZE);
}
