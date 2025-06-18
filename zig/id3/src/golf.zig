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

fn GolfFieldOffset(comptime field_name: []const u8) comptime_int {
    return id3.Id3FieldOffset(GolfConditions, field_name);
}

test "golf field offsets" {
    std.debug.print("Testing golf field offsets\n", .{});
    try std.testing.expect(GolfFieldOffset("id") == 0);
    try std.testing.expect(GolfFieldOffset("outlook") == 1);
    try std.testing.expect(GolfFieldOffset("windy") == 5);
}

pub fn GolfFieldType(comptime field_name: []const u8) type {
    return id3.Id3FieldType(GolfConditions, field_name);
}

test "golf field types from name" {
    std.debug.print("Testing golf field types from name\n", .{});
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

export fn multiply(a: i32, b: i32) i32 {
    return a * b;
}

test "basic multiply functionality" {
    try testing.expect(multiply(3, 7) == 21);
}
