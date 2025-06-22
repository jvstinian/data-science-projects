const std = @import("std");
const id3 = @import("id3.zig");
const golf = @import("golf.zig");

// Golf Conditions and related types
// TODO: Which of the following are still needed?
const Outlook = golf.Outlook;
const WhetherToPlay = golf.WhetherToPlay;
const Windy = golf.Windy;
const HumidityBucket = golf.HumidityBucket;
const GolfConditions = golf.GolfConditions;

// const GolfFieldOffset = golf.GolfFieldOffset;
// TODO: Which of the following are still needed?
const GolfFieldType = golf.GolfFieldType;
const GolfFieldContext2 = golf.GolfFieldContext2;
const Id3SorterStruct = id3.Id3SorterStruct;
const Id3FieldProcessors = id3.Id3FieldProcessors;
const AltId3FieldProcessors = id3.AltId3FieldProcessors;
const Id3Entropy = id3.Id3Entropy;

// TODO: Which of the following are still needed?
const enum_fields: [3][*:0]const u8 = .{ "outlook", "windy", "humidity" };
const enum_fields2: [3][]const u8 = .{ "outlook", "windy", "humidity_bucket" };
const enum_and_target_fields: [6][*:0]const u8 = enum_fields ++ [3][*:0]const u8{ "humidity_bucket", "temperature", "play" };

// const enum_and_target_fields0 = .{ "outlook", "windy", "humidity", "humidity_bucket", "temperature", "play" };
// const enum_and_target_fields: [6][*:0]const u8 = enum_and_target_fields0; // [_][*:0]const u8{ "outlook", "windy", "humidity", "humidity_bucket", "temperature", "play" };
// const enum_fields2 = enum_and_target_fields0[0..3];
// // enum_fields2[0] = std.mem.span(enum_and_target_fields0[0]);
// // enum_fields2[1] = std.mem.span(enum_and_target_fields0[1]);
// // enum_fields2[2] = std.mem.span(enum_and_target_fields0[2]);

const enum_and_target_fields1: [6][:0]const u8 = .{ "outlook", "windy", "humidity", "humidity_bucket", "temperature", "play" };

test "Testing comparison of string with sentinel to one without sentinel" {
    const sentinel: [:0]const u8 = "play";
    const without_sentinel: []const u8 = "play";
    try std.testing.expect(std.mem.eql(u8, sentinel, without_sentinel));

    try std.testing.expect(std.mem.eql(u8, enum_and_target_fields1[5], without_sentinel));
}

// const sorting_struct = Id3SorterStruct(GolfConditions, &enum_and_target_fields){};
// const sorting_processor = AltId3FieldProcessors(GolfConditions, &enum_and_target_fields).init();

pub fn main() !void {
    var train = golf.golfRecords;
    // var train = [_]GolfConditions{ GolfConditions{ .id = 0, .outlook = .sunny, .temperature = 85, .humidity = 85, .humidity_bucket = .gt75, .windy = .no, .play = .dont }, GolfConditions{ .id = 1, .outlook = .sunny, .temperature = 80, .humidity = 90, .humidity_bucket = .gt75, .windy = .yes, .play = .dont }, GolfConditions{ .id = 2, .outlook = .overcast, .temperature = 83, .humidity = 78, .humidity_bucket = .gt75, .windy = .no, .play = .do }, GolfConditions{ .id = 3, .outlook = .rain, .temperature = 70, .humidity = 96, .humidity_bucket = .gt75, .windy = .no, .play = .do }, GolfConditions{ .id = 4, .outlook = .rain, .temperature = 68, .humidity = 80, .humidity_bucket = .gt75, .windy = .no, .play = .do }, GolfConditions{ .id = 5, .outlook = .rain, .temperature = 65, .humidity = 70, .humidity_bucket = .le75, .windy = .yes, .play = .dont }, GolfConditions{ .id = 6, .outlook = .overcast, .temperature = 64, .humidity = 65, .humidity_bucket = .le75, .windy = .yes, .play = .do }, GolfConditions{ .id = 7, .outlook = .sunny, .temperature = 72, .humidity = 95, .humidity_bucket = .gt75, .windy = .no, .play = .dont }, GolfConditions{ .id = 8, .outlook = .sunny, .temperature = 69, .humidity = 70, .humidity_bucket = .le75, .windy = .no, .play = .do }, GolfConditions{ .id = 9, .outlook = .rain, .temperature = 75, .humidity = 80, .humidity_bucket = .gt75, .windy = .no, .play = .do }, GolfConditions{ .id = 10, .outlook = .sunny, .temperature = 75, .humidity = 70, .humidity_bucket = .le75, .windy = .yes, .play = .do }, GolfConditions{ .id = 11, .outlook = .overcast, .temperature = 72, .humidity = 90, .humidity_bucket = .gt75, .windy = .yes, .play = .do }, GolfConditions{ .id = 12, .outlook = .overcast, .temperature = 81, .humidity = 75, .humidity_bucket = .le75, .windy = .no, .play = .do }, GolfConditions{ .id = 13, .outlook = .rain, .temperature = 71, .humidity = 80, .humidity_bucket = .gt75, .windy = .yes, .play = .dont } };

    // stdout is for the actual output of your application, for example if you
    // are implementing gzip, then only the compressed bytes should be sent to
    // stdout, not any debugging messages.
    // const stdout_file = std.io.getStdOut().writer();
    // var bw = std.io.bufferedWriter(stdout_file);
    // const stdout = bw.writer();

    // try bw.flush(); // don't forget to flush!

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer {
        const deinit_status = gpa.deinit();
        //fail test; can't try in defer as defer is executed after we return
        if (deinit_status == .leak) std.testing.expect(false) catch @panic("TEST FAIL");
    }

    // const root: ID3NodeType = try build_node(&enum_fields2, enum_fields2.len, "play", &train, allocator);
    const root: id3.ID3NodeType(GolfConditions, &enum_fields2, "play") = try id3.ID3NodeType(GolfConditions, &enum_fields2, "play").buildNode(&enum_fields2, &train, allocator);
    defer root.deinit(); // deinitialize the root node to free memory
    try root.print();

    // Building a tree using humidity to test the use of a non-enum field
    const enum_fields9: [3][]const u8 = .{ "outlook", "windy", "humidity" };
    const root2: id3.ID3NodeType(GolfConditions, &enum_fields9, "play") = try id3.ID3NodeType(GolfConditions, &enum_fields9, "play").buildNode(&enum_fields9, &train, allocator);
    try root2.print();
    defer root2.deinit(); // deinitialize the root node to free memory
}

// TODO: Probably have to consider implementing compareFn and upperBound in a similar way to what was done for sort.
// REFERENCES: https://ziglang.org/documentation/master/std/#std.sort.upperBound
