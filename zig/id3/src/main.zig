const std = @import("std");
const id3 = @import("id3.zig");
const golf = @import("golf.zig");

const GolfConditions = golf.GolfConditions;

const attribute_fields: [3][]const u8 = .{ "outlook", "windy", "humidity_bucket" };

pub fn main() !void {
    var train = golf.golfRecords;

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer {
        const deinit_status = gpa.deinit();
        //fail test; can't try in defer as defer is executed after we return
        if (deinit_status == .leak) std.testing.expect(false) catch @panic("TEST FAIL");
    }

    const stdout_file = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout_file);
    const stdout = bw.writer();

    try stdout.print("Running ID3 algorithm on golf data set using fields outlook, windy, and humidity_bucket\n", .{});
    try bw.flush();

    const root: id3.ID3NodeType(GolfConditions, &attribute_fields, "play") = try id3.ID3NodeType(GolfConditions, &attribute_fields, "play").buildNode(&attribute_fields, &train, allocator);
    defer root.deinit(); // deinitialize the root node to free memory
    try root.print();

    try stdout.print("Running ID3 algorithm on golf data set using fields outlook, windy, and humidity\n", .{});
    try bw.flush();

    // Building a tree using humidity to test the use of a non-enum field
    const attribute_fields_alternate: [3][]const u8 = .{ "outlook", "windy", "humidity" };
    const root_alternate: id3.ID3NodeType(GolfConditions, &attribute_fields_alternate, "play") = try id3.ID3NodeType(GolfConditions, &attribute_fields_alternate, "play").buildNode(&attribute_fields_alternate, &train, allocator);
    try root_alternate.print();
    defer root_alternate.deinit(); // deinitialize the root node to free memory
}
