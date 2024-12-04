const std = @import("std");

const RunConfig = struct { file: []const u8 };

const ArgParseError = error{MissingFilenameError};

const MapSize = struct { nrow: usize, ncol: usize };

const MAP_MAX_WIDTH = 200;
const MAP_MAX_HEIGHT = 100;

fn process_args(argsIteratorPtr: *std.process.ArgIterator) ArgParseError!RunConfig {
    var cfg = RunConfig{ .file = "" };
    var fileprovided = false;

    var argsIterator = argsIteratorPtr.*;

    const cmd = argsIterator.next() orelse "readfile";

    const stdout = std.io.getStdOut().writer();
    while (argsIterator.next()) |arg| {
        if (std.mem.eql(u8, arg, "--help")) {
            stdout.print("{s} [--help] [--file FILE]\n", .{cmd}) catch {};
        } else {
            cfg.file = arg;
            fileprovided = true;
        }
    }

    if (fileprovided) {
        return cfg;
    } else {
        return ArgParseError.MissingFilenameError;
    }
}

// https://ziglang.org/documentation/master/std/#std.unicode
// https://zig.news/dude_the_builder/unicode-basics-in-zig-dj3
pub fn main() !void {
    var argsIterator = std.process.args();
    defer argsIterator.deinit();
    const cfg: RunConfig = process_args(&argsIterator) catch |err| {
        const stderr = std.io.getStdErr().writer();
        switch (err) {
            ArgParseError.MissingFilenameError => {
                try stderr.print("File name not provided", .{});
            },
        }
        std.process.exit(1);
    };

    // const cmd = argsIterator.next() orelse "readfile";

    // const stdout = std.io.getStdOut().writer();
    // try stdout.print("In command {s}\n", .{cmd});
    // while (argsIterator.next()) |arg| {
    //     try stdout.print("In command {s}, got argument {s}\n", .{ cmd, arg });
    //     if (std.mem.eql(u8, arg, "--help")) {
    //         try stdout.print("{s} [--help] [--file FILE]\n", .{cmd});
    //     }
    // }

    const stdout = std.io.getStdOut().writer();
    try stdout.print("\nProvided file: {s}", .{cfg.file});
    try stdout.print("\nProvided filename length: {}", .{cfg.file.len});
    // const file2: []const u8 = cfg.file;
    // try stdout.print("Length of filename: {}", .{file2.len});

    const map_width = MAP_MAX_WIDTH;
    const map_height = MAP_MAX_HEIGHT;
    // const buf_size = 100;
    // try to open the file
    // const init = "z" ** 100;
    // const buf: [100]u8 = .{'z'} ** 100;
    // const buf: []u8 = "zzzzzzzzzz";

    // this works
    // var init: [buf_size]u8 = .{'-'} ** buf_size;
    // const buf: []u8 = init[0..];
    // var init: [buf_size]u8 = undefined;
    // const buf: []u8 = init[0..];

    // const map_file: []const u8 = "/home/justinian/Code/datascience-miscellaneous/zig/readfile/maps/bridge";
    // const map_file = "/home/justinian/Code/datascience-miscellaneous/zig/readmapfile/maps/bridge";
    const map_file = cfg.file; //  "/home/justinian/Code/datascience-miscellaneous/zig/readmapfile/maps/bridge"; // TODO: Improve this

    var map_obstacles: [map_height][map_width]u8 = undefined;
    for (&map_obstacles) |*row| {
        @memset(row, 0);
    }
    const map_size: MapSize = read_map_obstacles(map_file, &map_obstacles) catch |err| {
        switch (err) {
            MapReadError.FileOpenError => {
                try stdout.print("Caught error opening map file {s}", .{map_file});
            },
            MapReadError.FileReadError => {
                try stdout.print("Caught error reading map file {s}", .{map_file});
            },
            MapReadError.Utf8ByteSequenceLengthError => {
                try stdout.print("Caught error reading Utf8 byte sequence from map file {s}", .{map_file});
            },
            MapReadError.OutOfBoundsError => {
                try stdout.print("The map provided in the file {s} is too large to be processed", .{map_file});
            },
        }
        std.process.exit(2);
    };
    try stdout.print("\nThe map has {} rows and {} columns", .{ map_size.nrow, map_size.ncol });
    for (map_obstacles[0..map_size.nrow]) |row| {
        stdout.print("\n", .{}) catch {};
        for (row[0..map_size.ncol]) |col| {
            stdout.print("{d}", .{col}) catch {};
        }
    }

    // const file = try std.fs.openFileAbsolute("/home/justinian/Code/datascience-miscellaneous/zig/readfile/maps/bridge", .{});
    // defer file.close();

    // var keep_going: bool = true;
    // var chars_read: usize = 0;
    // while (keep_going) {
    //     chars_read = file.read(buf) catch 0;
    //     if (chars_read > 0) {
    //         const slen = std.unicode.utf8ByteSequenceLength(buf[0]) catch 0;
    //         try stdout.print("UTF-8 Byte Sequence Length: {d}", .{slen});
    //         if (slen == 1) {
    //             try stdout.print("Unicode as ascii: '{c}'", .{buf[0]});
    //         } else if (slen == 2) {
    //             const uchar: u21 = std.unicode.utf8Decode2(buf[0..2]) catch 0;
    //             try stdout.print("2-byte Unicode as hex: 0x{x}", .{uchar});
    //         } else if (slen == 3) {
    //             const uchar: u21 = std.unicode.utf8Decode3(buf[0..3]) catch 0;
    //             try stdout.print("3-byte Unicode as hex: 0x{x}", .{uchar});
    //         } else if (slen == 4) {
    //             const uchar: u21 = std.unicode.utf8Decode4(buf[0..3]) catch 0;
    //             try stdout.print("3-byte Unicode as hex: 0x{x}", .{uchar});
    //         }
    //     }
    //     try stdout.print("{s}", .{buf[0..chars_read]});
    //     // for (buf) |c| {
    //     //     if (c == '\n') {
    //     //         try stdout.print("Found newline!", .{});
    //     //     }
    //     // }
    //     if (chars_read < buf_size) {
    //         keep_going = false;
    //     }
    // }
}

// fn simple_array() [3]u8 {
//     const ret: [3]u8 = [_]u8{ 2, 3, 4 };
//     return ret;
// }
//
// fn simple_array2(inputslice: []u8) void {
//     var ret: [3]u8 = [_]u8{ 5, 6, 7 };
//     @memcpy(inputslice, &ret);
// }

const MapReadError = error{ FileOpenError, FileReadError, Utf8ByteSequenceLengthError, OutOfBoundsError };

fn read_map_obstacles(map_file: []const u8, map_obstacles: *[MAP_MAX_HEIGHT][MAP_MAX_WIDTH]u8) MapReadError!MapSize {
    const file = std.fs.openFileAbsolute(map_file, .{}) catch {
        return MapReadError.FileOpenError;
    };
    defer file.close();

    var init: [4]u8 = undefined;
    const buf: []u8 = init[0..];

    var map_size = MapSize{ .nrow = 0, .ncol = 0 };
    // var nrow: usize = 0;
    // var ncol: usize = 0;

    var buffer_start_idx: usize = 0;
    var buffer_capacity: usize = 4;
    var chars_read: usize = 0;
    var rowidx: usize = 0;
    var colidx: usize = 0;

    const stdout = std.io.getStdOut().writer();

    while (true) {
        chars_read = file.read(buf[buffer_start_idx..]) catch {
            stdout.print("Error reading into buffer", .{}) catch {};
            return MapReadError.FileReadError;
        };
        // stdout.print("Read {d} chars", .{chars_read}) catch {};
        const slen: usize = std.unicode.utf8ByteSequenceLength(buf[0]) catch |err| {
            stdout.print("Error calling utf8ByteSequenceLength {}", .{err}) catch {};
            return MapReadError.Utf8ByteSequenceLengthError;
        };

        var uchar: u21 = undefined;

        if (slen == 1) {
            uchar = @as(u21, buf[0]);
        } else if (slen == 2) {
            uchar = std.unicode.utf8Decode2(buf[0..2]) catch 0;
        } else if (slen == 3) {
            uchar = std.unicode.utf8Decode3(buf[0..3]) catch 0;
        } else if (slen == 4) {
            uchar = std.unicode.utf8Decode4(buf[0..4]) catch 0;
        } else {
            return MapReadError.Utf8ByteSequenceLengthError;
        }

        if ((slen == 1) and (uchar == @as(u21, '\n'))) {
            if (colidx >= map_size.ncol) {
                map_size.ncol = colidx;
            }
            rowidx += 1;
            colidx = 0;
        } else {
            if ((uchar == @as(u21, 'w')) or (uchar == @as(u21, 'W'))) {
                // TODO: Move the following logic out of the branches here.
                //       We can do this once before writing to the array.
                if ((rowidx >= MAP_MAX_HEIGHT) or (colidx >= MAP_MAX_WIDTH)) {
                    return MapReadError.OutOfBoundsError;
                }
                map_obstacles[rowidx][colidx] = 1;
            } else if (uchar == 'o') { // TODO
                if ((rowidx >= MAP_MAX_HEIGHT) or (colidx >= MAP_MAX_WIDTH)) {
                    return MapReadError.OutOfBoundsError;
                }
                map_obstacles[rowidx][colidx] = 2;
            } else if (uchar == 0x2593) {
                if ((rowidx >= MAP_MAX_HEIGHT) or (colidx >= MAP_MAX_WIDTH)) {
                    return MapReadError.OutOfBoundsError;
                }
                map_obstacles[rowidx][colidx] = 1;
            } else if (uchar == 0x2612) {
                if ((rowidx >= MAP_MAX_HEIGHT) or (colidx >= MAP_MAX_WIDTH)) {
                    return MapReadError.OutOfBoundsError;
                }
                map_obstacles[rowidx][colidx] = 3;
            }
            // increment column index regardless of whether there was a character match
            colidx += 1;
        }

        // if (slen == 1) {
        //     if ((buf[0] == 'w') or (buf[0] == 'W')) {
        //         if ((rowidx >= 100) or (colidx >= 200)) {
        //             return MapReadError.OutOfBoundsError;
        //         }
        //         map_obstacles[rowidx][colidx] = 1;
        //         colidx += 1;
        //     } else if (buf[0] == 'o') {
        //         if ((rowidx >= 100) or (colidx >= 200)) {
        //             return MapReadError.OutOfBoundsError;
        //         }
        //         map_obstacles[rowidx][colidx] = 2;
        //         colidx += 1;
        //     } else if (buf[0] == '\n') {
        //         // if (colidx > 0) {
        //         //     nrow = rowidx + 1;
        //         // }
        //         if (colidx >= map_size.ncol) {
        //             map_size.ncol = colidx;
        //         }
        //         stdout.print("\nRow {d} has length {d}", .{ rowidx, (colidx + 1) }) catch {};
        //         rowidx += 1;
        //         colidx = 0;
        //     } else { // some other character
        //         colidx += 1;
        //     }
        // } else if (slen == 2) {
        //     const uchar: u21 = std.unicode.utf8Decode2(buf[0..2]) catch 0;
        //     stdout.print("2-byte Unicode as hex: 0x{x}", .{uchar}) catch {};
        //     colidx += 1;
        // } else if (slen == 3) {
        //     const uchar: u21 = std.unicode.utf8Decode3(buf[0..3]) catch 0;
        //     stdout.print("3-byte Unicode as hex: 0x{x}", .{uchar}) catch {};
        //     if (uchar == 0x2593) {
        //         if ((rowidx >= 100) or (colidx >= 200)) {
        //             return MapReadError.OutOfBoundsError;
        //         }
        //         map_obstacles[rowidx][colidx] = 1;
        //     } else if (uchar == 0x2612) {
        //         if ((rowidx >= 100) or (colidx >= 200)) {
        //             return MapReadError.OutOfBoundsError;
        //         }
        //         map_obstacles[rowidx][colidx] = 3;
        //     }
        //     colidx += 1;
        // } else if (slen == 4) {
        //     const uchar: u21 = std.unicode.utf8Decode4(buf[0..4]) catch 0;
        //     stdout.print("4-byte Unicode as hex: 0x{x}", .{uchar}) catch {};
        //     colidx += 1;
        // } else {
        //     stdout.print("Error: utf8ByteSequenceLength returned 0", .{}) catch {};
        //     return MapReadError.Utf8ByteSequenceLengthError;
        // }

        if ((rowidx >= MAP_MAX_HEIGHT) or (colidx >= MAP_MAX_WIDTH)) { // TODO: Is this the way to go? Update: I don't think this is needed
            return MapReadError.OutOfBoundsError;
        }

        if (chars_read < buffer_capacity) {
            // end of file
            stdout.print("Read {d} characters while the capacity is {d}", .{ chars_read, buffer_capacity }) catch {};
            // TODO: Need to process the rest of the buffer
            var buffer_remaining: usize = buffer_start_idx + chars_read;
            std.mem.copyForwards(u8, buf, buf[slen..]);
            buffer_remaining -= slen;
            while (buffer_remaining > 0) {
                const bytes_processed: usize = try read_buffer_into_map_obstacles(buf, map_obstacles, &map_size, &colidx, &rowidx);
                std.mem.copyForwards(u8, buf, buf[bytes_processed..]);
                buffer_remaining -= bytes_processed;
            }
            break;
        }
        // TODO: check rowidx and colidx for validity
        // stdout.print("\nslen: {d}", .{slen}) catch {};
        if (slen < 4) {
            std.mem.copyForwards(u8, buf, buf[slen..]);
        }

        buffer_start_idx = (4 - slen);
        buffer_capacity = 4 - buffer_start_idx;
    }

    // set nrow
    if (colidx > 0) {
        map_size.nrow = rowidx + 1; // the file doesn't end with a newline
    } else {
        map_size.nrow = rowidx;
    }

    return map_size;
}

fn read_buffer_into_map_obstacles(buf: []u8, map_obstacles: *[MAP_MAX_HEIGHT][MAP_MAX_WIDTH]u8, map_size_ptr: *MapSize, colidx_ptr: *usize, rowidx_ptr: *usize) MapReadError!usize {
    const buffer_capacity: usize = buf.len;

    const slen: usize = std.unicode.utf8ByteSequenceLength(buf[0]) catch {
        return MapReadError.Utf8ByteSequenceLengthError;
    };

    if (buffer_capacity < slen) {
        return MapReadError.Utf8ByteSequenceLengthError;
    }

    var uchar: u21 = undefined;

    if (slen == 1) {
        uchar = @as(u21, buf[0]);
    } else if (slen == 2) {
        uchar = std.unicode.utf8Decode2(buf[0..2]) catch 0;
    } else if (slen == 3) {
        uchar = std.unicode.utf8Decode3(buf[0..3]) catch 0;
    } else if (slen == 4) {
        uchar = std.unicode.utf8Decode4(buf[0..4]) catch 0;
    } else {
        return MapReadError.Utf8ByteSequenceLengthError;
    }

    if ((slen == 1) and (uchar == @as(u21, '\n'))) {
        if (colidx_ptr.* >= map_size_ptr.*.ncol) {
            map_size_ptr.*.ncol = colidx_ptr.*;
        }
        rowidx_ptr.* += 1;
        colidx_ptr.* = 0;
    } else {
        if ((rowidx_ptr.* >= MAP_MAX_HEIGHT) or (colidx_ptr.* >= MAP_MAX_WIDTH)) {
            return MapReadError.OutOfBoundsError;
        }
        if ((uchar == @as(u21, 'w')) or (uchar == @as(u21, 'W'))) {
            map_obstacles[rowidx_ptr.*][colidx_ptr.*] = 1;
        } else if (uchar == 'o') { // TODO
            map_obstacles[rowidx_ptr.*][colidx_ptr.*] = 2;
        } else if (uchar == 0x2593) {
            map_obstacles[rowidx_ptr.*][colidx_ptr.*] = 1;
        } else if (uchar == 0x2612) {
            map_obstacles[rowidx_ptr.*][colidx_ptr.*] = 3;
        }
        // increment column index regardless of whether there was a character match
        colidx_ptr.* += 1;
    }
    return slen;
}
