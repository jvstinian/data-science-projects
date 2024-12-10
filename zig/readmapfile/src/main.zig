const std = @import("std");

const RunConfig = struct { cmd: []const u8, file: []const u8, help_flag: bool = false };

const ArgParseError = error{MissingFilenameError};

const MapSize = struct { nrow: usize, ncol: usize };

const MapArtifacts = enum(u3) { Nothing, Wall, Box, PlayerSpawn, ZombieSpawn, ObjectiveLocation };

const MAP_MAX_WIDTH = 200;
const MAP_MAX_HEIGHT = 100;

fn process_args(argsIteratorPtr: *std.process.ArgIterator) ArgParseError!RunConfig {
    var cfg = RunConfig{ .cmd = "", .file = "" };
    var fileprovided = false;

    var argsIterator = argsIteratorPtr.*;

    cfg.cmd = argsIterator.next() orelse "readfile";

    while (argsIterator.next()) |arg| {
        if (std.mem.eql(u8, arg, "--help")) {
            cfg.help_flag = true;
            break;
        } else {
            cfg.file = arg;
            fileprovided = true;
        }
    }

    const status: bool = fileprovided or cfg.help_flag;
    if (status) {
        return cfg;
    } else {
        return ArgParseError.MissingFilenameError;
    }
}

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();

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
    if (cfg.help_flag) {
        stdout.print("{s} [--help] [--file FILE]\n", .{cfg.cmd}) catch {};
        std.process.exit(0);
    }

    const map_file = cfg.file;

    var map_obstacles: [MAP_MAX_HEIGHT][MAP_MAX_WIDTH]MapArtifacts = undefined;
    for (&map_obstacles) |*row| {
        @memset(row, MapArtifacts.Nothing);
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
    for (map_obstacles[0..map_size.nrow]) |row| {
        stdout.print("\n", .{}) catch {};
        for (row[0..map_size.ncol]) |col| {
            const machar: u8 = switch (col) {
                MapArtifacts.Wall => 'W',
                MapArtifacts.Box => 'B',
                MapArtifacts.ObjectiveLocation => 'o',
                MapArtifacts.PlayerSpawn => 'p',
                MapArtifacts.ZombieSpawn => 'z',
                else => ' ',
            };
            stdout.print("{c}", .{machar}) catch {};
        }
    }
    try stdout.print("\nThe map has {} rows and {} columns", .{ map_size.nrow, map_size.ncol });
}

const MapReadError = error{ FileOpenError, FileReadError, Utf8ByteSequenceLengthError, OutOfBoundsError };

fn read_map_obstacles(map_file: []const u8, map_obstacles: *[MAP_MAX_HEIGHT][MAP_MAX_WIDTH]MapArtifacts) MapReadError!MapSize {
    const file = std.fs.openFileAbsolute(map_file, .{}) catch {
        return MapReadError.FileOpenError;
    };
    defer file.close();

    var init: [4]u8 = undefined;
    const buf: []u8 = init[0..];

    var map_size = MapSize{ .nrow = 0, .ncol = 0 };

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

        // end of file
        if (chars_read < buffer_capacity) {
            var buffer_remaining: usize = buffer_start_idx + chars_read;
            // std.mem.copyForwards(u8, buf, buf[slen..]); // TODO: Remove
            // buffer_remaining -= slen;
            while (buffer_remaining > 0) {
                const bytes_processed: usize = try read_buffer_into_map_obstacles(buf, map_obstacles, &map_size, &colidx, &rowidx);
                std.mem.copyForwards(u8, buf, buf[bytes_processed..]);
                buffer_remaining -= bytes_processed;
            }
            break;
        }

        const slen: usize = try read_buffer_into_map_obstacles(buf, map_obstacles, &map_size, &colidx, &rowidx);

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

fn read_buffer_into_map_obstacles(buf: []u8, map_obstacles: *[MAP_MAX_HEIGHT][MAP_MAX_WIDTH]MapArtifacts, map_size_ptr: *MapSize, colidx_ptr: *usize, rowidx_ptr: *usize) MapReadError!usize {
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
            map_obstacles[rowidx_ptr.*][colidx_ptr.*] = MapArtifacts.Wall;
        } else if ((uchar == @as(u21, 'b')) or (uchar == @as(u21, 'B'))) {
            map_obstacles[rowidx_ptr.*][colidx_ptr.*] = MapArtifacts.Box;
        } else if (uchar == 'o') {
            map_obstacles[rowidx_ptr.*][colidx_ptr.*] = MapArtifacts.ObjectiveLocation;
        } else if ((uchar == @as(u21, 'p')) or (uchar == @as(u21, 'P'))) {
            map_obstacles[rowidx_ptr.*][colidx_ptr.*] = MapArtifacts.PlayerSpawn;
        } else if ((uchar == @as(u21, 'z')) or (uchar == @as(u21, 'Z'))) {
            map_obstacles[rowidx_ptr.*][colidx_ptr.*] = MapArtifacts.ZombieSpawn;
        } else if (uchar == 0x2593) {
            map_obstacles[rowidx_ptr.*][colidx_ptr.*] = MapArtifacts.Wall;
        } else if (uchar == 0x2612) {
            map_obstacles[rowidx_ptr.*][colidx_ptr.*] = MapArtifacts.Box;
        }
        // increment column index regardless of whether there was a character match
        colidx_ptr.* += 1;
    }
    return slen;
}
