const std = @import("std");
const testing = std.testing;

export fn multiply(a: i32, b: i32) i32 {
    return a * b;
}

test "basic multiply functionality" {
    try testing.expect(multiply(3, 7) == 21);
}
