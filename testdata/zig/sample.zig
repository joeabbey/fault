const std = @import("std");
const fs = @import("fs");
const json = @import("json");

pub const Config = struct {
    name: []const u8,
    port: u16,
    debug: bool,
};

const InternalState = enum {
    idle,
    running,
    stopped,
};

pub var global_allocator: std.mem.Allocator = undefined;
var internal_count: usize = 0;

pub fn init(allocator: std.mem.Allocator) !void {
    global_allocator = allocator;
}

pub fn parseConfig(data: []const u8) !Config {
    var parser = json.Parser.init(global_allocator);
    defer parser.deinit();
    const result = try parser.parse(data);
    return Config{
        .name = result.get("name").?.string,
        .port = @intCast(result.get("port").?.integer),
        .debug = result.get("debug").?.bool,
    };
}

fn internalHelper(x: i32, y: i32) i32 {
    return x + y;
}

pub fn serve(config: Config) !void {
    var server = std.net.StreamServer.init(.{});
    defer server.deinit();
    try server.listen(std.net.Address.parseIp("0.0.0.0", config.port) catch unreachable);
    while (true) {
        const conn = try server.accept();
        defer conn.stream.close();
        // handle connection
    }
}
