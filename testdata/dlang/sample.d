module myapp.main;

import std.stdio;
import std.string;
import std.json : parseJSON, JSONValue;
static import std.algorithm;

class Config {
    string name;
    int port;
    bool debugMode;

    this(string name, int port, bool debugMode = false) {
        this.name = name;
        this.port = port;
        this.debugMode = debugMode;
    }

    string toString() {
        return "Config(" ~ name ~ ":" ~ to!string(port) ~ ")";
    }
}

struct Point {
    double x;
    double y;

    double distanceTo(Point other) {
        import std.math : sqrt;
        return sqrt((x - other.x) ^^ 2 + (y - other.y) ^^ 2);
    }
}

interface Handler {
    void handle(string request);
}

enum Status {
    active,
    inactive,
    archived,
}

alias JsonMap = JSONValue[string];

template Singleton(T) {
    T instance;
    T getInstance() {
        if (instance is null) {
            instance = new T();
        }
        return instance;
    }
}

private class InternalCache {
    int[string] store;
}

void main(string[] args) {
    auto config = new Config("myapp", 8080);
    writeln(config.toString());
}

auto parseConfig(string data) {
    auto json = parseJSON(data);
    return new Config(
        json["name"].str,
        cast(int) json["port"].integer
    );
}

private void internalHelper() {
    // internal use only
}
