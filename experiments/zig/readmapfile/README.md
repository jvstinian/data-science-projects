# Reading Zombsole Maps With Zig

We set up a simple zig example reading the UTF-8 encoded zombsole maps.
At this time, the maps are processed into an array over an enum type
representing the map elements, namely walls, boxes, objectives, and
player and zombie spawns.

zig 0.13.0 was used for this project.


# Build

## Build Using Zig

Build using 
```
zig build
```

or build and run using 
```
zig build run -- MAP_FILE
```
where `MAP_FILE` is the absolute path to a zombsole map file.

## Build With NixOS

This project includes a flake, and can be built with
```
nix build
``` 
or run with 
```
nix run github:jvstinian/data-science-projects?dir=zig/readmapfile#default -- MAP_FILE
```


# References

* [zig memcpy](https://ziglang.org/documentation/master/#memcpy)
* [zig std.unicode](https://ziglang.org/documentation/master/std/#std.unicode)
* [processing command line arguments in zig](https://renatoathaydes.github.io/zig-common-tasks/#get-command-line-args)
* [unicode basics in zig](https://zig.news/dude_the_builder/unicode-basics-in-zig-dj3)
