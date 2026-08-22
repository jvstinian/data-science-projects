# Environment

We use SDL2, pkg-config, and gnatmake.
A suitable environment in Nix can be acquired using
```
nix-shell -p gcc gdb SDL2 pkg-config
```

# Build

Use
```
make cg
```
to build the executable `cg`.

# References
* [SDL.h](https://github.com/libsdl-org/SDL/blob/SDL2/include/SDL.h)

