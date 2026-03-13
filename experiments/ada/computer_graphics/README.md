# Environment

We use SDL2, pkg-config, and gnatmake.
A suitable environment in Nix can be acquired using
```
nix-shell -p gnat SDL2 pkg-config
```

# Build

Use
```
gnatmake cg.adb -cargs `pkg-config --cflags sdl2` -largs `pkg-config --libs sdl2`
```
to build the executable `cg`.

