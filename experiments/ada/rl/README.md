# Cartpole In Ada

This is a simple rewrite of the Cartpole environment in the python `gymnasium` package.
The purpose is simply to see what an implementation might look like in Ada.

# Build and Run

To build this example, `gnat` is required.  To build, run
```
gnatmake main.adb
```
This builds an executable `main` which simply runs the environment using random actions till termination.

# References

[gymnasium cartpole](https://github.com/Farama-Foundation/Gymnasium/blob/3ba85ed7131d23dcf284a19f4a2a1db17ea73224/gymnasium/envs/classic_control/cartpole.py)
