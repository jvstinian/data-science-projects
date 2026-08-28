# ALE C Bindings

This project produces C bindings for some functionality in the
[Arcade Learning Environment](https://github.com/Farama-Foundation/Arcade-Learning-Environment/tree/v0.11.2) (ALE)
library.

See the ACKNOWLEDGMENTS.md file for license information for the ALE project.

# Differences Compared To Python Implementation

There is one adjustment we made when translating the python implementation that
might be worth noting.  This concerns the setting of the seed for
reproducibility, and primarily impacts the game loading in the constructor
(our `atari_make` method) and the reset methods.

The key observation is that the `ale_seed` must be set prior to the ROM loading,
otherwise it is ignored.  This means that when passing a valid seed to the
reset method, the ROM must be reloaded.  In the python implementation,
the method for reloading the ROM also constructs the ROM file path and
performs the MD5 hash validation.

In our implementation, it is preferable if any errors are handled in
the `atari_make` call rather than in the `atari_reset` call, so we decided
to refactor the ROM load function.  We instead construct the ROM file path
and validate the MD5 hash of the file in the `atari_make` method,
returning an error if an issue was encountered.  The ROM file path
is preserved and used in the `atari_reset` method to load the ROM
without performing the validation again after the ale seed has been set.

Also, in the constructor in the python version, a default seed is set and the
ROM is loaded.  The loading of the ROM appears to be performed so that
additional member variables such as the action and observation space can
be defined.  As the additional variables are not used in the same way
in these bindings, we instead simply set a default ale seed without
loading the ROM.  The ROM is only loaded in one of the reset calls.

It should be noted that we have two reset calls, `atari_reset` and
`atari_reset_omit_seed`, depending on whether a seed will be
provided when resetting.  
`atari_reset` has the functionality described above.
`atari_reset_omit_seed` resets the game without reloading the ROM.
