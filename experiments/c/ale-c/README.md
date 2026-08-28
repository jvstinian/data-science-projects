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

It should be noted that we have two reset calls, `atari_reset` and
`atari_reset_omit_seed`, depending on whether a seed will be
provided when resetting.  
`atari_reset` has the functionality described above.
`atari_reset_omit_seed` resets the game without reloading the ROM.
