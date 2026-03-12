# 1.4.0 

Updates to the demo environment have been made to address additional constraints introduced
in newer versions of `gymnasium`.
With these changes the environment now satisfies the requirements of the `check_env` function.
In particular, episodes can be reproduced by setting the seed when resetting the environment.

Changes to the demo environment include
* making the feedback size consistent across the game and gymnasium environment,
* using a single numpy generator for random number generation throughout the application,
  which allows for setting the seed and supports reproducibility of episodes,
* making the termination value in the step method a boolean rather than an integer,
* fixing the bounds in the observation space,
* adding the ability to reset the orientation of the agent for reproducibility, and
* adding a test that calls gymnasium's `check_env` on the custom PRLP demo gymnasium environment.

# 1.3.0 

Switching from gym to gymnasium.

# 1.2.0 

Adjusting the reset method in the gym implementation to return both an initial observation and an info dictionary.
This is to align with the gym specification.

# 1.1.0 

Adding a gym wrapper for the demo environment.

# 1.0.0 

This is the original version of the demo environment.  
While some changes might have been made by the 
contributors to this repo as outlined in the README,
this code should generally be in agreement with the code
found in the repo associated with the book _Python Reinforcement Learning Projects_.

Changes made by this repo's contributors to the original code include,
but are not limited to, setting up this environment as a python package and
adding a nix flake for reproducibility, building, a development environment, and
inclusion into downstream projects.

