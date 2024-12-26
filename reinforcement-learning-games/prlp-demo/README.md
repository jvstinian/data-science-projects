# Adapted Python Reinforcement Learning Projects Demo Environment

This project is the pellet-eating demo environment from the code for the book _Python Reinforcement Learning Projects_.
It is intended to be used for reinforcement learning, and features an agent
that eats pellets, and is rewarded or penalized based on whether the pellet is nutritious or poisonous, which
is indicated by the color of the pellet.

We make some minor modifications to the code, mainly 
* adapting the code to be used as a python package, 
* any adjustments needed to support newer versions of python, 
* a nix flake to support development, builds, and inclusion into downstream projects, and
* minor adjustments for our own experimentation.

Code that has been added (by the contributors to this repo) as part of this project includes 
* a Gym environment wrapper for the demo game, and
* some basic unit tests.

