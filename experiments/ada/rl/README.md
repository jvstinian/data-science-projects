# Reinforcement Learning Environments And Algorithms

We adapt some environments from the Python Gymnasium package and some board games
for use with Reinforcement Learning techniques.

## Environments

The following environments are included in this package:

* Frozen lake
* Cliffwalking
* Blackjack
* Cartpole
* Car Rental
* Line walk
* Tic Tac Toe
* Ataxx
* Hex

Frozen Lake, Cliffwalking, Blackjack, and Cartpole are 
simple rewrites of the environments in the Python Gymnasium package.
For some of the environments some functionality removed or added.


## Algorithms

We provide some of the classical algorithms from Reinforcement Learning.
In particular, we provide Dynamic Programming, Monte Carlo, and Temporal Difference
techniques for use with the environments.

At this time we do not provide any techniques using Neural Networks.


# Build and Run

This package can be built with Alire using
```
alr build
```
This builds an executable `main` which simply runs the environment using random actions till termination.


# References

* [gymnasium cartpole](https://github.com/Farama-Foundation/Gymnasium/blob/3ba85ed7131d23dcf284a19f4a2a1db17ea73224/gymnasium/envs/classic_control/cartpole.py)
* Sutton, R. S., & Barto, A. G. (1998). Reinforcement learning: An introduction (1st ed.). MIT Press.

