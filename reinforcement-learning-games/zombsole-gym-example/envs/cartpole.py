import numpy as np
import gym
from gym.core import Env


class Wrapper(Env):
    """Wraps the environment to allow a modular transformation.

    This class is the base class for all wrappers. The subclass could override
    some methods to change the behavior of the original environment without touching the
    original code.

    .. note::

        Don't forget to call ``super().__init__(env)`` if the subclass overrides :meth:`__init__`.

    """
    def __init__(self, env):
        self.env = env
        self.action_space = self.env.action_space
        self.observation_space = self.env.observation_space
        self.reward_range = self.env.reward_range
        self.metadata = self.env.metadata

    def __getattr__(self, name):
        if name.startswith('_'):
            raise AttributeError("attempted to get missing private attribute '{}'".format(name))
        return getattr(self.env, name)

    @property
    def spec(self):
        return self.env.spec

    @classmethod
    def class_name(cls):
        return cls.__name__

    def step(self, action):
        return self.env.step(action)

    def reset(self, **kwargs):
        return self.env.reset(**kwargs)

    def render(self, **kwargs):
        return self.env.render(**kwargs)

    def close(self):
        return self.env.close()

    def seed(self, seed=None):
        return self.env.seed(seed)

    # def compute_reward(self, achieved_goal, desired_goal, info):
    #     return self.env.compute_reward(achieved_goal, desired_goal, info)

    def __str__(self):
        return '<{}{}>'.format(type(self).__name__, self.env)

    def __repr__(self):
        return str(self)

    @property
    def unwrapped(self):
        return self.env.unwrapped


class ObservationWrapper(Wrapper):
    def reset(self, **kwargs):
        observation, info = self.env.reset(**kwargs)
        return self.observation(observation), info

    def step(self, action):
        observation, reward, done, truncated, info = self.env.step(action)
        return self.observation(observation), reward, done, truncated, info

    def observation(self, observation):
        raise NotImplementedError


class CartPoleObservationWrapper(ObservationWrapper):
    def __init__(self, render_mode="human"):
        game = gym.make('CartPole-v1', render_mode=render_mode)
        super().__init__(game)

    def get_frame_size(self):
        return (4,1)

    def observation(self, observation):
        # print("observation: ", observation)
        newshape = (1,) + self.get_frame_size()
        return np.asarray(
            observation, dtype=np.float32
        ).reshape(
            newshape
        )

    # Overriding render
    def render(self, **kwargs):
        return self.env.render(**kwargs)

