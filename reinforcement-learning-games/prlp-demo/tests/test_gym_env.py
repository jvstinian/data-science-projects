# tests/test_gym_env.py
import pytest
from prlp_demo.gym_env import DemoGymEnv
import gym


def test_gym_env_registry():
    assert "prlp_demo/Demo-v1" in gym.envs.registry.keys()

