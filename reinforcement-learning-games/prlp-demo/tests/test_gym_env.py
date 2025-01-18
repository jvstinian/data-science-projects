# tests/test_gym_env.py
import pytest
from prlp_demo.gym_env import DemoGymEnv
import gymnasium as gym


def test_gym_env_registry():
    assert "prlp/Demo-v0" in gym.envs.registry.keys()

def test_gym_env_frame_size():
    env = DemoGymEnv(
        enable_rendering=False
    )
    observation, _ = env.reset()
    feedback_size = env.get_frame_size()
    expected_observation_shape = (1,) + feedback_size
    assert observation.shape == expected_observation_shape

def test_gym_env_step():
    env = DemoGymEnv(
        enable_rendering=False
    )
    observation, _ = env.reset()
    step_count = 0
    done = False
    while (not done) and (step_count < 100):
        _, _, done, _, _ = env.step(env.action_space.sample())
        step_count += 1
    
    env.close()

    assert done or (step_count == 100)

