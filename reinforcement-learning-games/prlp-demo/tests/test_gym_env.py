# tests/test_gym_env.py
from contextlib import contextmanager
import pytest
from prlp_demo.gym_env import DemoGymEnv
import gymnasium as gym
from gymnasium.utils.env_checker import check_env


def test_gym_env_registry():
    assert "prlp/Demo-v0" in gym.envs.registry.keys()

def test_gym_env_frame_size():
    env = DemoGymEnv(
        render_mode=None
    )
    observation, _ = env.reset()
    feedback_size = env.get_frame_size()
    expected_observation_shape = feedback_size  # TODO: Remove unnecessary variable
    assert observation.shape == expected_observation_shape

def test_gym_env_step():
    env = DemoGymEnv(
        render_mode=None
    )
    observation, _ = env.reset()
    step_count = 0
    done = False
    while (not done) and (step_count < 100):
        _, _, done, _, _ = env.step(env.action_space.sample())
        step_count += 1
    
    env.close()

    assert done or (step_count == 100)

@contextmanager
def not_raises(exception):
    try:
        yield
    except exception as ex:
        raise pytest.fail("DID RAISE {0}".format(ex))

def test_gym_make_env_v0():
    env = gym.make("prlp/Demo-v0", render_mode=None)
    with not_raises(Exception):
        check_env(env.unwrapped, skip_render_check=True)

