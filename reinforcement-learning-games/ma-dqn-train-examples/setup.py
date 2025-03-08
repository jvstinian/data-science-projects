#!/usr/bin/env python

from setuptools import setup, find_packages

setup(
        name='ma-dqn-train-examples',
        version='1.0',
        # Modules to import from other scripts:
        # packages=find_packages(include=["dqn"]),
        packages=find_packages(),
        # Executables
        scripts=["train_dqn.py", "eval_dqn.py"],
)
