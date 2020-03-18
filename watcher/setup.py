from setuptools import setup

# https://python-packaging.readthedocs.io/en/latest/minimal.html
setup(
    author="Radon Rosborough",
    author_email="radon.neon@gmail.com",
    description="Internal utilities for straight.el.",
    license="MIT",
    install_requires=["psutil==5.6.6"],
    name="straight-watcher",
    url="https://github.com/raxod502/straight.el",
    version="1.0-dev",
)
