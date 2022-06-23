from setuptools import setup

# https://python-packaging.readthedocs.io/en/latest/minimal.html
setup(
    author="Radian LLC",
    author_email="contact+straight@radian.codes",
    description="Internal utilities for straight.el.",
    license="MIT",
    install_requires=["psutil==5.6.6"],
    name="straight-watcher",
    url="https://github.com/radian-software/straight.el",
    version="1.0-dev",
)
