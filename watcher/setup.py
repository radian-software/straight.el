from setuptools import setup

# https://python-packaging.readthedocs.io/en/latest/minimal.html
setup(
    author="Radian LLC",
    author_email="contact+straight@radian.codes",
    description="Internal utilities for straight.el.",
    license="MIT",
    install_requires=["psutil>=5.6.6,<6.0.0", "packaging>=24.0,<25.0"],
    name="straight-watcher",
    url="https://github.com/radian-software/straight.el",
    version="1.0-dev",
    # Must be specified explicitly from setuptools>=61
    # https://github.com/pypa/setuptools/issues/4013
    py_modules=["straight_watch", "straight_watch_callback"],
)
