#!/usr/bin/env python3

import os
import sys

print("straight.el nohup initialized successfully")

# So why do we have a Python script that is just a wrapper around
# execing nohup? Well, when we invoke nohup directly, we get no
# feedback about whether the command it's running is working, or
# whether it just failed immediately, for example due to the Python
# virtualenv being broken. However, if we execute this wrapper script
# using the Python in the virtualenv, we'll find that out right away,
# since we are using the same execution path that will eventually be
# used for the actual command run by nohup.
os.execv("/bin/nohup", sys.argv)
