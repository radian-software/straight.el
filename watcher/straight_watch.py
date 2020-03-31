#!/usr/bin/env python3

import os
import psutil
import shlex
import signal
import subprocess
import sys

SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))
CALLBACK_SCRIPT = os.path.join(SCRIPT_DIR, "straight_watch_callback.py")


def kill_previous_watcher(pid_file):
    try:
        with open(pid_file, "r") as f:
            lines = f.read().splitlines()
    except OSError:
        return
    if len(lines) != 2:
        return
    try:
        pid = int(lines[0])
        create_time = float(lines[1])
    except ValueError:
        return
    try:
        prev_process = psutil.Process(pid)
    except psutil.NoSuchProcess:
        return
    real_create_time = prev_process.create_time()
    if abs(create_time - real_create_time) > 0.1:
        return
    os.kill(pid, signal.SIGUSR1)
    os.remove(pid_file)


def write_process_data(pid_file):
    pid = os.getpid()
    create_time = psutil.Process(pid).create_time()
    os.makedirs(os.path.dirname(pid_file), exist_ok=True)
    with open(pid_file, "w") as f:
        print(pid, file=f)
        print(create_time, file=f)


def start_watch(repos_dir, modified_dir):
    callback_cmd = [CALLBACK_SCRIPT, repos_dir, modified_dir]
    callback_sh = " ".join(map(shlex.quote, callback_cmd))
    cmd = ["watchexec", "--no-vcs-ignore", "-p", "-d", "100", callback_sh]
    cmd_sh = " ".join(map(shlex.quote, cmd))
    print("$ " + cmd_sh, file=sys.stderr)
    subprocess.run(cmd, cwd=repos_dir, check=True)


def handle_interrupt(signum, frame):
    sys.exit(0)


def die(message):
    print(message, file=sys.stderr)
    sys.exit(1)


def usage():
    return """\
usage: python -m straight_watch start <pid-file> <repos-dir> <modified-dir>
       python -m straight_watch stop <pid-file>\
"""


def main(args):
    if not args:
        die(usage())
    try:
        if args[0] == "start":
            [pid_file, repos_dir, modified_dir] = args[1:]
        elif args[0] == "stop":
            [pid_file] = args[1:]
        else:
            die(usage())
    except ValueError:
        die(usage())
    kill_previous_watcher(pid_file)
    if args[0] == "stop":
        sys.exit(0)
    # We will change the working directory for the callback script, so
    # make the paths absolute.
    repos_dir = os.path.abspath(repos_dir)
    modified_dir = os.path.abspath(modified_dir)
    write_process_data(pid_file)
    start_watch(repos_dir, modified_dir)
    # We should never get here. Normal exit is getting signalled with
    # SIGUSR1.
    sys.exit(1)


if __name__ == "__main__":
    signal.signal(signal.SIGUSR1, handle_interrupt)
    main(sys.argv[1:])
