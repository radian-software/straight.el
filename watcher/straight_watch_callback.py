#!/usr/bin/env python3

import os
import pathlib
import sys

WATCHEXEC_VAR_COMMON = "WATCHEXEC_COMMON_PATH"

WATCHEXEC_VARS = [
    "WATCHEXEC_CREATED_PATH",
    "WATCHEXEC_REMOVED_PATH",
    "WATCHEXEC_RENAMED_PATH",
    "WATCHEXEC_WRITTEN_PATH",
    "WATCHEXEC_META_CHANGED_PATH",
]

def die(message):
    print(message, file=sys.stderr)
    sys.exit(1)

def usage():
    return (
        "usage: python -m straight_watch_callback <repos-dir> <modified-dir>")

def path_contains(parent, child):
    parent = pathlib.Path(parent).resolve()
    child = pathlib.Path(child).resolve()
    return parent in child.parents

def path_strip(parent, child):
    parent = pathlib.Path(parent).parts
    child = pathlib.Path(child).parts
    return child[len(parent)]

def main(args):
    if len(args) != 2:
        die(usage())
    repos_dir, modified_dir = args
    paths = []
    for var in WATCHEXEC_VARS:
        if var in os.environ:
            paths.extend(os.environ[var].split(os.pathsep))
    if not paths:
        die("straight_watch_callback.py: watchexec gave no modified files")
    if WATCHEXEC_VAR_COMMON in os.environ:
        common = os.environ[WATCHEXEC_VAR_COMMON]
        paths = [os.path.abspath(common + path) for path in paths]
    paths = sorted(set(paths))
    repos = set()
    for path in paths:
        print("detect modification: {}".format(path), file=sys.stderr)
        if path_contains(repos_dir, path):
            repo = path_strip(repos_dir, path)
            repos.add(repo)
    if repos:
        os.makedirs(modified_dir, exist_ok=True)
        repos = sorted(repos)
        for repo in repos:
            print("--> mark for rebuild: {}".format(repo), file=sys.stderr)
            with open(os.path.join(modified_dir, repo), "w"):
                pass

if __name__ == "__main__":
    main(sys.argv[1:])
