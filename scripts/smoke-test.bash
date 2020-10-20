#!/usr/bin/env bash

set -e
set -o pipefail

mkdir -p "$HOME/.emacs.d/straight/repos/"
ln -sf "$PWD" "$HOME/.emacs.d/straight/repos/straight.el"

# We need to test with a package that supports Emacs 24.5 here.
emacs --batch -l "$HOME/.emacs.d/straight/repos/straight.el/bootstrap.el" \
      --eval "(straight-use-package 'use-package)"                        \
      --eval "(use-package company :straight t)"
