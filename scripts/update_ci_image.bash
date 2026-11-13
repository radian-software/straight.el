#!/usr/bin/env bash

set -euo pipefail

if (( "$#" != 1 )); then
    echo >&2 "usage: $0 EMACS_VERSION"
    exit 1
fi

tag="$1"

docker pull "silex/emacs:${tag}"
docker tag "silex/emacs:${tag}" "ghcr.io/radian-software/emacs-base:${tag}"
docker push "ghcr.io/radian-software/emacs-base:${tag}"

make docker                                          \
     DOCKER_BASE=ghcr.io/radian-software/emacs-base  \
     VERSION="${tag}"                                \
     CMD="make lint test"

docker tag "straight.el:${tag}" "ghcr.io/radian-software/straight-ci:${tag}"
docker push "ghcr.io/radian-software/straight-ci:${tag}"
