#!/usr/bin/env bash

set -e
set -o pipefail

tag="${1:-30}"

args=(bash)
if [[ -n "$2" ]]; then
    args=("${args[@]}" -c "$2")
fi

if [[ -z "${USE_PODMAN:-}" ]]; then
    docker=(docker)
    if [[ "$OSTYPE" != darwin* ]] && [[ "$EUID" != 0 ]] \
           && [[ -z "${NO_SUDO_DOCKER:-}" ]]; then
        docker=(sudo -E "${docker[@]}")
    fi
else
    docker=(podman)
fi

image="straight.el:$tag"
if [[ -z "${NO_BUILD_DOCKER:-}" ]]; then
    "${docker[@]}" build . -t "${image}" \
                   --build-arg "VERSION=$tag"    \
                   --build-arg "BASE=${DOCKER_BASE:-silex/emacs}"
else
    image="${DOCKER_BASE:-silex/emacs}:$tag"
fi

it=()

if [[ -t 0 ]]; then
    it+=(-it)
fi

exec "${docker[@]}" run "${it[@]}" --rm -v "$PWD:/src" \
     --entrypoint=/src/scripts/docker-pid1.bash \
     "${image}" "${args[@]}"
