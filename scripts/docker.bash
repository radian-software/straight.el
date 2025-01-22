#!/usr/bin/env bash

set -e
set -o pipefail

tag="${1:-29}"

args=(bash)
if [[ -n "$2" ]]; then
    args=("${args[@]}" -c "$2")
fi

docker() {
    if [[ "$OSTYPE" != darwin* ]] && [[ "$EUID" != 0 ]] \
    && [[ -z "${NO_SUDO_DOCKER:-}" ]]; then
        command sudo docker "$@"
    else
        command docker "$@"
    fi
}

docker build . -t "straight.el:$tag" \
       --build-arg "UID=$UID"        \
       --build-arg "VERSION=$tag"    \
       --build-arg "BASE=${DOCKER_BASE:-silex/emacs}"

it=()

if [[ -t 0 ]]; then
    it+=(-it)
fi

docker run "${it[@]}" --rm -v "$PWD:/home/docker/src" \
       "straight.el:$tag" "${args[@]}"
