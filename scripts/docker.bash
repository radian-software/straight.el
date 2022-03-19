#!/usr/bin/env bash

set -e
set -o pipefail

tag="${1:-latest}"

args=(bash)
if [[ -n "$2" ]]; then
    args=("${args[@]}" -c "$2")
fi

docker() {
    if [[ "$OSTYPE" != darwin* ]] && [[ "$EUID" != 0 ]]; then
        command sudo docker "$@"
    else
        command docker "$@"
    fi
}

docker build . -t "straight.el:$tag" \
       --build-arg "UID=$UID"        \
       --build-arg "VERSION=$tag"

it=()

if [[ -t 0 ]]; then
    it+=(-it)
fi

docker run "${it[@]}" --rm -v "$PWD:/home/docker/src" \
       "straight.el:$tag" "${args[@]}"
