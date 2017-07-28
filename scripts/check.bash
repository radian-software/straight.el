#!/usr/bin/env bash

set -e
set -o pipefail
cd "$(dirname "$0")"/..

checkers=("$@")

fail=
for checker in "${checkers[@]}"; do
    if ! scripts/check3.bash "$checker"; then
        fail=yes
    fi
done

if [[ -n $fail ]]; then
    exit 1
fi
