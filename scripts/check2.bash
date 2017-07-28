#!/usr/bin/env bash

set -e
set -o pipefail
cd "$(dirname "$0")"/..

checker=$1
file=$2

output=$(scripts/check1.bash "$checker" "$file" 2>&1 || true)

if [[ -n $output ]]; then
    if [[ -n $file ]]; then
        echo -n "==> Running $checker checker on $file "
        echo        "produced the following errors:"
    fi
    echo "$output"
    exit 1
fi
