#!/usr/bin/env bash

set -e
set -o pipefail
cd "$(dirname "$0")"/..

checker=$1

case "$checker" in
    compile)
        # Dependency order is important here.
        files=(straight.el bootstrap.el install.el)
        ;;
    checkdoc)
        files=(straight.el)
        ;;
    shellcheck)
        files=(scripts/*.bash)
        ;;
    longlines)
        files=(*.el *.md .gitignore .travis.yml scripts/*.bash)
        ;;
    toc)
        files=()
        ;;
    *)
        echo "Invalid checker: $checker" 1>&2
        exit 1
        ;;
esac

fail=
if (( ${#files[@]} > 0 )); then
    for file in "${files[@]}"; do
        if ! scripts/check2.bash "$checker" "$file"; then
            fail=yes
        fi
    done
else
    if ! scripts/check2.bash "$checker"; then
        fail=yes
    fi
fi

if [[ -n $fail ]]; then
    case "$checker" in
        # Reserved for future expansion, in case we want to print a
        # hint message when there are errors.
    esac
    exit 1
fi
