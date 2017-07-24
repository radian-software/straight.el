#!/usr/bin/env bash

fail=
for file in *.el; do
    output="$(egrep '.{80}' < $file | egrep -v '\[[0-9]+\]: ')"
    if [[ -n "$output" ]]; then
        echo "warning: found lines that are too long in $file:"
        echo "$output"
        fail="yes"
    fi
done
if [[ -n "$fail" ]]; then
    echo -n "hint: if your lines are URLs, format them correctly "
    echo    "and they will be ignored"
    exit 1
fi
