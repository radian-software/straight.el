#!/usr/bin/env bash

set -e
set -o pipefail
cd "$(dirname "$0")"/..

checker=$1
file=$2

if [[ -n $file ]]; then
    if [[ ! -f $file ]]; then
        echo "Nonexistent file: $file" 1>&2
        exit 1
    fi

    case $checker in
        compile)
            # If byte-compilation fails, at least we won't have a
            # stale *.elc.
            rm -f "${file}c"
            emacs -Q --batch --eval                      \
                  "(progn                                \
                     (setq byte-compile-error-on-warn t) \
                     (push default-directory load-path)  \
                     (batch-byte-compile))"              \
                  "$file"                                \
                | grep -v '^Warning (emacs):$'
            ;;
        checkdoc)
            emacs --batch --eval                          \
                  "(progn                                 \
                     (setq sentence-end-double-space nil) \
                     (checkdoc-file \"$file\"))"          \
                | grep -v "^Warning (emacs):$"
            ;;
        shellcheck)
            shellcheck "$file"
            ;;
        longlines)
            sed '/<!-- toc -->/,/<!-- tocstop -->/d' "$file" \
                | sed '/longlines-start/,/longlines-stop/d'  \
                | grep -E '.{80}'                            \
                | grep -E -v '\[.+\]: (#|http)'              \
                | sed 's/^/long line: /'
            ;;
        *)
            echo "Invalid checker: $checker" 1>&2
            exit 1
            ;;
    esac
else
    case "$checker" in
        toc)
            markdown-toc -i README.md
            ;;
        *)
            echo "Invalid checker: $checker" 1>&2
            exit 1
            ;;
    esac
fi
