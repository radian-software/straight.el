#!/usr/bin/env bash

set -e
set -o pipefail

packages="

# needed to run build system
make

# needed for 'make help'
bsdmainutils

# for cloning repositories
git

# needed for 'make toc'
npm

# just in case we want root
sudo

# for testing ability of straight.el to build and install info
texinfo
install-info

"

export DEBIAN_FRONTEND=noninteractive
apt-get update
apt-get install --no-install-recommends -y $(grep -v "^#" <<< "$packages")
rm -rf /var/lib/apt/lists/*

npm install -g markdown-toc

rm "$0"
