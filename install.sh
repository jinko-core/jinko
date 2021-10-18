#!/bin/sh

bin_dir=$HOME/.jinko/bin
lib_dir=$HOME/.jinko/libs

echo "Setting up standard library..."

mkdir -p $bin_dir
mkdir -p $lib_dir

set -e

# If the stdlib folder does not exist, we want to error out immediately
cp -r stdlib $lib_dir

echo "Installing jinko binary..."

# FIXME: Later on, this should check if jinko is already installed or if this installation
# truly is a version upgrade.
# It would also be nice if this script was written directly in jinko... ;)

# If a Cargo.toml file is present in the current folder, then we are installing from
# source. Simply run cargo install and setup the jinko environment
if [ -f Cargo.toml ]; then
    echo "Compiling jinko from source... "
    # Force install to force update any existing jinko installation
    cargo build --release
    echo -n "jinko --version: "
    target/release/jinko -v
    cp target/release/jinko $bin_dir
else
    echo -n "Installing jinko from a release... "
    jinko --version
    # We assume that a binary named jinko exists in the current directory. Otherwise, this
    # is a very erroneous state.
    cp jinko $bin_dir
fi

echo -e "\e[95mjinko\e[0m is installed! Remember to add '$HOME/.jinko/bin/' to your path"
