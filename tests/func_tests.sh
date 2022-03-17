#!/bin/sh

ftf --version > /dev/null
ftf_exists=$?

if [ $ftf_exists -ne 0 ]
then
    printf '\n`ftf` not found. Install it using the following command\n\n'
    printf '\tcargo install ftf\n\n'

    exit 1
fi

cargo build

# Print a newline for cleaner formatting
echo ""

if [ $# -ne 0 ]
then
    printf "Test files:\n$@\n\n"

    for file in $@; do
        ftf -f $file
    done
else
    files=$(find tests -name "*.yml")

    printf "Test files:\n$files\n\n"

    ftf -f $files
fi
