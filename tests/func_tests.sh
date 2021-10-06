#!/bin/sh

ft --version > /dev/null
ft_exists=$?

if [ $ft_exists -ne 0 ]
then
    printf '\n`ft` not found. Install it using the following command\n\n'
    printf '\tcargo install --git https://github.com/cohenarthur/ft\n\n'

    exit 1
fi

cargo build

# Print a newline for cleaner formatting
echo ""

if [ $# -ne 0 ]
then
    printf "Test files:\n$@\n\n"

    for file in $@; do
        ft -f $file
    done
else
    files=$(find tests -name "*.yml")

    printf "Test files:\n$files\n\n"

    ft -f $files
fi
