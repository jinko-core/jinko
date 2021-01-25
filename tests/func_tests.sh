#!/bin/sh

ft --version > /dev/null
ft_exists=$?

if [ $ft_exists -ne 0 ]
then
    printf '\n`ft` not found. Install it using the following command\n\n'
    printf '\tcargo install --git https://github.com/cohenarthur/ft\n\n'

    exit 1
fi

files=$(find tests -name "*.yml")

printf "Test files:\n$files\n\n"

cargo build

ft -f $files
