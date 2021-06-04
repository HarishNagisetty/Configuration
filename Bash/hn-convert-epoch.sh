#! /usr/bin/env bash

# hn-convert-epoch.sh
#  Convert the trailing number of all arguments from an epoch
#  timestamp to a human readable timestamp.

if [[ $# -lt 1 ]]; then
    printf "Usage: $(basename $0) args\n"
    exit 1
fi

for i in "$@"; do
    stamp=$(echo "$i" | grep -oP '\d{9,10}$')
    if [[ -n "$stamp" ]]; then
        printf "$i: "
        date -d @$stamp
    fi
done
