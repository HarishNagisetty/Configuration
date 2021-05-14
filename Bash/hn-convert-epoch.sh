#! /usr/bin/env bash

# hn-convert-epoch.sh
#  Convert the trailing number of all arguments from an epoch
#  timestamp to a human readable timestamp.


for i in "$@"; do
    stamp=$(echo "$i" | grep -oP '\d+$')
    printf "$i: "
    date -d @$stamp
done
