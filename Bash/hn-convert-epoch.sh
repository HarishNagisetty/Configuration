#! /usr/bin/env bash

# hn-convert-epoch.sh
#  Convert the trailing number of all arguments from an epoch
#  timestamp to a human readable timestamp.

if [[ $# -lt 1 ]]; then
    printf "Usage: $(basename $0) args\n"
    exit 1
fi

STAMPS=""

# Put each timestamp on its own line.
for i in "$@"; do
    # This millennium is 9 to 10 digits.
    stamp=$(echo "$i" | grep -oP '\d{9,10}$')
    if [[ -n "$stamp" ]]; then
        STAMPS="$STAMPS\n$stamp"
    fi
done

SORTED=$(printf "$STAMPS" | sort | uniq)

IFS=$'\n'
for i in $SORTED; do
    # Print all arguments containing this timestamp.
    for j in "$@"; do
        arg=$(echo "$j" | grep -P "$i\$")
        if [[ -n "$arg" ]]; then
            printf "$arg: "
        fi
    done
    date -d @$i
done
