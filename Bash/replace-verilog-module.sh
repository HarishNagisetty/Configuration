#! /usr/bin/env bash

# replace-verilog-module.sh
#  Replace Verilog module definition in a large netlist
#
# Author: Harish Nagisetty

if [[ $# -lt 2 ]]; then
    printf "Usage: $(basename $0) netlist_file module_file [output_file]\n"
    exit 1
fi

MODULE=$(grep -m 1 "^[[:space:]]*module[[:space:]]\+[[:alpha:]]" $2 | 
    sed 's/\s*module\s\+\(\w\+\).*/\1/')
if [[ -z "$MODULE" ]]; then
    printf "Could not find module name in $2\n"
    exit 1
fi

START=$(grep -n -m 1 "module[[:space:]]\+$MODULE\>" $1 | sed 's/\([0-9]*\).*/\1/')

if [[ -n "$START" ]]; then

    LEN=$(tail -n +$START $1 | grep -n -m 1 "endmodule" | sed 's/\([0-9]*\).*/\1/')

    if [[ -n "$LEN" ]]; then
        printf "Replacing $LEN lines starting from line no. $START.\n"
        TMP_FILE="/tmp/"$(basename $1).$(date +%s)
        DST_FILE=$1
        if [[ -n $3 ]]; then
            DST_FILE=$3
        fi
        head -n $(($START - 1)) $1 > $TMP_FILE
        cat $2 >> $TMP_FILE
        tail -n +$(($START + $LEN + 1)) $1 >> $TMP_FILE
        mv $TMP_FILE $DST_FILE
        if [[ $? -ne 0 ]]; then
            printf "Unable to move $TMP_FILE to $DST_FILE\n"
        fi
    else
        printf "Could not find \"endmodule\" after line $START in $1\n"
        exit 1
    fi
else
    printf "Could not find \"module $2\"\n"
    exit 1
fi
