#! /usr/bin/env bash

# hn-get-module.sh
#  Get single Verilog module definition from a large netlist

if [[ $# -lt 2 ]]; then
    printf "Usage: $(basename $0) netlist_file module_name [output_file]\n"
    exit 1
fi

START=$(grep -n -m 1 "module[[:space:]]\+$2\>" $1 | sed 's/\([0-9]*\).*/\1/')

if [[ -n "$START" ]]; then

    LEN=$(tail -n +$START $1 | grep -n -m 1 "endmodule" | sed 's/\([0-9]*\).*/\1/')

    if [[ -n "$LEN" ]]; then
        printf "Saving $LEN lines starting from #$START.\n"
        if [[ -n $3 ]]; then
            tail -n +$START $1 | head -n $LEN > $3
        else
            tail -n +$START $1 | head -n $LEN > $2.sv.$(date +%s)
        fi
    else
        printf "Could not find \"endmodule\" after line $START\n"
        exit 1
    fi
else
    printf "Could not find \"module $2\"\n"
    exit 1
fi
