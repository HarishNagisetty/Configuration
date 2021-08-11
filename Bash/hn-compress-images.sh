#! /usr/bin/env bash

# hn-compress-image.sh
#  Compress images with JPG

print_usage () {
    printf "Usage: $(basename $1) output_directory input_files...\n"
    exit 1
}

if [[ $# -lt 2 ]]; then print_usage $0; fi

OUT_DIR=$1
if [[ ! (-d ${OUT_DIR}) ]]; then
    printf "$OUT_DIR is not a directory.\n"
    print_usage $0
fi
shift

for f in "$@" ; do
    if [[ -f $f ]]; then
        mkdir -p "$OUT_DIR/$(dirname $f)"
        convert ./"$f" -quality 80% "$OUT_DIR/${f%.*}.jpg" || {
            printf "Error processing file: \"$f\"\n"
        }
    fi
done
