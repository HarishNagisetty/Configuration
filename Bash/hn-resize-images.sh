#! /usr/bin/env bash

# hn-resize-image.sh
#  Resize images with JPG

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
        if [[ ! -f "$(basename -- "$f")" ]]; then
            echo "Creating directory."
            mkdir -p "$OUT_DIR/$(dirname -- "$f")"
        fi
        convert ./"$f" -resize 50% "$OUT_DIR/${f%.*}.jpg" || {
            printf "Error processing file: \"$f\"\n"
        }
    fi
done
