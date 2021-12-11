#! /usr/bin/env bash

DATA_DIR=$(mktemp -p /tmp -d chromium-data.XXXXXX.d)

chromium-browser --user-data-dir="$DATA_DIR" #--incognito

rm -rf "$DATA_DIR"
