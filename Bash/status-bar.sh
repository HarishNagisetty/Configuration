#! /usr/bin/env bash

# status-bar.sh
#  Generate a status bar for sway/i3

# "21 days"
uptime_formatted=$(uptime | cut -d ',' -f1  | cut -d ' ' -f4,5)

# "Mon Aug 9 12:00:31"
date_formatted=$(date "+%a %b %-d %H:%M:%S")

# "5.13.6"
linux_version=$(uname -r | cut -d '-' -f1)

# "charging 74%"
# "discharging 74%"
battery_status=$(upower --show-info $(upower --enumerate | grep BAT) |\
                     egrep "state|percentage" | awk '{print $2}' |\
                     tr '\n' ' ' | sed -e 's/[[:space:]]*$//')

# "32% on"
# "32% off"
volume_status=$(amixer -M get Master | grep 'Right:' |\
                    awk -F'[][]' '{ print $2 " " $4 }')

# "4.6G + 0.8G"
memory_formatted=$(free -m | awk '/^Mem/ {printf("%.1fG +", $3/1024)};
/^Swap/ {printf(" %.1fG", $3/1024)};')

# "0.76"
load_status=$(uptime | awk '{print $10}' | cut -d "," -f 1)

echo "load $load_status, $memory_formatted, up $uptime_formatted, \
vol $volume_status, $battery_status, $date_formatted"

