#!/bin/sh
set -e
export PATH="/run/current-system/sw/bin/:$PATH"
[ -f /sys/class/power_supply/BAT0/capacity ] && cat /sys/class/power_supply/BAT0/capacity > /tmp/bat
[ -f /sys/class/power_supply/BAT0/status ] && cat /sys/class/power_supply/BAT0/status  >> /tmp/bat
exit 0
