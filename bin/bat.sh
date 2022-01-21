#!/bin/sh

export PATH="/run/current-system/sw/bin/:$PATH"
cat /sys/class/power_supply/BAT0/capacity > /tmp/bat
cat /sys/class/power_supply/BAT0/status  >> /tmp/bat
