#!/bin/sh

export PATH="/run/current-system/sw/bin/:$PATH"
cap=$(sed 1q /tmp/bat)
batStatus=$(tail -n1 /tmp/bat)
if [ $cap -lt 20 ] && [ $batStatus == 'Discharging' ]; then
    notify-send 'Battery Level' "$( tr '\n' ' ' < /tmp/bat)"
fi
