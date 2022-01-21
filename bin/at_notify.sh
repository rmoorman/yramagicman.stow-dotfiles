#!/bin/sh

export PATH="/run/current-system/sw/bin/:$PATH"
notify-send 'Reminder' $(cat /tmp/remind)
