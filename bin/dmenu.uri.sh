#!/bin/sh
sort -ru ~/.surf/history.txt | dmenu -l 10 -b -i | xprop -id "$( cat ~/.surf/id )" -f SURF_URI 8s -set _SURF_URI
