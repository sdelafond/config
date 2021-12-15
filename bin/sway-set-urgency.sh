#!/bin/sh 

# FIXME: only works for kitty right now; alacritty spawns its
# bell:command directly from PID 1, making it difficult to identify
# the terminal the alert came in

exec >> /tmp/sway-set-urgency.log 2>&1

set -x

date -Iseconds
# echo $@
# echo $PPID
# echo $$
# pstree -p
swaymsg "[pid=$PPID]" urgent enable

exit 0
