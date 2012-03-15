#!/bin/sh
#########################################################################
# Bring up or down sound volume with amixer, with on screen display
# the total volume range is 58.5dB, so we change 4.5dB each run, it takes
# 13 run to go from 0 to 58.5dB
#
# Usage: audio-volume.sh [up|down]
#
# Author: Huahai Yang
# Last upated: Oct. 16, 2007
########################################################################

if amixer | grep -E '^Simple.*Master' ; then
  MIXER=Master
else
  MIXER=Front
fi
STEP=5

case $1 in
  up|down)
    amixer -c 0 set $MIXER unmute > /dev/null
    [ $1 = up ] && UP="+" || UP="-"
    amixer -c 0 set $MIXER ${STEP}dB$UP > /dev/null
    STRING=$(amixer -c 0 get $MIXER | perl -i -ne 'if ( /\[(\d+%)\]/ ) { print "$1\n" ; exit 0 ; }') ;;
  mute)
    amixer -c 0 set $MIXER mute > /dev/null
    STRING="mute" ;;
esac

killall osd_cat 2> /dev/null

#echo "Sound Volume: $STRING" | osd_cat -d 1 -p bottom -A left -c green&
awesome-notify.sh "$STRING" "sound"

