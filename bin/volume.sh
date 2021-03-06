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

AMIXER_OPTS_MUTE_=""
case $(hostname -s) in
  x1|mp)
    AMIXER_OPTS_VOLUME="-c 1" ;;
  *)
    AMIXER_OPTS_VOLUME="-c 0" ;;
esac

if amixer | grep -E '^Simple.*Master' ; then
  MIXER=Master
else
  MIXER=Front
fi
STEP=5

case $1 in
  up|down)
    amixer $AMIXER_OPTS_MUTE set $MIXER unmute > /dev/null
    [ $1 = up ] && UP="+" || UP="-"
    amixer $AMIXER_OPTS_VOLUME set $MIXER ${STEP}dB$UP > /dev/null
    STRING=$(amixer $AMIXER_OPTS_VOLUME get $MIXER | perl -ne 'if ( /\[(\d+%)\]/ ) { print "$1\n" ; exit 0 ; }') ;;
  mute)
    amixer $AMIXER_OPTS_MUTE set $MIXER mute > /dev/null
    STRING="mute" ;;
esac
