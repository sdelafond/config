#!/bin/sh

STEP=3
SINK=$(pacmd list-sinks | perl -ne 'print $1 if m/\* index: (\d+)/')

case $1 in
  up|down)
    [ $1 = up ] && DIRECTION="+" || DIRECTION="-"
    pactl set-sink-mute $SINK 0
    pactl set-sink-volume $SINK ${DIRECTION}${STEP}%
    STRING=$(pacmd dump-volumes | awk '/^Sink '${SINK}'/ {print $8}') ;;
  mute)
    pactl set-sink-mute $SINK 1
    STRING="mute" ;;
esac
