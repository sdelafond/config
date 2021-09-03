#!/bin/bash

# constants
DIR=/sys/class/backlight/intel_backlight
STEP=10 # in percent

## main

# CLI
case $1 in
  -inc) op="+" ;;
  -dec) op="-" ;;
  *) exit 1 ;;
esac

max=$(cat $DIR/max_brightness)
actual=$(cat $DIR/actual_brightness)

new=$(bc -l <<< "$actual $op ($max * $STEP / 100)")
new=${new%%.*} 
if [[ $new -gt $max ]] ; then
  new=$max
fi

echo $new | sudo tee $DIR/brightness
