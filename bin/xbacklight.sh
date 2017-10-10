#! /bin/sh

DIR=/sys/class/backlight/intel_backlight

actual=$(cat $DIR/actual_brightness)

case $1 in
  -inc) op="+" ;;
  -dec) op="-" ;;
esac

if [[ -z "$2" ]] && [[ $actual -gt 100 ]] ; then
  step=25
elif [[ $actual -gt 75 ]] ; then
  step=15
elif [[ $actual -gt 50 ]] ; then
  step=10
else
  step=5
fi

new=$(( $actual $op $step ))
echo $new | sudo tee $DIR/brightness
