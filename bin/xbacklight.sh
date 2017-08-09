#! /bin/sh

DIR=/sys/class/backlight/intel_backlight

actual=$(cat $DIR/actual_brightness)

case $1 in
  -inc) op="+" ;;
  -dec) op="-" ;;
esac

new=$(( $actual $op ( $2 * 2 ) ))
echo $new | sudo tee $DIR/brightness
