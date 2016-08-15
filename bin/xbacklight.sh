#! /bin/sh

DIR=/sys/class/backlight/intel_backlight

actual=$(cat $DIR/actual_brightness)
case $1 in
  -inc) new=$(( $actual + ( $2 * 10 ) )) ;;
  -dec) new=$(( $actual - ( $2 * 10 ) )) ;;
esac

echo $new | sudo tee $DIR/brightness
