#! /bin/sh

DIR=/sys/devices/pci0000:00/0000:00:02.0/drm/card0/card0-eDP-1/intel_backlight

actual=$(cat $DIR/actual_brightness)
case $1 in
  -inc) new=$(( $actual + ( $2 * 10 ) )) ;;
  -dec) new=$(( $actual - ( $2 * 10 ) )) ;;
esac

echo $new | sudo tee $DIR/brightness
