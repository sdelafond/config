#!/bin/sh

# constants
STEP=5%

## main

# CLI
case $1 in
  -inc) op="+" ;;
  -dec) op="-" ;;
  *) exit 1 ;;
esac

brightnessctl set -n ${op}${STEP}
