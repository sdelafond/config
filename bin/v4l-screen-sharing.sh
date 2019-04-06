#! /bin/bash

SCREEN="${1:-DP-4}"
DEVICE="${2:-/dev/video2}"

# Directly build ffmpeg resolution and offsets arguments from xrandr
# output:
# "DP-4 connected primary 2560x1440+1920+0 ..."
# -->
# -s 2560x1440 -i +1920,0
RES_OFFSET=$(xrandr -q | perl -nle 'print "-s $1 -i +$2,$3" if m/^'$SCREEN'.*?(\d+x\d+)\+(\d+)\+(\d+)/')

sudo modprobe v4l2loopback exclusive_caps=1

ffmpeg -f x11grab -r 30 $RES_OFFSET -vf hflip -vcodec rawvideo -pix_fmt yuv420p -f v4l2 $DEVICE
