#tpb &
# eval `dbus-launch`
xrdb -load ~/.Xdefaults

# keyboard
setxkbmap -layout us
xmodmap ~/.xmodmaprc
xkbset m
xset r 108

## mouse
# middle-click = paste
xinput set-prop $XINPUT_ID Evdev Middle Button Emulation 1
xinput set-prop $XINPUT_ID Evdev Middle Button Timeout 50
# accel
xset m $MOUSE_ACCEL $MOUSE_THRESHOLD
#xmodmap -e 'pointer = 1 8 3 4 5 6 7 2 9'

# screen scleep
xscreensaver -nosplash &
xset dpms 0 0 0
xset s off

# clipboard sync
autocutsel -selection CLIPBOARD -fork
autocutsel -selection PRIMARY -fork

# clients
gkrellm &
xterm-screen &

# WM
awesome # -c /etc/xdg/awesome/rc.lua
#sawfish
