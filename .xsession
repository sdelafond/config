eval $(dbus-launch)
xrdb -load ~/.Xdefaults

# declare that we'll map mouse keys
xkbset m
xkbset exp =m

# emulate middle button
XINPUT_ID=$(xinput | perl -nle 'print $1 if m/id=(\d+).+pointer/' | tail -1)
xinput set-prop $XINPUT_ID "Evdev Middle Button Emulation" 1
xinput set-prop $XINPUT_ID "Evdev Middle Button Timeout" 50

# mouse acceleration
xset m $MOUSE_ACCEL $MOUSE_THRESHOLD

# keyboard layout
setxkbmap -layout us
xmodmap ~/.xmodmaprc
xset r 108

#xmodmap -e 'pointer = 1 8 3 4 5 6 7 2 9'

# screen sleep
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
exec awesome # -c /etc/xdg/awesome/rc.lua
#sawfish
