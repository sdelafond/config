eval $(dbus-launch)
xrdb -load ~/.Xdefaults

# declare that we'll map mouse keys
xkbset m
xkbset exp "=m"

# emulate middle button
XINPUT_ID=$(xinput | perl -nle 'print $1 if m/id=(\d+).+pointer/i' | tail -1)
xinput set-prop $XINPUT_ID "Evdev Middle Button Emulation" 1
xinput set-prop $XINPUT_ID "Evdev Middle Button Timeout" 50
# single-finger scrolling only
xinput set-prop "SynPS/2 Synaptics TouchPad" "Synaptics Edges" 0 0 0 4807
xinput set-prop "SynPS/2 Synaptics TouchPad" "Synaptics Edge Scrolling" 1 1 0

# mouse acceleration
xset m 4 10

# keyboard layout
setxkbmap -layout us
xmodmap ~/.xmodmaprc
xset r 108

#xmodmap -e 'pointer = 1 8 3 4 5 6 7 2 9'

# screen sleep
xscreensaver -nosplash &
xset dpms 0 0 0
xset s off
case $HOST in
  mp)
    xrandr --output VGA1 --auto --output DP1 --auto --left-of VGA1 ;;
  centurion)
    xrandr --output DVI-I-1 --auto --output HDMI-0 --auto --left-of DVI-I-1 ;;    
esac

# clipboard sync
autocutsel -selection CLIPBOARD -fork
autocutsel -selection PRIMARY -fork

# clients
gkrellm &
xterm-screen &

# WM
export _JAVA_AWT_WM_NONREPARENTING=1 # for awesome+java
exec awesome # -c /etc/xdg/awesome/rc.lua
#sawfish
