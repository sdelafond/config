xscreensaver -nosplash &
#tpb &
eval `dbus-launch`
# manual
setxkbmap -layout us
xset m 2 100
xset r 108 # repeat on backspace
xkbset m
xkbset exp =m
xmodmap ~/.xmodmaprc
#xmodmap -e 'pointer = 1 8 3 4 5 6 7 2 9'
xrdb -load ~/.Xdefaults
xset dpms 0 0 0
xset s off
autocutsel -selection CLIPBOARD -fork
autocutsel -selection PRIMARY -fork
gkrellm &
#xterm-screen &
urxvt &
pulseaudio &
awesome # -c /etc/xdg/awesome/rc.lua
#sawfish
