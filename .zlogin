local wai="`who am i`" # this is used several times, make it a local variable

# remapping a few keys
source ~/.zsh.console

# the following line cannot be in zshenv since 'who am i' requires a tty
#export LOGIN_HOST=`who am i | awk '{print $6}' | sed -e 's/[()]//g'`
export LOGIN_HOST=${${=wai}[5]//[()]}

[ -f ~/alsa.settings ] && alsactl -f ~/alsa.settings restore

if [[ -z $LOGIN_HOST ]] || [[ $LOGIN_HOST != $HOST ]] ; then
  echo $DISPLAY >| ~/.remote-display
  # start tmux or screen if possible
# [[ ($TERM != (screen*|vt100) || -z "$TMUX") && $HOST_SHORT != (hp|foooo) ]] && sc remote
  [[ ($TERM != (screen*|vt100) || 0 == 1) && $HOST_SHORT != (hp|foooo) ]] && sc
fi
