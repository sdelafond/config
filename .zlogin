local wai="$(who am i)" # this is used several times, make it a local variable

# remapping a few keys
source ~/.zsh.console

# the following line cannot be in zshenv since 'who am i' requires a tty
#export LOGIN_HOST=`who am i | awk '{print $6}' | sed -e 's/[()]//g'`
export LOGIN_HOST=${${=wai}[5]//[()]}

[ -f ~/alsa.settings ] && alsactl -f ~/alsa.settings restore

if [[ -z "$LOGIN_HOST" ]] || [[ $LOGIN_HOST != $HOST ]] ; then
  echo $DISPLAY >| ~/.remote-display
  # start tmux or screen if possible
  if [[ -n "$TERM" && $TERM != (tmux*|screen*|vt100) && $HOST_SHORT != (x1|seb-debian) ]] ; then 
    source ~/.zsh.function && sc
  fi
fi
