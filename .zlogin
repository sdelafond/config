local wai="`who am i`" # this is used several times, make it a local variable

# remapping a few keys
source ~/.zsh.console

# the following line cannot be in zshenv since 'who am i' requires a tty
#export LOGIN_HOST=`who am i | awk '{print $6}' | sed -e 's/[()]//g'`
export LOGIN_HOST=${${=wai}[5]//[()]}

[ -f ~/alsa.settings ] && alsactl -f ~/alsa.settings restore

#[[ $HOST = hippie ]] && sudo chmod 755 /var/run/screen

if [[ -z $LOGIN_HOST ]] || [[ $LOGIN_HOST != $HOST ]] ; then
  echo $DISPLAY >| ~/.screen-display
  # start screen if it's version 4.x only
  whence screen > /dev/null && [[ ${$(screen -v)[3]} == 4* ]] && [[ $TERM != (screen*|vt100) ]] && [[ $HOST != (hp|hippie|seb-debian) ]] && screen -R -D -A -S main
  # at this point screen is started, let's see if it's running our usual programs
  if [[ $HOST == centurion ]] && ! psg mutt > /dev/null && ! psg slrn > /dev/null ; then
    screen -p 1 mutt
    screen -p 2 irssi
    screen -p 3 slrn
  fi
fi
