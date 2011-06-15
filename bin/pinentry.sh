#! /bin/zsh

# set -x
# exec > /tmp/keychain.log 2>&1

display=$2

source ~/.zshrc
keychain-init

if [[ -n "$display" && (`hostname` = mephisto || "$display" != localhost:1*) ]] ; then
  if [ -f /usr/bin/pinentry-gtk-2 ] ; then
    binary=/usr/bin/pinentry-gtk-2
  elif [ -f /usr/bin/pinentry-gtk ] ; then
    binary=/usr/bin/pinentry-gtk
  elif [ -f /usr/bin/pinentry-qt ] ; then
    binary=/usr/bin/pinentry-qt
  else # fallback
    binary=/usr/bin/pinentry
  fi
else
  if ps aux | grep -qE '[g]pg.*/tmp/mutt' && [ -f /usr/bin/pinentry-curses ] ; then
    binary=/usr/bin/pinentry-curses
  elif [ -f /usr/bin/mew-pinentry ] ; then
    binary=/usr/bin/mew-pinentry
  elif [ -f /usr/bin/pinentry ] ; then
    binary=/usr/bin/pinentry
  elif [ -f /usr/bin/pinentry-curses ] ; then
    binary=/usr/bin/pinentry-curses
  fi
fi

$binary -g "$@"
