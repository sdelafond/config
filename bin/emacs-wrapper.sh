#! /usr/bin/env zsh

# set -x
# exec > /tmp/ew.log 2>&1

# Copyright (C) 2008-2017 Sebastien Delafond (sdelafond@gmail.com)
# Author: Sebastien Delafond (sdelafond@gmail.com)
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

AUTO_BACKGROUND=yes
TMUX_TITLE="emacs"

# you shouldn't have to modify anything below this

if [[ -n "$TMUX" ]] ; then
  MARKER=$(tmux display-message -p '#{session_name}')
else
  MARKER=$STY
fi
SERVER_WINDOW_FILE=/tmp/emacsserver-window-${MARKER}
DEFAULT_MSG_WAIT=2 # in seconds, if not defined in ~/.screenrc
USER_CONFIGURED_MSG_WAIT=$(awk '/msgwait/{print $2}' ~/.screenrc 2> /dev/null)
DEFAULT_MSG_WAIT=${USER_CONFIGURED_MSG_WAIT:-$DEFAULTMSGWAIT}
OUR_MSG_WAIT=0.1 # so we don't pause 2 entire seconds when switching

# no -g
local indice=${argv[(i)-g]}
argv[$indice,$(($indice+1))]=()

isInScreen() {
  echo $TERMCAP | grep -q -E '^SC'
}

isInTmux() {
  [[ -n "$TMUX" ]]
}

isParentShell() {
  [[ $(ps h -p $PPID -o comm 2> /dev/null | tail -1) = "${SHELL//*\/}" ]] && [[ "$@" != */tmp/zsh* ]]
}

runEmacsClient() {
  emacsclient -s $MARKER "$@"
}

runRegularEmacs() {
  emacs -nw "$@"
}

selectWindow() {
  isInTmux && selectTmuxWindow $1 || selectScreenWindow $1
}

selectTmuxWindow() {
  tmux select-window -t :$1
}

selectScreenWindow() {
  setScreenMsgWait $OUR_MSG_WAIT # do not hold screen with messages
  screen -X focus
  screen -X select $1
  setScreenMsgWait $DEFAULT_MSG_WAIT # reset default
}

setScreenMsgWait() {
  screen -X msgwait $1
}

startEmacsServer() {
  if isInScreen; then 
    screen=screen
  else
    screen=(tmux new-window -n $TMUX_TITLE)
  fi

  $screen emacs-server.sh
}

if ( isInScreen || isInTmux ) && [[ -z "$SUDO_COMMAND" ]] ; then
  if isInTmux ; then
    oldWindow=$(tmux list-windows | awk -F: '/active/ {print $1}')
  else
    oldWindow=$WINDOW
  fi

  if [[ ! -f $SERVER_WINDOW_FILE ]] ; then # gonna have to start emacs server
    startEmacsServer
    while [ ! -f $SERVER_WINDOW_FILE ] ; do sleep .1 ; done # wait for it
  fi

  selectWindow $(cat $SERVER_WINDOW_FILE)

  if [[ $AUTO_BACKGROUND = "yes" ]] && isParentShell "$@"; then
    { runEmacsClient "$@" > /dev/null 2>&1 ; selectWindow $oldWindow } &
  else
    runEmacsClient "$@"
    selectWindow $oldWindow
  fi
else
  runRegularEmacs "$@"
fi

