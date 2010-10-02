#! /usr/bin/env zsh

# Copyright (C) 2008 Sebastien Delafond (sdelafond@gmx.net)
# Author: Sebastien Delafond (sdelafond@gmx.net)
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

# you shouldn't have to modify anything below this

SERVER_WINDOW_FILE=/tmp/emacsserver-window-$STY
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

isParentShell() {
  [ $(ps h -p $PPID -o comm 2> /dev/null | tail -1) = "${SHELL//*\/}" ]
}

runEmacsClient() {
  emacsclient "$@" > /dev/null
  selectScreenWindow $WINDOW
}

runRegularEmacs() {
  emacs -nw "$@"
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
  screen -X setenv SERVER_WINDOW_FILE $SERVER_WINDOW_FILE
  screen emacs -nw --eval '(progn (server-start)
                           (shell-command "echo $WINDOW > $SERVER_WINDOW_FILE")
                           (add-hook '\''kill-emacs-hook (lambda() (delete-file (getenv "SERVER_WINDOW_FILE")))))'
}

if [ -z "$SUDO_COMMAND" -a "$HOME" != "/root" ] && isInScreen ; then
  if [ -f $SERVER_WINDOW_FILE ] ; then
    selectScreenWindow `cat $SERVER_WINDOW_FILE` # switch to server window
  else # gonna have to start emacs server
    startEmacsServer
    while [ ! -f $SERVER_WINDOW_FILE ] ; do sleep .1 ; done # wait for it
  fi
  if [ $AUTO_BACKGROUND = "yes" ] && isParentShell ; then
    runEmacsClient "$@" &
  else
    runEmacsClient "$@"
  fi
else
  runRegularEmacs "$@"
fi
