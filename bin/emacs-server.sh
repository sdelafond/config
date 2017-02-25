#! /bin/sh

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

if [ -n "$TMUX" ] ; then
  MARKER=$(tmux display-message -p '#{session_name}')
  export TERM="screen-256color" # tmux-256color freaks out emacs
else
  MARKER=$STY
fi
export SERVER_WINDOW_FILE=/tmp/emacsserver-window-${MARKER}

emacs -nw --eval '(progn
                  (setq server-name "'$MARKER'")
                  (server-start)
                  (remove-hook '\''kill-buffer-query-functions '\''server-kill-buffer-query-function)
		  (shell-command "tmux display-message -p '\''#{window_index}'\'' > '$SERVER_WINDOW_FILE'")
                  (add-hook '\''kill-emacs-hook (lambda() (delete-file (getenv "SERVER_WINDOW_FILE")))))'
