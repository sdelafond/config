# paths
local usrlocal=/usr/local
typeset -U path manpath fpath # no dupes
path=(~/bin $usrlocal/bin $usrlocal/sbin /bin /usr/bin /usr/bin/X11 /usr/X11R6/bin /sbin /usr/sbin /usr/games $path)
for d in ~/bin/**(@N) ~/bin/**(/N) ; do
  path=($d $path)
done
manpath=(/usr/share/man /usr/local/man /usr/X11R6/man /opt/vmware/man $manpath)
fpath=(~/.zsh/functions $fpath)
export LD_LIBRARY_PATH=/usr.local/lib:$LD_LIBRARY_PATH
export XTERM="urxvt &"

#export TZ=Europe/Paris
