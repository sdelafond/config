# test function
is4 () {
  [[ $ZSH_VERSION == 4.* ]] || return 1
  local minorVersion=${1:-0}
  [[ $(echo $ZSH_VERSION | awk -F. '{print $2}') -ge $minorVersion ]]
  local revision=${2:-0}
  [[ $(echo $ZSH_VERSION | awk -F. '{print $3}') -ge $revision ]]
}

# run zsh version 4 if it's available
ZSH4=/usr/local/bin/zsh
if ! is4 && [[ $OSTYPE == solaris* ]] && [ -f $ZSH4 ] && [ $SHELL != $ZSH4 ] ; then
  SHELL=$ZSH4 $ZSH4 -l && echo "about to exit" && sleep 3 # last parts for debug
  exit
fi

###################################
# Modules & Options
###################################

# modules 
is4 && zmodload -i zsh/complist 
is4 && zmodload -i zsh/parameter
is4 2 && _comp_setup+=$'\ntypeset -a userdirs'
is4 && zmodload -i zsh/mathfunc 
is4 3 10 && zmodload -i zsh/pcre

# modes
is4 && autoload -U zed
is4 && autoload -U zmv
is4 && autoload -U edit-command-line 
is4 && autoload -U compinit && compinit
is4 && autoload -U colors && colors 
is4 && autoload -U url-quote-magic && zle -N self-insert url-quote-magic
is4 && autoload -U select-word-style && select-word-style bash

is4 && autoload -U read-from-minibuffer
regex-edit () {
  # runs a user-entered regex on everything but the command itself
  local REPLY words
  words=(${=BUFFER})
  read-from-minibuffer "Regexp:"
  [[ -n $REPLY ]] && BUFFER="$words[1] $(echo $words[2,${#words}] | sed ${REPLY})"
}
zle -N regex-edit
bindkey "^[e" regex-edit

is4 3 && autoload -U keeper && {
  keeper
  bindkey '^Xk' insert-kept-result
  bindkey '^XK' expand-kept-result    # shift-K to get expansion
  zle -C insert-kept-result complete-word _generic
  zle -C expand-kept-result complete-word _generic
  zstyle ':completion:*-kept-result:*' completer _insert_kept
  zstyle ':completion:insert-kept-result:*' menu yes select
  zle -C _expand_word complete-word _expand_word_and_keep
  zstyle ':completion:*' insert-kept menu
#  accept-line() { RBUFFER+="|keep"; zle .accept-line }
#  zle -N accept-line
}
is4 && autoload -U insert-files && {
  zle -N insert-files
  bindkey '^Xf' insert-files
}
is4 && autoload -U predict-on && { 
  zle -N predict-on
  zle -N predict-off
  bindkey '^Xc' predict-on
  bindkey '^Xv' predict-off
  zstyle ':predict' verbose true
  zstyle ':toggle' true
}

is4 3 9 && autoload -Uz vcs_info && {
  zstyle ':vcs_info:*' disable bzr cdv darcs mtn tla hg p4 svk
  zstyle ':vcs_info:*' enable cvs svn git

  zstyle ':vcs_info:*' check-for-changes true
  zstyle ':vcs_info:*' formats        "[%s %b%c%u]"
  zstyle ':vcs_info:*' actionformats  "[%s %b|%a%c%u]"
  zstyle ':vcs_info:*' stagedstr      "%{${fg_bold[yellow]}%}↺%{${fg_no_bold[default]}%}"
  zstyle ':vcs_info:*' unstagedstr    "%{${fg_bold[yellow]}%}⚡%{${fg_no_bold[default]}%}"

  vcs_stuff() {
    vcs_info
    if [[ -n $vcs_info_msg_0_ ]] ; then
      PSEXTRA="%{$fg_no_bold[magenta]%}$vcs_info_msg_0_%{$fg_no_bold[default]%} "
    else
      unset PSEXTRA
    fi
  }
  
  # note: you'll also need "setopt prompt_subst"
}

# _hist_complete() {
#   _main_complete _history
# }
# zle -C hist-complete menu-select _hist_complete
# bindkey '^H'  hist-complete

# complete most recent files
zstyle ':completion:most-recent-file:*' match-original both
zstyle ':completion:most-recent-file:*' file-sort modification
zstyle ':completion:most-recent-file:*' file-patterns '*:all\ files'
#zstyle ':completion:most-recent-file:*' hidden all
zstyle ':completion:most-recent-file:*' completer _files
zle -C most-recent-file menu-complete _generic
bindkey '^XL' most-recent-file

# complete files, for when zsh is confused at to what to complete on
zstyle ':completion:files:*' match-original both
#zstyle ':completion:files:*' file-sort modification
zstyle ':completion:files:*' file-patterns '*:all\ files'
#zstyle ':completion:files:*' hidden all
zstyle ':completion:files:*' completer _files
zle -C files menu-complete _generic
bindkey '^Xl' files

zstyle ':completion:*' menu yes=long-list
zstyle ':completion:*' menu select=2

WORDCHARS=
WORDCHARS_EXTENDED='*?_-.[]~=&;!#$%^(){}<>|:@,\\'
WORDCHARS_EXTENDED_SPACE="$WORDCHARS_EXTENDED "
WORDCHARS_EXTENDED_SLASH="${WORDCHARS_EXTENDED}/"
function is-escaped() {
  [[ "${BUFFER[$CURSOR-1,CURSOR-1]}" = "\\" ]]
}
unescape-forward-word() {
  while is-escaped ; do zle .forward-word ; done
}
unescape-backward-word() {
  while is-escaped ; do zle .backward-word ; done
} 
backward-big-word() {
  local WORDCHARS="$1" ; zle .backward-word ; unescape-backward-word
}
forward-big-word() {
  local WORDCHARS="$1" ; zle .forward-word ; unescape-forward-word
}
kill-big-word() { 
  local WORDCHARS="$1" ; zle .kill-word
}
backward-kill-big-word() { 
  local WORDCHARS="$1" ; zle .backward-kill-word
}
transpose-big-words() { 
  local WORDCHARS="$1" ; zle .transpose-words
}

backward-kill-big-word-space() { backward-kill-big-word "$WORDCHARS_EXTENDED_SLASH"}
kill-big-word-space() { kill-big-word "$WORDCHARS_EXTENDED_SLASH"}
transpose-big-words-slash() { transpose-big-words "$WORDCHARS_EXTENDED_SLASH" }
backward-big-word-space() { backward-big-word "$WORDCHARS_EXTENDED_SLASH" }
backward-big-word-slash() { backward-big-word "$WORDCHARS_EXTENDED_SPACE" }
forward-big-word-slash() { forward-big-word "$WORDCHARS_EXTENDED_SPACE" }
forward-big-word-space() { forward-big-word "$WORDCHARS_EXTENDED_SLASH" }
backward-small-word() { forward-big-word "$WORDCHARS_EXTENDED" }
forward-small-word() { forward-big-word "$WORDCHARS_EXTENDED" }
zle -N backward-big-word-space
zle -N backward-big-word-slash
zle -N forward-big-word-space
zle -N forward-big-word-slash
zle -N kill-big-word-space
zle -N backward-kill-big-word-space
zle -N transpose-big-words-slash
zle -N backward-small-word
zle -N forward-small-word
bindkey "^[B" backward-small-word
bindkey "^[F" forward-small-word
bindkey "^[^b" backward-big-word-space
bindkey "^[^f" forward-big-word-space
bindkey "^[^p" backward-big-word-slash
bindkey "^[^n" forward-big-word-slash
bindkey "^[t" transpose-words
bindkey "^[^t" transpose-big-words-slash
bindkey "^[^d" kill-big-word-space
bindkey "^[^" backward-kill-big-word-space

dirname-previous-word () {
  autoload -U modify-current-argument
  modify-current-argument '${ARG:h}'
}
zle -N dirname-previous-word
bindkey '^q' dirname-previous-word


# mailcheck
#mailpath=(/var/mail/${USERNAME})

# help
autoload run-help
alias run-help > /dev/null && unalias run-help

#options
       setopt append_history
       setopt NO_auto_cd
       setopt NO_auto_menu
       setopt auto_name_dirs 
       setopt auto_pushd 
       setopt autolist
is4 && setopt bare_glob_qual 
       setopt NO_beep
is4 && setopt NO_check_jobs 
       setopt NO_clobber
       setopt cdable_vars
       setopt complete_in_word
       setopt correct
       setopt extended_glob
       setopt extended_history 
       setopt NO_flow_control
       setopt glob_complete                                                   
       setopt hash_cmds 
       setopt hash_dirs 
       setopt hist_allow_clobber 
       setopt hist_ignore_space
is4 && setopt hist_save_no_dups 
is4 && setopt hist_ignore_all_dups 
       setopt hist_reduce_blanks
       setopt hist_verify
is4 && setopt inc_append_history
       setopt ignore_eof
       setopt ksh_option_print
is4 && setopt list_packed 
is4 && setopt NO_list_rows_first 
       setopt mark_dirs
       setopt NO_menucomplete
       setopt NO_multios
       setopt NO_nomatch
       setopt nohup
       setopt NO_notify
       setopt path_dirs
       setopt NO_print_exit_value                                             
       setopt prompt_subst
       setopt pushd_ignore_dups 
       setopt NO_pushd_minus 
       setopt pushd_silent
       setopt pushd_to_home                                                   
       setopt rc_expand_param                                                 
       setopt rc_quotes                                                       
       setopt NO_singlelinezle
is4 && setopt share_history

###################################
# Environment
###################################
literal="RRR"
typeset -xA extensions # this dictionary is also used by the lst() function
extensions=()
extensions[backup]="${literal}~ ${literal}# bak ${litteral}svn-commit.tmp"
extensions[docs]="calendar chm doc dvi emacs html ics odf odt org pdf pps ppt ps reg rtf sub srt tex txt todo vcf xls xml"
extensions[archives]="ace apk arj bin bundle bz2 cab cdr deb dmg ear exe gz img iso jar lzh pgdump rar rpm tar taz tgz udeb udf war xpi z zip"
extensions[video]="3gp asf avi divx flv ifo m1v m2v mkv mov mp2 mp4 mpe mpeg mpg ram rm wmv xvid yuv"
extensions[audio]="au mp3 ogg ogv wav wma"
extensions[pics]="bmp gif jpeg jpg pbm png ppm tga tif xbm xcf xpm"
extensions[code]="${literal}Makefile a bash c c++ class cpp diff el elz hs jacl java js jy ko lua o out patch pl pm py pyc pyo rb sh so sql tcl zsh"

# add the uppercase extensions too
for key in ${(k)extensions[@]} ; do
  extensions[$key]="$extensions[$key] ${(U)extensions[$key]}"
done

if is4 ; then # use something smart to associate colors and extensions
  local -A colors
  colors=()
  colors[backup]="02;90"
  colors[docs]="02;37"
  colors[archives]="01;31"
  colors[video]="01;33"
  colors[audio]="00;36"
  colors[pics]="00;32"
  colors[code]="01;35"
  LS_COLORS='ex=01;35:no=00:fi=00;37:di=01;36:ln=01;36:pi=40;33:so=01;35:do=01;35:bd=40;33;01:cd=40;33;01:or=04;31'
#  LS_COLORS='no=00:fi=00;37:di=01;02;36:ln=01;36:pi=40;33:so=01;35:do=01;35:bd=40;33;01:cd=40;33;01:or=04;31'
  for key in ${(k)extensions[@]} ; do
    color=${colors[$key]}
    # join on '=${color}:*.', and prepend to LS_COLORS
    LS_COLORS='*.'${(ej,=${color}:*.,)${=${extensions[$key]}}}=${color}:$LS_COLORS
  done
  export LS_COLORS=${LS_COLORS//\*.${literal}/\*}
else # hardcode everything...
  export LS_COLORS='*Makefile=01;35:*.a=01;35:*.bash=01;35:*.c=01;35:*.c++=01;35:*.class=01;35:*.cpp=01;35:*.diff=01;35:*.el=01;35:*.elz=01;35:*.jacl=01;35:*.java=01;35:*.js=01;35:*.jy=01;35:*.ko=01;35:*.lua=01;35:*.o=01;35:*.out=01;35:*.patch=01;35:*.pl=01;35:*.pm=01;35:*.py=01;35:*.pyc=01;35:*.pyo=01;35:*.rb=01;35:*.sh=01;35:*.so=01;35:*.sql=01;35:*.tcl=01;35:*.zsh=01;35:*MAKEFILE=01;35:*.A=01;35:*.BASH=01;35:*.C=01;35:*.C++=01;35:*.CLASS=01;35:*.CPP=01;35:*.DIFF=01;35:*.EL=01;35:*.ELZ=01;35:*.JACL=01;35:*.JAVA=01;35:*.JS=01;35:*.JY=01;35:*.KO=01;35:*.LUA=01;35:*.O=01;35:*.OUT=01;35:*.PATCH=01;35:*.PL=01;35:*.PM=01;35:*.PY=01;35:*.PYC=01;35:*.PYO=01;35:*.RB=01;35:*.SH=01;35:*.SO=01;35:*.SQL=01;35:*.TCL=01;35:*.ZSH=01;35:*.calendar=02;37:*.doc=02;37:*.dvi=02;37:*.emacs=02;37:*.html=02;37:*.ics=02;37:*.odf=02;37:*.odt=02;37:*.pdf=02;37:*.pps=02;37:*.ppt=02;37:*.ps=02;37:*.reg=02;37:*.rtf=02;37:*.srt=02;37:*.tex=02;37:*.txt=02;37:*.todo=02;37:*.xls=02;37:*.xml=02;37:*.CALENDAR=02;37:*.DOC=02;37:*.DVI=02;37:*.EMACS=02;37:*.HTML=02;37:*.ICS=02;37:*.ODF=02;37:*.ODT=02;37:*.PDF=02;37:*.PPS=02;37:*.PPT=02;37:*.PS=02;37:*.REG=02;37:*.RTF=02;37:*.SRT=02;37:*.TEX=02;37:*.TXT=02;37:*.TODO=02;37:*.XLS=02;37:*.XML=02;37:*.bmp=00;32:*.gif=00;32:*.jpeg=00;32:*.jpg=00;32:*.pbm=00;32:*.png=00;32:*.ppm=00;32:*.tga=00;32:*.tif=00;32:*.xbm=00;32:*.xcf=00;32:*.xpm=00;32:*.BMP=00;32:*.GIF=00;32:*.JPEG=00;32:*.JPG=00;32:*.PBM=00;32:*.PNG=00;32:*.PPM=00;32:*.TGA=00;32:*.TIF=00;32:*.XBM=00;32:*.XCF=00;32:*.XPM=00;32:*.ace=01;31:*.arj=01;31:*.bin=01;31:*.bz2=01;31:*.cdr=01;31:*.deb=01;31:*.dmg=01;31:*.ear=01;31:*.exe=01;31:*.gz=01;31:*.iso=01;31:*.jar=01;31:*.lzh=01;31:*.pgdump=01;31:*.rar=01;31:*.rpm=01;31:*.tar=01;31:*.taz=01;31:*.tgz=01;31:*.udeb=01;31:*.udf=01;31:*.war=01;31:*.xpi=01;31:*.z=01;31:*.zip=01;31:*.ACE=01;31:*.ARJ=01;31:*.BIN=01;31:*.BZ2=01;31:*.CDR=01;31:*.DEB=01;31:*.DMG=01;31:*.EAR=01;31:*.EXE=01;31:*.GZ=01;31:*.ISO=01;31:*.JAR=01;31:*.LZH=01;31:*.PGDUMP=01;31:*.RAR=01;31:*.RPM=01;31:*.TAR=01;31:*.TAZ=01;31:*.TGZ=01;31:*.UDEB=01;31:*.UDF=01;31:*.WAR=01;31:*.XPI=01;31:*.Z=01;31:*.ZIP=01;31:*.3gp=01;33:*.asf=01;33:*.avi=01;33:*.divx=01;33:*.flv=01;33:*.ifo=01;33:*.m1v=01;33:*.m2v=01;33:*.mkv=01;33:*.mov=01;33:*.mp2=01;33:*.mp4=01;33:*.mpe=01;33:*.mpeg=01;33:*.mpg=01;33:*.ram=01;33:*.rm=01;33:*.wmv=01;33:*.xvid=01;33:*.yuv=01;33:*.3GP=01;33:*.ASF=01;33:*.AVI=01;33:*.DIVX=01;33:*.FLV=01;33:*.IFO=01;33:*.M1V=01;33:*.M2V=01;33:*.MKV=01;33:*.MOV=01;33:*.MP2=01;33:*.MP4=01;33:*.MPE=01;33:*.MPEG=01;33:*.MPG=01;33:*.RAM=01;33:*.RM=01;33:*.WMV=01;33:*.XVID=01;33:*.YUV=01;33:*~=02;90:*#=02;90:*.bak=02;90:*~=02;90:*#=02;90:*.BAK=02;90:*.au=00;36:*.mp3=00;36:*.ogg=00;36:*.wav=00;36:*.wma=00;36:*.AU=00;36:*.MP3=00;36:*.OGG=00;36:*.WAV=00;36:*.WMA=00;36:ex=01;35:no=00:fi=00;37:di=01;36:ln=01;36:pi=40;33:so=01;35:do=01;35:bd=40;33;01:cd=40;33;01:or=04;31'
fi

# history
HISTSIZE=1000000
SAVEHIST=1000000
HISTFILE=~/.zsh_history

HELPDIR=~/.zsh/help

MAILCHECK=0

# logins/logouts watch
LOGCHECK=30 # in seconds
WATCH=all
WATCHFMT="[%D %T] %n has %a %l from %M"

# misc
if [[ -n "$CYGWIN" ]] ; then
  EDITOR_ZSH="emacs"
else
  EDITOR_ZSH=$HOME/bin/jed
fi
export EDITOR=$EDITOR_ZSH
export VISUAL=$EDITOR

if which less > /dev/null ; then
  export PAGER="less"
#  export LESS='--RAW-CONTROL-CHARS --tabs=8 -r'
#  export LESSOPEN='| /usr/bin/lesspipe %s'
#  export LESSCLOSE='/usr/bin/lesspipe %s %s'
else
  export PAGER=more
fi

export CVS_RSH=ssh
local -a javas ; javas=(/usr/lib/jvm/java*sun*(DN) /usr/local/j2*(DN))
if [[ ${#javas} -gt 0 ]] ; then
  export JAVA_HOME=$javas[-1]
  export JAVA=${JAVA_HOME}/bin/java
  path=($path $JAVA_HOME/bin)
fi

export DEBEMAIL="sdelafond@gmail.com"
export GIT_AUTHOR_EMAIL=$DEBEMAIL
export GIT_AUTHOR_NAME="Sébastien Delafond"

# short hostname
export HOST_SHORT=${HOST/.*}

# let's make sure our TERM is known to the system
infocmp $TERM > /dev/null 2>&1 || export TERM=${TERM/-256color}

ZSH_CONFIG_FILES=(~/.z(log|sh)^(_*|*~)(.,@) ~/.zsh)

# ulimit
limit coredumpsize 0 # don't allow coredumps

###################################
# Key bindings
###################################
bindkey '^W' kill-region

###################################
# Completion settings
###################################

# completion styles
is4 && zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
typeset -U otherHosts
otherHosts=(`awk '/;(connect|run|ssh) [a-z][^@]+$/ {print $3}' ~/.zsh_history | grep -vE (esg|ps-|c-5) | sort -u` $_hosts)
is4 && zstyle '*' hosts $otherHosts
is4 && zstyle ':completion:*:processes' command 'ps h -u ${USER} --forest -o pid,cmd'
is4 && zstyle ':completion:*:processes-names' command 'ps -u ${USER} -o command'
is4 && zstyle ':completion:*:*:kill:*:processes' list-colors "=(#b) #([0-9]#)*=0=01;32"
is4 && zstyle ':completion:*:*:kill:*:processes' sort false
is4 && zstyle ':completion:*:*:killall:*:processes-names' list-colors "=*=01;32"
is4 && zstyle ':completion:*:warnings' format "%B$fg[red]%}---- no match for: $fg[white]%d%b"
is4 && zstyle ':completion:*:messages' format '%B%U---- %d%u%b'
is4 && zstyle ':completion:*:corrections' format '%B---- %d (errors %e)%b'
is4 && zstyle ':completion:*' verbose 'yes'
is4 && zstyle ':completion:*' file-sort name
is4 && zstyle ':completion:*' menu select=long

# completion for functions
is4 && compdef _connect-run connect run
is4 && compdef _cvs cvsseb
is4 && compdef _svn svnseb
is4 && compdef _hosts dig digs
is4 && compdef '_deb_packages expl uninstalled' i
is4 && compdef '_deb_packages expl installed' rp
is4 && compdef _python-doc pydoc-html
is4 && compdef '_files -W $HELPDIR' run-help
is4 && compdef _smartsudo s
is4 && compdef _initd-service se
is4 && compdef _locales setlocale
is4 && compdef _which what

###################################
# HOST/OSTYPE specificities
###################################

# OS specificities
case $OSTYPE in
  solaris*)
    path=(/opt/sfw/bin /opt/sfw/sbin /opt/csw/bin /opt/csw/sbin /usr/ucb /usr/ccs/bin /usr.local/bin /usr.local/sbin /usr.local/local/bin /usr.local/local/sbin $path)
    manpath=(/usr.local/man $manpath)
    export MANPATH
    [ -d /opt/csw/share/terminfo ] && export TERMINFO=/opt/csw/share/terminfo
    case $TERM in
      rxvt) export TERM=xterm ;;
      screen) who am i | grep -qv :S && export TERM=vt100 ;;
    esac ;;
  darwin*)
    [ "${TERM}" = "rxvt" ] && export TERM=xterm ;;
  *bsd*)
    if [[ $OSTYPE == openbsd* ]] ; then
      export CVSROOT=anoncvs@anoncvs3.usa.openbsd.org:/cvs
      export PKG_PATH=http://ftp.arcane-networks.fr/pub/OpenBSD/$(uname -r)/packages/$(uname -m)
      export PERL5LIB=/usr/local/libdata/perl5/site_perl
    fi ;;
esac

# host specificities
local -A hostnicks
hostnicks[centurion]="home"
hostnicks[weshyo]="frisco"
case $HOST_SHORT in
  centurion)
    export MAKEFLAGS="-j4" ;;
  hippie|hp)
    export MAKEFLAGS="-j2"
    [[ $TERM = screen* ]] && unset DISPLAY ;;
  vb)
    export TERM=cygwin ;;
  seb-debian)
    ;;
  *)
    [[ $TERM = screen* ]] && unset DISPLAY ;;
esac

source ~/.zsh.prompt
source ~/.zsh.alias
source ~/.zsh.function

for dir in ~/.config-* ; do
  case "$dir" in *dontsource*|*base*) continue ;; esac
  for file in $dir/.z*(N) ; do
    source $file
  done
done

local hostFile=~/.zsh_$HOST
[[ -f $hostFile ]] && source $hostFile

export GIT_COMMITTER_EMAIL=$GIT_AUTHOR_EMAIL
export GIT_COMMITTER_NAME=$GIT_AUTHOR_NAME

trap 'zreload-do' USR1
