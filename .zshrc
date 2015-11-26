zmodload -i zsh/pcre
is4 () {
  [[ $ZSH_VERSION =~ '[45].*' ]] || return 1
  local minorVersion=${1:-0}
  [[ $(echo $ZSH_VERSION | awk -F. '{print $2}') -ge $minorVersion ]]
  local revision=${2:-0}
  [[ $(echo $ZSH_VERSION | awk -F. '{print $3}') -ge $revision ]]
}

###################################
# Modules & modes
###################################

zmodload -i zsh/complist 
zmodload -i zsh/parameter
_comp_setup+=$'\ntypeset -a userdirs'
zmodload -i zsh/mathfunc 

# modes
autoload -U zed
autoload -U zmv
autoload -U edit-command-line 
autoload -U compinit && compinit
is4 && autoload -U colors && colors 
autoload -U url-quote-magic && zle -N self-insert url-quote-magic
autoload -U select-word-style && select-word-style bash
autoload run-help
autoload run-help-git
autoload run-help-sudo
autoload run-help-s

# vcs-info
autoload -Uz vcs_info && {
  # note that you'll also need to "setopt prompt_subst", as below
  zstyle ':vcs_info:*' disable bzr cdv cvs darcs mtn tla hg p4 svk
  zstyle ':vcs_info:*' enable cvs svn git

  ### git: Show +N/-N when your local branch is ahead-of or behind remote HEAD.
  function +vi-git-st() {
    local ahead behind
    local -a gitstatus

    # for git prior to 1.7
    # ahead=$(git rev-list origin/${hook_com[branch]}..HEAD | wc -l)
    ahead=$(git rev-list ${hook_com[branch]}@{upstream}..HEAD 2>/dev/null | wc -l)
    (( $ahead )) && gitstatus+=( "(↑${ahead})" )

    # for git prior to 1.7
    # behind=$(git rev-list HEAD..origin/${hook_com[branch]} | wc -l)
    behind=$(git rev-list HEAD..${hook_com[branch]}@{upstream} 2>/dev/null | wc -l)
    (( $behind )) && gitstatus+=( "(↓${behind})" )

    hook_com[misc]+=${(j:/:)gitstatus}
  }

  ### Show symbol
  function +vi-symbol() {
    case ${hook_com[vcs_orig]} in
      "git") hook_com[vcs]="±" ;;
      "cvs") hook_com[vcs]="❌" ;;
      "svn") hook_com[vcs]="®" ;;
      "git-svn") hook_com[vcs]="±®" ;;
    esac
  }

  ### git: Show remote branch name for remote-tracking branches
  function +vi-git-remotebranch() {
    local remote

    # Are we on a remote-tracking branch?
    remote=${$(git rev-parse --verify ${hook_com[branch]}@{upstream} \
      --symbolic-full-name 2>/dev/null)/refs\/remotes\/}

    if [[ -n "${remote}" ]] && [[ ! $remote =~ (origin|alioth)/${hook_com[branch]} ]] && [[ ! $remote =~ refs/heads ]] ; then
      hook_com[branch]="${hook_com[branch]}→${remote}"
    fi
  }

  function +vi-git-localname() {
    local old_base_name="${hook_com[base-name]}"
    local new_base_name="${REPOSITORIES[$old_base_name]}"
    if [[ -n "${new_base_name}" ]] ; then
      hook_com[base-name]="${new_base_name}"
    else
      hook_com[base-name]="${old_base_name}"
    fi      
  }

  function +vi-git-untracked() {
    blacklisted-vcs-dir && return 0
    if [[ -n $(git ls-files --other --exclude-standard 2> /dev/null) ]] ; then
      hook_com[unstaged]+="%{${fg_bold[yellow]}%}!%{${fg_no_bold[default]}%}"
    fi
  }

  function blacklisted-vcs-dir() {
    local d
    local -a blacklisted_dirs
    blacklisted_dirs=( "dd-wrt/src" "dd-wrt/firmware-mod-kit" "src/router" "kernel" )
    for d in ${blacklisted_dirs}; do
      [[ ${PWD}/ == */${d}/* ]] && return 0
    done
    return 1
  }

  zstyle -e ':vcs_info:*' check-for-changes 'blacklisted-vcs-dir && reply=(false) || reply=(true)'
  zstyle ':vcs_info:*' stagedstr            "%{${fg_bold[yellow]}%}↺%{${fg_no_bold[default]}%}"
  zstyle ':vcs_info:*' unstagedstr          "%{${fg_bold[yellow]}%}⚡%{${fg_no_bold[default]}%}"
  zstyle ':vcs_info:*' formats              "[%s%b%m%c%u]" "%s%r"
  zstyle ':vcs_info:*' actionformats        "[%s%b%m|%a%c%u]" "%s%r"
  zstyle ':vcs_info:svn*+set-message:*'     hooks symbol
  zstyle ':vcs_info:cvs*+set-message:*'     hooks symbol
  zstyle ':vcs_info:git-svn+set-message:*'  hooks symbol git-st git-stash git-untracked
  zstyle ':vcs_info:git+set-message:*'      hooks symbol git-st git-stash git-untracked git-remotebranch git-localname

  vcs_stuff() {
    vcs_info
    if [[ -n $vcs_info_msg_0_ ]] ; then
      PSEXTRA="%F{magenta}${vcs_info_msg_0_}%f "
    else
      unset PSEXTRA
    fi
  }
}

###################################
# Line editing
###################################

bindkey -e # emacs-style, please

# also highlight like in our emacs theme
zle_highlight=(region:bg=17 special:standout suffix:bold,bg=136 isearch:bg=23,bold,standout)

# minibuffer regex substitution
autoload -U read-from-minibuffer
regex-edit () {
  # runs a user-entered regex on everything but the command itself
  local REPLY words
  words=(${=BUFFER})
  read-from-minibuffer "Regexp:"
  [[ -n $REPLY ]] && BUFFER="$words[1] $(echo $words[2,${#words}] | sed ${REPLY})"
}
zle -N regex-edit
bindkey "^[E" regex-edit

# keeper
autoload -U keeper && {
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

# insert files
autoload -U insert-files && {
  zle -N insert-files
  bindkey '^Xf' insert-files
}

# predict (not used much)
autoload -U predict-on && { 
  zle -N predict-on
  zle -N predict-off
  bindkey '^Xc' predict-on
  bindkey '^Xv' predict-off
  zstyle ':predict' verbose true
  zstyle ':toggle' true
}

# _hist_complete() {
#   _main_complete _history
# }
# zle -C hist-complete menu-select _hist_complete
# bindkey '^H'  hist-complete

bindkey '^r' history-incremental-pattern-search-backward

# complete most recent files
zstyle ':completion:most-recent-file:*' match-original both
zstyle ':completion:most-recent-file:*' file-sort modification
zstyle ':completion:most-recent-file:*' file-patterns '*:all\ files'
#zstyle ':completion:most-recent-file:*' hidden all
zstyle ':completion:most-recent-file:*' completer _files
zle -C most-recent-file menu-complete _generic
bindkey '^XL' most-recent-file

# complete files, for when zsh is confused as to what to complete on
zstyle ':completion:files:*' match-original both
#zstyle ':completion:files:*' file-sort modification
zstyle ':completion:files:*' file-patterns '*:all\ files'
#zstyle ':completion:files:*' hidden all
zstyle ':completion:files:*' completer _files
zle -C files menu-complete _generic
bindkey '^Xl' files

# last typed word
insert-last-typed-word() { zle insert-last-word -- 0 -1 };
zle -N insert-last-typed-word
bindkey "^[m" insert-last-typed-word

# edit command-line
autoload edit-command-line && zle -N edit-command-line
bindkey '\ee' edit-command-line

# complete word from history
zle -C hist-complete complete-word _generic
zstyle ':completion:hist-complete:*' completer _history
bindkey "^X^X" hist-complete

# command-line navigation
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
backward-small-word() { backward-big-word "$WORDCHARS_EXTENDED" }
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

# eats up path components one at a time, instead of relying on "word"
# definition
dirname-previous-word () {
  autoload -U modify-current-argument
  modify-current-argument '${ARG:h}'
}
zle -N dirname-previous-word
bindkey '^q' dirname-previous-word

bindkey '^W' kill-region

###################################
# Completion settings
###################################

# default completion style
zstyle ':completion:*' menu yes=long-list
zstyle ':completion:*' menu select=2

# case insensitive completion for cd etc *N*
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'

# completion styles
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
typeset -U otherHosts
otherHosts=($(awk '/;(co|com|connect|connect-mosh|run|ssh) [a-z][^@]+$/ && !/(esg|ps|c-5)/ {print $3}' ~/.zsh_history | sort -u) $_hosts)
zstyle '*' hosts $otherHosts
zstyle ':completion:*:processes' command 'ps h -u ${USER} --forest -o pid,cmd'
zstyle ':completion:*:processes-names' command 'ps -u ${USER} -o command'
zstyle ':completion:*:*:kill:*:processes' list-colors "=(#b) #([0-9]#)*=0=01;32"
zstyle ':completion:*:*:kill:*:processes' sort false
zstyle ':completion:*:*:killall:*:processes-names' list-colors "=*=01;32"
zstyle ':completion:*:warnings' format "%B$fg[red]%}---- no match for: $fg[white]%d%b"
zstyle ':completion:*:messages' format '%B%U---- %d%u%b'
zstyle ':completion:*:corrections' format '%B---- %d (errors %e)%b'
zstyle ':completion:*' verbose 'yes'
zstyle ':completion:*' file-sort name
zstyle ':completion:*' menu select=long

# completion for some custom functions
compdef _connect-run co com connect connect-mosh run
compdef _hosts dig digs
compdef '_deb_packages expl uninstalled' i
compdef '_deb_packages expl installed' rp
compdef _python-doc pydoc-html
compdef '_files -W $HELPDIR' run-help
compdef _smartsudo s
compdef _initd-service se
compdef _locales setlocale
compdef _which what

# mailcheck
#mailpath=(/var/mail/${USERNAME})

###################################
# options
###################################
setopt    append_history
setopt NO_auto_cd
setopt NO_auto_menu
setopt    auto_name_dirs
setopt    auto_pushd
setopt    autolist
setopt    bare_glob_qual
setopt NO_beep
setopt NO_check_jobs
setopt NO_clobber
setopt    cdable_vars
setopt    complete_in_word
setopt    correct
setopt    extended_glob
setopt    extended_history
setopt NO_flow_control
setopt    glob_complete
setopt    hash_cmds
setopt    hash_dirs
setopt    hist_allow_clobber
setopt    hist_ignore_space
setopt    hist_save_no_dups
setopt    hist_ignore_all_dups
setopt    hist_reduce_blanks
setopt    hist_verify
setopt    inc_append_history
setopt    ignore_eof
setopt    ksh_option_print
setopt    list_packed
setopt NO_list_rows_first
setopt    mark_dirs
setopt NO_menucomplete
setopt NO_multios
setopt NO_nomatch
setopt    nohup
setopt NO_notify
setopt    path_dirs
setopt NO_print_exit_value
setopt    prompt_subst
setopt    pushd_ignore_dups
setopt NO_pushd_minus
setopt    pushd_silent
setopt    pushd_to_home
setopt    rc_expand_param
setopt    rc_quotes
setopt    re_match_pcre
setopt NO_singlelinezle
setopt    share_history

###################################
# Environment
###################################

## colored ls
literal="RRR"
typeset -xA extensions # this dictionary is also used by the lst() function
extensions=()
extensions[backup]="${literal}~ ${literal}# bak ${literal}svn-commit.tmp"
extensions[docs]="calendar chm csv doc docx dvi emacs html ics odf ods odt org pdf pps ppt pptx ps reg rtf sub srt tex txt todo vcf xls xlsx xml"
extensions[archives]="7z ace apk arj bin bundle bz2 cab cdr dat deb dmg ear exe gz img iso jar lzh ova pgdump qcow qcow2 rar rpm tar taz tgz udeb udf vmdk war xpi z zip"
extensions[video]="3gp asf avi divx flv ifo m1v m2v mkv mov mp2 mp4 mpe mpeg mpg ram rm wmv xvid yuv"
extensions[audio]="au mp3 ogg ogv wav wma"
extensions[pics]="bmp dng gif jpeg jpg pbm png ppm tga tif xbm xcf xpm"
extensions[code]="${literal}Makefile a bash c c++ class cpp diff el elz hs jacl java js jy ko lua o out patch pl pm py pyc pyo rb sh so sql tcl zsh"

# add the uppercase extensions too
for key in ${(k)extensions[@]} ; do
  extensions[$key]="$extensions[$key] ${(U)extensions[$key]}"
done

local -A colors
colors=()
colors[backup]="04;90"
colors[docs]="03;37"
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

## history
HISTSIZE=1000000
SAVEHIST=1000000
HISTFILE=~/.zsh_history

# logins/logouts watch
LOGCHECK=30 # in seconds
WATCH=all
WATCHFMT="[%D %T] %n has %a %l from %M"

## misc
HELPDIR=~/.zsh/help
MAILCHECK=0

# editor & visual
if [[ -n "$CYGWIN" ]] || [[ ! -f $HOME/bin/emacs-wrapper.sh ]] ; then
  EDITOR_ZSH="emacs"
else
  EDITOR_ZSH=$HOME/bin/emacs-wrapper.sh
fi
export EDITOR=$EDITOR_ZSH
export VISUAL=$EDITOR_ZSH

if which less > /dev/null ; then
  export PAGER="less"
#  export LESS='--RAW-CONTROL-CHARS --tabs=8 -r'
#  export LESSOPEN='| /usr/bin/lesspipe %s'
#  export LESSCLOSE='/usr/bin/lesspipe %s %s'
else
  export PAGER=more
fi

export CVS_RSH=ssh

# JAVA_HOME
local -a javas ; javas=(/usr/lib/jvm/java*sun*(DN) /usr/lib/jvm/java*openjdk*(DN) /usr/local/j2*(DN))
if [[ ${#javas} -gt 0 ]] ; then
  export JAVA_HOME=$javas[-1]
  export JAVA=${JAVA_HOME}/bin/java
  path=($path $JAVA_HOME/bin)
fi

# EC2 API tools
local -a ec2_apis ; ec2_apis=(/opt/ec2-api*(DN))
if [[ ${#ec2_apis} -gt 0 ]] ; then
  export EC2_HOME=$ec2_apis[-1]
  path=($path $EC2_HOME/bin)
fi

# EC2 AMI tools
local -a ec2_amis ; ec2_amis=(/opt/ec2-ami*(DN))
if [[ ${#ec2_amis} -gt 0 ]] ; then
  path=($path ${ec2_amis[-1]}/bin)
fi

cb=~/.config-base

# # makeflags
# export MAKEFLAGS="-j$(grep -c '^processor\s:' /proc/cpuinfo)"

# HOSTNAME
export HOSTNAME=$(hostname -s)

# short hostname
export HOST_SHORT=${HOST/.*}

# let's make sure our TERM is known to the system
infocmp $TERM > /dev/null 2>&1 || export TERM=${TERM/-256color}

# to be handily copied to remote machines
ZSH_CONFIG_FILES=(~/.z(log|sh)^(_*|*~)(.,@) ~/.zsh)

# ulimit
ulimit -c unlimited # don't limit coredumps

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
  centurion|x230|seb-debian|x1) # no fallback to unset'ing DISPLAY
    ;;
  vb)
    export TERM=cygwin ;;
  *)
    [[ $TERM = screen* ]] || [[ -n "$TMUX" ]] && unset DISPLAY ;;
esac

GPG_KEY="DAF6CE93"

# aliases for repositories
typeset -A REPOSITORIES

# always source these config files...
source ~/.zsh.alias
source ~/.zsh.function

# maybe overloaded later
[[ $HOST == *puppet-master* ]] || set-git-info sdelafond@gmail.com "Sébastien Delafond"

# ... but only some of these
for dir in ~/.config-* ; do
  case "$dir" in *dontsource*|*base*) continue ;; esac
  for file in $dir/.z*(N) ; do
    [[ $file == *~ ]] || source $file
  done
done
for file in ~/.zsh_*(N) ; do
  [[ $file == *history* ]] || [[ $file == *~ ]] || source $file
done

# prompt may use info from context-specific files
source ~/.zsh.prompt

local hostFile=~/.zsh_$HOST
[[ -f $hostFile ]] && source $hostFile

# reload config on USR1
trap 'zreload-do' USR1
