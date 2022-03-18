###################################
# Modules & modes
###################################

zmodload -i zsh/complist 
zmodload -i zsh/parameter
_comp_setup+=$'\ntypeset -a userdirs'
zmodload -i zsh/mathfunc 
zmodload -i zsh/pcre

# modes
autoload -U zed
autoload -U zmv
autoload -U edit-command-line 
autoload -U compinit && compinit
autoload -U colors && colors
autoload -U url-quote-magic && zle -N self-insert url-quote-magic
autoload -U select-word-style && select-word-style bash
autoload run-help
autoload run-help-git
autoload run-help-sudo
autoload run-help-s

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
autoload -Uz copy-earlier-word && {
  zle -N copy-earlier-word
  bindkey "^[m" copy-earlier-word
}

# edit command-line
autoload edit-command-line && {
  zle -N edit-command-line
  bindkey '\ee' edit-command-line
}

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

# syntax highlighting
local f="/usr/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh"
if [[ -f $f ]] ; then
  source /usr/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
  ZSH_HIGHLIGHT_HIGHLIGHTERS=(main brackets pattern)
  ZSH_HIGHLIGHT_STYLES[command]='fg=cyan,bold'
  ZSH_HIGHLIGHT_STYLES[arg0]='fg=cyan'
  ZSH_HIGHLIGHT_STYLES[alias]='fg=yellow,bold'
  ZSH_HIGHLIGHT_STYLES[function]='fg=yellow,bold'
  ZSH_HIGHLIGHT_STYLES[reserved-word]='bg=magenta,bold,reverse'
  ZSH_HIGHLIGHT_STYLES[builtin]='bg=blue,bold,reverse'
  ZSH_HIGHLIGHT_STYLES[path]='fg=cyan,underline'
  ZSH_HIGHLIGHT_STYLES[path_prefix]='fg=cyan,underline'
  ZSH_HIGHLIGHT_STYLES[globbing]='fg=cyan,bold'
  ZSH_HIGHLIGHT_STYLES[bracket-level-1]='fg=yellow,bold'
  ZSH_HIGHLIGHT_STYLES[bracket-level-1]='fg=yellow,bold'
  ZSH_HIGHLIGHT_STYLES[bracket-level-2]='fg=magenta,bold'
  ZSH_HIGHLIGHT_STYLES[bracket-level-3]='fg=green,bold'
  ZSH_HIGHLIGHT_STYLES[bracket-level-4]='fg=cyan,bold'
  ZSH_HIGHLIGHT_STYLES[cursor-matchingbracket]='fg=yellow,bold'
  ZSH_HIGHLIGHT_STYLES[bracket-error]='fg=red,bold'
fi

# fzf
# FZF_DEFAULT_COMMAND='fd --type f'
export FZF_DEFAULT_OPTS="--bind=ctrl-a:accept,ctrl-e:accept,enter:ignore,ctrl-j:ignore --no-mouse --bind='alt-v:page-up,ctrl-v:page-down' --multi --ansi --tabstop=4"
if [[ $(fzf --version | perl -pe 's/(\d+\.\d+).*/$1/') -gt 0.23 ]] ; then
  export FZF_DEFAULT_OPTS="$FZF_DEFAULT_OPTS --color 'fg+:italic,bg+:26:reverse,hl:178:underline,hl+:190:reverse:underline:bold'"
fi

# use fzf-provided widgets, but change their bindings
source /usr/share/doc/fzf/examples/key-bindings.zsh /usr/local/share/examples/fzf/shell/key-bindings.zsh 2> /dev/null
bindkey '^t' transpose-chars
bindkey '\ec' capitalize-word
bindkey '^x^i' fzf-file-widget
bindkey '^x^d' fzf-cd-widget

FZF_CTRL_T_COMMAND=$FZF_DEFAULT_COMMAND
FZF_CTRL_T_OPTS='--preview "batcat --style=numbers --color=always {} | head -500"'
# FZF_CTRL_R_OPTS to pass additional options
# FZF_ALT_C_COMMAND to override the default command
# FZF_ALT_C_OPTS to pass additional options

source /usr/share/doc/fzf/examples/completion.zsh /usr/local/share/examples/fzf/shell/completion.zsh 2> /dev/null
# FZF_COMPLETION_TRIGGER='**' # default
# FZF_COMPLETION_OPTS    (default: empty)

is_in_git_repo() {
  git rev-parse HEAD > /dev/null 2>&1
}

fzf-down() {
  fzf --height 50% "$@" --border
}

fzf_git_f() { # files
  git -c color.status=always status --short |
  fzf-down -m --ansi --nth 2..,.. \
    --preview '(git diff --color=always -- {-1} | sed 1,4d; cat {-1}) | head -500' |
  cut -c4- | sed 's/.* -> //'
}

fzf_git_b() { # branches
  is_in_git_repo || return
  git branch -a --color=always | grep -v '/HEAD\s' | sort |
  fzf-down --ansi --multi --tac --preview-window right:70% \
    --preview 'git log --oneline --graph --date=short --pretty="format:%C(auto)%cd %h%d %s" $(sed s/^..// <<< {} | cut -d" " -f1) -- | head -'$LINES |
  sed 's/^..//' | cut -d' ' -f1 |
  sed 's#^remotes/##'
}

fzf_git_t() { # tags
  git tag --sort -version:refname |
  fzf-down --multi --preview-window right:70% \
    --preview 'git show --color=always {} | head -'$LINES
}

fzf_git_h() { # hashes
  git log --date=short --format="%C(green)%C(bold)%cd %C(auto)%h%d %s (%an)" --graph --color=always |
  fzf-down --ansi --no-sort --reverse --multi --bind 'ctrl-s:toggle-sort' \
    --header 'Press CTRL-S to toggle sort' \
    --preview 'grep -o "[a-f0-9]\{7,\}" <<< {} | head -1 | xargs git show --color=always | head -'$LINES |
  grep -o "[a-f0-9]\{7,\}" | head -1
}

fzf_git_r() { # remotes
  git remote -v | awk '{print $1 "\t" $2}' | uniq |
  fzf-down --tac \
    --preview 'git log --color=always --oneline --graph --date=short --pretty="format:%C(auto)%cd %h%d %s" --remotes={1} | head -200' |
  cut -d$'\t' -f1
}

join-lines() {
  local item
  while read item; do
    echo -n "${(q)item} "
  done
}

bind-git-helper() {
  local c
  for c in $@; do
    eval "fzf-git-$c-widget() { is_in_git_repo || return ; local result=\$(fzf_git_$c | join-lines); zle reset-prompt; LBUFFER+=\$result }"
    eval "zle -N fzf-git-$c-widget"
    eval "bindkey '^x^$c' fzf-git-$c-widget"
  done
}
bind-git-helper f b t h r
unset -f bind-git-helper

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
compdef _connect-run co com connect connect-mosh run ssh
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
extensions[debian]="build buildinfo changes dsc ${literal}changelog ${literal}control ${literal}rules ${literal}compat ${literal}config ${literal}copyright ${literal}dirs ${literal}files ${literal}watch ${literal}postinst ${literal}postrm ${literal}preinst ${literal}prerm postinst preinst postrm prerm links ${literal}links ${literal}docs docs ${literal}templates templates"
extensions[docs]="calendar chm conf csv doc docx dvi emacs html ics odf ods odt org pdf pgn pps ppt pptx ps reg rtf sub srt tex txt todo vcf xls xlsx xml"
extensions[archives]="7z ace apk arj bin bundle bz2 cab cdr dat deb dmg firmware ear exe gz img ipk iso jar lzh lzma otf ova pgdump qcow qcow2 rar raw rootfs rpm squashfs tar taz tgz ttf udeb udf vmdk war xpi xz z zip"
extensions[video]="3gp asf avi divx flv ifo m1v m2v mkv mov mp2 mp4 mpe mpeg mpg ram rm webm wmv xvid yuv"
extensions[audio]="aac au mp3 ogg ogv wav wma"
extensions[pics]="bmp dng gif jpeg jpg pbm png ppm svg tga tif xbm xcf xpm"
extensions[code]="${literal}Makefile a bash c c++ class cpp debdiff diff dtb el elz h hs jacl java js json jy ko lua o out patch pl pm py pyc pyo rb sh so sql tcl yaml yml zsh"

# add the uppercase extensions too
for key in ${(k)extensions[@]} ; do
  extensions[$key]="$extensions[$key] ${(U)extensions[$key]}"
done

local -A colors
colors=()
colors[backup]="04;38;5;71"
colors[debian]="03;38;5;38"
colors[docs]="03;38;5;71"
colors[archives]="01;38;5;202"
colors[video]="01;33"
colors[audio]="00;36"
colors[pics]="00;32"
colors[code]="01;38;5;200"
LS_COLORS="ex=${colors[code]}:no=00:fi=00;37:di=01;38;50:ln=01;36:pi=40;33:so=01;35:do=01;35:bd=40;33;01:cd=40;33;01:or=04;38;5;154"
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
if [[ $TERM == tmux-256color ]] && [[ ! -f /usr/share/terminfo/t/tmux-256color ]] ; then
  export TERM="screen-256color"
fi
[[ $OSTYPE != "linux" ]] || infocmp $TERM > /dev/null 2>&1 || export TERM=${TERM/-256color}

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
  centurion|hulk|x230|seb-debian|x1|g8) # no fallback to unset'ing DISPLAY
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

# (maybe overloaded later)
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

# virtualenv
local vew="/usr/share/virtualenvwrapper/virtualenvwrapper.sh"
if [ -f $vew ] ; then
  export WORKON_HOME=~/.virtualenvs 
  source $vew
fi

export LIBVIRT_DEFAULT_URI=qemu:///system

# update prompt
[[ -n "$is_console" ]] || setTerminalTitle

# reload config on USR1
trap 'zreload-do' USR1
