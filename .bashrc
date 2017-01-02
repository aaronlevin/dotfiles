# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=2000

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.  shopt -s checkwinsize

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
#shopt -s globstar

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "${debian_chroot:-}" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color) color_prompt=yes;;
esac

# uncomment for a colored prompt, if the terminal has the capability; turned
# off by default to not distract the user: the focus in a terminal window
# should be on the output of commands, not on the prompt
#force_color_prompt=yes

if [ -n "$force_color_prompt" ]; then
    if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
	# We have color support; assume it's compliant with Ecma-48
	# (ISO/IEC-6429). (Lack of such support is extremely rare, and such
	# a case would tend to support setf rather than setaf.)
	color_prompt=yes
    else
	color_prompt=
    fi
fi

if [ "$color_prompt" = yes ]; then
    PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
else
    PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
fi
unset color_prompt force_color_prompt

# If this is an xterm set the title to user@host:dir
case "$TERM" in
xterm*|rxvt*)
    PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
    ;;
*)
    ;;
esac

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# some more ls aliases
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  fi
fi

# nix support
. "/home/aaronlevin/.nix-profile/etc/profile.d/nix.sh"

# VPN Alias
alias vpnrestart='sudo /etc/init.d/openvpn restart'
alias vpnstop='sudo /etc/init.d/openvpn stop'
alias vpnstop='sudo /etc/init.d/openvpn stop'
alias kubectl-k2='docker run --rm -ti docker.dev.s-cloud.net/kubectl-k2:latest kubectl'

# ps1 stuff
# Add current git branch to PS1
function __git_inside_repo {
  git rev-parse &>/dev/null
}

PS1_TOGGLE="CONCISE"

function __git_ps1 {
  if __git_inside_repo; then
    local branch=$(git rev-parse --abbrev-ref HEAD 2>/dev/null)
    if [ "$branch" == "HEAD" ]; then
      branch=$(git rev-parse --short HEAD 2>/dev/null)
    fi
    echo -n "($branch) "
  fi
}

II="concise"
alias ii='. $HOME/bin/ii-toggle'
ii

# PATH
HOME_PATH="$HOME/bin:$HOME/.soundcloud/cd-tools"
RBENV_PATH="$HOME/.rbenv/bin"
MY_PYTHON_PATH="$HOME/.local/bin"
NPM_PATH="$HOME/.npm-packages/bin"
export PATH="$RBENV_PATH:$HOME_PATH:$MY_PYTHON_PATH:$NPM_PATH:$PATH"

# Ruby support: https://github.com/rbenv/rbenv#how-rbenv-hooks-into-your-shell
eval "$(rbenv init -)"

# haskell support
export NIX_GHC_LIBDIR="$HOME/.nix-profile/lib/ghc-$(ghc --version | grep -oP 'version.*' | sed 's/version //g')"

# sbt support
export SBT_OPTS="-Xms1024m -Xms1024m -XX:ReservedCodeCacheSize=128m -XX:MaxMetaspaceSize=1024M"

alias sc-cqlsh="ssh cassandra -t 'cqlsh-2.1.13 10.33.19.57 -u cassandra -p cassandra'"
alias sc-bcqlsh="ssh bcassandra -t 'cqlsh-2.1.13 10.33.19.56 -u cassandra -p cassandra'"

source ~/.emojis
